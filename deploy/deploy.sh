#!/usr/bin/env bash
# Two-phase Fly.io deploy for the RustViz playground.
#
# Why two phases:
# Fly's autoscaler stops a Machine after ~40 s of "no incoming traffic"
# even if the Machine is in the middle of doing useful work. Our entrypoint
# pulls the rustviz/rustviz-runner image from GHCR on first boot of each
# fresh Machine (5-15 min via fuse-overlayfs); that easily collides with
# the autoscaler's patience. fly.toml is therefore committed with
# `auto_stop_machines = 'off'`, which is the only value that makes a
# fresh `fly deploy` actually finish — Machines stay alive through the
# cold-pull window. After they're healthy, the script flips each
# Machine's per-Machine service config to 'stop' via the Machines API
# so the running fleet idles cheaply. Steps:
#
#   1. fly scale count + fly deploy --strategy immediate (parallel).
#   2. Poll `fly status --json` until every Machine's health check passes.
#   3. Run `fly machine update --autostop=stop` against every Machine.
#      No second `fly deploy`, no Firecracker-VM recreation, no second
#      cold-pull window. Takes seconds, not minutes.
#
# After step 3, the runner image is cached on each Machine's local
# filesystem, so future cold starts after auto-stop take ~10 s. Cost in
# steady state: ~$2-3 / mo for the IP and Machine baseline.
#
# Pass --keep-warm to skip step 3 (Machines never auto-stop; ~$24/mo
# *per always-running Machine*).
#
# The script also ensures the fleet is at $RV_FLY_MACHINES (default 10)
# Machines on every deploy. Idle Machines auto-stop, so the extra capacity
# costs nothing in steady state — it's there so the edge proxy can spill
# concurrent load to additional Machines when one gets saturated, e.g. when
# someone posts the URL on Hacker News. With hard_limit = 5 per Machine in
# fly.toml::http_service.concurrency, ten Machines = ~50 concurrent compile
# capacity at peak, which covers a meaningful HN-frontpage spike.
#
# Usage:
#   deploy/deploy.sh             # cheap mode, two-phase
#   deploy/deploy.sh --keep-warm # always-warm mode, skips phase 2
#
# Env:
#   RV_FLY_MACHINES         fleet size; default 10
#   RV_DEPLOY_TIMEOUT_SECS   how long to wait for the URL to come back
#                            up after a deploy; default 1800 (30 min)
#
# Prerequisites:
#   * `fly` (flyctl) installed and authenticated (`fly auth login`).
#   * The Fly app already exists (`fly launch --copy-config --no-deploy`
#     done once).
#   * The runner image has been published to ghcr.io/rustviz/rustviz-runner
#     and the package is public. (Run `gh workflow run runner-image.yml`
#     once, then flip the package to public on GitHub.)
#
# fly.toml is never mutated by this script. Phase 1 just runs
# `fly deploy` against the committed fly.toml (which has
# `auto_stop_machines = 'off'`); phase 2 uses `fly machine update`
# directly against the Machines API and doesn't read fly.toml at all.

set -euo pipefail

cd "$(dirname "$0")/.."

# --- arg parsing --------------------------------------------------------
KEEP_WARM=0
for arg in "$@"; do
    case "$arg" in
        --keep-warm) KEEP_WARM=1 ;;
        -h|--help)
            sed -n '2,30p' "$0"
            exit 0
            ;;
        *) echo "Unknown argument: $arg" >&2; exit 2 ;;
    esac
done

# --- preflight ----------------------------------------------------------
# Resolve the fly CLI: locally `brew install flyctl` provides both `fly`
# and `flyctl`, but superfly/flyctl-actions/setup-flyctl in CI installs
# only `flyctl` (no `fly` symlink). Pick whichever is on PATH and use it
# via $FLY everywhere below; xargs and other subprocesses see the
# already-expanded binary name, not a shell alias.
if command -v fly >/dev/null 2>&1; then
    FLY=fly
elif command -v flyctl >/dev/null 2>&1; then
    FLY=flyctl
else
    echo "fly CLI not on PATH; install it with 'brew install flyctl'" >&2
    exit 1
fi
"$FLY" auth whoami >/dev/null 2>&1 || { echo "Not logged in. Run '$FLY auth login' first." >&2; exit 1; }
command -v jq >/dev/null 2>&1 || { echo "jq is required for the health-check polling loop. brew install jq" >&2; exit 1; }

APP_NAME=$(awk -F"'" '/^app =/ {print $2; exit}' fly.toml)
URL="https://${APP_NAME}.fly.dev/"

# --- progress helpers ---------------------------------------------------
# All script-emitted progress lines go through these so they share a
# consistent format ("[MM:SS] message") and a consistent in-place update
# behavior. Output goes to stderr to keep stdout clean for any future
# piping; in an interactive terminal it's interleaved naturally with
# fly's own stdout output.
START_TS=$(date +%s)
elapsed() {
    local now diff
    now=$(date +%s)
    diff=$(( now - START_TS ))
    printf '%02d:%02d' $(( diff / 60 )) $(( diff % 60 ))
}
# In-place update: \r returns to start of line, \033[K clears to EOL,
# so consecutive `status` calls overwrite each other on the same row.
# Use while a step is in flight and you want a ticking display.
status() { printf '\r\033[K[%s] %s' "$(elapsed)" "$*" >&2; }
# Same as status() but commits the line with a trailing newline. Use
# for one-shot announcements ("==> Phase 2: …") and for the final line
# of an in-flight phase, so the next bit of output lands on a fresh row.
say()    { status "$*"; printf '\n' >&2; }

# Polls `fly status --json` until every Machine in the app process group
# is reporting all health checks passing. Returns 0 on success, 1 on
# timeout.
#
# Why we do this in the script rather than relying on fly deploy's own
# health-check wait: with --strategy immediate the deploy returns as soon
# as Machines are recreated, regardless of check status. fly deploy's own
# --wait-timeout (default 2 min) is also too short for our cold-pull
# bootstrap (5-15 min/Machine through fuse-overlayfs). Owning the wait
# in the script lets us pick a deadline that matches reality.
#
# Args: <timeout_secs>
wait_for_fleet_healthy() {
    local timeout_secs="$1"
    local deadline=$(( $(date +%s) + timeout_secs ))
    say "==> Waiting for every Machine to pass its HTTP health check (up to $((timeout_secs / 60)) min)"
    while true; do
        # `fly status --json` returns an object with a Machines array; each
        # Machine has a Checks array. We want every Machine in the app
        # process group to have all its checks passing.
        local json summary passing total
        json=$("$FLY" status --json 2>/dev/null) || json='{}'
        # `unique_by(.id)` is load-bearing: Fly's API occasionally returns
        # the same Machine twice in `fly status --json`; without dedupe the
        # count ends up like "0/11" for a 10-Machine fleet and the loop
        # never sees passing == total.
        summary=$(printf '%s' "$json" | jq -r '
            .Machines // []
            | unique_by(.id)
            | map(select(.config.metadata.fly_process_group == "app" or
                         (.config.metadata.fly_process_group // "app") == "app"))
            | "\(map(select(.checks // [] | all(.status == "passing"))) | length)/\(length)"
        ' 2>/dev/null) || summary='?/?'

        passing=${summary%/*}
        total=${summary#*/}

        if [ "$total" != "0" ] && [ "$total" != "?" ] && [ "$passing" = "$total" ]; then
            say "    ${total}/${total} Machines healthy"
            return 0
        fi

        if [ "$(date +%s)" -gt "$deadline" ]; then
            printf '\n' >&2
            return 1
        fi

        status "    ${summary} Machines healthy; waiting…"
        sleep 15
    done
}

# --- phase 1 ------------------------------------------------------------
say "==> Phase 1: fly deploy (auto_stop_machines = 'off', Machines won't be killed mid-bootstrap)"

# Scale BEFORE deploy so all RV_FLY_MACHINES Machines exist when fly deploy
# rolls them. If we deployed first and scaled afterward, fly deploy would
# only roll the existing fleet (often just 2, Fly's HA default for a fresh
# app), then scale-up would create the rest from the just-deployed image
# and each of those would do its own ~30 min cold-pull *sequentially* with
# the rolling deploy. Scaling first lets --strategy immediate roll all of
# them in parallel, single wallclock-window for everyone.
DESIRED_COUNT="${RV_FLY_MACHINES:-10}"
say "==> Ensuring fleet size of ${DESIRED_COUNT} Machines before the deploy roll"
"$FLY" scale count "$DESIRED_COUNT" --yes

# --strategy immediate: replace all Machines in parallel rather than rolling
# them one-at-a-time. Each new Machine has to pull the ~1 GiB runner image
# from GHCR and extract it via fuse-overlayfs (~15-30 min/Machine on first
# boot), so rolling 10 Machines sequentially would be ~2.5 hours of
# wallclock; in parallel it's ~30 min. Tradeoff is a brief few-minute
# window during the swap where requests can 503 because the new fleet is
# still bootstrapping. Fine for a research tool with sparse traffic;
# obviously bad for a high-availability service.
"$FLY" deploy --strategy immediate

# 30-minute timeout. fuse-overlayfs's per-layer extraction is significantly
# slower than kernel overlay2 — a fresh-Machine pull of the ~1 GiB runner
# image extracts each layer through userspace FUSE, and the Rust toolchain
# layers (hundreds of MiB each) extract one at a time. We've measured ~20+
# min on cold pulls. Routine redeploys (image already on the rootfs)
# complete in a couple minutes. Override with RV_DEPLOY_TIMEOUT_SECS.
TIMEOUT_SECS="${RV_DEPLOY_TIMEOUT_SECS:-1800}"

if ! wait_for_fleet_healthy "$TIMEOUT_SECS"; then
    cat >&2 <<EOF
ERROR: not all Machines passed health checks within $((TIMEOUT_SECS / 60)) minutes.
The fleet is currently running with auto_stop_machines = 'off' (bootstrap
config), so Machines won't auto-stop while you investigate. Useful next
steps:

  fly logs                        # see what entrypoint is doing
  fly status --all                # check Machine state + per-check status
  fly ssh console                 # poke around inside

Re-run this script when you've fixed the underlying issue.
EOF
    exit 1
fi

# --- phase 2 ------------------------------------------------------------
if [ "$KEEP_WARM" -eq 1 ]; then
    say "==> --keep-warm passed; leaving auto_stop = off (~\$24/mo per Machine)."
    exit 0
fi

say "==> Phase 2: flipping each Machine's auto_stop from 'off' to 'stop' (no redeploy)"
# `fly machine update` does bounce the Machine (it stops, applies the
# new config, and starts again). What makes it cheap here is that
# fly.toml has `persist_rootfs = 'always'` set on the [[vm]] block, so
# the bounce preserves /var/lib/docker — dockerd comes back up to find
# the runner image already cached on rootfs, no re-pull, ~30 s end to
# end. Without persist_rootfs='always' (Fly's default is 'never'), this
# update would wipe rootfs and trigger a 5-15 min cold pull on every
# Machine, with the just-applied auto_stop='stop' setting active during
# the no-traffic re-pull window — i.e. the autoscaler would kill the
# Machines before they came back. That's the deadlock persist_rootfs is
# protecting us from.
#
# We background the xargs so we can show our own ticking status line
# instead of fly machine update's ~10 lines/sec "Waiting for X to
# become healthy (started, 0/1)" noise. fly's stdout/stderr go to
# /dev/null; xargs blocks until every fly machine update returns
# (which itself only returns once the Machine is back to passing
# checks), so when the wait is done the fleet is fully updated.
mapfile -t IDS < <("$FLY" machines list --app "$APP_NAME" --json | jq -r '.[].id' | sort -u)
say "    Updating ${#IDS[@]} Machines (auto_stop → stop)"

printf '%s\n' "${IDS[@]}" \
    | xargs -P 10 -I {} "$FLY" machine update {} \
        --app "$APP_NAME" \
        --autostop=stop \
        --autostart=true \
        --yes >/dev/null 2>&1 &
UPDATE_PID=$!

while kill -0 "$UPDATE_PID" 2>/dev/null; do
    status "    Updating ${#IDS[@]} Machines (auto_stop → stop)…"
    sleep 1
done
wait "$UPDATE_PID"
say "    ${#IDS[@]}/${#IDS[@]} Machines updated"

say "==> Done. Machines match fly.toml's canonical config; they'll auto-stop when idle."
say "    Cold starts after idle take ~10 s (runner image stays on each Machine's rootfs)."
