#!/usr/bin/env bash
# Destroy-and-recreate Fly.io deploy for the RustViz playground.
#
# Every deploy starts from a clean fleet:
#
#   1. Destroy every existing Machine (parallel).
#   2. Bring up a SINGLE source Machine via `fly scale count 1` +
#      `fly deploy --strategy immediate`, and wait for it to pass its
#      HTTP check. This Machine pays the one and only fuse-overlayfs
#      cold pull of the runner image (~5-15 min) for the whole deploy.
#   3. Clone the source Machine $RV_FLY_MACHINES-1 times in parallel
#      via `fly machine clone --detach`. Each clone inherits a copy of
#      the source's rootfs — including dockerd's cached runner image —
#      so they boot in ~30 s without their own cold pull.
#   4. Wait for every clone to pass its HTTP check.
#   5. Run `fly machine update --autostop=stop` against every Machine
#      to flip the per-Machine service config from the bootstrap-friendly
#      'off' (set in fly.toml) to the cost-saving 'stop' for steady
#      state. Don't poll for health afterwards — the autoscaler
#      immediately starts auto-stopping idle Machines under the new
#      setting, and Consul health probes don't count as traffic, so a
#      poll-for-all-healthy loop is unwinnable.
#
# Why destroy-and-recreate instead of in-place updates: every previous
# attempt to incrementally update an existing fleet hit a different
# stale-state edge case — Machines stopped pre-deploy that fly deploy
# updates but doesn't auto-start, fly machine update timing out vs the
# autoscaler that just took effect, drift between Machines created
# under different fly.toml settings, etc. Nuking the fleet sidesteps
# all of it for the cost of a few minutes of downtime per deploy
# (acceptable for a research tool with sparse traffic).
#
# Why source-and-clone instead of N parallel cold pulls: one Machine
# pulling once + N-1 fast clones is dramatically cheaper than N
# Machines pulling in parallel (which is bandwidth-capped by GHCR and
# per-Machine fuse-overlayfs throughput, plus susceptible to one
# Machine of N getting stuck mid-pull and timing out the whole deploy).
#
# In steady state between deploys, the auto-stop / auto-start cycle
# still benefits from `persist_rootfs = 'always'` on the [[vm]] block
# in fly.toml: an auto-stopped Machine that gets traffic comes back
# in ~10 s without re-pulling the runner image. Cost in steady state:
# ~$2-3 / mo for the IP and Machine baseline.
#
# Pass --keep-warm to skip step 4 (Machines never auto-stop; ~$24/mo
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
# `auto_stop_machines = 'off'` so freshly-created Machines aren't
# killed by the autoscaler mid-cold-pull). Phase 2 uses
# `fly machine update` directly against the Machines API to flip
# auto_stop on each running Machine, doesn't touch fly.toml.

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
# Skip the `auth whoami` preflight when FLY_API_TOKEN is set. flyctl
# honors that env var for deploy/status/machine-update calls, but
# `auth whoami` itself reads ~/.fly/config.yml (populated by
# `fly auth login`), so it returns "not logged in" in CI even though
# the rest of flyctl is perfectly authenticated. Locally without a
# token, the check still helps with a friendly error.
if [ -n "${FLY_API_TOKEN:-}" ]; then
    # Strip CR/LF from the token. `gh secret set` via stdin or a file
    # commonly picks up a trailing newline; flyctl then sends it as
    # part of the `Authorization: <token>` header and Go's net/http
    # rejects it with "invalid header field value for Authorization".
    #
    # Importantly we strip ONLY \r and \n, not all whitespace: deploy
    # tokens from `fly tokens create deploy` come back as
    # `FlyV1 fm2_lJPE…` — the space between the version prefix and
    # the token body is part of the token format Fly expects in the
    # Authorization header. Stripping it (e.g. with tr -d '[:space:]')
    # mangles the token into `FlyV1fm2_…`, which the API accepts as
    # syntactically valid bytes but rejects as a "token validation
    # error".
    FLY_API_TOKEN=$(printf '%s' "$FLY_API_TOKEN" | tr -d '\r\n')
    export FLY_API_TOKEN
else
    "$FLY" auth whoami >/dev/null 2>&1 || { echo "Not logged in. Run '$FLY auth login' first or set FLY_API_TOKEN." >&2; exit 1; }
fi
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

# Destroy every existing Machine before scaling + deploying. We accept
# a few minutes of downtime per deploy in exchange for never having
# to debug stale-state edge cases:
#
#   - Stopped Machines that fly deploy applies a new release to but
#     doesn't auto-start (script then hangs waiting for them to pass
#     health checks they can't satisfy until traffic forces a start)
#   - Machines created under an older fly.toml that still have its
#     persist_rootfs / auto_stop / VM-size config baked in
#   - Half-rolled fleets from a previous deploy that crashed mid-way
#
# Every deploy starts from zero: `fly machines list` empty → `fly
# scale count $DESIRED_COUNT` creates $DESIRED_COUNT fresh Machines
# from the new fly.toml → fly deploy rolls them to the new release.
# Tradeoffs: this forces a fuse-overlayfs cold pull on every Machine
# every deploy (~5–15 min/Machine, all in parallel), and there's a
# brief window where the public URL serves 503s because every Machine
# is mid-bootstrap. Fine for a research tool that deploys infrequently
# and tolerates a few minutes of downtime.
mapfile -t EXISTING_IDS < <("$FLY" machines list --app "$APP_NAME" --json | jq -r '.[].id' | sort -u)
if [ "${#EXISTING_IDS[@]}" -gt 0 ]; then
    say "==> Destroying ${#EXISTING_IDS[@]} existing Machine(s) for a clean redeploy"
    printf '%s\n' "${EXISTING_IDS[@]}" \
        | xargs -P 10 -I {} "$FLY" machine destroy {} --app "$APP_NAME" --force >/dev/null 2>&1
fi

# Build out the fleet in two stages: bring up ONE fresh Machine, let
# it pay the fuse-overlayfs cold pull (~5-15 min), then `fly machine
# clone` it $DESIRED_COUNT-1 times. Each clone inherits a copy of the
# source's rootfs — including dockerd's already-extracted runner image
# at /var/lib/docker — so clones boot in ~30 s instead of running
# their own cold pull. Total wallclock: ~10-15 min for the source,
# plus ~1-2 min to clone the rest in parallel, vs the previous
# ~10-15 min × 10 Machines (parallelized but still capped by GHCR
# bandwidth and per-Machine fuse-overlayfs throughput).
DESIRED_COUNT="${RV_FLY_MACHINES:-10}"
TIMEOUT_SECS="${RV_DEPLOY_TIMEOUT_SECS:-1800}"

say "==> Scaling to 1 source Machine + fly deploy (this Machine pays the cold pull)"
"$FLY" scale count 1 --yes
# --strategy immediate so fly deploy doesn't try to roll a non-existent
# fleet one-at-a-time; with 1 Machine in play, "immediate" just means
# "apply the new release to that Machine right away".
"$FLY" deploy --strategy immediate

if ! wait_for_fleet_healthy "$TIMEOUT_SECS"; then
    cat >&2 <<EOF
ERROR: source Machine did not pass health checks within $((TIMEOUT_SECS / 60)) minutes.
The fleet is currently running with auto_stop_machines = 'off' (bootstrap
config), so it won't auto-stop while you investigate. Useful next steps:

  fly logs                        # see what entrypoint is doing
  fly status --all                # check Machine state + per-check status
  fly ssh console                 # poke around inside

Re-run this script when you've fixed the underlying issue.
EOF
    exit 1
fi

# Source Machine is healthy and has the runner image cached on its
# rootfs. Clone it $DESIRED_COUNT-1 times so the rest of the fleet
# inherits the cache.
if [ "$DESIRED_COUNT" -gt 1 ]; then
    SOURCE_ID=$("$FLY" machines list --app "$APP_NAME" --json | jq -r '.[].id' | head -n 1)
    if [ -z "$SOURCE_ID" ]; then
        echo "ERROR: couldn't find a source Machine to clone from" >&2
        exit 1
    fi
    CLONE_COUNT=$(( DESIRED_COUNT - 1 ))
    say "==> Cloning Machine ${SOURCE_ID} ${CLONE_COUNT} time(s) — clones inherit cached runner image"

    # --detach so each fly machine clone returns once the API has
    # accepted the create request, instead of blocking on the new
    # Machine's health check. We poll the fleet ourselves below
    # (same pattern as the source-Machine wait above).
    CLONE_LOG=$(mktemp -t fly-machine-clone.XXXXXX) || exit 1
    seq "$CLONE_COUNT" \
        | xargs -P 10 -I {} "$FLY" machine clone "$SOURCE_ID" \
            --app "$APP_NAME" \
            --detach >"$CLONE_LOG" 2>&1 &
    CLONE_PID=$!

    while kill -0 "$CLONE_PID" 2>/dev/null; do
        status "    Cloning ${CLONE_COUNT} Machine(s)…"
        sleep 1
    done
    if ! wait "$CLONE_PID"; then
        printf '\n' >&2
        echo "ERROR: at least one fly machine clone failed. Last 60 lines of combined output:" >&2
        tail -n 60 "$CLONE_LOG" >&2
        rm -f "$CLONE_LOG"
        exit 1
    fi
    rm -f "$CLONE_LOG"
    say "    ${CLONE_COUNT}/${CLONE_COUNT} Machine clones created"

    if ! wait_for_fleet_healthy "$TIMEOUT_SECS"; then
        cat >&2 <<EOF
ERROR: clones did not pass health checks within $((TIMEOUT_SECS / 60)) minutes.
The fleet is at auto_stop_machines = 'off' so nothing will auto-stop while
you investigate. fly logs / fly status --all / fly ssh console as usual.
EOF
        exit 1
    fi
fi

# --- phase 2 ------------------------------------------------------------
if [ "$KEEP_WARM" -eq 1 ]; then
    say "==> --keep-warm passed; leaving auto_stop = off (~\$24/mo per Machine)."
    exit 0
fi

say "==> Phase 2: flipping each Machine's auto_stop from 'off' to 'stop' (no redeploy)"
# Phase 2 changes a single per-Machine knob (auto_stop=off → stop) via
# the Machines API. `fly machine update` bounces the Machine to apply
# the new config; persist_rootfs='always' on the [[vm]] block in
# fly.toml keeps /var/lib/docker across the bounce, so dockerd comes
# back to a cached runner image with no re-pull. fly's default is
# 'never', under which this update would wipe rootfs and trigger a
# 5-15 min cold pull while auto_stop='stop' is already in force —
# the autoscaler would then kill the Machine mid-bootstrap. That's
# the deadlock persist_rootfs protects us from.
#
# We pass --skip-health-checks so each fly machine update returns as
# soon as the Machines API accepts the config change. We then DON'T
# poll for fleet health afterwards — the moment auto_stop='stop' is
# applied, the autoscaler starts watching each Machine for traffic,
# and health-check probes don't count as traffic (they take a
# separate network path). After ~40s of "no incoming traffic" the
# autoscaler stops a freshly-bounced Machine. So a poll-for-all-
# healthy loop is unwinnable here: Machines auto-stop faster than
# they become healthy. The fly machine update return code IS the
# success signal — once it's nonzero-free, the config is applied and
# we're in the desired steady state (Machines stopped or running per
# real traffic, not per our deploy harness).
#
# Stdout + stderr of the parallel xargs go to a tmpfile rather than
# /dev/null so a real failure produces a useful error message
# (previously: "exit code 123" with no idea which Machine or why).
mapfile -t IDS < <("$FLY" machines list --app "$APP_NAME" --json | jq -r '.[].id' | sort -u)
say "    Updating ${#IDS[@]} Machines (auto_stop → stop)"

UPDATE_LOG=$(mktemp -t fly-machine-update.XXXXXX) || exit 1
trap 'rm -f "$UPDATE_LOG"' EXIT

printf '%s\n' "${IDS[@]}" \
    | xargs -P 10 -I {} "$FLY" machine update {} \
        --app "$APP_NAME" \
        --autostop=stop \
        --autostart=true \
        --skip-health-checks \
        --yes >"$UPDATE_LOG" 2>&1 &
UPDATE_PID=$!

while kill -0 "$UPDATE_PID" 2>/dev/null; do
    status "    Updating ${#IDS[@]} Machines (auto_stop → stop)…"
    sleep 1
done
if ! wait "$UPDATE_PID"; then
    printf '\n' >&2
    echo "ERROR: fly machine update failed for at least one Machine. Last 60 lines of combined output:" >&2
    tail -n 60 "$UPDATE_LOG" >&2
    exit 1
fi
say "    ${#IDS[@]}/${#IDS[@]} Machine config updates accepted"

# Quick sanity check: verify every Machine's services-config really
# does report auto_stop='stop' now. If fly machine update silently
# succeeded but didn't actually apply the change for some Machine,
# we want to know — better than discovering it weeks later when an
# always-running Machine starts costing $24/mo.
say "    Verifying per-Machine auto_stop config"
NON_STOP=$("$FLY" machines list --app "$APP_NAME" --json \
    | jq -r '[.[] | .config.services[]? | .auto_stop_machines]
             | map(select(. != "stop")) | length')
if [ "$NON_STOP" != "0" ]; then
    echo "ERROR: ${NON_STOP} Machine service entries do not report auto_stop='stop' after update" >&2
    "$FLY" machines list --app "$APP_NAME" --json \
        | jq '[.[] | {id, services: [.config.services[]? | .auto_stop_machines]}]' >&2
    exit 1
fi

say "==> Done. Machines match fly.toml's canonical config; they'll auto-stop when idle."
say "    Cold starts after auto-stop take ~10 s (runner image stays on each Machine's rootfs)."
