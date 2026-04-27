#!/usr/bin/env bash
# Two-phase Fly.io deploy for the RustViz playground.
#
# Why two phases:
# Fly's autoscaler stops a Machine after ~40 s of "no incoming traffic"
# even if the Machine is in the middle of doing useful work. Our entrypoint
# pulls the rustviz/rustviz-runner image from GHCR on first boot of each
# fresh Machine (~30-60 s); even that short window can collide with the
# autoscaler's patience. Empirically, `min_machines_running = 1` does NOT
# protect the Machine during the warmup phase of a fresh deploy (the
# autoscaler sends SIGINT before the Machine is registered in the
# min-pool). The reliable fix is to disable auto_stop entirely during
# bootstrap, then re-enable it for steady state.
#
#   1. Set auto_stop_machines = 'off' and min_machines_running = 1 in
#      fly.toml, then deploy.
#   2. Poll the public URL until it responds (up to 5 min on a fresh
#      Machine, ~10 s on a routine redeploy).
#   3. Set auto_stop_machines = 'stop' and min_machines_running = 0, then
#      deploy again. Auto-stop is back on for cheap steady-state idling.
#
# After step 3, the runner image is cached on each Machine's local
# filesystem, so future cold starts after auto-stop take ~10 s. Cost in
# steady state: ~$2-3 / mo for the IP and Machine baseline.
#
# Pass --keep-warm to skip step 3 (Machine never auto-stops; ~$24 / mo
# *per always-running Machine* — the fleet still has count = $RV_FLY_MACHINES
# total, but only $RV_FLY_MACHINES of them stay running).
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
#   * fly.toml is committed-clean — this script edits it in place and
#     would clobber unrelated uncommitted edits.

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
command -v fly >/dev/null 2>&1 || { echo "fly CLI not on PATH; install it with 'brew install flyctl'" >&2; exit 1; }
fly auth whoami >/dev/null 2>&1 || { echo "Not logged in. Run 'fly auth login' first." >&2; exit 1; }
command -v jq >/dev/null 2>&1 || { echo "jq is required for the health-check polling loop. brew install jq" >&2; exit 1; }

if ! git diff --quiet fly.toml 2>/dev/null; then
    echo "fly.toml has uncommitted changes; commit or stash them before deploying." >&2
    git diff fly.toml >&2
    exit 1
fi

APP_NAME=$(awk -F"'" '/^app =/ {print $2; exit}' fly.toml)
URL="https://${APP_NAME}.fly.dev/"

# --- helpers ------------------------------------------------------------
# Args: <min_machines_running> <auto_stop_machines>
set_autoscale() {
    local min="$1"
    local stop="$2"
    perl -i -pe "s|^(\s*min_machines_running = )\d+|\${1}$min|" fly.toml
    perl -i -pe "s|^(\s*auto_stop_machines = )'[^']*'|\${1}'$stop'|" fly.toml
}

# Polls `fly status --json` until every Machine in the app process group
# is reporting all health checks passing. Returns 0 on success, 1 on
# timeout. Caller is expected to set RESTORE='' before calling on error
# paths if it wants to preserve the bootstrap fly.toml config.
#
# Why we do this in the script rather than relying on fly deploy's own
# health-check wait: our http_service.checks block has a 30-minute
# grace_period (the runner image's first cold pull through fuse-overlayfs
# legitimately takes that long), and fly deploy's --wait-timeout default
# is only ~5 min. With --strategy immediate the deploy returns as soon as
# Machines are recreated, regardless of check status, which lets us own
# the timing window with a deadline of our own.
#
# Args: <timeout_secs>
wait_for_fleet_healthy() {
    local timeout_secs="$1"
    local deadline=$(( $(date +%s) + timeout_secs ))
    echo "==> Waiting for every Machine to pass its HTTP health check..."
    echo "    (up to $((timeout_secs / 60)) min)"
    while true; do
        # `fly status --json` returns an object with a Machines array; each
        # Machine has a Checks array. We want every Machine in the app
        # process group to have all its checks passing.
        local json summary passing total
        json=$(fly status --json 2>/dev/null) || json='{}'
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
            echo "==> All ${total} Machines passing health check"
            return 0
        fi

        if [ "$(date +%s)" -gt "$deadline" ]; then
            return 1
        fi

        echo "    ${summary} Machines healthy; waiting…"
        sleep 15
    done
}

# Phase 2 (and any error path) restores fly.toml to the canonical
# steady-state config so the working tree stays clean if the script
# aborts mid-flight. Set RESTORE='' to skip the restore (e.g. when
# we want to leave the bootstrap config in place after a failure).
RESTORE=1
trap '[ "${RESTORE:-1}" = "1" ] && set_autoscale 0 stop || true' EXIT

# --- phase 1 ------------------------------------------------------------
echo "==> Phase 1: deploying with auto_stop_machines = off and min_machines_running = 1"
set_autoscale 1 off

# Scale BEFORE deploy so all RV_FLY_MACHINES Machines exist when fly deploy
# rolls them. If we deployed first and scaled afterward, fly deploy would
# only roll the existing fleet (often just 2, Fly's HA default for a fresh
# app), then scale-up would create the rest from the just-deployed image
# and each of those would do its own ~30 min cold-pull *sequentially* with
# the rolling deploy. Scaling first lets --strategy immediate roll all of
# them in parallel, single wallclock-window for everyone.
DESIRED_COUNT="${RV_FLY_MACHINES:-10}"
echo "==> Ensuring fleet size of ${DESIRED_COUNT} Machines before the deploy roll"
fly scale count "$DESIRED_COUNT" --yes

# --strategy immediate: replace all Machines in parallel rather than rolling
# them one-at-a-time. Each new Machine has to pull the ~1 GiB runner image
# from GHCR and extract it via fuse-overlayfs (~15-30 min/Machine on first
# boot), so rolling 10 Machines sequentially would be ~2.5 hours of
# wallclock; in parallel it's ~30 min. Tradeoff is a brief few-minute
# window during the swap where requests can 503 because the new fleet is
# still bootstrapping. Fine for a research tool with sparse traffic;
# obviously bad for a high-availability service.
fly deploy --strategy immediate

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
fly.toml has been left at auto_stop_machines = 'off' / min_machines_running = 1
so Machines won't auto-stop while you investigate. Useful next steps:

  fly logs                        # see what entrypoint is doing
  fly status --all                # check Machine state + per-check status
  fly ssh console                 # poke around inside

Re-run this script when you've fixed the underlying issue.
EOF
    # don't restore in the EXIT trap — leave at the bootstrap config
    RESTORE=
    exit 1
fi

# --- phase 2 ------------------------------------------------------------
if [ "$KEEP_WARM" -eq 1 ]; then
    echo "==> --keep-warm passed; leaving auto_stop = off, min_machines_running = 1."
    echo "    Machine never auto-stops; ~\$24/mo at shared-cpu-2x."
    RESTORE=
    exit 0
fi

echo "==> Phase 2: re-enabling auto_stop with min_machines_running = 0 so the Machine idles cheap"
set_autoscale 0 stop
RESTORE=
# Phase 2 uses default rolling strategy (NOT --strategy immediate). The
# image hash is unchanged from phase 1, so rolling does an in-place
# restart of each Machine — same VM, same rootfs, dockerd's image cache
# preserved, no re-pull. fly deploy waits for each Machine's health
# check to pass before moving to the next, which is exactly what we
# want. We bump --wait-timeout from its 120s default because our boot
# sequence (dockerd init ~60s + grace_period 60s + a couple of check
# ticks) routinely exceeds 2 min; 600s is generous enough that a slow
# Machine never trips the per-Machine deadline.
fly deploy --wait-timeout 600

echo "==> Done. Cold starts after idle take ~10 s (runner image already on each Machine's rootfs)."
