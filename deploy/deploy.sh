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
#   RV_FLY_MACHINES   fleet size; default 10
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

# Phase 2 (and any error path) restores fly.toml to the canonical
# steady-state config so the working tree stays clean if the script
# aborts mid-flight. Set RESTORE='' to skip the restore (e.g. when
# we want to leave the bootstrap config in place after a failure).
RESTORE=1
trap '[ "${RESTORE:-1}" = "1" ] && set_autoscale 0 stop || true' EXIT

# --- phase 1 ------------------------------------------------------------
echo "==> Phase 1: deploying with auto_stop_machines = off and min_machines_running = 1"
set_autoscale 1 off
fly deploy

echo "==> Waiting for ${URL} to respond (up to 15 min for fresh-volume bootstraps)..."
deadline=$(( $(date +%s) + 900 ))
while ! curl --max-time 10 -fsS "$URL" -o /dev/null 2>/dev/null; do
    if [ "$(date +%s)" -gt "$deadline" ]; then
        cat >&2 <<EOF
ERROR: ${URL} did not respond within 15 minutes.
fly.toml has been left at auto_stop_machines = 'off' / min_machines_running = 1
so the Machine won't auto-stop while you investigate. Useful next steps:

  fly logs                        # see what entrypoint is doing
  fly status --all                # check Machine state
  fly ssh console                 # poke around inside

Re-run this script when you've fixed the underlying issue.
EOF
        # don't restore in the EXIT trap — leave at the bootstrap config
        RESTORE=
        exit 1
    fi
    sleep 10
done
echo "==> ${URL} is live"

# --- ensure fleet size --------------------------------------------------
# Machine count is server-side state on Fly, not in fly.toml. We set it on
# every deploy so the fleet stays at the desired size if it ever drifts
# (e.g. someone ran `fly scale count 1` ad-hoc to debug). With auto_stop on,
# the extra Machines stay stopped when idle and cost essentially nothing;
# they exist purely so the edge proxy has somewhere to spill load when one
# Machine gets saturated. Default 10; override with RV_FLY_MACHINES.
DESIRED_COUNT="${RV_FLY_MACHINES:-10}"
echo "==> Ensuring fleet size of ${DESIRED_COUNT} Machines (idle ones auto-stop)"
fly scale count "$DESIRED_COUNT" --yes

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
fly deploy

echo "==> Done. Cold starts after idle take ~10 s (runner image cached on volume)."
