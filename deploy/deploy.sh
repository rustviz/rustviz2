#!/usr/bin/env bash
# Two-phase Fly.io deploy for the RustViz playground.
#
# Why two phases:
# Fly's autoscaler stops a Machine after ~40 s of "no incoming traffic"
# when min_machines_running = 0, even if the Machine is in the middle of
# doing useful work. Our entrypoint pre-builds the rustviz/rustviz-runner
# image on first boot, which takes 5-10 min on a fresh volume — well past
# the autoscaler's patience. So we:
#
#   1. Set min_machines_running = 1 in fly.toml, deploy. The Machine
#      stays alive long enough for the inner build to finish.
#   2. Poll the public URL until it responds (up to 15 min).
#   3. Set min_machines_running = 0, deploy again. Auto-stop is back on.
#
# After step 3, the runner image is cached on the rustviz_docker volume,
# so future cold starts take ~10 s and the Machine can safely idle. Cost
# in this state: ~$2-3 / mo for the volume + IP.
#
# Pass --keep-warm to skip step 3 (Machine never auto-stops; ~$24 / mo).
#
# Usage:
#   deploy/deploy.sh             # cheap mode, two-phase
#   deploy/deploy.sh --keep-warm # always-warm mode, skips phase 2
#
# Prerequisites:
#   * `fly` (flyctl) installed and authenticated (`fly auth login`).
#   * The Fly app already exists (`fly launch --copy-config --no-deploy`
#     done once).
#   * The persistent volume exists
#     (`fly volumes create rustviz_docker --size 10 --region <region>`).
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
set_min_machines() {
    local val="$1"
    perl -i -pe "s|^(\s*min_machines_running = )\d+|\${1}$val|" fly.toml
}

# Phase 2 (and any error path) restores fly.toml so the working tree
# stays clean if the script aborts mid-flight.
RESTORE_TO=0
trap '[ -n "${RESTORE_TO:-}" ] && set_min_machines "$RESTORE_TO" || true' EXIT

# --- phase 1 ------------------------------------------------------------
echo "==> Phase 1: deploying with min_machines_running = 1"
set_min_machines 1
RESTORE_TO=0
fly deploy

echo "==> Waiting for ${URL} to respond (up to 15 min for fresh-volume bootstraps)..."
deadline=$(( $(date +%s) + 900 ))
while ! curl --max-time 10 -fsS "$URL" -o /dev/null 2>/dev/null; do
    if [ "$(date +%s)" -gt "$deadline" ]; then
        cat >&2 <<EOF
ERROR: ${URL} did not respond within 15 minutes.
fly.toml has been left at min_machines_running = 1 so the Machine
won't auto-stop while you investigate. Useful next steps:

  fly logs                        # see what entrypoint is doing
  fly status --all                # check Machine state
  fly ssh console                 # poke around inside

Re-run this script when you've fixed the underlying issue.
EOF
        # don't restore in the EXIT trap — leave at 1 for the operator
        RESTORE_TO=
        exit 1
    fi
    sleep 10
done
echo "==> ${URL} is live"

# --- phase 2 ------------------------------------------------------------
if [ "$KEEP_WARM" -eq 1 ]; then
    echo "==> --keep-warm passed; leaving min_machines_running = 1 (Machine never auto-stops)."
    RESTORE_TO=1
    exit 0
fi

echo "==> Phase 2: setting min_machines_running = 0 so the Machine auto-stops when idle"
set_min_machines 0
RESTORE_TO=0
fly deploy

echo "==> Done. Cold starts after idle take ~10 s (runner image cached on volume)."
