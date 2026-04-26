#!/usr/bin/env bash
# Deploy entrypoint: bring up dockerd, ensure the runner image exists,
# then exec rv-serve. Runs as PID 1 inside the docker:dind base; tini
# wraps us so signal handling is sane.

set -euo pipefail

LOG() { printf '[entrypoint] %s\n' "$*" >&2; }

# 1. Start dockerd in the background. The default dind entrypoint takes
#    care of cgroup setup, certificate handling for TLS, etc.; we just
#    point it at the host socket.
LOG "starting dockerd..."
dockerd-entrypoint.sh dockerd \
    --host=unix:///var/run/docker.sock \
    --storage-driver=overlay2 \
    > /var/log/dockerd.log 2>&1 &

# 2. Wait up to 60s for dockerd to accept connections.
LOG "waiting for dockerd to be ready..."
for i in $(seq 1 60); do
    if docker info >/dev/null 2>&1; then
        LOG "dockerd ready after ${i}s"
        break
    fi
    sleep 1
    if [ "$i" = "60" ]; then
        LOG "ERROR: dockerd did not become ready in 60s"
        tail -50 /var/log/dockerd.log >&2 || true
        exit 1
    fi
done

# 3. Ensure the runner image exists. On first boot we build it from the
#    /opt/runner-context tree baked into this image. Subsequent boots reuse
#    the cached image from /var/lib/docker (mounted on a volume in fly.toml).
if ! docker image inspect rustviz/rustviz-runner:latest >/dev/null 2>&1; then
    LOG "runner image not present, building (one-time, ~3-5 min)..."
    # --network=host: the inner dockerd's default bridge network has no
    # working DNS resolver inside Fly Machines, so build steps that hit the
    # internet (rustup, cargo registry) fail with NXDOMAIN. Sharing the host
    # network namespace lets buildkit use the Fly Machine's resolver.
    docker build --network=host \
        -t rustviz/rustviz-runner:latest \
        -f /opt/runner-context/runner/Dockerfile \
        /opt/runner-context
    LOG "runner image built"
else
    LOG "runner image already cached"
fi

# 4. Exec rv-serve. Bind addr + RV_RUNNER come from the env (set in
#    Dockerfile / fly.toml).
LOG "starting rv-serve on ${RV_BIND}"
cd /app
exec /usr/local/bin/rustviz_serve
