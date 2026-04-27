# syntax=docker/dockerfile:1.7
#
# Deploy image for the RustViz playground. Multi-stage:
#   1. rust-builder: compile rv-serve against the workspace's pinned nightly.
#   2. frontend-builder: vite build for the SPA.
#   3. final: docker:dind base — rv-serve runs alongside an in-VM dockerd
#      and shells out to `docker run rustviz/rustviz-runner` per request
#      (see SECURITY.md). The runner image is built at first boot from
#      the sources baked into /opt/runner-context.

ARG RUST_NIGHTLY=nightly-2025-08-20

# ---------- 1. rust-builder ----------
# Alpine base shares its libc (musl) with the docker:dind final stage, so
# the rv-serve binary we produce here drops directly into stage 3 without
# cross-compilation.
FROM rust:1.83-alpine AS rust-builder
ARG RUST_NIGHTLY
RUN apk add --no-cache musl-dev gcc make perl pkgconfig \
 && rustup toolchain install ${RUST_NIGHTLY} --profile minimal \
        --component rust-src,rustc-dev,llvm-tools-preview \
 && rustup default ${RUST_NIGHTLY}

WORKDIR /src
# Build context is the repo root. Copy enough of the workspace that cargo
# can resolve and build rv-serve. (rustviz2-plugin is excluded from the
# rv-serve binary's dep tree, so it's not compiled here even though its
# manifest is part of the workspace resolution graph.)
COPY rust-toolchain.toml Cargo.toml Cargo.lock ./
COPY rustviz2-plugin/Cargo.toml ./rustviz2-plugin/Cargo.toml
COPY rustviz2-plugin/src/ ./rustviz2-plugin/src/
COPY rustviz2/ ./rustviz2/
COPY mdbook-rustviz/ ./mdbook-rustviz/
COPY rv-serve/Cargo.toml ./rv-serve/Cargo.toml
COPY rv-serve/src/ ./rv-serve/src/

RUN cargo build --release --locked -p rustviz_serve

# ---------- 2. frontend-builder ----------
FROM node:20-alpine AS frontend-builder
WORKDIR /src
COPY rv-serve/frontend/package.json rv-serve/frontend/package-lock.json ./
RUN npm ci --no-audit --no-fund
COPY rv-serve/frontend/ ./
RUN npm run build

# ---------- 3. final (docker:dind) ----------
FROM docker:27-dind
RUN apk add --no-cache bash curl tini

# rv-serve binary (built on Alpine in stage 1, so already musl).
COPY --from=rust-builder /src/target/release/rustviz_serve /usr/local/bin/rustviz_serve

# Frontend bundle (Vite output). The Vite build copies frontend/public/
# into dist/, so ex-assets/{helpers.js,visualization.css} ride along.
WORKDIR /app
COPY --from=frontend-builder /src/dist/ /app/frontend/dist/

# Build context for the runner image, baked in so the entrypoint can
# `docker build` it on first boot. runner/Dockerfile overwrites Cargo.toml
# itself, so we ship the workspace Cargo.toml as-is.
COPY rust-toolchain.toml Cargo.toml Cargo.lock /opt/runner-context/
COPY rustviz2-plugin/ /opt/runner-context/rustviz2-plugin/
COPY runner/ /opt/runner-context/runner/

COPY deploy/entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENV RV_BIND=0.0.0.0:8080 \
    RV_RUNNER=docker

EXPOSE 8080
ENTRYPOINT ["tini", "--", "/usr/local/bin/entrypoint.sh"]
