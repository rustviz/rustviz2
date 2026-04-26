#!/usr/bin/env bash
# Reproduces a working build of rustviz2 on macOS/Linux.
#
# A committed Cargo.lock pins transitive deps that have since moved past
# rustc 1.80 / edition 2024 — without those pins, fresh resolution from
# crates.io fails on the pinned nightly. Toolchain refresh would remove the
# need for this; until then the lockfile is the floor.
set -euo pipefail

cd "$(dirname "$0")"

# 1. Toolchain (rust-toolchain.toml triggers rustup to auto-install
#    nightly-2025-08-20 with the rustc-dev/rust-src components we need).
rustup show active-toolchain >/dev/null

# 2. Build & install the rustc plugin (provides `cargo rv-plugin`).
cargo install --path rustviz2-plugin --locked

# 3. Build the Vite frontend that the playground serves.
( cd rv-serve/frontend && npm install && npm run build )

# 4. ex-assets/ holds the SVG hover/highlight layer. helpers.js is checked in
#    (patched for inline-SVG embedding); visualization.css is copied from the
#    mdbook test fixture as-is. Only seed missing files — never clobber edits.
mkdir -p rv-serve/ex-assets
[ -f rv-serve/ex-assets/visualization.css ] || \
    cp test-book/mdbook_plugin/visualization.css rv-serve/ex-assets/
[ -f rv-serve/ex-assets/helpers.js ] || \
    cp test-book/mdbook_plugin/helpers.js rv-serve/ex-assets/

# 5. Build the rest of the workspace.
cargo build --workspace --release

# 6. Build the sandboxed runner image if Docker is available locally. This
#    is required before exposing rv-serve to untrusted input — see
#    SECURITY.md. Skipped silently when docker is not on PATH so devs who
#    only iterate against RV_RUNNER=local don't need it installed.
if command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    echo "Building rustviz/rustviz-runner image..."
    docker build -t rustviz/rustviz-runner:latest -f runner/Dockerfile .
else
    cat <<'WARN'
Skipping runner image build: docker is not available.

For local dev, set RV_RUNNER=local to run the plugin in-process
(NEVER do this on a public deployment — see SECURITY.md).
WARN
fi

cat <<'EOF'

Setup complete. To run the playground:

  RV_RUNNER=local cd rv-serve && cargo run --release   # local dev
  cd rv-serve && cargo run --release                   # docker (default)
  open http://127.0.0.1:8080/

To iterate on the frontend in dev mode (hot reload, proxies API to :8080):

  cd rv-serve/frontend && npm run dev
  open http://127.0.0.1:3000/

To run the rustc plugin directly (host toolchain) against test-crate:

  cd test-crate && cargo rv-plugin -w
EOF
