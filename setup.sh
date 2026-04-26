#!/usr/bin/env bash
# Reproduces a working build of rustviz2 on macOS/Linux as of 2026-04-26.
# The repo ships no Cargo.lock; this script generates one and pins transitive
# deps that have since moved past rustc 1.80 / edition 2024.
set -euo pipefail

cd "$(dirname "$0")"

# 1. Toolchain (the rust-toolchain.toml in the repo will trigger auto-install).
rustup show active-toolchain >/dev/null

# 2. Generate a lockfile using stable cargo so it can parse manifests that
#    require edition2024 in their Cargo.toml. We then pin transitive deps so
#    the nightly-2024-05-20 cargo can actually build.
if [ ! -f Cargo.lock ]; then
  rustup run stable cargo generate-lockfile
fi

pin() { rustup run stable cargo update -p "$1" --precise "$2" >/dev/null; }

# clap suite (4.6+ depends on clap_lex 1.1+ which needs edition 2024)
pin clap 4.5.20
pin clap_builder 4.5.20
pin clap_derive 4.5.18
pin clap_complete 4.5.18
pin clap_lex 0.7.7
# misc
pin indexmap 2.6.0
pin tempfile 3.10.1
pin ignore 0.4.23
pin globset 0.4.15
pin time 0.3.36
pin time-macros 0.2.18
pin time-core 0.1.2
pin litemap 0.7.3
pin idna_adapter 1.2.0
# actix transitively pulls rand 0.10 → wasip3 → edition 2024 in newer versions
pin actix-web 4.9.0
pin actix-http 3.9.0
pin actix-files 0.6.6
# pest 2.8 requires rustc 1.83
pin pest 2.7.10
pin pest_derive 2.7.10
pin pest_generator 2.7.10
pin pest_meta 2.7.10

# 3. Build & install the rustc plugin (provides `cargo rv-plugin`).
cargo install --path rustviz2-plugin --locked

# 4. Build the React frontend that the playground serves.
( cd rv-serve/frontend && npm install && npm run build )

# 5. The frontend index.html references /ex-assets/{book.js,helpers.js,...}.
#    The playground needs a *different* book.js and a patched helpers.js
#    from what mdbook ships, because the playground inlines SVGs (no
#    contentDocument) and isn't an mdbook page (no hljs / playpen / gtag).
#    The patched files in rv-serve/ex-assets/ are checked in alongside
#    this script — only seed them if missing so we don't clobber edits.
mkdir -p rv-serve/ex-assets
[ -f rv-serve/ex-assets/visualization.css ] || \
    cp test-book/mdbook_plugin/visualization.css rv-serve/ex-assets/
if [ ! -f rv-serve/ex-assets/book.js ]; then
    cat > rv-serve/ex-assets/book.js <<'JS'
"use strict";
window.gtag = window.gtag || function () {};
window.dataLayer = window.dataLayer || [];
JS
fi
[ -f rv-serve/ex-assets/helpers.js ] || \
    cp test-book/mdbook_plugin/helpers.js rv-serve/ex-assets/  # NOTE: needs patches for inline SVG; see helpers.js

# 6. Build the workspace.
cargo build --workspace --release

cat <<'EOF'

Setup complete. To run the playground:

  cd rv-serve && cargo run --release
  open http://127.0.0.1:8080/

To run the rustc plugin against test-crate:

  cd test-crate && cargo rv-plugin -w
EOF
