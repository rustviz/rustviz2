# RustViz 2

**RustViz** generates interactive timeline visualizations of ownership and
borrowing for short Rust programs. It is meant as a teaching aid: paste a
snippet, see exactly when each binding becomes the resource owner, when
references go in and out of scope, and which lines those events correspond
to.

RustViz 2 is the compiler-integrated rewrite of the project. Earlier RustViz
read hand-annotated source; RustViz 2 plugs into `rustc` directly and walks
HIR/MIR, so the diagram reflects the real borrow checker's view of your
program rather than a hand-curated approximation.

RustViz is a project of the [Future of Programming Lab](http://fplab.mplse.org/)
at the University of Michigan.

> **Try it live:** <https://rustviz.github.io/playground/>
> (compile API at <https://rustviz-playground.fly.dev/>)

![screenshot placeholder](rustviz2-plugin/src/svg_generator/rv2_example.png)

---

## Architecture at a glance

```
              GET /  (CDN, instant)
   browser ─────────────────────▶  GitHub Pages
                                   rustviz.github.io/playground/
                                   (Vite SPA, ex-assets)

              POST /submit-code (cold start ~10s
                                  after Fly auto-stop,
                                  cached afterward)
   browser ─────────────────────▶  rv-serve (Actix-web on Fly)
                                          │
                                          │ docker run --network=none --read-only …
                                          ▼
                                   ┌────────────────────────┐
                                   │  rustviz-runner image  │  ephemeral container per request
                                   │  (nightly + plugin)    │  tmpfs /work, capped CPU/RAM/PIDs
                                   └──────────┬─────────────┘
                                              │ cargo rv-plugin
                                              ▼
                                   ┌────────────────────────┐
                                   │   rustviz2-plugin      │  rustc plugin: walks HIR/MIR,
                                   │   (rustc_private)      │  emits two SVGs on stdout
                                   └────────────────────────┘
```

The frontend is a static Vite bundle, hosted on GitHub Pages, so the page
loads instantly even when no one has visited recently. The compile API on
Fly is allowed to auto-stop and cold-start; that latency only shows up
after the user clicks "Generate Visualization", where a couple-second
delay is expected. CORS in `rv-serve/src/main.rs` allows the Pages origin
to call `/submit-code`.

The same `rv-serve` binary still serves the SPA + API from a single origin
in the all-in-one Fly deploy (and in local development), so neither
hosting mode is special-cased in the application code.

Workspace members:

| Crate                    | Role |
|--------------------------|------|
| **`rustviz2-plugin`**    | The rustc plugin. Built on Will Crichton's `rustc_plugin`/`rustc_utils` crates (same family as Flowistry/Aquascope). Provides `cargo rv-plugin`. |
| **`rustviz2`**           | Thin user-facing library. `Rustviz::new(code)` runs the plugin against `code` (in a sandboxed Docker container by default) and returns the rendered code-panel and timeline-panel SVGs. |
| **`mdbook-rustviz`**     | mdbook preprocessor that turns ` ```rv ``` ` fenced blocks into embedded SVGs. |
| **`rv-serve`**           | Actix-web playground: serves the React/CodeMirror SPA and exposes `POST /submit-code`. |

---

## Quick start (local)

Requirements: `rustup`, `node` 20+, and (for the sandboxed backend) `docker`
or [Colima](https://github.com/abiosoft/colima). The pinned nightly toolchain
is auto-installed by `rustup` from `rust-toolchain.toml`.

```sh
git clone https://github.com/rustviz/rustviz2
cd rustviz2
./setup.sh                    # toolchain, plugin install, frontend build, runner image
cd rv-serve && cargo run --release
open http://127.0.0.1:8080/
```

Iterating on the frontend with hot reload:

```sh
cd rv-serve/frontend && npm run dev   # serves at http://127.0.0.1:3000/
# (proxies /submit-code + /ex-assets to rv-serve at :8080, so leave
#  `cargo run` running in another terminal)
```

If you don't have Docker installed and just want to poke at the server,
set `RV_RUNNER=local`. **Never** do this on a public deployment — see
[`SECURITY.md`](SECURITY.md).

---

## Deploy

Production runs in two pieces:

- **Static SPA on GitHub Pages**, at <https://rustviz.github.io/playground/>.
  Built from `rv-serve/frontend/` by `.github/workflows/pages.yml` and
  pushed to the `rustviz/playground` repo on every change. Loads instantly
  even when no one has visited recently.
- **Compile API on Fly.io**, at <https://rustviz-playground.fly.dev/>.
  Auto-stops when idle to keep costs at ~$2–3/mo; cold starts cost ~10 s
  on first request after idle. Allowed origins are listed in
  `rv-serve/src/main.rs::cors`.

The same `rv-serve` binary also still works as an all-in-one server (SPA
+ API on a single origin); the GitHub Pages split is just a latency
optimization for the static page-load.

### First-time setup (Fly compile API)

```sh
fly auth login                                  # browser OAuth
fly launch --copy-config --no-deploy            # creates the app
gh workflow run runner-image.yml                # builds & pushes runner to GHCR
# Wait for the workflow to finish (~30 min first time);
# then mark the package public:
#   GitHub → Org → Packages → rustviz-runner →
#     Package settings → Change visibility → Public.
./deploy/deploy.sh                              # two-phase Fly deploy
```

The first boot of each Fly Machine pulls the `rustviz/rustviz-runner`
image from GHCR (~30 s for ~600 MB). It's then cached on the Machine's
local filesystem; subsequent cold starts after auto-stop take ~10 s.

### Routine deploys

```sh
./deploy/deploy.sh
```

When you change `runner/**` or `rustviz2-plugin/**`,
`.github/workflows/runner-image.yml` automatically republishes the
sandbox image to GHCR; the next `./deploy/deploy.sh` picks it up on
each Machine's first boot.

Push a `vX.Y.Z` tag and the `.github/workflows/deploy.yml` workflow
runs `flyctl deploy --remote-only` for you (requires a `FLY_API_TOKEN`
repo secret).

### Scaling for traffic spikes

`fly.toml` ships a single Fly Machine in cheap-mode (auto-stops when
idle). To handle a traffic surge — e.g. someone posts the playground
URL on Hacker News:

```sh
fly scale count 5                # five Machines, all auto-start/stop
fly scale memory 4096            # bump each to 4 GB if needed
```

Each new Machine pulls the runner image from GHCR independently on
first boot, then auto-stops when the surge settles. Idle cost stays at
~$2–3/mo regardless of fleet size; surge cost is ~$5–10 for an HN
front-page-day's worth of running.

After the surge, scale back down with `fly scale count 1` to free the
extra Machine slots.

### First-time setup (GitHub Pages SPA)

```sh
# 1. Create the receiving repo
gh repo create rustviz/playground --public \
  --description "Static front-end for the RustViz playground"

# 2. Enable Pages on rustviz/playground via Settings → Pages →
#    Source: Deploy from a branch → main / root.

# 3. Generate a deploy keypair
ssh-keygen -t ed25519 -f /tmp/playground_deploy_key -N "" -C playground-deploy

# 4. Add the *public* key as a write-enabled deploy key on rustviz/playground
gh api -X POST repos/rustviz/playground/keys \
  -f title=playground-deploy -F read_only=false \
  -f key="$(cat /tmp/playground_deploy_key.pub)"

# 5. Add the *private* key as a secret on rustviz/rustviz2
gh secret set PAGES_DEPLOY_KEY --repo rustviz/rustviz2 < /tmp/playground_deploy_key

# 6. Clean up
rm /tmp/playground_deploy_key /tmp/playground_deploy_key.pub
```

After that, every push to `main` (when the change touches
`rv-serve/frontend/**`) triggers `.github/workflows/pages.yml`, which
builds the SPA in `pages` mode and pushes the `dist/` tree to
`rustviz/playground` for serving at
<https://rustviz.github.io/playground/>.

### Adding a new SPA origin

If you ever stand up the SPA at another URL (custom domain, mirror), add
that origin to the CORS allowlist in `rv-serve/src/main.rs` and redeploy
the API. The allowlist is the gate — without it the new origin's browsers
will refuse to call `/submit-code`.

### Why a script instead of `fly deploy` directly

`fly.toml` sets `auto_stop_machines = 'stop'` with `min_machines_running = 0`,
so the Fly Machine idles down to zero cost when no traffic is hitting it
(~$2–3/mo for the volume + IP). The catch: Fly's autoscaler stops the
Machine after ~40 s of "no incoming traffic", which collides with our
5–10 min first-boot bootstrap. Empirically, bumping
`min_machines_running` to 1 alone is *not* enough — during the warmup
phase of a fresh deploy, the autoscaler can SIGINT the Machine before
it's registered as part of the min-pool. The reliable fix is to disable
auto-stop entirely during bootstrap. `deploy/deploy.sh` handles the
timing window automatically:

1. Edits `fly.toml` to set `auto_stop_machines = 'off'` and
   `min_machines_running = 1`, runs `fly deploy`.
2. Polls the public URL until it responds (up to 15 min).
3. Edits `fly.toml` back to `auto_stop_machines = 'stop'` and
   `min_machines_running = 0`, runs `fly deploy` again so auto-stop is
   in force for steady state.

Pass `--keep-warm` to skip step 3 if you want the Machine to never
auto-stop (~$24/mo).

### Security

Read [`SECURITY.md`](SECURITY.md) before exposing the playground to the
public internet. The runner image's sandbox flags are not optional.

---

## Limitations

RustViz 2 is a research tool. It supports a meaningful subset of Rust but
not all of it. Currently unsupported (or known to misbehave):

- For-loops
- Conditional `let` bindings
- Borrows that occur inside conditionals
- Chained method calls (`x.get().get_mut()`)
- Lifetime annotations
- Borrows over struct members

The plugin has a TODO list with more detail in
[`rustviz2-plugin/README.md`](rustviz2-plugin/README.md).

---

## Security

The playground compiles untrusted Rust source. Proc-macro expansion in user
code is arbitrary code execution, so the plugin always runs inside a
sandboxed container. The full threat model and the operator checklist are
in [`SECURITY.md`](SECURITY.md). Report findings to `comar@umich.edu`.

---

## Contributing

Issues and PRs welcome. The project follows standard GitHub flow; keep
each PR focused on a single concern, and run `./setup.sh` plus
`cargo build --workspace --locked` before opening one.

---

## License

[MIT](LICENSE).

## Citing

If you use RustViz in academic work, please cite the
[VL/HCC 2022 paper](https://web.eecs.umich.edu/~comar/rustviz-vlhcc22.pdf).
