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

> **Try it live:** <https://rustviz-playground.fly.dev/>

![screenshot placeholder](rustviz2-plugin/src/svg_generator/rv2_example.png)

---

## Architecture at a glance

```
                ┌────────────────────────┐
   browser ──▶  │  rv-serve (Actix-web)  │  static files + POST /submit-code
                └──────────┬─────────────┘
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

The repo ships a top-level `Dockerfile` and a `fly.toml` for a one-Machine
deploy on [Fly.io](https://fly.io). The deploy image runs Docker-in-Docker
inside a Fly Machine; rv-serve shells out to `docker run` per request, so
the same per-request sandbox documented in `SECURITY.md` is in force in
production.

### First-time setup

```sh
fly auth login                                                 # browser OAuth
fly launch --copy-config --no-deploy                           # creates the app
fly volumes create rustviz_docker --size 10 --region <region>  # runner-image cache
./deploy/deploy.sh                                             # two-phase deploy
```

The first boot inside the Machine builds the `rustviz/rustviz-runner` image
(downloads the Rust nightly, compiles the plugin) and takes 5–10 minutes.
The image is then cached on the `rustviz_docker` volume, so future cold
starts take ~10 s.

### Routine deploys

```sh
./deploy/deploy.sh
```

Or push a `vX.Y.Z` tag and the `.github/workflows/deploy.yml` workflow runs
`flyctl deploy --remote-only` for you (requires a `FLY_API_TOKEN` repo
secret).

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

MIT. See per-crate `Cargo.toml` for details.

## Citing

If you use RustViz in academic work, please cite the
[VL/HCC 2022 paper](https://web.eecs.umich.edu/~comar/rustviz-vlhcc22.pdf).
