# tedi_web

Design upstream: [Figma — tedi board UX](https://www.figma.com/design/AKNYVRnUHnGrnoFN6geWii) (foundations from `assets/tokens.css`, components, screens, flows).
What code renders now: [Figma — tedi — renders](https://www.figma.com/design/LKVOrmtWpG3N87rVQyOZoj), a page per `nix run .#figma`.

The client holds every card and runs `tedi_core::board` over them; every edit is a server fn calling
`tedi_task_operations` in-process — sync, merge, stage and rank are never re-done here.
Edits on their way there: `src/ui/sync.rs`. The address bar: `src/ui/route.rs`.

Two processes on one store: `tedi web` serves the API (and, in production, the client bundle); `dx` builds and
hot-reloads the client, proxying `/api/` to it (`tedi_web/Dioxus.toml`).

```sh
tedi web --addr 127.0.0.1:7341                                      # add --mock for the mock GitHub
(cd tedi_web && dx serve --platform web --port "$PORT" --open false)
```

Production: `dx bundle --web --release -p tedi_web` writes the bundle; `tedi web --addr … --public <bundle>/public
--pull-every 10m` serves it. The team instance is `nix build .#tedi-web-container` (its contract in `flake.nix`), released on every tag and deployed by EV-invest/devops.

The client's stylesheet is built by `tedi_web/build.rs` with `tailwindcss`; `wasm-bindgen` is pinned to the CLI
the flake carries. Both come with `nix develop`.
