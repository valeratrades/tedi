# e2e: the web board against Figma

Renders the board in headless chromium over a seeded mock store and screenshots it, to compare with the Figma
frames ([file](https://www.figma.com/design/AKNYVRnUHnGrnoFN6geWii)) pixel for pixel.

```sh
tedi_web/e2e/shoot.sh 01-board            # -> tedi_web/e2e/out/01-board.png   (1440x900; 06* are 390x844)
tedi_web/e2e/shoot.sh stop                # stop the server it started
tedi_web/e2e/pending.mjs 7351             # an edit shows before it lands, over the freshly seeded server shoot.sh left running
E2E_REBUILD=1 tedi_web/e2e/shoot.sh 01-board   # after UI/model changes: rebuild tedi + the client bundle (debug, ~1.5 min)
E2E_RESEED=1  tedi_web/e2e/shoot.sh 01-board   # rebuild the store from seed/
tedi_web/e2e/figma.sh [screen]            # reference/*.png from Figma (needs FIGMA_TOKEN; reference/ is committed)
```

Screens and frames: 01-board 48:5386, 02-view 10:1607, 03-library 11:3139, 04-workspace 12:3114, 05-scope 48:6437,
06a-mobile 13:5217, 06b-mobile 13:5488. Compare `out/<s>.png` with `reference/<s>.png`.

## Specs and renders

```sh
nix run .#e2e                  # specs/: the address against the screen (url), a view's order as labels (sort), the wait for the board (loading)
nix run .#figma                # every page the address can name, at 1440 and 390, onto a new page of `figmaFile` (flake.nix), "tedi — renders"
```

Both rebuild, reseed and serve on ports of their own (`specs/stack.ts`), behind a proxy that signs each browser context in as a viewer
with `libraries/04-workspace.json`. The renders are code's, never read back; the loop is the `figma-render` skill.
## Layout

- `serve.sh`: builds, seeds and serves; `shoot.sh` and the specs run it.
- `seed/state.json`: the GitHub mock's state (issues, labels, assignees, `blocked by`, linked and listed PRs, sprint milestones).
- `seed/positions.json`: what the mock cannot hold, written into the store after `tedi --mock pull`: kanban / pull_requests
  states (`.states.json`) and label colours (`.meta.json`).
- `config.toml`: two views, scopes EV ⊃ {EV-Estate, EV-Main, EV-Service-Arb}, column layouts, sprints' milestone repo.
- `libraries/<screen>.json`: the viewer's library (copied to `$XDG_DATA_HOME/tedi/web/mock_user.json` before each shot):
  tiles per band, current scope, workspaces, kept views. Layouts are tilers schema v3 (xl: 64 steps; 01: board tile 42, issue tile 22);
  `on_screen.columns` is empty, the app seeds a column per group. `workspace` names the active workspace tab.
- Store, state and cache live in `$E2E_HOME` (default `/tmp/tedi-e2e`); the server is on `E2E_PORT` (7351), clock `2026-11-23T12:00Z`.
  To use `dx serve` instead of the bundle, run the server on 7341 (`E2E_PORT=7341`) and point chromium at the dx port.

## Not reproducible

- Screens needing UI not built: library panel (03), view builder (02), scope editor modal (05), mobile drawer (06b, interaction state), toasts ("Moved #23", "synced 2m ago"), "title changed on GitHub" conflict card.
- PR review state (review / approved) and checks (2/3 ✓): the mock's linked PR is `url + open|draft` only; the PRs view groups by the `pull_requests` workflow, seeded to match.
- Label colours: the mock lists names only; colours are injected into `.meta.json` and are guesses (Figma's chips are not read).
- Counts (Triage 86, EV 153, ...): only the shown issues are seeded.
- Viewer is `mock_user` (hardcoded in the mock), so `assignee:@me` matches nothing and Figma's "VT" avatar is a seeded `valeratrades` assignee.
- Viewer scope `core` is top-level in the library (a viewer's scope cannot sit under a team scope); Figma nests it under EV.
- Figma's own data is inconsistent across frames (Triage query names two buckets but shows four; PR #70 in ev-main appears under a service-arb scope; i:6 on every row of 02/03): 01 is followed.
- "1 issue doesn't parse": a broken file is seeded in ev-main/gitops; the current UI does not show it.
