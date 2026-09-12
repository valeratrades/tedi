---
name: tedi-format
description: How to read and write tedi task files — issues, blockers, milestones, sprints. Use when editing a file that opens with `- [ ] Title <!-- @user https://github.com/…/issues/N -->`, anything under an `issues/` store or `urgent.md`, a `# Blockers` section, a `<!-- pending -->` / `<!-- virtual -->` / `<!-- new comment -->` marker, a `!n` / `!b` / `!c` / `!s` / `!u` shorthand, an `o/r#42` ref, or a sprint keyed `1d` / `2w` / `1Q` / `1y`.
---

# tedi format

Three levels, and nothing else:

```
Milestone   target system state      "site is presentable to clients"
   └ Issue     scoped objective      "fix the subscribe button"
       └ Blockers   exact actions    "ensure button is reactive"
```

Containment — anything not drawn is forbidden:

```
Sprint ──┬─→ Milestone ──→ Issue ──┬─→ Issue (child)
         └─→ Issue ────────────────┴─→ Blockers
```

A Milestone never holds another Milestone (state implies state; explicit edges would go stale).
Blockers are owned by Issues only. No view holds Blockers directly.

Write nothing you can leave out. No filler, no restating the title, no "this PR adds".

## The issue file

One issue = one file = one top-level list item. Line 1, column 0, is the title line;
everything the issue owns is indented two spaces under it.

```md
- [ ] (bug) Fix subscribe button <!-- @valeratrades https://github.com/o/r/issues/42 -->
  server rejects the second click; suspect the debounce wrapper

  <!--omitted {{{always-->
  <!-- @someone https://github.com/o/r/issues/42#issuecomment-991 -->
    reproduced on staging
  <!-- new comment -->
  can't reproduce locally
  <!--,}}}-->

  # Blockers
  - ensure button is reactive
    the handler is bound before hydration
    - move binding into onMount

  - [ ] [Rate-limit the endpoint](./43_-_Rate_limit.md) <!-- @valeratrades https://github.com/o/r/issues/43 -->
```

Order is fixed: title · body · comments · `# Blockers` · child links.

### Title line

`- [state] (labels) Title <!-- marker -->`

| state   | means                          |
|---------|--------------------------------|
| `[ ]`   | open                           |
| `[.]`   | partial — work started         |
| `[?]`   | maybe — may not be worth doing |
| `[x]`   | closed, done                   |
| `[-]`   | closed, not planned            |
| `[123]` | duplicate of #123              |

`[.]`/`[?]` ride GitHub as the managed labels `p:partial`/`p:maybe` — set the checkbox,
never the label. Other labels go in `(a, b) ` before the title, comma-separated.

| marker                       | means                                                |
|------------------------------|------------------------------------------------------|
| `<!-- @user URL -->`         | lives on GitHub, authored by `user`                  |
| `<!-- URL -->`               | lives on GitHub, author unknown                      |
| `<!-- pending -->` / `!n`    | will be created on GitHub at next sync               |
| `<!-- virtual -->`           | local only, never synced                             |

**Never hand-write a `@user URL` marker.** It claims an issue that already exists upstream;
composition fails loudly rather than adopting it. New issue → `!n`.

### Body and comments

Body is your prose. It is *not* a restatement of the title — put constraints, the why, and
what you already ruled out.

Comments live between `<!--omitted {{{always-->` and `<!--,}}}-->` (a vim fold, so they
collapse). Each opens with its own marker: `<!-- @user URL#issuecomment-N -->` for an
existing one, `<!-- new comment -->` (or `!c`) for one you are writing.

Text that is not yours renders indented one extra level. That indent is ownership display —
leave it alone, and don't edit inside it.

### Children

A child is one line: `- [state] [Title](./rel.md) <!-- marker -->`. The link is navigational
(`gf` jumps), the marker is identity, the filename is derived — rename nothing by hand, and
a `.bak` suffix just means closed. To change a child, open the child's file. A subtree is
never embedded; nesting deeper than one link is not a shorthand, it is a parse error waiting.

A plain `- [ ]` list inside the body stays body content. It becomes children only if its
first item carries a marker, or if it follows the blockers list.

## Blockers

```md
  # Blockers
  - make enough money
  - buy a yacht
    - find the best model
      - call the guy who knows about them
      - compile importance ranking of features
    - find a seller
  - go on a cruise
```

**Read bottom → top.** The last line is what you are doing now; `pop` removes it and the
line above becomes current. Append at the bottom.

Nesting carries two directions at once:

```
- buy a yacht          ← blocked by everything indented under it
  - find the best model      ← must finish before "find a seller" is useful
    - call the guy …               depth = prerequisite
  - find a seller              breadth = contemporary, also unresolved
```

A deeper item must complete before its parent can. A sibling below it at the same level is
not its prerequisite, but its hanging state still voids the point of finishing the parent.

Other rules:

- Any non-`- ` line indented under an item is a comment on it. Free prose, no semantics.
- Once an item has a nested child, stop adding comments to it.
- An item whose entire text is one issue ref — `#42`, `repo#42`, `o/r#42`, or a full URL —
  **delegates**: the work continues in that issue. Sprint selection skips over these.
- Blockers are not the issue's success criteria. They are only the part already sorted out,
  so `- evaluate the path, given grown knowledge` is a legitimate final item.
- The section ends at a `---` rule or at the first checkbox list. `# Blockers` must be an H1
  at the issue's own indent; an indented one belongs to whatever list item holds it.

## Editor shorthands

Typed while editing, expanded on save.

| type | where             | does                                          |
|------|-------------------|-----------------------------------------------|
| `!n` | end of title line | mark pending — create this issue on sync      |
| `!b` | own line          | insert `# Blockers`                           |
| `!c` | own line          | start a new comment                           |
| `!s` | own line, or `# Blockers !s` | make this block the active selection |
| `!u` | last line         | abort — the edit is discarded entirely        |

## Views: sprints, urgent, milestones

Read this when you are arranging work, not when you are editing one issue.

A **TaskView** is a markdown document of headers and *links*. Sprints, bottlenecks and
searches are all this one primitive. It stores links only — never issue content.

```
sprint 1d  (stored in the GitHub milestone description, keyed by timeframe:
│           1d, 2w, 1Q, 1y, 3y, 7y — `M` is months, `m` would be minutes)
│ priority runs top → bottom; the period's selection is not expected to complete
├─ # Must          ← managed section: today's musts
│   └ - o/r#42
├─ # whatever      ← your own headers, free
│   └ - o/r#77
└─ - https://github.com/o/r/milestone/3
```

- A sprint holds milestones and issues. Never blockers.
- A milestone holds issues and prose, and may carry a deadline. No priority order inside —
  everything in it is required. Not being in any milestone does not stop an issue from
  entering a sprint.
- `urgent` is the special lowest sprint: `issues/urgent.md`, local, never synced, no
  sections, and plain-text items are allowed beside refs. Milestone refs are rejected.
  It prunes its own closed issues; plain text is never pruned.
- A bare `#N` resolves its repo from the parent item's text, so a category line like
  `- discretionary_engine` above it is meaningful context, not decoration.

While you edit a sprint, each issue ref is expanded in place into that issue's exact file
bytes, wrapped in `<!--{{{1-->` / `<!--}}}1-->` folds. Edit it there as you would its own
file — title, state, labels, comments and blockers all commit back through the same parse.
A block that does not parse fails the edit rather than being dropped. On save the view
collapses back to bare links and the folds vanish; they are presentation only.

## Where the truth lives

If the repo is at hand, these override anything above:

| path                          | read it                                  |
|-------------------------------|------------------------------------------|
| `docs/ARCHITECTURE.md`        | always first — the primitives and their composition |
| `tedi_core/src/issue.rs`      | `Issue::render` / `VirtualIssue::parse` — the exact bytes |
| `tedi_core/src/blockers.rs`   | blocker nesting, `pop`/`add`/`set`       |
| `tedi_core/src/marker.rs`     | every marker and shorthand               |
| `tedi_core/src/taskview.rs`   | sprints, sections, expand/collapse       |
