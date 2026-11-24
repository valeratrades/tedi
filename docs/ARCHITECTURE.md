## todo primitives
the framework fundamentally consists of 3 primitive levels:
- `Milestone` — it reasons about target **system state**
  eg: "site is presentable to clients", "get to v1", "improve code quality"
- `Issue` — has definite **objectives** associated
  eg: "fix button", "research colorscheme preferences", "abolish OOP in shave_yak crate", "add CI pipeline"
- `Blockers` — is compiled list of **exact actions** needed
  eg: "wait for upload", "pay for claude code", "ensure button is reactive"

### Milestone
this is the primitive through which the targetting of all projects happens. 

may or may not contain any number of issues. Unlike issues, they have inherent time component, - deadlines can be set on it.

no priority ordering exists within it. All tasks contained are assumed to be necessary for completion of the objective.

can have associated discussions/dumps with unstructured thoughts on the subject.

### Issue
fundamental pieces of scoped work. This is the level of precision, that day-to-day planning operates with.

A completed issue (`[x]`) never has an open descendant: a user edit producing one is rejected, so completing a subtree means closing each of its nodes in the same save. Other close reasons (not planned, duplicate) carry no such constraint. Remote state violating it (GitHub allows it) is pulled as-is; the next edit of that tree must resolve it.

Dropping a child's link from its parent detaches it, on either side: the sub-issue relation is gone, and the child lives on as a top-level issue.

Labels follow the scheme `v_flakes/github/labels.nix` puts on every repo (`t:` type, `i:` importance 0–9, `h:` Fibonacci hours, `c:` categories, `ext:`); they are stored as the strings GitHub holds, and `tedi_core::labels` reads them typed; their colours are read from GitHub with the repo's labels, on every pull. An edit may not leave two values of `t:`, `i:` or `h:`, nor add a label its repo lacks — rejected before anything is written. Remote state breaking either is pulled as-is.

An issue's properties are **inherent** — what GitHub holds, edited only through the sync — or **extended**: tedi's own, kept beside the store (`.states.json` per project), never on GitHub, following `!mv`. The one extended kind is a workflow state.

A **workflow** (`tedi_core::workflow`) is data a team declares in one self-contained file: an ordered tree of buckets (a state is a leaf; "forward" is preorder), each bucket's `on_enter` hooks, an intake (where an issue absent from the store sits), and rules. tedi ships presets (`kanban`, `pull_requests`) in the same format; a team forks one by copying it. Nothing in the framework knows a particular workflow.
```
drop on a column / tedi move ─┐
pull stores an issue ─► rules ┴► move(link, workflow, to, seen, mover)
                                   ├ seen ≠ held ⇒ nothing lands
                                   ├ on_enter of each bucket entered, outermost first:
                                   │   all edits, made against the card shown — or a refusal
                                   ├ edits → the field-edit sync (a clash ⇒ nothing lands); none on a PR
                                   └ state written, under the store lock
```
A pull tries each workflow's rules in order on every issue and pull request it stores, making at most one move per issue per workflow. Hooks are builtins or shared libraries over a C ABI ([`tedi_hooks`](../tedi_hooks/README.md)), loaded at startup from the operator's config. Rank (`tedi_core::rank`) orders open issues so a blocker sorts above what it blocks, importance flowing up the `blocked by` graph. 

### Blockers
blockers form a definitive ordering, covering the tasks necessary for completion of a given objective.
Note that the `Blockers` list is not a superset of the underlying Task Objectives, - Blocker is only reasoning about the parts we are aware about and have had sorted explicitly. For example, - a blocker sequence can easily end in an `- evaluate the path, given grown knowledge` item. Thus we keep them distinctly separate from the success-criterions asosciated with the task they have been added on.

blockers go in depth and in breadth: a given item can have definite items its implementation is directly blocked by, but it can also still have same-level contemporaries, which are not directly related to its completetion, but whose hanging state is invalidating any utility/possibility of completing our task in the first place:
```md
objective: good times

# Blockers
- go on a cruise
- buy a yacht
  - find a seller 
  - find the best model
    - call the guy who knows about them
    - compile importance ranking of features
- make enough money
```

> NB: read *bottom* -> *top*, not the other way around. The current task is the one at the bottom, and gets `pop`ed once finished.

An item that is just an issue ref is a GitHub `blocked by` link: every one the blockers name is linked there, and dropping it unlinks it. A link made on GitHub alone joins the blockers at the top (last in sequence), written as the shortest ref from the issue's own repo. The link set GitHub held at the last sync is persisted with the issue, so a link removed on GitHub since is a remote edit of the blockers: the ref leaves them through the same merge as any field (conflicting if they were edited locally too). Either end of a link missing locally is pulled, with the whole tree it lives in.

### Composition Rules
how the system components interact with each other.

firstly, nothing can contain [Sprints](#sprints), - if it is present, it's always the top level primitive, encompassing all the others. Can contain other [Milestones](#Milestone) and [Issues](#issue).
Same thing goes for [Blockers](#blockers) pin.

[Milestones](#milestone) contain exclusively [Issues](#issue).
For normal Milestones, we choose to prohibit explicit inclusion of other [Milestones](#milestone), - as we're reasoning about state, thus most milestones implicitly depend on each other already, - allowing drawing explicit connections would open the door for inconsistencies. One semi-explicit embed is allowed, - but it's done more so through shape: `v1.0` milestone is likely a pre-requisite of `v1.1`, `v1.2` and so on.

[Issues](#issue) mostly own other issues, or just plaintext points of what needs to happen. Most often, as soon as the work starts, the Blockers become apparent.

[Blockers](#blockers)  are only ever owned by Issues, as the Milestones and Milestone-based primitives assume self-contained selections, - thus all their parts are effectively blockers; and the potential case of each one being blocked by the same factor is an exception, and framework doesn't optimize for it. 

### TaskView
a primitive exposing a view into a slice of task space. Can contain Milestones and Issues (never blockers).
Absorbs the parsing/serialization AST: a markdown document whose `#`..`######` headers partition it into sections (`BTreeMap<header-path, Vec<TaskItem>>`), each holding recursively-nested components (`TaskItem` = `Milestone(MilestoneRef) | Issue(IssueRef) | Virtual(text)`). Expansion — replacing an issue ref with the issue's full `Display` — is a render concern (`render(expansions)`); collapse turns any expanded component back into a bare link.

Many user-facing interfaces are implemented at this level, - Sprints, Bottlenecks, Searches (eg all issues with `bug` label), etc

#### Sprints
a selection of work, to be done in the given time period.

here we have a series of durations, like `1d`, `2w`, `1Q`, `1y` etc.
This is less so a fundamental building component of the system, and rather a view into it, - our compiled selection of the tasks for each period.
They assume top-down priority ordering, - as we admit impossibility of consistent 100% completion of the period's selection.

When Github is reached depends on the sync mode:
- **Full** (default): nothing reaches an editor before it has been reconciled with Github: the sprint's own body, every milestone it inlines, and every issue it expands are pulled first. The buffer is what an edit is reasoned against.
- **On exit** (`--sync-on-exit`, what `tme` runs): the editor opens on local state; Github is reached only once it closes. Only what is missing locally is fetched before; after, every issue it expanded is pulled, edited or not. Closing it untouched still lands what a prior run left unsynced (local past consensus).
- **Offline**: never.

A pending conflict is always reconciled against a fresh pull, Offline included; its resolution becomes consensus.

One writer at a time: from a merge to its consensus commit, a writer holds the store lock (a flock on `issues/.lock`, behind an in-process mutex), so the CLI and a server never interleave; consensus is read under it, and every consensus commit asserts it.

A conflict is data: `Conflict { link, clashes: [{ node, field, consensus, local, remote }], files }`. Each clashing node is recorded to its own file under `issues/.conflicts/{owner}/{repo}/`, named by lineage (`1_5.md`), both sides between markers; consensus never commits it. It blocks only that line — the node, its ancestors and descendants — and removing the markers is the resolution.

A buffer is folded back against the state it was rendered from, never against a pull made after it. A field changed on both sides since consensus is a conflict, never a timestamp race: neither edit was made knowing of the other. One changed on one side only is that side's, timestamps aside: GitHub can change a field without a newer one.

A field edit (`Modifier::Field`: what the web board and `tedi set` send) carries the value it was made against. If, after the pull, the field moved off it to anything but the same new value, the edit comes back as a `Conflict` naming that field, and nothing is written or recorded: the store holds no divergence, so nobody else's line is blocked. Answering it is another edit, made against the value it clashed with.

A sprint stores links only, so an issue expanded into an edit buffer has no second home there: its block is sliced back out of the buffer verbatim and committed whole through the same `VirtualIssue::parse` its own file goes through (`Modifier::Write`) — body, state, labels, comments and blockers alike. A block that doesn't compose, or whose edit is refused, is never dropped silently: that issue stays as stored, the buffer goes to rejected-changes with a warning naming it, and the rest of the save lands.

`!mv [owner/]repo` on an issue's line sends it to another repo on save, its subtree with it; a moved child detaches from its parent. Every stored reference follows it, archived milestones aside. Across owners GitHub can't transfer, so the tree is recreated there and the originals closed as duplicates pointing at it.

An unlinked `- [ ] task` becomes an issue on save. Its checkbox may name where: `- [repo] task` (owner of its milestone, else the current user) or `- [owner/repo] task`; unnamed, a milestone's task lands in the milestone's repo, any other turns virtual. Listing existing issues directly under it makes it their parent: it is created in their project, and they become its sub-issues. Without the checkbox it stays a plain-text category header.

Normal sprints store their `TaskView` in the GitHub milestone `description` (keyed by timeframe). On rollover (the first online edit past its due date) the period is archived as a closed `YYYY/MM/DD_{tf}` milestone. An issue closed by the time a sprint is opened leaves it: not shown, and dropped from the stored body with that save. **Urgent** is the special lowest-precision sprint: stored locally (`issues/urgent.md`, no GitHub sync), section-less, and free to hold plain-text items alongside issue refs; milestone refs are rejected on edit. It prunes on every processing rather than on rollover: closed links are dropped (the file is deleted if nothing else remains) — deferred while an edit session holds the urgent lock. Plain-text items are never cleanup fodder.

##### Selection
`Selected` holds one selected issue per sprint (`sprints_selection.json`); the *active* selection is that of the lowest existing sprint (urgent while it has an open issue, else `1d`, …). `sprints select` moves it: `--up`/`--down` step among the selection's siblings — circular, skipping issues whose current blocker delegates to another issue (a blocker ref) — and at the root walk only the day's working set (`# Must` plus the section under it); `--in`/`--out` descend into / ascend out of the tree; a [search](#search) pattern (or the picker) reaches the whole view. `--prev`/`--next` walk back and forth through the sprint's selection history (every explicit selection change is recorded; entries no longer open in the view are skipped). When the selection closes or leaves the view it auto-advances to the top open item; when no open issues remain the selection is cleared and the next sprint up becomes active. An issue not stored locally is unknown, not closed: selecting, resolving or validating onto one errors, naming what to fetch (movement alone skips it with a notice, to stay usable offline).
Header sections tedi assigns meaning to are the `ManagedSection` variants — currently just `# Must`, the day's musts, whose completion `sprints selected list` reports above its blockers.
The selected issue is where day-to-day blocker work lands: `sprints selected add/pop/set/list/open` route through the issue coordinator (`modify_and_sync_issue` + `Modifier::Blocker*`) — blockers keep zero runtime, the issue owns save/sync. `sprints selected resume/halt` drive a per-issue Clockify timer (keyed by the selected `repo#number`, description = title + current blocker); a selection change restarts it on the new issue.

#### Views
A view is a named board query (`tedi_core::board`: `repo:` `assignee:` `state:<workflow>/<bucket>` (holding its sub-buckets) `t:` `i:` `h:` `is:` `label:`, free text through [search](#search)) with a grouping and an order, laid out as columns. A card is an issue or a pull request (`is:issue`, `is:pr`). Each clause takes a `,` list of values, any of which holds; `-key:` holds none of them; `none` is the value unset (`t:none`, `i:none`). An order is a list of keys (`importance,hours`), each ordering only what those before it tie on, `-` reversing one (`-updated`); unset sorts last either way. Grouped by `state:<workflow>`, every leaf is a group, empty ones included: the kanban, where a drop on a column is a move. The query string is the whole of a view's narrowing: the board's quick filters (a facet's values with their counts, a click on a card's value) only rewrite it. `tedi view` opens one as a task view — one `#` section per group, its issues expanded (pull requests left out) — through the sprint flow's expand-and-fold-back; it stores nothing, so only the issues it expanded change. The web board runs the same query in the browser.

A scope is a named slice of repos, nesting narrower ones; it narrows, and never chooses which views exist — every view is offered in every scope. The team's are config; a viewer keeps their own beside them, never under a team scope's name. The web screen sits in one scope at a time, everything on it — facet counts included — narrowed by the scope's `repo:` clause.

#### Bottlenecks
a primitive over Milestones; name is immutable, can contain other milestones. In many ways it's much like [sprints](#sprints), - except it reasons in terms of current global priorities, serving as guideline for compiling the latter.

Bottlenecks are represented with a single Milestone, listing the necessary constituents for getting it resolved.
If a milestone technically covers a wider area of tasks, that could be added under its umbrella, but are not critical to its completion, - they should probably be aggregated under a secondary one, or not aggregated at all. Note that not being owned by any milestone, doesn't prevent the Issue from being includable in [Sprints](#sprints).

## Crates
```
tedi_md → tedi_core → tedi_task_primitives → tedi_task_operations → tedi_web (server) → tedi
            ↑  │ │  ↑            ↑    ↑            ↑                                     ↑
   tedi_utils ─┘ │  │   tedi_adapters ┘    └ tedi_utils  │                                 tedi_eval
                 │  └─────────────→ tedi_hooks ─────────┘
                 └───────────────────────────────────────→ tedi_web (client, wasm)
```
- `tedi_md` — markdown primitives: owned pulldown_cmark `Events` ⇄ `String`.
- `tedi_core` — the pure model: the primitives above (Issue/Blockers/Milestone), their locators/markers, and parse/serialize over `Events`. No IO, no async, no transport. A primitive cannot reach fs/network/app-state — the crate boundary enforces it. It builds for wasm (the web client runs the board model and reads conflicts): `v_utils` with `macros` only. Its `board` is the board model — cards, query, views, grouping — and `workflow` the workflow tree; `tedi_task_operations::board` cards the store and edits through the sync, `tedi_task_operations::workflow` moves cards.
- `tedi_hooks` — workflows as run: a workflow file parsed and checked, its hooks loaded (builtins, or C-ABI libraries) and called. A hook only asks; what lands is the operations' call.
- `tedi_adapters` — transport at the edge: `GithubClient`, Clockify. Depends on core for the domain locator (`RepoInfo`).
- `tedi_task_primitives` — what a stored issue *is*: local/remote sources+sinks, storage layout, path resolution, the `LazyIssue` loading protocol, the GitHub mock.
- `tedi_task_operations` — what we *do* with issues: sync/merge/touch/conflict, sprint flows + per-sprint selection, per-issue Clockify.
- `tedi_eval` — performance evaluation and manual-stats tracking.
- `tedi_utils` — leaf utilities on plain data, no tedi deps. `search`: the one engine every issue search goes through ([below](#search)), and its skim picker.
- `tedi_web` — the board in a browser (dioxus). The client (wasm, `--features web`, built by `dx`) holds every card and runs `tedi_core::board` over them; the server half (`--features server`, linked into `tedi web`) is server fns over `tedi_task_operations`, a GitHub App webhook, the periodic pull of `watch`, and the client bundle. An edit is a `Modifier::Field` made against what the card showed, a drop a move made against the state it showed; changes reach open boards as server-sent ticks. Each viewer (the user a gate names in `--viewer-header`, else the instance's own GitHub user) has a library beside the store, never in it (`$XDG_DATA_HOME/tedi/web/{viewer}.json`): what their screen shows (tiles and columns per band, each view as last edited and whether its columns are locked), saved as it settles, and the open workspace kept current with it; and the filters, workspaces, scopes and column layouts (arrangement and tints) they keep. The team's are config — `board.toml`, read by the flake — read-only on the board; an object's versions are the commits of that file changing it. Every move between team, library, screen and another viewer is a copy remembering its upstream, so either side shows how far the two moved apart:
  ```
  board.toml ──Fork──► library ──Open──► screen
       ▲                │   ▲               │
       └─Propose (a PR)─┘   └──Save / autosave
  library ──share link──► another viewer: a pinned version, read-only until forked
  ```
- `tedi` — interface only: clap enums/dispatch, config, shell init. Parses args, resolves config, calls ops. Owns `config` (v_utils `LiveSettings` binds app identity to the crate that derives it).

## Search
Every issue search — `open`, `sprints select`, `sprints search`, `open --touch` — goes through `tedi_utils::search`. A candidate is a title, its context (containing scopes, shown before it) and a body; the `-e` count picks the kind.
- **Fuzzy** (default) / **Exact** (`-e`; `'x` flips a term back to fuzzy) share one grammar: whitespace-separated terms AND (`\ ` escapes a space), a lone `|` ORs its neighbours; `'x` substring, `'x'` whole word, `^x` prefix, `x$` suffix, `^x$` equality, anchored to the field tested. `!x` excludes on the own title, `!!x` on any field. Smartcase per term.
- **Regex** (`-ee`) / **RegexLine** (`-eee`, anchored): one regex over the displayed row; the body is not consulted. An invalid regex on the CLI is an error.

Ranking: each term takes its best hit by (contiguous over fuzzy — a scatter is a typo, not a hit; then field: title > context > body; then quality: whole word > word prefix > substring; then matcher score); word boundaries are string edges and non-alphanumerics. An item's score is the sum over its terms, so more terms in the title wins; ties keep input order. The body is never fuzzy-matched — a fuzzy term tests it as a substring. Selection maps back by index, never by display string.
A pattern resolves without the picker when one item matches, or when exactly one item has every term contiguous in its row (title or context — not body).

## Sources

### Remote

GitHub is reached through one module in `tedi_task_primitives` — `LazyIssue<RemoteSource>` in, `Sink<Remote>` out, for both issues and milestones. Field by field, in both directions: [`tedi_task_primitives/remote.typ`](../tedi_task_primitives/remote.typ).

tedi acts as `github_token`'s user or as a `github_app`; the App reaches each account through that account's installation, each with its own rate budget, and edits as its bot (`<slug>[bot]`).

`tedi pull [owner|owner/repo]…` (the config's `watch` when none are named) stores every issue those hold: one not stored yet arrives with its whole tree, one stored is pulled through the same merge as any other sync. It lists a repo a page of whole issues per GraphQL call, least recently updated first, and keeps the update time of the last page it stored whole: the next pull, or one picking up after the rate limit cut it short, lists from there. A clash it meets is recorded as a conflict and doesn't hold the cursor back.

A pull request is the *how* of the issues it closes: no blockers, no children, never edited by tedi. Pull mirrors a repo's into `.prs.json` beside its issues, newest first down to the newest the last whole walk stored (GraphQL lists them by no `since`); the board cards them read-only.

### Local

Each issue is one file under `issues/{owner}/{repo}/…`; a node with children is a directory `{n}_-_{title}/` holding `__main__.md` plus one file per child. `impl Display for Issue` is the single rendering — the exact bytes on disk, in the editor, and embedded in a sprint: title line (`- [ ] (labels, @assignees) Title`) · body · comments (folded) · `# Blockers` · child issues as `[Title](./rel.md)` links. One level only — a child is a link, and the subtree is loaded from the child files (`LazyIssue<Local>`), never from the buffer. Editing opens the real file in place; `VirtualIssue::parse` is the inverse of `Display` (a link parses to a shallow child).
