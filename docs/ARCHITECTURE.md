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

may or may not contain any number of issues, and [milestones of other repos](#composition-rules): for a milestone, blocked-on is contains. Unlike issues, they have inherent time component, - deadlines can be set on it.

hosts issues of any repo, and an issue sits in at most one milestone a repo. Its own repo's holds it through GitHub's `milestone` field, any other's by naming it in the description. Remote state putting it in two of one repo is pulled as-is.

no priority ordering exists within it. All tasks contained are assumed to be necessary for completion of the objective.

can have associated discussions/dumps with unstructured thoughts on the subject.

### Issue
fundamental pieces of scoped work. This is the level of precision, that day-to-day planning operates with.

A completed issue (`[x]`) never has an open descendant: a user edit producing one is rejected, so completing a subtree means closing each of its nodes in the same save. Other close reasons (not planned, duplicate) carry no such constraint. Remote state violating it (GitHub allows it) is pulled as-is; the next edit of that tree must resolve it.

Dropping a child's link from its parent detaches it, on either side: the sub-issue relation is gone, and the child lives on as a top-level issue.

Labels follow the scheme `v_flakes/github/labels.nix` puts on every repo (`t:` type, `i:` importance 0–9, `h:` Fibonacci hours, `c:` categories, `ext:`); they are stored as the strings GitHub holds, and `tedi_core::labels` reads them typed; their colours are read from GitHub with the repo's labels, on every pull. An edit may not leave two values of `t:`, `i:` or `h:`, nor add a label its repo lacks — rejected before anything is written. Remote state breaking either is pulled as-is.

An issue's properties are **inherent** — what GitHub holds, edited only through the sync — or **extended**: tedi's own, kept beside the store (`.states.json` per project), never on GitHub, following `!mv`. The one extended kind is a workflow state. An instance holds workflows (`board` in its config) or asks the one that does (`home`, a `tedi web`): with a home no rule runs and no state is read here, and a move, a view and [what is picked up](#what-is-picked-up) are its answers, so a state is told the same everywhere.

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
how the system components own each other. One chain, typed by `tedi_core::chain`: containment, [scopes](#views) and [Bottlenecks](#sprints) are read off it.
```
 Org (main) ─► Org ─► Repo                      place: who holds it
    └──────────────────► Repo ─► Milestone, Issue

 Sprint ─► Bottlenecks ─► Bottlenecks ─► … ─► Milestone ─► Milestone ─► … ─► Issue ─► Issue ─► Blockers      ownership: what it needs
 (person)  └─────── each an org's ───────┘     └─ each of another repo, any org ─┘
```
| owner | may own | never |
|---|---|---|
| [Sprint](#sprints), planning (titled by a timeframe) | Bottlenecks, Milestone, Issue | a planning one |
| [Sprint](#sprints), Bottlenecks (`{org}/.github`, titled `Bottlenecks`) | Bottlenecks, Milestone (its own repo's too), Issue | a planning one; itself, through others too |
| [Milestone](#milestone) | Milestone, Issue | a sprint; a milestone of a repo already on the chain |
| [Issue](#issue) | Issue, [Blockers](#blockers) | any milestone |

Owning is a list item that is a ref; a link in prose owns nothing.

A chain never names one repo twice: within a repo milestones relate through their shape (`v1.0` → `v1.1`). A loop is one such chain. Sprint and Bottlenecks are roots: their own repo doesn't count.

An edit may not introduce a forbidden edge: `open_interactions/milestone.rs` refuses it, drawn, before anything is written — against the milestone before the edit, and on the merge result against GitHub's, as two legal edits can close a loop together. An edge refused only by a merge stays in the local body, unsynced and told, until its milestone is edited. GitHub state holding one is pulled as-is and told, so a walk over the chain must survive a loop. What an edit introduces is known: fetched with everything it contains, under the name GitHub holds it by now, or, offline, an error.

Orgs owning orgs is config (`orgs`, one level: GitHub's can't nest); owning one doesn't pull it.

Blockers are owned by Issues alone: Milestones and Milestone-based primitives assume self-contained selections, so all their parts are effectively blockers.

### TaskView
a primitive exposing a view into a slice of task space. Can contain Milestones and Issues (never blockers).
Absorbs the parsing/serialization AST: a markdown document whose `#`..`######` headers partition it into sections (`BTreeMap<header-path, Vec<TaskItem>>`), each holding recursively-nested components (`TaskItem` = `Milestone(MilestoneRef) | Issue(IssueRef) | Virtual(text)`). Expansion — replacing an issue ref with the issue's full `Display` — is a render concern (`render(expansions)`); collapse turns any expanded component back into a bare link.

Many user-facing interfaces are implemented at this level, - Sprints, what is picked up, Searches (eg all issues with `bug` label), etc

#### Sprints
a collection of musts. What it is a must *by* tells its kind:
- **Planning** (`1d`, `2w`, `1Q`, `1y`…): before the period rolls over.
- **Bottlenecks**: first. The necessary constituents of getting what stands in front of everything else resolved; a guideline for compiling the planning ones. A wider area of tasks that could be added under its umbrella, but is not critical to its completion, is another milestone's, or no one's.

The milestone's title makes it one (`tedi_core::Sprint`); a Bottlenecks sits in its org's `.github` alone, none is created elsewhere, and one found there is an ordinary milestone, told. Acting on a planning one (rollover, `selected.md`) is its repo owner's.

A workflow may name the leaf (`bottlenecks = "selected"`) an open card in the tree of a Bottlenecks stands no earlier than, where the view is grouped by it. Those that press are the scope's: of its org and of the main org owning that; terms' are of the orgs common to all the repos a clause of theirs names; without one, there are none. Closed milestones pass nothing on. It is layout alone: the query runs on held states and the card takes its own place in the order, its blockers following; a drop before the leaf is refused, any other is a move made against the held state.

Everything in a sprint is required: its headers only group, its leading prose is its description, and its completion is over all of it, a milestone it holds being done when closed. They assume top-down priority ordering, - as we admit impossibility of consistent 100% completion of the period's selection.

Opening a planning sprint opens [what is picked up](#what-is-picked-up) and its org's Bottlenecks beside it, a tab each, folded back as one: an issue rendered in several tabs is edited in whichever, its untouched copies reverting nothing.

When Github is reached depends on the sync mode:
- **Full** (default): nothing reaches an editor before it has been reconciled with Github: the sprint's own body, every milestone it inlines, and every issue it expands are pulled first. The buffer is what an edit is reasoned against.
- **On exit** (`--sync-on-exit`, what `tme` runs): the editor opens on local state; Github is reached only once it closes. Only what is missing locally is fetched before; after, every issue it expanded is pulled, edited or not. Closing it untouched still lands what a prior run left unsynced (local past consensus).
- **Offline**: never.

A pending conflict is always reconciled against a fresh pull, Offline included; its resolution becomes consensus.

One writer at a time: from a merge to its consensus commit, a writer holds the store lock (a flock on `issues/.lock`, behind an in-process mutex), so the CLI and a server never interleave; consensus is read under it, and every consensus commit asserts it. A milestone's commit holds its own files alone, and an issue's no milestone: what another left unsynced is not folded into consensus.

A conflict is data: `Conflict { link, clashes: [{ node, field, consensus, local, remote }], files }`. Each clashing node is recorded to its own file under `issues/.conflicts/{owner}/{repo}/`, named by lineage (`1_5.md`), both sides between markers; consensus never commits it. It blocks only that line — the node, its ancestors and descendants — and removing the markers is the resolution.

A buffer is folded back against the state it was rendered from, never against a pull made after it: a milestone's body edit carries the body it was rendered from, and lands over whatever is stored by then. A field changed on both sides since consensus is a conflict, never a timestamp race: neither edit was made knowing of the other. One changed on one side only is that side's, timestamps aside: GitHub can change a field without a newer one.

A field edit (`Modifier::Field`: what the web board and `tedi set` send) carries the value it was made against. If, after the pull, the field moved off it to anything but the same new value, the edit comes back as a `Conflict` naming that field, and nothing is written or recorded: the store holds no divergence, so nobody else's line is blocked. Answering it is another edit, made against the value it clashed with.

The web board shows an edit before it is sent, laid over the server's board; one that doesn't land is taken off it, with whatever was made on top, and said so.

A sprint stores links only, so an issue expanded into an edit buffer has no second home there: its block is sliced back out of the buffer verbatim and committed whole through the same `VirtualIssue::parse` its own file goes through (`Modifier::Write`) — body, state, labels, comments and blockers alike. A block that doesn't compose, or whose edit is refused, is never dropped silently: that issue stays as stored, the buffer goes to rejected-changes with a warning naming it, and the rest of the save lands.

`!mv [owner/]repo` on an issue's line sends it to another repo on save, its subtree with it; a moved child detaches from its parent. Every stored reference follows it, archived milestones aside. Across owners GitHub can't transfer, so the tree is recreated there and the originals closed as duplicates pointing at it.

An unlinked `- [ ] task` becomes an issue on save. Its checkbox may name where: `- [repo] task` (owner of its milestone, else the current user) or `- [owner/repo] task`; unnamed, a milestone's task lands in the milestone's repo, any other turns virtual. Listing existing issues directly under it makes it their parent: it is created in their project, and they become its sub-issues. Without the checkbox it stays a plain-text category header.

A sprint stores its `TaskView` in its GitHub milestone's `description`. On a planning one's rollover (the first online edit past its due date) the period is archived as a closed `YYYY/MM/DD_{tf}` milestone. An issue closed by the time a sprint is opened leaves it: not shown, and dropped from the stored body with that save. **Urgent** is the special lowest-precision sprint: stored locally (`issues/urgent.md`, no GitHub sync), section-less, and free to hold plain-text items alongside issue refs; milestone refs are rejected on edit. It is no collection of musts. It prunes on every processing rather than on rollover: closed links are dropped (the file is deleted if nothing else remains) — deferred while an edit session holds the urgent lock. Plain-text items are never cleanup fodder.

##### What is picked up
`selected.md`, beside a repo's sprints: the issues its owner has open, as the home's kanban has them, one section per bucket, the one configured (`milestones.selected`) under no header. It is a TUI over the kanban, storing nothing of its own there: a save is the diff of its sections against the answer it was rendered from, sent as moves.
```
 line under another header ─ move(seen, to)      link added ─ move(wherever, to)
 line gone ───────────────── move(seen, intake)  text ─────── an issue, then move(wherever, to)
 closed, or `!mv`d ───────── nothing: closing is the home's rule; a moved issue is placed at its new link
```
What is stored is the document and the home's last answer (`.selected.json`), and an unsent move is their difference: an edit offline is replayed by the next one online, before it asks for a new answer. One of the two alone is never fetched, so nothing is moved by it. A refused move leaves its line where the home has it; a home out of reach costs this tab alone.

What `1d` or urgent holds is at least in the configured bucket: each asking of the home moves there what it has further back, and a line of theirs goes no further back than it. What somebody else has started stays theirs.

##### Selection
`Selected` holds one selected issue per sprint (`sprints_selection.json`); the *active* selection is that of the lowest existing sprint (urgent while it has an open issue, else `1d`, …). It is not the kanban's `selected` bucket: a bucket holds what is picked up, this is what is being worked on. `sprints select` moves it: `--up`/`--down` step among the selection's siblings — circular, skipping issues whose current blocker delegates to another issue (a blocker ref) — and at the root walk the sprint, then the lines of `selected.md` at its bucket or any forward of it; `--in`/`--out` descend into / ascend out of the tree; a [search](#search) pattern (or the picker) reaches the whole of both, and its org's Bottlenecks, asking the home first what is picked up by now. Urgent is followed by what is picked up beside the sprint last focused. All else is read from local files: status bars poll it offline. `--prev`/`--next` walk back and forth through the sprint's selection history (every explicit selection change is recorded; entries no longer open in the view are skipped). A milestone stands for the first workable issue it owns, descending into the milestones it contains. When the selection closes or leaves the view it auto-advances to the top open item; when no open issues remain the selection is cleared and the next sprint up becomes active. An issue not stored locally is unknown, not closed: selecting, resolving or validating onto one errors, naming what to fetch (movement alone skips it with a notice, to stay usable offline).
`sprints selected list` reports the sprint's musts and their completion above the selected issue's blockers.
The selected issue is where day-to-day blocker work lands: `sprints selected add/pop/set/list/open` route through the issue coordinator (`modify_and_sync_issue` + `Modifier::Blocker*`) — blockers keep zero runtime, the issue owns save/sync. `sprints selected resume/halt` drive a per-issue Clockify timer (keyed by the selected `repo#number`, description = title + current blocker); a selection change restarts it on the new issue.

#### Views
A query (`tedi_core::board`: `repo:` `assignee:` `state:<workflow>/<bucket>` (holding its sub-buckets) `t:` `i:` `h:` `is:` `label:`, free text through [search](#search)) narrows the cards: issues and pull requests (`is:issue`, `is:pr`). Each clause takes a `,` list of values, any of which holds; `-key:` holds none of them; `none` is the value unset (`t:none`, `i:none`). An order is a list of keys (`importance,hours`), each ordering only what those before it tie on, `-` reversing one (`-created`: oldest first); unset sorts last either way. Whatever the order, an open card blocking one above it in its group, or brought in from outside the query for blocking one there, is shown right above that one. Among a workflow's columns a further step is above: a blocker in an earlier one is shown in the column of what it blocks, its state as held. Grouped by `state:<workflow>`, every leaf is a group, empty ones included: the kanban, where a drop on a column is a move. `tedi view` opens a query as a task view — one `#` section per group, its issues expanded (pull requests left out) — through the sprint flow's expand-and-fold-back; it stores nothing, so only the issues it expanded change.

On the web board, which cards and in what order is the workspace's; how they are boxed is a view's:
```
DYNAMIC, the workspace's, autosaved          STATIC, the library's: kept, forked, shared, proposed
Workspace { tiles per band, lens }           View   { name, group_by, shown, boxes }
Lens      { scope, search, sort }  ◄─copy──  Scope  { repos, terms, children }   a named starting point
   point or terms  text                      Boxes  { arrangement per band, tints, hidden }
                                             Columns{ by, boxes }   laid onto a view: its boxes copied
shown = scope ∧ search ─sort─► cards ─group_by─► boxes − hidden
```
Every tile of a workspace runs its lens; a facet's values and a click on a card's rewrite its search. A workspace's scope is a point of the [chain](#composition-rules), or terms: never both. A point covers what it holds or owns: an org its repos and those of the orgs it owns, a milestone everything it owns transitively, an issue its descendants; one nothing of which is on the board is said so. As text it is its GitHub URL, which no term starts as. A scope has a name only in the library: applied, it is its terms, and editing either later doesn't touch the other. The team's scopes are config; a viewer keeps their own beside them, never under a team scope's name.

A viewer has a workspace ("ws 1") from the first open. Without one open, or with no arrangement of it for a band, nothing is shown where views go.

A state of the web screen worth pointing at is in its address (`tedi_web/src/ui/route.rs`), and a move between such states is a link; picking a scope is an edit of the workspace, so it is none. An id in it names a view, workspace or column layout: a viewer's through renames, the team's by its name. A bare `/` is the workspace last selected.

## Crates
```
tedi_md → tedi_core → tedi_task_primitives → tedi_task_operations → tedi_web (server) → tedi
            ↑  │ │  ↑            ↑    ↑            ↑                                     ↑
   tedi_utils ─┘ │  │   tedi_adapters ┘    └ tedi_utils  │                                 tedi_eval
                 │  └─────────────→ tedi_hooks ─────────┘
                 └───────────────────────────────────────→ tedi_web (client, wasm)
```
- `tedi_md` — markdown primitives: owned pulldown_cmark `Events` ⇄ `String`.
- `tedi_core` — the pure model: the primitives above (Issue/Blockers/Milestone), their locators/markers, and parse/serialize over `Events`. No IO, no async, no transport. A primitive cannot reach fs/network/app-state — the crate boundary enforces it. It builds for wasm (the web client runs the board model and reads conflicts): `v_utils` with `macros` only. Its `chain` is who owns what above the issue tree, `board` the board model — cards, query, order, grouping, scopes — and `workflow` the workflow tree; `tedi_task_operations::board` cards the store and edits through the sync, `tedi_task_operations::workflow` moves cards.
- `tedi_hooks` — workflows as run: a workflow file parsed and checked, its hooks loaded (builtins, or C-ABI libraries) and called. A hook only asks; what lands is the operations' call.
- `tedi_adapters` — transport at the edge: `GithubClient` and whom it acts as, the home, Clockify. Depends on core for what it carries.
- `tedi_task_primitives` — what a stored issue *is*: local/remote sources+sinks, storage layout, path resolution, the `LazyIssue` loading protocol, the GitHub mock.
- `tedi_task_operations` — what we *do* with issues: sync/merge/touch/conflict, sprint flows + per-sprint selection, per-issue Clockify.
- `tedi_eval` — performance evaluation and manual-stats tracking.
- `tedi_utils` — leaf utilities on plain data, no tedi deps. `search`: the one engine every issue search goes through ([below](#search)), and its skim picker.
- `tedi_web` — the board in a browser (dioxus). The client (wasm, `--features web`, built by `dx`) holds every card and runs `tedi_core::board` over them; the server half (`--features server`, linked into `tedi web`) is server fns over `tedi_task_operations`, a GitHub App webhook, the periodic pull of `watch`, and the client bundle. An edit is a `Modifier::Field` made against what the card showed, a comment's change against the body shown, a milestone set against the one its repo's was shown to be, a drop a move made against the state it showed; changes reach open boards as server-sent ticks. A sprint is a target of the board like any open milestone, read from the store; only its own file plans.

  Each viewer brings an account: GitHub tokens by owner (`$XDG_DATA_HOME/tedi/web/accounts/`, never sent back) and the scopes they want stored, pulled as them after the team's `watch`.
  ```
  reach(account) = team watch ∪ the repos its own keys listed on the last pull of its scopes
  credential(account, owner) = its key for owner, else the instance's
  mover(account) = its keys' login; none ⇒ moves refused
  ```
  **Reach** is the privacy boundary: private repos share a store with the team's. Every operation over the board takes what it acts on as `tedi_task_operations::reach` let it in, and the board is read, ranked and cached per reach. The **actor** (client and login) is scoped to the task over the process' own, and decides whose comment an edit is; `current_user` stays the store's identity: stored bytes don't vary by who asked. An ungated instance is its operator's alone, who reaches all. As a [home](#issue) it answers the same calls for a CLI, which passes the gate with a session of its own (`tedi login`).

  Each viewer (the user a gate names in `--viewer-header`, else the instance's own GitHub user) has a library beside the store, never in it (`$XDG_DATA_HOME/tedi/web/{viewer}.json`): the [workspaces, views, scopes and column layouts](#views) they keep. Their own workspaces are written as they are edited, each edit naming its workspace and the one part it sets, so two tabs don't undo each other; a view, and a workspace of the team's, is edited as a draft over the kept one until it is saved or reverted. What a dock shows once arranged is no edit, whatever this window cut or seeded. The server brings what is kept to its own shape as it starts, before it serves any (`tedi_web/src/server/migrate.rs`); the code reading it knows one shape. The team's are config — `board.toml`, read by the flake — read-only on the board; an object's versions are the commits of that file changing it. Every move between team, library, screen and another viewer is a copy remembering its upstream, so either side shows how far the two moved apart:
  ```
  board.toml ──Fork──► library ──Open──► screen
       ▲                │   ▲               │
       └─Propose (a PR)─┘   └──Save; a workspace, as it is edited
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

`tedi pull [owner|owner/repo]…` (the config's `watch` when none are named) stores every issue those hold: one not stored yet arrives with its whole tree, one stored is pulled through the same merge as any other sync. It lists a repo a page of whole issues per GraphQL call, least recently updated first, and keeps the update time of the last page it stored whole: the next pull, or one picking up after the rate limit cut it short, lists from there. A clash it meets is recorded as a conflict and doesn't hold the cursor back. The repo's open milestones, sprints among them, are pulled with it through their own three-way merge, each with the milestones it contains and the issues those host, of whatever repo; so is who its issues can be assigned to.

A pull request is the *how* of the issues it closes: no blockers, no children; whose review it waits on is asked straight on GitHub, and nothing else of it is edited by tedi. Pull mirrors a repo's into `.prs.json` beside its issues, newest first down to the newest the last whole walk stored (GraphQL lists them by no `since`); the board cards them read-only.

### Local

Each issue is one file under `issues/{owner}/{repo}/…`; a node with children is a directory `{n}_-_{title}/` holding `__main__.md` plus one file per child. `impl Display for Issue` is the single rendering — the exact bytes on disk, in the editor, and embedded in a sprint: title line (`- [ ] (labels, @assignees) Title`) · body · comments (folded) · `# Blockers` · child issues as `[Title](./rel.md)` links. One level only — a child is a link, and the subtree is loaded from the child files (`LazyIssue<Local>`), never from the buffer. Editing opens the real file in place; `VirtualIssue::parse` is the inverse of `Display` (a link parses to a shallow child).
