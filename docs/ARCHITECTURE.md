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

A sprint stores links only, so an issue expanded into an edit buffer has no second home there: its block is sliced back out of the buffer verbatim and committed whole through the same `VirtualIssue::parse` its own file goes through (`Modifier::Write`) — body, state, labels, comments and blockers alike. A block that doesn't compose fails the edit rather than being dropped.

Normal sprints store their `TaskView` in the GitHub milestone `description` (keyed by timeframe). **Urgent** is the special lowest-precision sprint: stored locally (`issues/urgent.md`, no GitHub sync), section-less, and free to hold plain-text items alongside issue refs; milestone refs are rejected on edit. Once every issue in it is closed, the closed links are pruned (the file is deleted if nothing else remains) — deferred while an edit session holds the urgent lock. Plain-text items are never cleanup fodder.

##### Selection
`Selected` holds one selected issue per sprint (`sprints_selection.json`); the *active* selection is that of the lowest existing sprint (urgent while it has an open issue, else `1d`, …). `sprints select [pattern|--next|--prev]` moves it — circular, skipping issues whose current blocker delegates to another issue (a blocker ref). `--next`/`--prev` walk only the day's working set (`# Must` plus the section under it); pattern/fzf reaches the whole view. When the selection closes or leaves the view it auto-advances to the top open item; when no open issues remain the selection is cleared and the next sprint up becomes active.
Header sections tedi assigns meaning to are the `ManagedSection` variants — currently just `# Must`, the day's musts, whose completion `sprints selected list` reports above its blockers.
The selected issue is where day-to-day blocker work lands: `sprints selected add/pop/set/list/open` route through the issue coordinator (`modify_and_sync_issue` + `Modifier::Blocker*`) — blockers keep zero runtime, the issue owns save/sync. `sprints selected resume/halt` drive a per-issue Clockify timer (keyed by the selected `repo#number`, description = title + current blocker); a selection change restarts it on the new issue.

#### Bottlenecks
a primitive over Milestones; name is immutable, can contain other milestones. In many ways it's much like [sprints](#sprints), - except it reasons in terms of current global priorities, serving as guideline for compiling the latter.

Bottlenecks are represented with a single Milestone, listing the necessary constituents for getting it resolved.
If a milestone technically covers a wider area of tasks, that could be added under its umbrella, but are not critical to its completion, - they should probably be aggregated under a secondary one, or not aggregated at all. Note that not being owned by any milestone, doesn't prevent the Issue from being includable in [Sprints](#sprints).

## Crates
```
tedi_md → tedi_core → tedi_task_primitives → tedi_task_operations → tedi
              ↑                 ↑                                     ↑
        tedi_adapters ──────────┘                                 tedi_eval
```
- `tedi_md` — markdown primitives: owned pulldown_cmark `Events` ⇄ `String`.
- `tedi_core` — the pure model: the primitives above (Issue/Blockers/Milestone), their locators/markers, and parse/serialize over `Events`. No IO, no async, no transport. A primitive cannot reach fs/network/app-state — the crate boundary enforces it.
- `tedi_adapters` — transport at the edge: `GithubClient`, Clockify. Depends on core for the domain locator (`RepoInfo`).
- `tedi_task_primitives` — what a stored issue *is*: local/remote sources+sinks, storage layout, path resolution, the `LazyIssue` loading protocol, the GitHub mock.
- `tedi_task_operations` — what we *do* with issues: sync/merge/touch/conflict, sprint flows + per-sprint selection, per-issue Clockify.
- `tedi_eval` — performance evaluation and manual-stats tracking.
- `tedi` — interface only: clap enums/dispatch, config, shell init. Parses args, resolves config, calls ops. Owns `config` (v_utils `LiveSettings` binds app identity to the crate that derives it).

## Sources

### Remote

GitHub is reached through one module in `tedi_task_primitives` — `LazyIssue<RemoteSource>` in, `Sink<Remote>` out, for both issues and milestones. Field by field, in both directions: [`tedi_task_primitives/remote.typ`](../tedi_task_primitives/remote.typ).

### Local

Each issue is one file under `issues/{owner}/{repo}/…`; a node with children is a directory `{n}_-_{title}/` holding `__main__.md` plus one file per child. `impl Display for Issue` is the single rendering — the exact bytes on disk, in the editor, and embedded in a sprint: title line · body · comments (folded) · `# Blockers` · child issues as `[Title](./rel.md)` links. One level only — a child is a link, and the subtree is loaded from the child files (`LazyIssue<Local>`), never from the buffer. Editing opens the real file in place; `VirtualIssue::parse` is the inverse of `Display` (a link parses to a shallow child).
