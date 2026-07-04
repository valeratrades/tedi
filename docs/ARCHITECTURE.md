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

Many user-facing interfaces are implemented at this level, - Sprints, Bottlenecks, Searches (eg all issues with `bug` label), etc

#### Sprints
a selection of work, to be done in the given time period. 

here we have a series of durations, like `1d`, `2w`, `1Q`, `1y` etc.
This is less so a fundamental building component of the system, and rather a view into it, - our compiled selection of the tasks for each period.
They assume top-down priority ordering, - as we admit impossibility of consistent 100% completion of the period's selection.

#### Bottlenecks
a primitive over Milestones; name is immutable, can contain other milestones. In many ways it's much like [sprints](#sprints), - except it reasons in terms of current global priorities, serving as guideline for compiling the latter.

Bottlenecks are represented with a single Milestone, listing the necessary constituents for getting it resolved.
If a milestone technically covers a wider area of tasks, that could be added under its umbrella, but are not critical to its completion, - they should probably be aggregated under a secondary one, or not aggregated at all. Note that not being owned by any milestone, doesn't prevent the Issue from being includable in [Sprints](#sprints).

## Crates
```
tedi_md → tedi_core → tedi_ops → tedi
              ↑           ↑
        tedi_adapters ────┘
```
- `tedi_md` — markdown primitives: owned pulldown_cmark `Events` ⇄ `String`.
- `tedi_core` — the pure model: the primitives above (Issue/Blockers/Milestone), their locators/markers, and parse/serialize over `Events`. No IO, no async, no transport. A primitive cannot reach fs/network/app-state — the crate boundary enforces it.
- `tedi_adapters` — transport at the edge: `GithubClient` (+ mock lives with ops), Clockify. Depends on core for the domain locator (`RepoInfo`).
- `tedi_ops` — operations over the primitives: local/remote sources+sinks, sync/merge/touch/conflict, blocker stack, sprint flows, the `LazyIssue` loading protocol.
- `tedi` — interface only: clap enums/dispatch, config, shell init. Parses args, resolves config, calls ops. Owns `config` (v_utils `LiveSettings` binds app identity to the crate that derives it).

## Sources

### Remote

#### Reading from GitHub → Issue

Uses `LazyIssue<Remote>` trait implemented in `remote/mod.rs`:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           GITHUB API RESPONSE                               │
├─────────────────────────────────────────────────────────────────────────────┤
│  GithubIssue {                                                              │
│    number: u64,                                                             │
│    title: String,                                                           │
│    body: Option<String>,                                                    │
│    labels: Vec<GithubLabel>,                                                │
│    user: GithubUser,                                                        │
│    state: String,           // "open" | "closed"                            │
│    state_reason: Option<String>,  // "completed" | "not_planned" | "duplicate"
│    updated_at: String,      // ISO 8601                                     │
│  }                                                                          │
│  + Vec<GithubComment> { id, body, user }                                    │
│  + Vec<GithubIssue>  (sub-issues)                                           │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
                    ┌───────────────────────────────┐
                    │   LazyIssue<Remote>           │
                    │   (remote/mod.rs)             │
                    │   - identity()                │
                    │   - contents()                │
                    │   - children()                │
                    └───────────────────────────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        ▼                           ▼                           ▼
┌───────────────────┐   ┌─────────────────────────┐   ┌─────────────────────┐
│ IssueLink.parse() │   │ CloseState::from_github │   │ split_blockers()    │
│ URL → owner/repo/ │   │ (state, state_reason)   │   │ body → (text,       │
│       number      │   │ → Open/Closed/NotPlanned│   │        BlockerSeq)  │
└───────────────────┘   └─────────────────────────┘   └─────────────────────┘
        │                           │                           │
        ▼                           ▼                           ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Issue struct                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│  identity: IssueIdentity {                                                  │
│    ancestry: Ancestry,                                                      │
│    linked: Option<LinkedIssueMeta> { user, link, ts },                      │
│  }                                                                          │
│  contents: IssueContents {                                                  │
│    title: String,                                                           │
│    labels: Vec<String>,                                                     │
│    state: CloseState,                                                       │
│    comments: Vec<Comment>,  // [0] = body (CommentIdentity::Body)           │
│                             // [1..] = comments (CommentIdentity::Created)  │
│    blockers: Blockers,                                                      │
│  }                                                                          │
│  children: Vec<Issue>,      // sub-issues, recursively                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

#### Pushing Issue → GitHub

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              Issue struct                                   │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        ▼                           ▼                           ▼
┌───────────────────┐   ┌─────────────────────────┐   ┌─────────────────────┐
│ close_state       │   │ issue.body()            │   │ children/comments   │
│   .to_github_     │   │ = comments[0].body      │   │ with Pending        │
│    state()        │   │ + blockers section      │   │ identity            │
│   .to_github_     │   │                         │   │                     │
│    state_reason() │   │                         │   │                     │
└───────────────────┘   └─────────────────────────┘   └─────────────────────┘
        │                           │                           │
        ▼                           ▼                           ▼
┌───────────────────┐   ┌─────────────────────────────┐   ┌─────────────────────┐
│ "open"/"closed"   │   │ String (body text           │   │ IssueAction::       │
│ + "completed"/    │   │  with blockers appended)    │   │   CreateIssue {     │
│   "not_planned"   │   │                             │   │     title, body,    │
│                   │   │                             │   │     closed, parent  │
│                   │   │                             │   │   }                 │
└───────────────────┘   └─────────────────────────────┘   └─────────────────────┘
        │                           │                           │
        └───────────────────────────┼───────────────────────────┘
                                    ▼
                    ┌───────────────────────────────┐
                    │   sync_local_issue_to_github  │
                    │   (sync.rs)                   │
                    └───────────────────────────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        ▼                           ▼                           ▼
┌───────────────────┐   ┌─────────────────────────┐   ┌─────────────────────┐
│ gh.update_issue_  │   │ gh.update_issue_body()  │   │ gh.create_issue()   │
│   state()         │   │ gh.create_comment()     │   │ gh.add_sub_issue()  │
│                   │   │ gh.update_comment()     │   │                     │
│                   │   │ gh.delete_comment()     │   │                     │
└───────────────────┘   └─────────────────────────┘   └─────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           GITHUB API CALLS                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│  PATCH /repos/{owner}/{repo}/issues/{number}                                │
│    { "state": "open"|"closed", "state_reason": "...", "body": "..." }       │
│  POST  /repos/{owner}/{repo}/issues/{number}/comments  { "body": "..." }    │
│  POST  /repos/{owner}/{repo}/issues  { "title": "...", "body": "..." }      │
│        → CreatedIssue { id, number, html_url }                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

#### Key Type Locations

| Type | File |
|------|------|
| `Issue`, `IssueContents`, `CloseState`, `IssueIdentity`, `TitleLine`/segmenter | `tedi_core/src/issue.rs` |
| `RepoInfo`, `IssueLink`, `IssueRef`, `IssueIndex`, `IssueSelector` | `tedi_core/src/locate.rs` |
| `Blockers`, `split_blockers()` | `tedi_core/src/blockers.rs` |
| `Milestone`, `serialize_blockers_view()`, `parse_blockers_from_embedded()` | `tedi_core/src/milestone.rs` |
| `IssueMarker`, `Marker` | `tedi_core/src/marker.rs` |
| `GithubIssue`, `GithubComment`, `CreatedIssue`, `GithubMilestone`, `list/create/update_milestone` | `tedi_adapters/src/github.rs` |
| `LazyIssue<S>` trait | `tedi_ops/src/lazy.rs` |
| `LazyIssue<Remote>` impl | `tedi_ops/src/remote/mod.rs` |
| `LazyIssue<Local>` impl, `MilestoneBlockerCache` | `tedi_ops/src/local/mod.rs` |
| `sync_local_issue_to_github()` | `tedi_ops/src/open_interactions/sync.rs` |
| sprint expand/refresh + blocker sync | `tedi_ops/src/sprints.rs` |
