#set page(width: 297mm, height: auto, margin: 14mm)
#set text(size: 9.5pt)
#set par(justify: false)
#show raw.where(block: true): it => block(
  fill: luma(248),
  inset: 8pt,
  radius: 3pt,
  width: 100%,
  text(size: 7.6pt, it),
)
#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node

= `remote` — the GitHub boundary

Read off `src/remote/{mod.rs, milestone.rs}`, `src/lazy.rs`, `src/sink.rs`, and the client trait in
`tedi_adapters/src/github.rs`. The census in §4 is counted off the module at render.

Everything GitHub knows about a task enters and leaves through this module. `tedi_core` holds the
model and never learns the transport; `tedi_task_operations` decides *when* to cross, and hands
this module a body it has already resolved.

== 1. Inbound — `LazyIssue<RemoteSource>`

```
GITHUB                                       │  what the boundary makes of it
─────────────────────────────────────────────┼───────────────────────────────────────────────────
GET issues/{n}                               │
    title, labels                            │  IssueContents.title / .labels
    state         "open" | "closed"          │  ┐ CloseState::from_github
    state_reason  completed | not_planned |  │  ┘   → Open | Closed | NotPlanned
                  duplicate                  │
    body          Option<String>             │  split_blockers → (body text, Blockers)
                                             │    `# Blockers` is no GitHub concept: it rides the
                                             │    tail of the body, is cut here, and `body()`
                                             │    reattaches it on the way out. The section has to
                                             │    be the tail for that round trip to close.
    user.login                               │  LinkedIssueMeta.user
GET issues/{n}/comments                      │  comments[1..], CommentIdentity::Created { user, id }
                                             │    comments[0] is always the body
GET issues/{n}/sub_issues                    │  children, recursively, each through this same path.
                                             │    A `duplicate` state_reason is dropped here, so a
                                             │    duplicate never enters the tree at all.
GQL timeline                                 │  IssueTimestamps { title, description, labels, state }
                                             │    REST carries one `updated_at` for the whole issue
                                             │    while the merge resolves field by field, so the
                                             │    stamps are read off the timeline instead
    comment.updated_at ?? created_at         │  IssueTimestamps.comments[i]
```

```
Three methods, so a caller pays for what it reads:

  parent_index(source)   lineage → IssueIndex. A `None` lineage walks `fetch_parent_issue` to the
                         root; a child source pushes its parent's number, so descending the tree
                         never walks back up.
  identity(&mut self)    memoized on `is_linked()`        ┐ both re-fetch the issue — each is an
  contents(&mut self)    memoized on `title.is_empty()`   ┘ entry point in its own right
  children(&mut self)    memoized on `!children.is_empty()`
  load(source)           all three, in that order

`RemoteSource::build` probes for `gh` before any of it, so a missing executable fails at
construction rather than mid-tree.
```

#align(center, diagram(
  spacing: (13mm, 9mm),
  node-corner-radius: 2pt,
  node-stroke: 0.7pt,
  label-size: 7.5pt,

  node((0, 0), align(center)[GitHub \ #text(7pt)[REST + GraphQL]], fill: luma(235), name: <gh>),

  node((-1.4, 1.2), [`identity()`], name: <id>),
  node((0, 1.2), [`contents()`], name: <cont>),
  node((1.5, 1.2), [`children()`], name: <kids>),

  edge(<gh>, <id>, "->", label: [issue + timeline], label-pos: 0.7, label-side: left),
  edge(<gh>, <cont>, "->", label: [and comments], label-size: 7pt),
  edge(<gh>, <kids>, "->", label: [sub-issues], label-size: 7pt),

  node((-1.4, 2.4), align(center)[`IssueLink` \ `IssueIndex`], name: <link>),
  node((0, 2.4), align(center)[`CloseState::from_github` \ `split_blockers`], name: <pure>),
  node((1.5, 2.4), align(center)[recurse \ #text(7pt)[`duplicate` dropped]], name: <rec>),

  edge(<id>, <link>, "->"),
  edge(<cont>, <pure>, "->"),
  edge(<kids>, <rec>, "->"),
  edge(<rec>, <gh>, "->", bend: 40deg, label: [per child], label-size: 7pt),

  node((0, 3.6), align(center)[`Issue`], fill: luma(235), name: <issue>),
  edge(<link>, <issue>, "->"),
  edge(<pure>, <issue>, "->"),
  edge(<rec>, <issue>, "->"),
))

== 2. Outbound — `Sink<Remote>`

```
sink(&mut self, old: Option<&Issue>)

  `old` is the state of the last pull. Every field compares against it, so a field the user did
  not touch makes no call.

  is_virtual            ─▶ Ok(false)                     local-only, never crosses
  is_local  (pending)   ─▶ POST issues                   creation comes first: everything under it
                           POST labels        if any     needs the number, and the child sink needs
                           PATCH state        if closed   it to root the children's parent_index
                           POST sub_issues    if parented
                           identity becomes linked
  body() != old.body()  ─▶ PATCH issues/{n} { body }     the whole body, text and blockers together
  labels                ─▶ PATCH issues/{n} { labels }
  to_github_state()     ─▶ PATCH issues/{n} { state }    compared at GitHub's granularity, so the
                                                         local Closed / NotPlanned split does not
                                                         travel
  comments, pending     ─▶ POST comments, one at a time  sequential: GitHub orders by creation
  comments, changed     ─▶ PATCH comments/{id}           skipped unless the current user wrote it
  comments, dropped     ─▶ DELETE comments/{id}
  children              ─▶ recurse against old.children[selector]

  `title` and `state_reason` have no call on this path — the client trait carries none.
```

#align(center, diagram(
  spacing: (15mm, 9mm),
  node-corner-radius: 2pt,
  node-stroke: 0.7pt,
  label-size: 7.5pt,

  node((0, 0), align(center)[`Issue`], fill: luma(235), name: <issue>),
  node((-1.7, 0), align(center)[`old`\ #text(7pt)[last pull]], name: <old>),

  node((0, 1.1), align(center)[`compute_node_diff` \ #text(7pt)[with `body()`, `to_github_state()`]], name: <diff>),
  edge(<issue>, <diff>, "->"),
  edge(<old>, <diff>, "->"),

  node((-1.7, 2.3), align(center)[create \ #text(7pt)[pending node]], name: <create>),
  node((-0.2, 2.3), align(center)[fields \ #text(7pt)[body · labels · state]], name: <fields>),
  node((1.3, 2.3), align(center)[comments \ #text(7pt)[create · update · delete]], name: <cmt>),
  node((2.8, 2.3), align(center)[children], name: <kids>),

  edge(<diff>, <create>, "->", label: [`is_local`], label-size: 7pt),
  edge(<diff>, <fields>, "->"),
  edge(<diff>, <cmt>, "->"),
  edge(<diff>, <kids>, "->"),
  edge(<create>, <fields>, "->", bend: -30deg, label: [number], label-size: 7pt),
  edge(<kids>, <diff>, "->", bend: -40deg, label: [recurse], label-size: 7pt),

  node((0.5, 3.5), align(center)[GitHub], fill: luma(235), name: <gh>),
  edge(<create>, <gh>, "->"),
  edge(<fields>, <gh>, "->"),
  edge(<cmt>, <gh>, "->"),
))

== 3. Milestones

```
A milestone crosses the same boundary with a smaller shape. The GitHub `description` IS the body,
and the hosted issue set is the `milestone` field carried by each issue.

  in   get_milestone.description  ──parse──▶  MilestoneBody, a TaskView
       list_milestone_issues      ──push_issue_links──▶ folded into that same body, so an issue
                                    assigned upstream still resolves when offline
       updated_at                 ──▶  all four stamps at once: GitHub exposes no per-field
                                       milestone timestamp

  out  description  ──▶ PATCH milestones/{n}           when the body or the due date moved
       hosted set   ──▶ PATCH issues/{n} { milestone } diffed against `old.hosted()`:
                                                       new ⇒ assign, gone ⇒ unassign

Only issues in the milestone's own repo can carry it, so a hosted link pointing elsewhere is
description-only. The body reaching this sink is already reconciled — the three-way merge that
decides what a deletion means lives in `tedi_task_operations::open_interactions::milestone`.
```

== 4. Client surface

// counted off the module, so an endpoint that appears or goes away shows up here on its own
#let files = ("src/remote/mod.rs", "src/remote/milestone.rs")
#let uniq(a) = a.fold((), (acc, x) => if x in acc { acc } else { acc + (x,) })
#let calls(f) = read(f).matches(regex("\\bgh\\s*\\.\\s*(\\w+)\\s*\\(")).map(m => m.captures.first())
#let per-file = files.map(f => calls(f))
#let names = uniq(per-file.flatten()).sorted()

#table(
  columns: (auto, auto, auto),
  stroke: 0.4pt + luma(180),
  align: (left, right, right),
  table.header([*`GithubClient` method*], [`issues`], [`milestones`]),
  ..names.map(n => (raw(n), ..per-file.map(cs => [#cs.filter(c => c == n).len()]))).flatten(),
)
