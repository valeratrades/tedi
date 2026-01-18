# Sources

## Remote

### Reading from GitHub → Issue

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
│    blockers: BlockerSequence,                                               │
│  }                                                                          │
│  children: Vec<Issue>,      // sub-issues, recursively                      │
└─────────────────────────────────────────────────────────────────────────────┘
```

### Pushing Issue → GitHub

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
│    state()        │   │ + join_with_blockers()  │   │ identity            │
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

### Key Type Locations

| Type | File |
|------|------|
| `Issue`, `IssueContents`, `CloseState`, `IssueIdentity` | `src/issue/types.rs` |
| `LazyIssue<S>` trait | `src/issue/types.rs` |
| `GithubIssue`, `GithubComment`, `CreatedIssue` | `src/github.rs` |
| `LazyIssue<Remote>` impl, `load_full_issue_tree()` | `src/open_interactions/remote/mod.rs` |
| `LazyIssue<Local>` impl | `src/open_interactions/local/mod.rs` |
| `split_blockers()`, `join_with_blockers()` | `src/issue/blocker.rs` |
| `sync_local_issue_to_github()` | `src/open_interactions/sync.rs` |
