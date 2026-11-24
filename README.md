# tedi
![Minimum Supported Rust Version](https://img.shields.io/badge/nightly-2026--06--29+-ab6000.svg)
[<img alt="crates.io" src="https://img.shields.io/crates/v/tedi.svg?color=fc8d62&logo=rust" height="20" style=flat-square>](https://crates.io/crates/tedi)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs&style=flat-square" height="20">](https://docs.rs/tedi)
![Lines Of Code](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-loc.json)
![Build time](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-build.json)
![Test time](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-test.json)
<br>
[![errors](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-errors.json)](https://github.com/valeratrades/tedi/actions/workflows/errors.yml?query=branch%3Amain)
[![warnings](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-warnings.json)](https://github.com/valeratrades/tedi/actions/workflows/warnings.yml?query=branch%3Amain)

Personal productivity CLI for task tracking, time management, and GitHub issue integration.

## Features

- **Blocker Tree**: Stack-based task management with priority tracking. Integrates with Clockify for automatic time tracking.
- **GitHub Issues**: Edit GitHub issues locally as markdown/typst files with full sync support (body, comments, sub-issues, state changes).
- **Milestones**: Sprint planning with daily/weekly/monthly/quarterly/yearly milestone tracking via GitHub milestones.
- **Manual Stats**: Track daily performance metrics (EV, focus time, etc.) with historical data.
- **Performance Evaluation**: Screenshot-based productivity tracking with AI analysis.
- **Clockify Integration**: Automatic time tracking tied to your current blocker task.
<!-- markdownlint-disable -->
<details>
<summary>
<h2>Installation</h2>
</summary>

```sh
nix build
```

## Configuration

Create `~/.config/todo/settings.toml`:

```toml
# Required for GitHub integration: a token, or a GitHub App (one rate budget per installation)
github_token = "ghp_..."
# [github_app]
# id = 123456
# private_key = { file = "/run/secrets/app.pem" }

# Required for Clockify integration
clockify_api_key = "..."
clockify_workspace_id = "..."

# Optional
default_extension = "md"  # or "typ" for typst
github_max_in_flight = 32  # GitHub requests in flight at once
home = "https://example.com/board"  # the `tedi web` holding your workflow states: `move` and `view` ask it (`tedi login` first, behind a gate)

[milestones]
url = "owner/repo"           # where your sprints live
selected = "kanban/selected" # the home's bucket opened beside a planning sprint
```


</details>
<!-- markdownlint-restore -->

## Usage
```sh
# Blocker management (main workflow)
todo blocker add "implement feature X"    # Add a new blocker
todo blocker                              # Open current blocker file in $EDITOR
todo blocker pop                          # Complete current blocker, move to next
todo blocker set projectname              # Switch to different project

# GitHub Issues
todo open https://github.com/owner/repo/issues/123  # Fetch and open issue
todo open -t owner/repo/my-issue                    # Create new issue (touch mode)
todo open pattern                                   # Fuzzy find local issue

# The board
tedi pull                                 # Store every issue of the config's `watch` orgs/repos
tedi pull EV-Main owner/repo              # …or of these
tedi view state:kanban/triage,kanban/selected t:bug i:>=5 --group state:kanban   # A query over the store, opened in $EDITOR; edits sync
tedi move https://github.com/o/r/issues/1 kanban/selected   # Into a workflow's bucket; entering it runs the bucket's hooks

# Sprints
tedi sprints edit 1d                      # A planning sprint; what you have picked up and the Bottlenecks open in tabs beside it
tedi sprints edit bottlenecks             # What comes first
tedi login                                # Sign in to the config's `home`, through the gate before it

# Time tracking
todo clockify start                       # Start tracking current blocker
todo clockify stop                        # Stop tracking

# Editor shorthands (inside issue files)
# !b  on its own line:   insert blockers section (expands to # Blockers)
# !c  on its own line:   insert new comment      (expands to <!-- new comment -->)
# !u  on last line:      undo — abort sync, treat as if no changes were made
# !mv [owner/]repo  on an issue line in a sprint/urgent buffer: move it (and its subtree) to another repo
# - [[owner/]repo] Title  a new line in a sprint/urgent buffer: create the issue in that repo on save

# Shell integration (add to your shell rc)
eval "$(todo init zsh)"                   # Or: bash, fish
```

## Tips
### Vim Fold Markers
Needs `foldmethod=marker` on markdown buffers.

Sprint views fold every multi-line component at level `base + depth` (base: milestone 1, category group 1, issue 3; depth: folded components around it). With `foldenable` and `foldlevel=2`, a sprint opens with its milestones and groups expanded and its issues closed.

Closed issues/sub-issues wrap their content in vim fold markers using `{{{always` suffix (nested one level deeper inside a sprint).
To auto-close these folds in nvim, add:
```lua
vim.opt.foldtext = [[substitute(getline(v:foldstart),'{{{]] .. [[always\s*$','{{{','')]] -- Custom foldtext that strips "always" from fold markers
vim.api.nvim_create_autocmd("BufReadPost", {
	callback = function()
		vim.defer_fn(function()
			vim.cmd([[silent! g/{{]] .. [[{always$/normal! zc]])
		end, 10)
	end,
})
```


<br>

#### Dev links
- <sup><a href="./docs/ARCHITECTURE.md">architecture</a></sup>
- <sup><a href="https://github.com/EV-Main/.github/blob/main/code/STYLE.md">style</a></sup>
- <sup><a href="https://github.com/EV-Main/.github/tree/main/code/conventions">conventions</a></sup>
- <sup><a href="https://github.com/EV-Main/.github/blob/main/code/conventions/git">contributing</a></sup>

#### License

<sup>
	Licensed under <a href="LICENSE">Blue Oak 1.0.0</a>
</sup>

<br>

<sub>
	Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be licensed as above, without any additional terms or conditions.
</sub>

