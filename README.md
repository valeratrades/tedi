# tedi
![Minimum Supported Rust Version](https://img.shields.io/badge/nightly-1.90+-ab6000.svg)
[<img alt="crates.io" src="https://img.shields.io/crates/v/tedi.svg?color=fc8d62&logo=rust" height="20" style=flat-square>](https://crates.io/crates/tedi)
[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-66c2a5?style=for-the-badge&labelColor=555555&logo=docs.rs&style=flat-square" height="20">](https://docs.rs/tedi)
![Lines Of Code](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/valeratrades/b48e6f02c61942200e7d1e3eeabf9bcb/raw/tedi-loc.json)
<br>
[<img alt="ci errors" src="https://img.shields.io/github/actions/workflow/status/valeratrades/tedi/errors.yml?branch=master&style=for-the-badge&style=flat-square&label=errors&labelColor=420d09" height="20">](https://github.com/valeratrades/tedi/actions?query=branch%3Amaster) <!--NB: Won't find it if repo is private-->
[<img alt="ci warnings" src="https://img.shields.io/github/actions/workflow/status/valeratrades/tedi/warnings.yml?branch=master&style=for-the-badge&style=flat-square&label=warnings&labelColor=d16002" height="20">](https://github.com/valeratrades/tedi/actions?query=branch%3Amaster) <!--NB: Won't find it if repo is private-->

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
<h3>Installation</h3>
</summary>

```sh
nix build
```

## Configuration

Create `~/.config/todo/settings.toml`:

```toml
# Required for GitHub integration
github_token = "ghp_..."

# Required for Clockify integration
clockify_api_key = "..."
clockify_workspace_id = "..."

# Optional
default_extension = "md"  # or "typ" for typst
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

# Milestones
todo milestones                           # Show current milestones
todo milestones push "goal description"   # Add goal to current milestone

# Time tracking
todo clockify start                       # Start tracking current blocker
todo clockify stop                        # Stop tracking

# Shell integration (add to your shell rc)
eval "$(todo init zsh)"                   # Or: bash, fish
```

## Tips
### Vim Fold Markers
closed issues/sub-issues wrap their content in vim fold markers using `{{{always` suffix.
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

<sup>
	This repository follows <a href="https://github.com/valeratrades/.github/tree/master/best_practices">my best practices</a> and <a href="https://github.com/tigerbeetle/tigerbeetle/blob/main/docs/TIGER_STYLE.md">Tiger Style</a> (except "proper capitalization for acronyms": (VsrState, not VSRState) and formatting). For project's architecture, see <a href="./docs/ARCHITECTURE.md">ARCHITECTURE.md</a>.
</sup>

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

