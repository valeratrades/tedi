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
