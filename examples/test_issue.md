- [ ] git issues editor <!-- issue: sth sth-->
	seeing how we already have logic for editing milestones, might as well generalize it, and then also add issues.
	
	noticed we have `sub-issues` concept, which is huge, - can just parse into header-delineated tree of relevant considerations
	
	for comments on issues we can borrow from `tg`, - just keep track of which ones I actually own.
	
	if we have two-way parsing into git issues with sub-issue translation, we could probably even automatically sync all our blocker files in the form of git issues lol
	<!-- upd: yep, that's happening -->
	
	as a consequence, would absolutely need to be able to embed individual issues when writing out 1d milestone
	
	and as a consequence, would want this be embeddable in all files opened through `todo`.
	
	and then marking an issues as completed will seize including it, unless --issues-include-completed is passed.
	<!-- Q: introducing concept of being "completed" raises a question of whether it's maybe better to compile to use markdown's native todos `- [ ]` -->
	
	could argue for them being different degrees of idea processing:
	1. tg: unstructured thoughts
	1. issues: strictured thoughts
	1. blockers: action items
	
	also, let's put blockers to be an optional ordered list of action items on each issue. Projects of blockers should now mirror structure leading to an issue. `set-project` will now select an issue to set as root. Doesn't have to be synced to git or have an associated repo, but we treat them as if they do
	
	- [x] test it
	
	- [ ] blocker rewrite
		get all the present functionality + legacy supported, over into integration with issues
	
		## Blocker
		- support for virtual blockers (to keep legacy blocker files usable)
		- move all primitives into new `blocker.rs`
		- get clockify integration
		- rename rewrite to `blocker`, and `blocker` to `blocker-legacy`. See what breaks
		- ensure existing are working
	
	- [ ] milestones: pattern to create an issue out of a `- [ ] ` block.
	
	- [ ] `merge` sync semantics
		let's not `sync` before open of the file, as we don't want the networking roundtrip.
		Instead, after the exit (spawn a bg thread,- want main exe to return fast), we pull from git, and (before pushing), we compare the issue state's decoding with that of pre-open file state. If mismatches, we want to open an actual PR (to the repo that persists the todos, - require having one). And we also make a mark to xdg state, that the local file has merge conflicts (+ a notify hook, if configured). And next time user tries to open the file, it will error and tell it to merge conflict introduced in the last change first.
	
	- [x] --bug don't shit the bed if local file has sub-issues listed out of order
	
	- [x] issue labels parsing
	
	- [x] issues should go after the *entire* body, not just main description, - comments too
	
	- [ ] graceful module failures
		if we get a parsing error in one of the modules, it should either exit cleanly, either warn and have a fallback, so as not to lead the rest of the application to the undefined behavior
				eg: currently have some weird shit with sub-issue bodies not being pushed to git (and then also nuked from the file lol)
	
	- [ ] start adding spaces in markdown too, just plug-fix markdown lsp issues by adding a single indent for those empty lines
	
	- [ ] `!s` shortcut to have blockers set on the block we added it in
	
	- [-] --last flag on open
	
	- [ ] close options
