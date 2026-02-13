//! Tests for --reset flag behavior with conflicts.
//!
//! Issue #46: --reset should not trigger conflicts when modifying issues after reset.
//! After `open --reset`, the consensus matches remote exactly, so any user edit
//! should be detected as LocalOnly (no merge needed).

use crate::common::{TestContext, git::GitExt, parse_virtual};

/// After --reset on an issue with sub-issues, marking a sub-issue closed should succeed.
/// No conflict should occur because consensus == remote after reset.
#[test]
fn test_reset_with_subissue_edit() {
	let ctx = TestContext::build("");

	// Remote has parent with one open sub-issue
	let remote_vi = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
	);
	ctx.remote(&remote_vi, None);

	// User edits: mark sub-issue as closed
	let edited = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [x] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
	);

	// --reset fetches remote and establishes consensus, then user edits
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}

/// After --reset on a simple issue, editing the body should succeed without conflict.
#[test]
fn test_reset_with_body_edit() {
	let ctx = TestContext::build("");

	let remote_vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\toriginal body\n");
	ctx.remote(&remote_vi, None);

	let edited = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tmodified body\n");

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict"),
		"Should succeed without conflict. stderr: {}",
		out.stderr
	);
}

/// Issue #46 bug scenario: --reset should discard local modifications to sub-issue files.
///
/// When sub-issues have their own files (directory format), a previous edit to those
/// files should NOT contaminate the consensus after --reset.
///
/// Scenario:
/// 1. Fetch issue hierarchy (creates sub-issue files)
/// 2. User modifies a sub-issue file (adds local content)
/// 3. User runs --reset on the root issue
/// 4. After reset, editing should not trigger false conflicts
///
/// The bug was that format_issue would embed LOCAL sub-issue content into consensus
/// instead of the remote API content, causing consensus != remote.
#[tokio::test]
async fn test_reset_discards_local_subissue_modifications() {
	let ctx = TestContext::build("");

	// 3-level hierarchy: grandparent -> parent -> child
	// Parent gets its own directory because it has children
	let remote_vi = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);
	let remote_state = ctx.remote(&remote_vi, None);

	// Step 1: Initial fetch to create local files
	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "Initial fetch failed. stderr: {}", out.stderr);

	// Step 2: User modifies the parent sub-issue (adds blockers)
	// We need to get the parent issue from the remote state's children
	let parent_issue = &remote_state[2]; // Issue #2 is the parent sub-issue

	let modified_parent = parse_virtual(
		"- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/2 -->\n\
		 \toriginal parent body\n\
		 \tADDED LOCAL CONTENT\n\
		 \n\
		 \t# Blockers\n\
		 \t- local blocker\n\
		 \n\
		 \t- [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\tchild body\n",
	);

	// Open and edit the parent sub-issue
	let out = ctx.open_issue(parent_issue).edit(&modified_parent).run();
	assert!(out.status.success(), "Parent edit failed. stderr: {}", out.stderr);

	// Step 3: --reset on grandparent should discard local modifications
	// and establish consensus == remote

	// Step 4: Edit after reset (mark child closed)
	let edited_after_reset = parse_virtual(
		"- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [x] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);

	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&edited_after_reset).run();

	assert!(
		out.status.success() && !out.stderr.contains("Conflict") && !out.stdout.contains("Merging"),
		"Should succeed without conflict or merge after reset. stderr: {}, stdout: {}",
		out.stderr,
		out.stdout
	);
}
