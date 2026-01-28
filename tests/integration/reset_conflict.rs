//! Test for issue #46 bug: --reset should not trigger conflicts when marking sub-issues.
//!
//! The root cause: when format_issue runs during --reset, if sub-issue files already
//! exist locally (from a previous fetch), it embeds the LOCAL content into the parent
//! file instead of the Github API content. This causes the consensus to differ from
//! what fetch_full_issue_tree returns, triggering false conflicts.
//!
//! Scenario that triggers the bug:
//! 1. Fetch issue once (creates sub-issue files with original content)
//! 2. Locally edit sub-issue files (add blockers, expand body, etc.)
//! 3. Run `--reset` on the parent issue
//! 4. format_issue embeds LOCAL sub-issue content (not Github API content)
//! 5. User makes a small change (e.g., mark sub-issue as closed)
//! 6. Post-editor sync fetches remote (gets Github API content for sub-issues)
//! 7. Consensus (with local content) != Remote (with API content) → FALSE CONFLICT

use tedi::Issue;

use crate::common::{TestContext, git::GitExt};

fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}

/// Scenario from issue #46:
/// 1. Run `todo open --reset <url>` to fetch issue with sub-issues
/// 2. Mark one sub-issue as closed
/// 3. Post-editor sync should succeed without conflict
///
/// The bug was that even though we just did --reset (making consensus = remote),
/// the post-editor sync was still detecting divergence and triggering a merge conflict.
#[test]
fn test_reset_then_mark_subissue_closed_no_conflict() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Set up remote with parent issue and one open sub-issue
	let parent = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
	);

	ctx.remote(&parent, None);

	// First: open via URL with --reset (this simulates `todo open --reset <url>`)
	// This fetches from remote, stores locally, and commits as consensus
	let out = ctx.open_url("o", "r", 1).args(&["--reset"]).run();
	eprintln!("First open stdout: {}", out.stdout);
	eprintln!("First open stderr: {}", out.stderr);
	assert!(out.status.success(), "First open with --reset should succeed. stderr: {}", out.stderr);

	// Now simulate user editing: mark the sub-issue as closed
	// The user would do this by changing `- [ ]` to `- [x]` in the editor
	let issue_path = ctx.dir_issue_path("o", "r", 1, "Parent Issue");

	// The post-editor sync should NOT trigger a conflict because:
	// - Consensus (from --reset) = remote state
	// - Local = modified (sub-issue closed)
	// - Remote = unchanged (sub-issue still open)
	// This should be detected as LocalOnly change (only local changed since consensus)
	let modified_parent = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [x] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tsub body\n",
	);
	let out = ctx.open(&issue_path).edit(&modified_parent).run();
	eprintln!("Second open stdout: {}", out.stdout);
	eprintln!("Second open stderr: {}", out.stderr);

	// THE BUG: This was triggering "Conflict detected" even though it should be a simple LocalOnly change
	assert!(
		out.status.success(),
		"Second open (marking sub-issue closed) should succeed without conflict. stderr: {}",
		out.stderr
	);
	assert!(!out.stderr.contains("Conflict"), "Should not mention conflict. stderr: {}", out.stderr);
	assert!(!out.stdout.contains("Merging remote"), "Should not attempt merge. stdout: {}", out.stdout);
}

/// Simpler version: just body change on a single issue (no sub-issues)
/// This establishes baseline behavior.
#[test]
fn test_reset_then_edit_body_no_conflict() {
	let ctx = TestContext::new("");
	ctx.init_git();

	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\toriginal body\n");
	ctx.remote(&issue, None);

	// First: open via URL with --reset
	let out = ctx.open_url("o", "r", 1).args(&["--reset"]).run();
	assert!(out.status.success(), "First open should succeed. stderr: {}", out.stderr);

	// Now edit the body
	let issue_path = ctx.flat_issue_path("o", "r", 1, "Test Issue");
	let mut modified = issue.clone();
	modified.contents.comments[0].body = tedi::Events::parse("modified body");

	// Second open: edit the body
	let out = ctx.open(&issue_path).edit(&modified).run();
	eprintln!("Second open stdout: {}", out.stdout);
	eprintln!("Second open stderr: {}", out.stderr);

	assert!(out.status.success(), "Edit should succeed. stderr: {}", out.stderr);
	assert!(!out.stderr.contains("Conflict"), "Should not have conflict. stderr: {}", out.stderr);
}

/// THE ACTUAL BUG SCENARIO:
/// When a sub-issue has its own sub-sub-issues, it gets its own file (directory format).
/// If user modifies that sub-issue file, then does --reset on the grandparent,
/// format_issue will embed the MODIFIED local content.
///
/// 1. Fetch grandparent (creates hierarchy with sub-issue files for issues with children)
/// 2. User MODIFIES a sub-issue file (adds blockers, expands content)
/// 3. Later, user runs --reset on the GRANDPARENT issue
/// 4. format_issue embeds the MODIFIED local sub-issue content
/// 5. User makes small edit
/// 6. Post-editor sync fetches remote - gets ORIGINAL API content
/// 7. Consensus != Remote → FALSE CONFLICT
#[test]
fn test_reset_with_preexisting_modified_subissue_files() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Set up remote with 3-level hierarchy:
	// grandparent (#1) -> parent (#2) -> child (#3)
	// This ensures parent (#2) gets its own folder because it has children
	let grandparent = parse(
		"- [ ] Grandparent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);

	ctx.remote(&grandparent, None);

	// Step 1: First fetch - creates local files
	// grandparent (#1) is at: 1_-_Grandparent_Issue/__main__.md
	// parent (#2) gets its own dir because it has children: 1_-_Grandparent_Issue/2_-_Parent_Issue/__main__.md
	let out = ctx.open_url("o", "r", 1).run();
	eprintln!("First fetch stdout: {}", out.stdout);
	eprintln!("First fetch stderr: {}", out.stderr);
	assert!(out.status.success(), "First fetch should succeed. stderr: {}", out.stderr);

	// Step 2: User opens and modifies the parent sub-issue (adds blockers, expands content)
	// This simulates what happens when user works on issues over time
	// The user opens the PARENT issue file, edits it, and saves - going through proper sink_local flow
	let parent_subissue_path = ctx.data_dir().join("issues/o/r/1_-_Grandparent_Issue/2_-_Parent_Issue/__main__.md");
	eprintln!("Looking for parent file at: {parent_subissue_path:?}");

	// Verify the file exists
	if !parent_subissue_path.exists() {
		// List what files were created
		let issues_dir = ctx.data_dir().join("issues/o/r");
		if issues_dir.exists() {
			eprintln!("Files in issues dir:");
			for entry in walkdir::WalkDir::new(&issues_dir).into_iter().filter_map(|e| e.ok()) {
				eprintln!("  {}", entry.path().display());
			}
		}
		panic!("Parent sub-issue file not found at expected path");
	}

	// User edits the parent sub-issue through the normal edit flow
	// The edit adds local content and blockers to the parent
	let modified_parent = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/2 -->\n\
		 \toriginal parent body\n\
		 \tADDED LOCAL CONTENT - this is only local\n\
		 \n\
		 \t# Blockers\n\
		 \t- local blocker task\n\
		 \n\
		 \t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\tchild body\n",
	);
	let out = ctx.open(&parent_subissue_path).edit(&modified_parent).run();
	eprintln!("Parent edit stdout: {}", out.stdout);
	eprintln!("Parent edit stderr: {}", out.stderr);
	assert!(out.status.success(), "Parent edit should succeed. stderr: {}", out.stderr);

	// Step 3: User runs --reset on the GRANDPARENT issue
	// BUG: format_issue will read the MODIFIED parent file and embed that content
	let out = ctx.open_url("o", "r", 1).args(&["--reset"]).run();
	eprintln!("Reset stdout: {}", out.stdout);
	eprintln!("Reset stderr: {}", out.stderr);
	assert!(out.status.success(), "Reset should succeed. stderr: {}", out.stderr);

	// Step 4: User makes a small edit (mark child issue as closed)
	let grandparent_path = ctx.data_dir().join("issues/o/r/1_-_Grandparent_Issue/__main__.md");

	let modified_grandparent = parse(
		"- [ ] Grandparent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tgrandparent body\n\
		 \n\
		 \t- [ ] Parent Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\toriginal parent body\n\
		 \n\
		 \t\t- [x] Child Issue <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t\tchild body\n",
	);

	// Step 5: Post-editor sync
	// BUG: This triggers divergence because:
	// - Consensus has parent with MODIFIED local content (from format_issue reading local file)
	// - Remote has parent with ORIGINAL API content
	// - These don't match → divergence detected → merge initiated
	let out = ctx.open(&grandparent_path).edit(&modified_grandparent).run();
	eprintln!("Edit stdout: {}", out.stdout);
	eprintln!("Edit stderr: {}", out.stderr);

	// After --reset, there should be NO merging. The consensus should match remote exactly.
	// Any change user makes should be detected as LocalOnly (user changed, remote unchanged from consensus).
	// If we see "Merging" it means --reset didn't properly reset to remote state.
	assert!(out.status.success(), "Edit after reset should succeed. stderr: {}", out.stderr);
	assert!(!out.stderr.contains("Conflict"), "Should not have conflict. stderr: {}", out.stderr);
	assert!(
		!out.stdout.contains("Merging"),
		"BUG: --reset should make consensus match remote, so no merge should be needed. stdout: {}",
		out.stdout
	);
}
