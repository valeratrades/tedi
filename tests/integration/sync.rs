//! Integration tests for sync conflict resolution.
//!
//! Tests the consensus-based sync logic where:
//! - Git commit state = last synced truth (consensus)
//! - Only conflict if BOTH local and remote changed since last sync
//! - Single-side changes auto-resolve
//!
//! Tests work with `Issue` directly - our canonical representation.
//! The mock Github layer translates to API format at the boundary.

use std::path::Path;

use todo::Issue;

use crate::common::{TestContext, git::GitExt};

fn parse(content: &str) -> Issue {
	Issue::parse_virtual(content, Path::new("test.md")).expect("failed to parse test issue")
}

#[test]
fn test_both_diverged_triggers_conflict() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(
		!status.success() || stderr.contains("Conflict") || stdout.contains("Merging"),
		"Should trigger conflict or merge when both diverged. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_both_diverged_with_git_initiates_merge() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	assert!(
		stdout.contains("Merging") || stdout.contains("merged") || stderr.contains("CONFLICT") || stderr.contains("Conflict"),
		"Expected merge activity or conflict. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_only_remote_changed_takes_remote() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// Local matches consensus (no uncommitted changes)
	let issue_path = ctx.consensus(&consensus);
	ctx.remote(&remote);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	assert!(status.success(), "Should succeed when only remote changed. stderr: {stderr}");
	assert!(
		stdout.contains("Remote changed") || stdout.contains("updated"),
		"Expected remote update message. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_only_local_changed_pushes_local() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal changed body\n");

	// Remote still matches consensus
	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&consensus);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	assert!(status.success(), "Should succeed when only local changed. stderr: {stderr}");
	// Should push local changes to remote
	assert!(
		stdout.contains("Updating") || stdout.contains("Synced") || stdout.contains("body"),
		"Expected push activity. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_reset_with_local_source_skips_sync() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	// Run with --reset flag
	let (status, stdout, stderr) = ctx.open(&issue_path).args(&["--reset"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	// With --reset, should reset to local state without sync
	assert!(status.success(), "Should succeed with --reset. stderr: {stderr}");

	// Local file should still have local changes (not overwritten by remote)
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("local body"), "Local changes should be preserved with --reset");
}

/// Opening via URL when no local file exists should create the file from remote.
#[test]
fn test_url_open_creates_local_file_from_remote() {
	let ctx = TestContext::new("");
	ctx.init_git(); // Need git initialized for commits

	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body content\n");
	ctx.remote(&remote);

	// No local file exists - URL open should create it
	let expected_path = ctx.flat_issue_path("o", "r", 1, "Test Issue");
	assert!(!expected_path.exists(), "Local file should not exist before open");

	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed creating from URL. stderr: {stderr}");

	// File should now exist with remote content
	assert!(expected_path.exists(), "Local file should be created");
	let content = std::fs::read_to_string(&expected_path).unwrap();
	assert!(content.contains("remote body content"), "Should have remote content. Got: {content}");
}

/// When opening via URL with --reset, local state should be completely replaced with remote.
/// No merge conflicts, no prompts - just nuke and replace.
#[test]
fn test_reset_with_remote_url_nukes_local_state() {
	let ctx = TestContext::new("");

	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body that should be nuked\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body wins\n");

	let issue_path = ctx.consensus(&local);
	ctx.remote(&remote);

	// Open via URL with --reset should nuke local and use remote
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).args(&["--reset"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed with --reset via URL. stderr: {stderr}");

	// Local file should now have remote content
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("remote body wins"), "Local should be replaced with remote. Got: {content}");
	assert!(!content.contains("local body that should be nuked"), "Local content should be gone");
}

/// When opening via URL with --reset and there's divergence, should NOT trigger merge conflict.
#[test]
fn test_reset_with_remote_url_skips_merge_on_divergence() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal diverged body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote diverged body\n");

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	// Open via URL with --reset should NOT trigger merge conflict
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).args(&["--reset"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Should succeed without merge conflict
	assert!(status.success(), "Should succeed without merge conflict. stderr: {stderr}");
	assert!(!stderr.contains("Conflict"), "Should not mention conflict with --reset");
	assert!(!stdout.contains("Merging"), "Should not attempt merge with --reset");

	// Local should have remote content
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("remote diverged body"), "Should have remote content. Got: {content}");
}

/// --pull flag should fetch and sync BEFORE opening editor.
/// This test verifies the fetch actually happens by checking stdout for fetch message.
#[test]
fn test_pull_fetches_before_editor() {
	let ctx = TestContext::new("");

	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body from github\n");

	let issue_path = ctx.consensus(&local);
	ctx.remote(&remote);

	// --pull should fetch from Github before opening editor
	let (status, stdout, stderr) = ctx.open(&issue_path).args(&["--pull"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed with --pull. stderr: {stderr}");

	// Should show fetch activity
	assert!(
		stdout.contains("Fetching") || stdout.contains("Pulling"),
		"Should show fetch/pull activity with --pull. stdout: {stdout}"
	);
}

/// --pull with diverged state should trigger conflict resolution (or auto-resolve).
#[test]
fn test_pull_with_divergence_runs_sync_before_editor() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal diverged body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote diverged body\n");

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	// --pull should attempt to sync/merge BEFORE editor opens
	let (_status, stdout, stderr) = ctx.open(&issue_path).args(&["--pull"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Should either succeed (auto-resolved) or fail with conflict
	// But importantly, it should attempt sync BEFORE editor
	assert!(
		stdout.contains("Merging") || stdout.contains("Conflict") || stderr.contains("Conflict") || stdout.contains("Pulling"),
		"Should attempt sync/merge with --pull before editor. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_closing_issue_syncs_state_change() {
	let ctx = TestContext::new("");

	let open_issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	let issue_path = ctx.consensus(&open_issue);
	ctx.remote(&open_issue);

	// Edit to close the issue
	let mut closed_issue = open_issue.clone();
	closed_issue.contents.state = todo::CloseState::Closed;

	let (status, stdout, stderr) = ctx.open(&issue_path).edit(&closed_issue).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");
	assert!(
		stdout.contains("state") || stdout.contains("closed") || stdout.contains("Updating"),
		"Expected state change sync. stdout: {stdout}"
	);
}

/// Sub-issues closed as duplicates should NOT appear in the pulled remote state.
/// Github marks these with state_reason="duplicate" - they should be filtered out entirely.
#[test]
fn test_duplicate_sub_issues_filtered_from_remote() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Create issues with proper CloseState
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");

	let mut normal_closed = parse("- [x] Normal Closed Sub <!-- @mock_user https://github.com/o/r/issues/2 -->\n\tsub body\n");
	normal_closed.contents.state = todo::CloseState::Closed;

	let mut duplicate = parse("- [x] Duplicate Sub <!-- @mock_user https://github.com/o/r/issues/3 -->\n\tduplicate body\n");
	duplicate.contents.state = todo::CloseState::Duplicate(2); // duplicate of #2

	// Build parent with children for remote
	let mut parent_with_children = parent.clone();
	parent_with_children.children = vec![normal_closed, duplicate];

	ctx.remote(&parent_with_children);

	// Open via URL to fetch from remote
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// Check that the normal closed sub-issue file exists (with .bak suffix)
	let issue_dir = ctx.dir_issue_path("o", "r", 1, "Parent Issue").parent().unwrap().to_path_buf();
	let normal_closed_path = issue_dir.join("2_-_Normal_Closed_Sub.md.bak");
	assert!(normal_closed_path.exists(), "Normal closed sub-issue file should exist");

	let closed_content = std::fs::read_to_string(&normal_closed_path).unwrap();
	assert!(closed_content.contains("Normal Closed Sub"), "Normal closed sub-issue content missing");
	assert!(closed_content.contains("[x]"), "Normal closed sub should show as [x]. Got: {closed_content}");

	// Duplicate sub-issue should NOT have a file at all
	let duplicate_path = issue_dir.join("3_-_Duplicate_Sub.md.bak");
	let duplicate_path_open = issue_dir.join("3_-_Duplicate_Sub.md");
	assert!(!duplicate_path.exists(), "Duplicate sub-issue should NOT have a file");
	assert!(!duplicate_path_open.exists(), "Duplicate sub-issue should NOT have a file");
}

/// Opening an issue twice when local matches remote should succeed (no-op).
/// This tests the case where you:
/// 1. Open an issue from URL (fetches remote)
/// 2. Open again without making changes
///
/// The second open should succeed, not fail with "Failed to commit remote state".
#[test]
fn test_open_unchanged_succeeds() {
	let ctx = TestContext::new("");
	ctx.init_git();

	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	ctx.remote(&issue);

	// First open via URL
	let (status, _stdout, stderr) = ctx.open_url("o", "r", 1).run();
	assert!(status.success(), "First open should succeed. stderr: {stderr}");

	// Second open - should also succeed (no-op since nothing changed)
	let issue_path = ctx.flat_issue_path("o", "r", 1, "Test Issue");
	let (status, _stdout, stderr) = ctx.open(&issue_path).run();
	assert!(status.success(), "Second open (unchanged) should succeed. stderr: {stderr}");
}

/// Opening an issue by number when remote state matches local should succeed.
/// Reproduces: https://github.com/valeratrades/todo/issues/83
/// The issue happens when:
/// 1. `todo open --reset <url>` fetches and stores remote state
/// 2. `todo open <number>` is called (by number, not path)
/// 3. Remote state hasn't changed, but the merge machinery still runs
/// 4. Git commit fails because there's nothing to commit
#[test]
fn test_open_by_number_unchanged_succeeds() {
	let ctx = TestContext::new("");
	ctx.init_git();

	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	ctx.remote(&issue);

	// First open via URL with --reset
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).args(&["--reset"]).run();
	eprintln!("First open stdout: {stdout}");
	eprintln!("First open stderr: {stderr}");
	assert!(status.success(), "First open should succeed. stderr: {stderr}");

	// Second open by number (simulating the failing case)
	// This uses the mock, so remote state is the same
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).run();
	eprintln!("Second open stdout: {stdout}");
	eprintln!("Second open stderr: {stderr}");
	assert!(status.success(), "Second open (unchanged) should succeed. stderr: {stderr}");
}

/// --reset should only apply to the first sync (before editor).
/// After the user makes changes, normal sync should happen.
/// Reproduces the issue where changes made after --reset don't sync.
#[test]
fn test_reset_syncs_changes_after_editor() {
	let ctx = TestContext::new("");
	ctx.init_git();

	let remote_issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body\n");
	ctx.remote(&remote_issue);

	// Create modified version (what user will change to)
	let mut modified_issue = remote_issue.clone();
	modified_issue.contents.state = todo::CloseState::Closed;

	// Open with --reset and make changes while editor is open
	let issue_path = ctx.flat_issue_path("o", "r", 1, "Test Issue");
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).args(&["--reset"]).edit_at(&issue_path, &modified_issue).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// The key assertion: sync should have happened after editor
	// We should see "Updating issue state" or similar in the output
	assert!(
		stdout.contains("Updating") || stdout.contains("Synced"),
		"Changes should be synced after editor. stdout: {stdout}"
	);
}

/// `!c` shorthand should expand to `<!-- new comment -->` and trigger comment creation.
/// When the user types `!c` on its own line, it should:
/// 1. Be expanded to `<!-- new comment -->` in the file
/// 2. Result in a new comment being created on Github
#[test]
fn test_comment_shorthand_creates_comment() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Start with an issue that has no comments
	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	let issue_path = ctx.consensus(&issue);
	ctx.remote(&issue);

	// Simulate user adding `!c` followed by comment content
	// After expansion, the file should have `<!-- new comment -->` marker
	let edited_content = "- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n\n\t!c\n\tMy new comment content\n";

	// Write the edited content (simulating what user typed in editor)
	std::fs::write(&issue_path, edited_content).unwrap();

	// Run open to trigger sync (which should expand !c and create the comment)
	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// Verify comment creation was triggered
	assert!(stdout.contains("Creating new comment"), "Should create a new comment when !c is used. stdout: {stdout}");
}

/// When local and remote have different sub-issues, force merge should preserve both.
/// This tests the scenario where:
/// - Local has sub-issue A that remote doesn't have
/// - Remote has sub-issue B that local doesn't have
/// - Local has an extra line in the description
/// After merge with --force (either side), consensus should contain both sub-issues.
///
/// Flag semantics:
/// - `--force` alone: prefer local on conflicts
/// - `--pull --force`: prefer remote on conflicts
#[cfg(false)] //TODO!!!!!: pending switch to tree-based source of truth and facet reflexivity
#[rstest]
#[case::prefer_local(&["--force"], true)]
#[case::prefer_remote(&["--pull", "--force"], false)]
fn test_force_merge_preserves_both_sub_issues(#[case] args: &[&str], #[case] expect_local_description: bool) {
	let ctx = TestContext::new("");

	// Local: parent with local-only sub-issue and modified description
	let local = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \textra local line\n\
		 \n\
		 \t- [ ] Local Sub <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tlocal sub body\n",
	);

	// Remote: parent with remote-only sub-issue (no extra description line)
	let remote = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Remote Sub <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\tremote sub body\n",
	);

	// Consensus: original state (no sub-issues, original description)
	let consensus = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n",
	);

	let issue_path = ctx.consensus(&consensus);
	ctx.local(&local);
	ctx.remote(&remote);

	let (status, stdout, stderr) = ctx.open(&issue_path).args(args).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed with {args:?}. stderr: {stderr}");

	// Read the final file (may have moved to directory format due to sub-issues)
	let content = std::fs::read_to_string(&issue_path).unwrap_or_else(|_| {
		let dir_path = ctx.issue_path(&local);
		std::fs::read_to_string(&dir_path).expect("Issue file should exist in flat or dir format")
	});

	eprintln!("Final content:\n{content}");

	// Both sub-issues should be present regardless of which side is preferred
	assert!(content.contains("Local Sub"), "Local sub-issue should be preserved with {args:?}. Got: {content}");
	assert!(content.contains("Remote Sub"), "Remote sub-issue should be added with {args:?}. Got: {content}");

	// Description line depends on which side is preferred
	if expect_local_description {
		assert!(content.contains("extra local line"), "Local description should be preserved with {args:?}. Got: {content}");
	} else {
		assert!(!content.contains("extra local line"), "Remote description should win with {args:?}. Got: {content}");
	}
}
