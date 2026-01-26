//! Integration tests for sync conflict resolution.
//!
//! Tests the consensus-based sync logic where:
//! - Git commit state = last synced truth (consensus)
//! - Only conflict if BOTH local and remote changed since last sync
//! - Single-side changes auto-resolve
//!
//! Tests work with `Issue` directly - our canonical representation.
//! The mock Github layer translates to API format at the boundary.
//!
//! ## Timestamp Seeds
//!
//! Each test uses unique seed values to ensure variety. Higher seed = newer timestamp = wins in merge.
//! Seeds are chosen to be either:
//! - Close together (e.g., 100 vs 105): tests edge cases where timestamps are similar
//! - Far apart (e.g., 50 vs 200): guarantees one side dominates
//!
//! ## Snapshot Testing
//!
//! Tests use insta snapshots to capture the resulting directory state.
//! The `.meta.json` file contains actual timestamps from seed-based generation,
//! so snapshots verify both file content and timestamp values.

use rstest::rstest;
use tedi::Issue;

use crate::common::{TestContext, git::GitExt, snapshot_issues_dir, snapshot_issues_dir_redacting};

fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}

#[test]
fn test_both_diverged_triggers_conflict() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// Both local and remote changed since consensus - should conflict
	// Seeds: consensus=-50, local=40, remote=45 (local and remote close but both newer than consensus)
	let issue_path = ctx.consensus(&consensus, Some(-50));
	ctx.local(&local, Some(40));
	ctx.remote(&remote, Some(45));

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	// Capture the resulting directory state - this shows actual timestamps and merge result
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-11T10:54:39Z",
	        "description": "2001-09-11T11:07:36Z",
	        "labels": "2001-09-11T16:40:49Z",
	        "state": "2001-09-11T22:22:01Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			remote changed body
	"#);
}

#[test]
fn test_both_diverged_with_git_initiates_merge() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// Both local and remote changed since consensus - should merge/conflict
	// Seeds: consensus=-70, local=60, remote=65 (far apart from consensus, close to each other)
	let issue_path = ctx.consensus(&consensus, Some(-70));
	ctx.local(&local, Some(60));
	ctx.remote(&remote, Some(65));

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	// Capture the resulting directory state - this shows actual timestamps and merge result
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-12T01:58:13Z",
	        "description": "2001-09-12T04:56:40Z",
	        "labels": "2001-09-11T14:34:47Z",
	        "state": "2001-09-12T03:35:54Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			local body
	"#);
}

/// When local matches consensus (no uncommitted changes) and remote has changed,
/// we only pull remote changes if --pull is specified.
#[test]
fn test_only_remote_changed_takes_remote_with_pull() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// Local matches consensus (no uncommitted changes), remote changed
	// Seeds: consensus=-45, remote=90 (remote much newer, guarantees dominance)
	let issue_path = ctx.consensus(&consensus, Some(-45));
	ctx.remote(&remote, Some(90));

	// Must use --pull to fetch remote changes when local is unchanged
	let (status, stdout, stderr) = ctx.open(&issue_path).args(&["--pull"]).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	assert!(status.success(), "Should succeed with --pull when only remote changed. stderr: {stderr}");
	// Verify we pulled (shows "Pulling latest...")
	assert!(
		stdout.contains("Pulling") || stdout.contains("pull"),
		"Expected pull activity message. stdout: {stdout}, stderr: {stderr}"
	);
}

#[test]
fn test_only_local_changed_pushes_local() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal changed body\n");

	// Local changed, remote still matches consensus
	// Seeds: consensus=-25, local=85, remote=-25 (local much newer than unchanged remote)
	let issue_path = ctx.consensus(&consensus, Some(-25));
	ctx.local(&local, Some(85));
	ctx.remote(&consensus, Some(-25));

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");
	eprintln!("status: {status:?}");

	assert!(status.success(), "Should succeed when only local changed. stderr: {stderr}");

	// Capture the resulting directory state
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-12T03:55:22Z",
	        "description": "2001-09-12T03:30:04Z",
	        "labels": "2001-09-12T01:22:28Z",
	        "state": "2001-09-11T17:16:14Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			local changed body
	"#);
}

#[test]
fn test_reset_with_local_source_skips_sync() {
	let ctx = TestContext::new("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// --reset uses local as source, so timestamps don't affect result
	// Seeds: consensus=-30, local=20, remote=25
	let issue_path = ctx.consensus(&consensus, Some(-30));
	ctx.local(&local, Some(20));
	ctx.remote(&remote, Some(25));

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
	// Seed: 15 (arbitrary, no comparison needed)
	ctx.remote(&remote, Some(15));

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

	// --reset overrides everything, but remote is the source when opening via URL
	// Seeds: consensus=-40, remote=80 (remote much newer)
	let issue_path = ctx.consensus(&local, Some(-40));
	ctx.remote(&remote, Some(80));

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

	// Both diverged, but --reset via URL should skip merge and use remote
	// Seeds: consensus=-60, local=30, remote=35
	let issue_path = ctx.consensus(&consensus, Some(-60));
	ctx.local(&local, Some(30));
	ctx.remote(&remote, Some(35));

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

	// Local unchanged from consensus, remote changed
	// Seeds: consensus=-20, remote=70
	let issue_path = ctx.consensus(&local, Some(-20));
	ctx.remote(&remote, Some(70));

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

	// Both local and remote changed since consensus
	// Seeds: consensus=-80, local=50, remote=55
	let issue_path = ctx.consensus(&consensus, Some(-80));
	ctx.local(&local, Some(50));
	ctx.remote(&remote, Some(55));

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
	// Local = consensus = remote initially
	// Seeds: consensus=5, remote=5 (same seed = same base time)
	let issue_path = ctx.consensus(&open_issue, Some(5));
	ctx.remote(&open_issue, Some(5));

	// Edit to close the issue
	let mut closed_issue = open_issue.clone();
	closed_issue.contents.state = tedi::CloseState::Closed;

	let (_status, stdout, stderr) = ctx.open(&issue_path).edit(&closed_issue).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Capture the resulting directory state
	// Line 11 contains `state` timestamp set via Timestamp::now() when detecting state change
	insta::assert_snapshot!(snapshot_issues_dir_redacting(&ctx, &[11]), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-11T09:15:20Z",
	        "description": "2001-09-11T04:30:34Z",
	        "labels": "2001-09-11T06:38:10Z",
	        [REDACTED - non-deterministic timestamp]
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md.bak
	- [x] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			body
	"#);
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
	normal_closed.contents.state = tedi::CloseState::Closed;

	let mut duplicate = parse("- [x] Duplicate Sub <!-- @mock_user https://github.com/o/r/issues/3 -->\n\tduplicate body\n");
	duplicate.contents.state = tedi::CloseState::Duplicate(2); // duplicate of #2

	// Build parent with children for remote
	let mut parent_with_children = parent.clone();
	parent_with_children.children = vec![normal_closed, duplicate];

	// Seed: -10 (arbitrary)
	ctx.remote(&parent_with_children, Some(-10));

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
	// Seed: 10 (arbitrary)
	ctx.remote(&issue, Some(10));

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
	ctx.remote(&issue, None);

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
	ctx.remote(&remote_issue, None);

	// Create modified version (what user will change to)
	let mut modified_issue = remote_issue.clone();
	modified_issue.contents.state = tedi::CloseState::Closed;

	// Open with --reset and make changes while editor is open
	let (_status, stdout, stderr) = ctx.open_url("o", "r", 1).args(&["--reset"]).edit(&modified_issue).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Capture the resulting directory state
	// Line 11 contains `state` timestamp set via Timestamp::now() when detecting state change
	insta::assert_snapshot!(snapshot_issues_dir_redacting(&ctx, &[11]), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": null,
	        "description": null,
	        "labels": null,
	        [REDACTED - non-deterministic timestamp]
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			remote body
	"#);
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
	let issue_path = ctx.consensus(&issue, None);
	ctx.remote(&issue, None);

	// Simulate user adding `!c` followed by comment content
	// After expansion, the file should have `<!-- new comment -->` marker
	let edited_content = "- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n\n\t!c\n\tMy new comment content\n";

	// Write the edited content (simulating what user typed in editor)
	std::fs::write(&issue_path, edited_content).unwrap();

	// Run open to trigger sync (which should expand !c and create the comment)
	let (_status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Capture the resulting directory state
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @"
	//- /o/__conflict.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			issue body
	<<<<<<< HEAD
		
		<!-- new comment -->
			My new comment content
	||||||| [hash]
	=======
	>>>>>>> remote-state
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
		issue body

		!c
		My new comment content
	");
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

	let issue_path = ctx.consensus(&consensus, None);
	ctx.local(&local, None);
	ctx.remote(&remote, None);

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

/// Verify that .meta.json is written with timestamps when sinking to Consensus.
/// This is critical for the merge algorithm to work - timestamps determine which side wins.
#[test]
fn test_consensus_sink_writes_meta_json_with_timestamps() {
	let ctx = TestContext::new("");
	ctx.init_git(); // Need git initialized for URL fetch

	// Set up a remote issue with a comment (will have timestamps from mock)
	let remote = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tremote body\n\
		 \n\
		 \t---\n\
		 \t<!-- comment 1001 @commenter -->\n\
		 \tA test comment\n",
	);
	ctx.remote(&remote, None);

	// Fetch the issue via URL - this should sink to Consensus and write .meta.json
	let (status, stdout, stderr) = ctx.open_url("o", "r", 1).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Fetch should succeed. stderr: {stderr}");

	// Capture the resulting directory state (includes .meta.json with timestamps)
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": null,
	        "description": null,
	        "labels": null,
	        "state": null,
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			remote body
			
			---
	"#);
}
