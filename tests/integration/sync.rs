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

use std::collections::HashMap;

use insta::assert_snapshot;
use rstest::rstest;
use tedi::{IssueLink, IssueTimestamps, LinkedIssueMeta};
use v_fixtures::FixtureRenderer;

use crate::common::{
	FixtureIssuesExt, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file, write_to_path},
	git::{GitExt as _, Seed},
	hollow_mock, parse, parse_virtual, render_fixture,
};

/// Fixture for tests where consensus, local, and remote all have different bodies.
/// The key difference between these tests is the seed values, which determine timestamps
/// and therefore which side "wins" the merge.
struct DivergedBodiesFixture {
	ctx: TestContext,
	local: tedi::Issue,
}

impl DivergedBodiesFixture {
	async fn new(consensus_seed: i64, local_seed: i64, remote_seed: i64) -> Self {
		let ctx = TestContext::build("");

		let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
		let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
		let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

		ctx.consensus_legacy(&consensus, Some(Seed::new(consensus_seed))).await;
		ctx.local_legacy(&local, Some(Seed::new(local_seed))).await;
		ctx.remote_legacy(&remote, Some(Seed::new(remote_seed)));

		Self { ctx, local }
	}
}

/// Tests that different timestamp seeds lead to different merge winners.
/// - remote_wins: seeds cause remote timestamps to win
/// - local_wins: seeds cause local timestamps to win
#[rstest]
#[case::remote_wins(-50, 40, 45, "remote changed body")]
#[case::local_wins(-70, 60, 65, "local body")]
#[tokio::test]
async fn test_both_diverged_merge_winner(#[case] consensus_seed: i64, #[case] local_seed: i64, #[case] remote_seed: i64, #[case] expected_body: &str) {
	let f = DivergedBodiesFixture::new(consensus_seed, local_seed, remote_seed).await;

	let out = f.ctx.open_issue(&f.local).run();

	// Verify the expected side won the merge
	let rendered = render_fixture(FixtureRenderer::try_new(&f.ctx).unwrap(), &out);
	assert!(rendered.contains(expected_body), "Expected body '{expected_body}' not found in:\n{rendered}");
}

/// When local matches consensus (no uncommitted changes) and remote has changed,
/// we only pull remote changes if --pull is specified.
#[tokio::test]
async fn test_only_remote_changed_takes_remote_with_pull() {
	let ctx = TestContext::build("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// Local matches consensus (no uncommitted changes), remote changed
	// Seeds: consensus=-45, remote=90 (remote much newer, guarantees dominance)
	ctx.consensus_legacy(&consensus, Some(Seed::new(-45))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(90)));

	// Must use --pull to fetch remote changes when local is unchanged
	let out = ctx.open_issue(&consensus).args(&["--pull"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);
	eprintln!("status: {:?}", out.status);

	assert!(out.status.success(), "Should succeed with --pull when only remote changed. stderr: {}", out.stderr);
	// Verify we pulled (shows "Pulling latest...")
	assert!(
		out.stdout.contains("Pulling") || out.stdout.contains("pull"),
		"Expected pull activity message. stdout: {}, stderr: {}",
		out.stdout,
		out.stderr
	);
}

#[tokio::test]
async fn test_only_local_changed_pushes_local() {
	let ctx = TestContext::build("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal changed body\n");

	// Local changed, remote still matches consensus
	// Seeds: consensus=-25, local=85, remote=-25 (local much newer than unchanged remote)
	ctx.consensus_legacy(&consensus, Some(Seed::new(-100))).await;
	ctx.local_legacy(&local, Some(Seed::new(100))).await;
	ctx.remote_legacy(&consensus, Some(Seed::new(-100)));

	let out = ctx.open_issue(&local).run();

	assert!(out.status.success(), "Should succeed when only local changed. stderr: {}", out.stderr);

	// Capture the resulting directory state
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-12T11:20:39Z",
	        "description": "2001-09-12T10:04:12Z",
	        "labels": "2001-09-12T01:55:52Z",
	        "state": "2001-09-12T00:39:25Z",
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

#[tokio::test]
async fn test_reset_with_local_source_skips_sync() {
	let ctx = TestContext::build("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote changed body\n");

	// --reset uses local as source, so timestamps don't affect result
	// Seeds: consensus=-30, local=20, remote=25
	ctx.consensus_legacy(&consensus, Some(Seed::new(-30))).await;
	ctx.local_legacy(&local, Some(Seed::new(20))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(25)));

	// Run with --reset flag
	let out = ctx.open_issue(&local).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);
	eprintln!("status: {:?}", out.status);

	// With --reset, should reset to local state without sync
	assert!(out.status.success(), "Should succeed with --reset. stderr: {}", out.stderr);

	// Local file should still have local changes (not overwritten by remote)
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("local body"), "Local changes should be preserved with --reset");
}

/// Opening via URL when no local file exists should create the file from remote.
#[tokio::test]
async fn test_url_open_creates_local_file_from_remote() {
	let ctx = TestContext::build("");

	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body content\n");
	// Seed: 15 (arbitrary, no comparison needed)
	ctx.remote_legacy(&remote, Some(Seed::new(15)));

	// No local file exists - URL open should create it
	let expected_path = ctx.flat_issue_path(("o", "r").into(), 1, "Test Issue");
	assert!(!expected_path.exists(), "Local file should not exist before open");

	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed creating from URL. stderr: {}", out.stderr);

	// File should now exist with remote content
	assert!(expected_path.exists(), "Local file should be created");
	let content = read_issue_file(&expected_path);
	assert!(content.contains("remote body content"), "Should have remote content. Got: {content}");
}

/// When opening via URL with --reset, local state should be completely replaced with remote.
/// No merge conflicts, no prompts - just nuke and replace.
#[tokio::test]
async fn test_reset_with_remote_url_nukes_local_state() {
	let ctx = TestContext::build("");

	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body that should be nuked\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body wins\n");

	// --reset overrides everything, but remote is the source when opening via URL
	// Seeds: consensus=-40, remote=80 (remote much newer)
	ctx.consensus_legacy(&local, Some(Seed::new(-40))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(80)));

	// Open via URL with --reset should nuke local and use remote
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed with --reset via URL. stderr: {}", out.stderr);

	// Local file should now have remote content
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("remote body wins"), "Local should be replaced with remote. Got: {content}");
	assert!(!content.contains("local body that should be nuked"), "Local content should be gone");
}

/// When opening via URL with --reset and there's divergence, should NOT trigger merge conflict.
#[tokio::test]
async fn test_reset_with_remote_url_skips_merge_on_divergence() {
	use std::path::PathBuf;

	use tedi::{Issue, IssueIndex};

	let ctx = TestContext::build("");
	let parent_idx = IssueIndex::repo_only(("o", "r").into());
	let hollow = hollow_mock(1, HashMap::default());

	let consensus = Issue::parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n",
		hollow.clone(),
		parent_idx,
		PathBuf::from("test.md"),
	)
	.unwrap();
	let local = Issue::parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal diverged body\n",
		hollow.clone(),
		parent_idx,
		PathBuf::from("test.md"),
	)
	.unwrap();
	let remote = Issue::parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote diverged body\n",
		hollow,
		parent_idx,
		PathBuf::from("test.md"),
	)
	.unwrap();

	// Both diverged, but --reset via URL should skip merge and use remote
	// Seeds: consensus=-60, local=30, remote=35
	ctx.consensus_legacy(&consensus, Some(Seed::new(-60))).await;
	ctx.local_legacy(&local, Some(Seed::new(30))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(35)));

	// Open via URL with --reset should NOT trigger merge conflict
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// Should succeed without merge conflict
	assert!(out.status.success(), "Should succeed without merge conflict. stderr: {}", out.stderr);
	assert!(!out.stderr.contains("Conflict"), "Should not mention conflict with --reset");
	assert!(!out.stdout.contains("Merging"), "Should not attempt merge with --reset");

	// Local should have remote content
	let issue_path = ctx.resolve_issue_path(&local);
	let content = read_issue_file(&issue_path);
	assert!(content.contains("remote diverged body"), "Should have remote content. Got: {content}");
}

/// --pull flag should fetch and sync BEFORE opening editor.
/// This test verifies the fetch actually happens by checking stdout for fetch message.
#[tokio::test]
async fn test_pull_fetches_before_editor() {
	let ctx = TestContext::build("");

	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body from github\n");

	// Local unchanged from consensus, remote changed
	// Seeds: consensus=-20, remote=70
	ctx.consensus_legacy(&local, Some(Seed::new(-20))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(70)));

	// --pull should fetch from Github before opening editor
	let out = ctx.open_issue(&local).args(&["--pull"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed with --pull. stderr: {}", out.stderr);

	// Should show fetch activity
	assert!(out.stderr.contains("pre-open sync"), "Should show fetch/pull activity with --pull. stdout: {}", out.stderr);
}

/// --pull with diverged state should trigger conflict resolution (or auto-resolve).
#[tokio::test]
async fn test_pull_with_divergence_runs_sync_before_editor() {
	let ctx = TestContext::build("");

	let consensus = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tconsensus body\n");
	let local = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlocal diverged body\n");
	let remote = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote diverged body\n");

	// Both local and remote changed since consensus
	// Seeds: consensus=-80, local=50, remote=55
	ctx.consensus_legacy(&consensus, Some(Seed::new(-80))).await;
	ctx.local_legacy(&local, Some(Seed::new(50))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(55)));

	// --pull should attempt to sync/merge BEFORE editor opens
	let out = ctx.open_issue(&local).args(&["--pull"]).run();

	// Should either succeed (auto-resolved) or fail with conflict
	// But importantly, it should attempt sync BEFORE editor
	assert!(
		out.stderr.contains("pre-open sync"), //Q: don't like reliance on impl-specific details
		"Should attempt sync/merge with --pull before editor; stderr:\n{}",
		out.stderr
	);

	// ensure we actualaly manage to auto-merge
	assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), r"", @r#"
	//- /o/r/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "timestamps": {
	        "title": "2001-09-12T05:40:40Z",
	        "description": "2001-09-12T05:40:20Z",
	        "labels": "2001-09-11T12:57:56Z",
	        "state": "2001-09-11T11:41:29Z",
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_Test_Issue.md
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			local diverged body
	"#)
}

#[tokio::test]
async fn test_closing_issue_syncs_state_change() {
	let ctx = TestContext::build("");

	let open_issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	// Local = consensus = remote initially
	// Seeds: consensus=5, remote=5 (same seed = same base time)
	ctx.consensus_legacy(&open_issue, Some(Seed::new(5))).await;
	ctx.remote_legacy(&open_issue, Some(Seed::new(5)));

	// Edit to close the issue
	let mut closed_issue = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	closed_issue.contents.state = tedi::CloseState::Closed;

	let out = ctx.open_issue(&open_issue).edit(&closed_issue, false).run();

	// Line 11 contains `state` timestamp set via Timestamp::now() when detecting state change
	let result_str = render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[11]), &out);

	insta::assert_snapshot!(result_str, @r#"
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
#[tokio::test]
async fn test_duplicate_sub_issues_filtered_from_remote() {
	let ctx = TestContext::build("");

	// Create issues with proper CloseState
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");

	let normal_closed = parse("- [x] Normal Closed Sub <!-- @mock_user https://github.com/o/r/issues/2 -->\n\tsub body\n");
	//normal_closed.contents.state = tedi::CloseState::Closed;

	let duplicate = parse("- [2] Duplicate Sub <!-- @mock_user https://github.com/o/r/issues/3 -->\n\tduplicate body\n");
	//duplicate.contents.state = tedi::CloseState::Duplicate(2); // duplicate of #2

	// Build parent with children for remote
	let mut parent_with_children = parent.clone();
	parent_with_children.children.insert(normal_closed.selector(), normal_closed);
	parent_with_children.children.insert(duplicate.selector(), duplicate);

	// Seed: -10 (arbitrary)
	ctx.remote_legacy(&parent_with_children, Some(Seed::new(-10)));

	// Open via URL to fetch from remote
	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/r/1_-_Parent_Issue/2_-_Normal_Closed_Sub.md.bak
	- [x] Normal Closed Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
			sub body
	//- /o/r/1_-_Parent_Issue/__main__.md
	- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			parent body
	");
}

/// Opening an issue twice when local matches remote should succeed (no-op).
/// This tests the case where you:
/// 1. Open an issue from URL (fetches remote)
/// 2. Open again without making changes
///
/// The second open should succeed, not fail with "Failed to commit remote state".
#[tokio::test]
async fn test_open_unchanged_succeeds() {
	let ctx = TestContext::build("");

	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	// Seed: 10 (arbitrary)
	ctx.remote_legacy(&issue, Some(Seed::new(10)));

	// First open via URL
	let out = ctx.open_url(("o", "r").into(), 1).run();
	assert!(out.status.success(), "First open should succeed. stderr: {}", out.stderr);

	// Second open - should also succeed (no-op since nothing changed)
	let out = ctx.open_issue(&issue).run();
	assert!(out.status.success(), "Second open (unchanged) should succeed. stderr: {}", out.stderr);
}

/// Opening an issue by number when remote state matches local should succeed.
/// Reproduces: https://github.com/valeratrades/todo/issues/83
/// The issue happens when:
/// 1. `todo open --reset <url>` fetches and stores remote state
/// 2. `todo open <number>` is called (by number, not path)
/// 3. Remote state hasn't changed, but the merge machinery still runs
/// 4. Git commit fails because there's nothing to commit
#[tokio::test]
async fn test_open_by_number_unchanged_succeeds() {
	let ctx = TestContext::build("");

	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	ctx.remote_legacy(&issue, None);

	// First open via URL with --reset
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).run();
	eprintln!("First open stdout: {}", out.stdout);
	eprintln!("First open stderr: {}", out.stderr);
	assert!(out.status.success(), "First open should succeed. stderr: {}", out.stderr);

	// Second open by number (simulating the failing case)
	// This uses the mock, so remote state is the same
	let out = ctx.open_url(("o", "r").into(), 1).run();
	eprintln!("Second open stdout: {}", out.stdout);
	eprintln!("Second open stderr: {}", out.stderr);
	assert!(out.status.success(), "Second open (unchanged) should succeed. stderr: {}", out.stderr);
}

/// --reset should only apply to the first sync (before editor).
/// After the user makes changes, normal sync should happen.
/// Reproduces the issue where changes made after --reset don't sync.
#[tokio::test]
async fn test_reset_syncs_changes_after_editor() {
	let ctx = TestContext::build("");

	let remote_issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body\n");
	ctx.remote_legacy(&remote_issue, None);

	// emulate user closing the issue after
	let mut modified_issue = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tremote body\n");
	modified_issue.contents.state = tedi::CloseState::Closed;
	let out = ctx.open_url(("o", "r").into(), 1).args(&["--reset"]).edit(&modified_issue, false).run();

	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[11]), &out), @r#"
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
	//- /o/r/1_-_Test_Issue.md.bak
	- [x] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			remote body
	"#);
}

/// `!c` shorthand should expand to `<!-- new comment -->` and trigger comment creation.
/// When the user types `!c` on its own line, it should:
/// 1. Be expanded to `<!-- new comment -->` in the file
/// 2. Result in a new comment being created on Github
#[tokio::test]
async fn test_comment_shorthand_creates_comment() {
	let ctx = TestContext::build("");

	// Start with an issue that has no comments
	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n");
	ctx.consensus_legacy(&issue, None).await;
	ctx.remote_legacy(&issue, None);

	// Simulate user adding `!c` followed by comment content
	// After expansion, the file should have `<!-- new comment -->` marker
	let edited_content = "- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tissue body\n\n\t!c\n\tMy new comment content\n";

	// Write the edited content (simulating what user typed in editor)
	let issue_path = ctx.resolve_issue_path(&issue);
	write_to_path(&issue_path, edited_content);

	// Run open to trigger sync (which should expand !c and create the comment)
	let out = ctx.open_issue(&issue).run();

	//eprintln!("stdout: {}", out.stdout);
	//eprintln!("stderr: {}", out.stderr);

	// Capture the resulting directory state
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /o/__conflict.md
	<<<<<<< HEAD
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			issue body
		
		<!-- new comment -->
			My new comment content
	||||||| [hash]
	=======
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			issue body
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
#[tokio::test]
async fn test_force_merge_preserves_both_sub_issues(#[case] args: &[&str], #[case] expect_local_description: bool) {
	let ctx = TestContext::build("");

	// Local: parent with local-only sub-issue and modified description
	let local = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \textra line from local\n\
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

	ctx.consensus_legacy(&consensus, Some(Seed::new(-100))).await;
	ctx.local_legacy(&local, Some(Seed::new(100))).await;
	ctx.remote_legacy(&remote, Some(Seed::new(100)));

	let out = ctx.open_issue(&local).args(args).run();

	// Snapshot the result - different expectations based on which side wins conflicts
	//#[codestyle::skip]
	if expect_local_description {
		// --force: local wins conflicts, so "extra local line" should be present
		insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
		//- /o/r/1_-_Parent_Issue/2_-_Local_Sub.md
		- [ ] Local Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
				local sub body
		//- /o/r/1_-_Parent_Issue/3_-_Remote_Sub.md
		- [ ] Remote Sub <!-- @mock_user https://github.com/o/r/issues/3 -->
				remote sub body
		//- /o/r/1_-_Parent_Issue/__main__.md
		- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
				parent body
				extra line from local
		");
	} else {
		// --pull --force: remote wins conflicts, so "extra local line" should NOT be present
		insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @r#"
		//- /o/r/1_-_Parent_Issue/2_-_Local_Sub.md
		- [ ] Local Sub <!-- @mock_user https://github.com/o/r/issues/2 -->
				local sub body
		//- /o/r/1_-_Parent_Issue/3_-_Remote_Sub.md
		- [ ] Remote Sub <!-- @mock_user https://github.com/o/r/issues/3 -->
				remote sub body
		//- /o/r/1_-_Parent_Issue/__main__.md
		- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
				parent body
		"#);
	}
}

/// Verify that .meta.json is written with timestamps when sinking to Consensus.
/// This is critical for the merge algorithm to work - timestamps determine which side wins.
#[tokio::test]
async fn test_consensus_sink_writes_meta_json_with_timestamps() {
	let ctx = TestContext::build("");

	// Set up a remote issue with a comment (will have timestamps from mock)
	let remote = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tremote body\n\
		 \n\
		 \t---\n\
		 \t<!-- comment 1001 @commenter -->\n\
		 \tA test comment\n",
	);
	ctx.remote_legacy(&remote, None);

	// Fetch the issue via URL - this should sink to Consensus and write .meta.json
	let out = ctx.open_url(("o", "r").into(), 1).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Fetch should succeed. stderr: {}", out.stderr);

	// Capture the resulting directory state (includes .meta.json with timestamps)
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
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
