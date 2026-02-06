//! Integration tests for file naming and placement.
//!
//! Tests the file naming conventions:
//! - Flat format: `{number}_-_{title}.md` for issues without sub-issues
//! - Directory format: `{number}_-_{title}/__main__.md` for issues with sub-issues
//!
//! Also tests that old file placements are automatically cleaned up when the
//! format changes (e.g., when an issue gains sub-issues).

use tedi::Issue;

use crate::common::{TestContext, are_you_sure::UnsafePathExt, git::GitExt, parse_virtual};

fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}

#[tokio::test]
async fn test_flat_format_preserved_when_no_sub_issues() {
	let ctx = TestContext::build("");

	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	ctx.consensus_legacy(&parent, None).await;
	ctx.remote_legacy(&parent, None);

	let out = ctx.open_issue(&parent).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	// Flat file should still exist
	assert!(ctx.flat_issue_path(("o", "r").into(), 1, "Parent Issue").exists(), "Flat format file should still exist");

	// Directory format should NOT exist
	assert!(!ctx.dir_issue_path(("o", "r").into(), 1, "Parent Issue").exists(), "Directory format should not be created");
}

#[tokio::test]
async fn test_old_flat_file_removed_when_sub_issues_appear() {
	let ctx = TestContext::build("");

	// Start with a flat issue locally
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	ctx.consensus_legacy(&parent, None).await;

	// Remote now has sub-issues - create a version with children for mock
	let with_children = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tchild body\n",
	);
	// Remote has the version with children
	ctx.remote_legacy(&with_children, None);

	// Need --pull since local == consensus (no uncommitted changes)
	let out = ctx.open_issue(&parent).args(&["--pull"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	// Old flat file should be removed
	assert!(!ctx.flat_issue_path(("o", "r").into(), 1, "Parent Issue").exists(), "Old flat format file should be removed");

	// New directory format should exist
	assert!(ctx.dir_issue_path(("o", "r").into(), 1, "Parent Issue").exists(), "Directory format file should be created");
}

#[tokio::test]
async fn test_old_placement_discarded_with_pull() {
	// This test verifies that when remote gains sub-issues and we use --pull,
	// the old flat file is cleaned up and replaced with the directory format.

	let ctx = TestContext::build("");

	// Set up a flat issue locally, committed to git
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	ctx.consensus_legacy(&parent, None).await;

	// Remote has sub-issues now (simulating someone else adding them)
	let with_children = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tchild body\n",
	);
	ctx.remote_legacy(&with_children, None);

	// Need --pull since local == consensus (no uncommitted local changes)
	let out = ctx.open_issue(&parent).args(&["--pull"]).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed. stderr: {}", out.stderr);

	// The critical assertion: old flat file must be gone
	let flat_path = ctx.flat_issue_path(("o", "r").into(), 1, "Parent Issue");
	assert!(!flat_path.exists(), "Old flat format file at {flat_path:?} should be removed when using --pull");

	// New directory format should exist with the main file
	let dir_path = ctx.dir_issue_path(("o", "r").into(), 1, "Parent Issue");
	assert!(dir_path.exists(), "Directory format file at {dir_path:?} should be created");

	// Sub-issue directory should exist
	let sub_issue_dir = ctx.xdg.data_dir().join("issues/o/r/1_-_Parent_Issue");
	assert!(sub_issue_dir.is_dir(), "Sub-issue directory should exist");
}

#[tokio::test]
async fn test_duplicate_removes_local_file() {
	let ctx = TestContext::build("");

	// Set up a local issue
	let original = parse("- [ ] Some Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	ctx.consensus_legacy(&original, None).await;
	ctx.remote_legacy(&original, None);

	// Modify the issue to mark it as duplicate
	let mut duplicate: tedi::VirtualIssue = original.clone().into();
	duplicate.contents.state = tedi::CloseState::Duplicate(999);

	// Sync the duplicate state
	let out = ctx.open_issue(&original).edit(&duplicate, false).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "Should succeed when marking as duplicate. stderr: {}", out.stderr);

	// Original file should be removed (duplicate self-eliminates)
	assert!(
		!ctx.flat_issue_path(("o", "r").into(), 1, "Some Issue").exists(),
		"Issue file should be removed after marking as duplicate"
	);
}

#[tokio::test]
async fn test_duplicate_reference_to_existing_issue_succeeds() {
	let ctx = TestContext::build("");
	let is_virtual = false;

	// Set up a local issue and a target duplicate issue
	let original = parse_virtual("- [ ] Some Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	let original_issue = ctx.consensus(&original, None, is_virtual).await;
	ctx.remote(&original, None, is_virtual);

	let dup_target = parse_virtual("- [ ] Target Issue <!-- @mock_user https://github.com/o/r/issues/2 -->\n\ttarget body\n");
	ctx.remote(&dup_target, None, is_virtual);

	// Modify the issue to mark it as duplicate of #2 (which exists)
	let mut duplicate = original.clone();
	duplicate.contents.state = tedi::CloseState::Duplicate(2);

	// Sync the duplicate state
	let out = ctx.open_issue(&original_issue).edit(&duplicate, is_virtual).run();

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// Should succeed because issue #2 exists
	assert!(out.status.success(), "Should succeed when marking as duplicate of existing issue. stderr: {}", out.stderr);

	// Original file should be removed (duplicate handling)
	assert!(
		!ctx.flat_issue_path(("o", "r").into(), 1, "Some Issue").exists(),
		"Issue file should be removed after successful duplicate marking"
	);
}
