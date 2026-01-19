//! Integration tests for file naming and placement.
//!
//! Tests the file naming conventions:
//! - Flat format: `{number}_-_{title}.md` for issues without sub-issues
//! - Directory format: `{number}_-_{title}/__main__.md` for issues with sub-issues
//!
//! Also tests that old file placements are automatically cleaned up when the
//! format changes (e.g., when an issue gains sub-issues).

use std::path::Path;

use todo::Issue;

use crate::common::{TestContext, git::GitExt};

fn parse(content: &str) -> Issue {
	Issue::parse_virtual(content, Path::new("test.md")).expect("failed to parse test issue")
}

#[test]
fn test_flat_format_preserved_when_no_sub_issues() {
	let ctx = TestContext::new("");

	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	let issue_path = ctx.consensus(&parent);
	ctx.remote(&parent);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// Flat file should still exist
	assert!(ctx.flat_issue_path("o", "r", 1, "Parent Issue").exists(), "Flat format file should still exist");

	// Directory format should NOT exist
	assert!(!ctx.dir_issue_path("o", "r", 1, "Parent Issue").exists(), "Directory format should not be created");
}

#[test]
fn test_old_flat_file_removed_when_sub_issues_appear() {
	let ctx = TestContext::new("");

	// Start with a flat issue locally
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	let issue_path = ctx.consensus(&parent);

	// Remote now has sub-issues - create a version with children for mock
	let with_children = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tchild body\n",
	);
	// Remote has the version with children
	ctx.remote(&with_children);

	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// Old flat file should be removed
	assert!(!ctx.flat_issue_path("o", "r", 1, "Parent Issue").exists(), "Old flat format file should be removed");

	// New directory format should exist
	assert!(ctx.dir_issue_path("o", "r", 1, "Parent Issue").exists(), "Directory format file should be created");
}

#[test]
fn test_old_placement_discarded_even_without_local_changes() {
	// This test verifies that when remote gains sub-issues but local has no changes,
	// the old flat file is still cleaned up and replaced with the directory format.

	let ctx = TestContext::new("");

	// Set up a flat issue locally, committed to git
	let parent = parse("- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tparent body\n");
	let issue_path = ctx.consensus(&parent);

	// Remote has sub-issues now (simulating someone else adding them)
	let with_children = parse(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t- [ ] Child Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tchild body\n",
	);
	ctx.remote(&with_children);

	// Open the issue (should sync and update format)
	let (status, stdout, stderr) = ctx.run_open(&issue_path);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	assert!(status.success(), "Should succeed. stderr: {stderr}");

	// The critical assertion: old flat file must be gone
	let flat_path = ctx.flat_issue_path("o", "r", 1, "Parent Issue");
	assert!(
		!flat_path.exists(),
		"Old flat format file at {flat_path:?} should be removed even when no local changes were made"
	);

	// New directory format should exist with the main file
	let dir_path = ctx.dir_issue_path("o", "r", 1, "Parent Issue");
	assert!(dir_path.exists(), "Directory format file at {dir_path:?} should be created");

	// Sub-issue directory should exist
	let sub_issue_dir = ctx.xdg.data_dir().join("issues/o/r/1_-_Parent_Issue");
	assert!(sub_issue_dir.is_dir(), "Sub-issue directory should exist");
}

#[test]
fn test_duplicate_reference_to_nonexistent_issue_fails() {
	let ctx = TestContext::new("");

	// Set up a local issue
	let original = parse("- [ ] Some Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	let issue_path = ctx.consensus(&original);
	ctx.remote(&original);

	// Modify the issue to mark it as duplicate of #999 (which doesn't exist)
	let mut duplicate = original.clone();
	duplicate.contents.state = todo::CloseState::Duplicate(999);

	// Try to sync the duplicate state
	let (status, stdout, stderr) = ctx.open(&issue_path).edit(&duplicate).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Should fail because issue #999 doesn't exist
	assert!(!status.success(), "Should fail when marking as duplicate of non-existent issue");
	assert!(
		stderr.contains("does not exist") || stderr.contains("999"),
		"Error should mention the missing issue. stderr: {stderr}"
	);

	// Original file should still exist (not removed)
	assert!(ctx.flat_issue_path("o", "r", 1, "Some Issue").exists(), "Issue file should still exist after failed duplicate");
}

#[test]
fn test_duplicate_reference_to_existing_issue_succeeds() {
	let ctx = TestContext::new("");

	// Set up a local issue and a target duplicate issue
	let original = parse("- [ ] Some Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tbody\n");
	let dup_target = parse("- [ ] Target Issue <!-- @mock_user https://github.com/o/r/issues/2 -->\n\ttarget body\n");
	let issue_path = ctx.consensus(&original);

	// Set up mock Github with both issues
	ctx.remote(&original);
	ctx.remote(&dup_target);

	// Modify the issue to mark it as duplicate of #2 (which exists)
	let mut duplicate = original.clone();
	duplicate.contents.state = todo::CloseState::Duplicate(2);

	// Sync the duplicate state
	let (status, stdout, stderr) = ctx.open(&issue_path).edit(&duplicate).run();

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// Should succeed because issue #2 exists
	assert!(status.success(), "Should succeed when marking as duplicate of existing issue. stderr: {stderr}");

	// Original file should be removed (duplicate handling)
	assert!(
		!ctx.flat_issue_path("o", "r", 1, "Some Issue").exists(),
		"Issue file should be removed after successful duplicate marking"
	);
}
