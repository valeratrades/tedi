//! Integration tests for path parsing edge cases.
//!
//! Tests that issue file paths are correctly parsed to extract issue identity.
//! Files can use either the numbered format `{number}_-_{title}.md` or just
//! `{title}.md` (title-only, for pending issues).
//!
//! These tests operate directly on raw file paths (not Issue objects) to test
//! error handling and edge cases in path-based opening.

use v_fixtures::FixtureRenderer;

use crate::{FixtureIssuesExt, common::TestContext};

/// When opening a file that uses title-only naming (no issue number),
/// the path is correctly found but the content must still be valid issue format.
///
/// This tests the fix for the bug where `extract_index_from_path` would fail
/// to parse title-only filenames, resulting in a confusing "issue file not found"
/// error pointing to the parent directory.
#[test]
fn test_open_file_without_issue_number_in_name_gives_clear_error() {
	let ctx = TestContext::new("");

	// Create a file that uses title-only naming but has invalid content
	let malformed_path = "issues/owner/repo/uni.md";
	ctx.write(malformed_path, "# Some content\nThis file has no issue number in its name.");

	// Construct absolute path manually since we can't parse this as an Issue
	let issue_file_path = ctx.data_dir().join(malformed_path);

	// Try to open this malformed file by absolute path
	let out = ctx.run(&["--mock", "--offline", "open", issue_file_path.to_str().unwrap()]);

	// Should fail because the content isn't valid issue format,
	// but the error should mention the actual file path (not just the parent directory)
	assert!(!out.status.success(), "Should fail when file content isn't valid issue format");

	// The error should mention parsing failure and the filename, not "file not found"
	assert!(out.stderr.contains("failed to parse issue file"), "Error should mention parse failure, got: {}", out.stderr);
	assert!(out.stderr.contains("uni.md"), "Error should mention the actual filename, got: {}", out.stderr);
	assert!(!out.stderr.contains("owner/repo/\n"), "Error should NOT be the old confusing 'file not found' for parent dir");
}

/// Same test but for a file in a subdirectory (like valeratrades/math/uni.md)
#[test]
fn test_open_file_in_repo_subdir_without_issue_number() {
	let ctx = TestContext::new("");

	// Create a file in a repo subdirectory without issue number
	let malformed_path = "issues/valeratrades/math/uni.md";
	ctx.write(malformed_path, "# University notes\nSome content.");

	// Construct absolute path manually since we can't parse this as an Issue
	let issue_file_path = ctx.data_dir().join(malformed_path);

	// Try to open this malformed file by absolute path
	let out = ctx.run(&["--mock", "--offline", "open", issue_file_path.to_str().unwrap()]);

	// Should fail because content isn't valid issue format, with clear error
	assert!(!out.status.success(), "Should fail when file content isn't valid issue format");

	// The error should mention parsing failure and the filename
	assert!(out.stderr.contains("failed to parse issue file"), "Error should mention parse failure, got: {}", out.stderr);
	assert!(out.stderr.contains("uni.md"), "Error should mention the actual filename, got: {}", out.stderr);
	assert!(
		!out.stderr.contains("valeratrades/math/\n"),
		"Error should NOT be the old confusing 'file not found' for parent dir"
	);
}

/// Test that nested issues work when the parent directory uses title-only naming (not synced to git).
///
/// Scenario: User creates a local issue "my_project" (no git number yet), then creates
/// a sub-issue under it. The directory structure is:
/// ```
/// issues/owner/repo/
///   my_project/           <- parent dir with title only (no number prefix)
///     __main__.md         <- parent issue file
///     task.md             <- child issue with title only
/// ```
///
/// This should work without requiring git sync first.
#[test]
fn test_nested_issue_under_unsynced_parent() {
	let ctx = TestContext::new("");

	// Create parent issue directory with title-only name (not synced to git)
	let parent_main = "issues/owner/repo/my_project/__main__.md";
	ctx.write(
		parent_main,
		"- [ ] My Project <!-- @user https://github.com/owner/repo/issues/new -->\n\tA project that hasn't been synced to github yet.\n",
	);

	// Create child issue under the unsynced parent
	let child_path = "issues/owner/repo/my_project/task.md";
	let child_content = "- [ ] Task <!-- @user https://github.com/owner/repo/issues/new -->\n\tA task under the unsynced parent.\n";
	ctx.write(child_path, child_content);

	// Construct absolute path to the child file
	let child_file_path = ctx.data_dir().join(child_path);

	// Open the child issue by absolute path
	let out = ctx.run(&["--mock", "--offline", "open", child_file_path.to_str().unwrap()]);

	// Should succeed
	assert!(out.status.success(), "Should succeed opening child under unsynced parent. stderr: {}", out.stderr);

	// Verify the file structure is preserved
	insta::assert_snapshot!(ctx.render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"");
}
