//! Integration tests for path parsing edge cases.
//!
//! Tests that issue file paths are correctly parsed to extract issue identity.
//! Files can use either the numbered format `{number}_-_{title}.md` or just
//! `{title}.md` (title-only, for pending issues).

use crate::common::TestContext;

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

	let issue_file_path = ctx.xdg.data_dir().join(malformed_path);

	// Try to open this file directly
	let (status, _stdout, stderr) = ctx.run_open(&issue_file_path);

	// Should fail because the content isn't valid issue format,
	// but the error should mention the actual file path (not just the parent directory)
	assert!(!status.success(), "Should fail when file content isn't valid issue format");

	// The error should mention parsing failure and the filename, not "file not found"
	assert!(stderr.contains("failed to parse issue file"), "Error should mention parse failure, got: {stderr}");
	assert!(stderr.contains("uni.md"), "Error should mention the actual filename, got: {stderr}");
	assert!(!stderr.contains("owner/repo/\n"), "Error should NOT be the old confusing 'file not found' for parent dir");
}

/// Same test but for a file in a subdirectory (like valeratrades/math/uni.md)
#[test]
fn test_open_file_in_repo_subdir_without_issue_number() {
	let ctx = TestContext::new("");

	// Create a file in a repo subdirectory without issue number
	let malformed_path = "issues/valeratrades/math/uni.md";
	ctx.write(malformed_path, "# University notes\nSome content.");

	let issue_file_path = ctx.xdg.data_dir().join(malformed_path);

	// Try to open this file directly
	let (status, _stdout, stderr) = ctx.run_open(&issue_file_path);

	// Should fail because content isn't valid issue format, with clear error
	assert!(!status.success(), "Should fail when file content isn't valid issue format");

	// The error should mention parsing failure and the filename
	assert!(stderr.contains("failed to parse issue file"), "Error should mention parse failure, got: {stderr}");
	assert!(stderr.contains("uni.md"), "Error should mention the actual filename, got: {stderr}");
	assert!(!stderr.contains("valeratrades/math/\n"), "Error should NOT be the old confusing 'file not found' for parent dir");
}
