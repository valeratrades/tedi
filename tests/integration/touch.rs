//! Integration tests for touch mode (--touch flag).
//!
//! Tests the regex-based path matching for opening/creating issues.

use crate::common::{TestContext, snapshot_issues_dir};

/// Test that touch mode matches issues by substring regex.
/// Path: owner/repo/partial_title should match 99_-_full_title.md
#[test]
fn test_touch_matches_issue_by_substring() {
	let ctx = TestContext::new(
		r#"
		//- /data/issues/testowner/testrepo/99_-_ancestry_resolve_for_ind.md
		- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
			body content here
	"#,
	);

	// Touch with partial match "ancestry" should find the issue
	let (status, stdout, stderr) = ctx.touch("testowner/testrepo/ancestry").run();

	// Should succeed and find the existing issue
	assert!(status.success(), "Expected success, got stderr: {stderr}");
	assert!(stdout.contains("Found existing issue"), "Expected to find existing issue, stdout: {stdout}, stderr: {stderr}");
}

/// Test that touch mode can create sub-issues under existing parents matched by substring.
/// Parent issue is in directory format (has __main__.md).
#[test]
fn test_touch_creates_subissue_under_parent_matched_by_substring() {
	let ctx = TestContext::new(
		r#"
		//- /data/issues/testowner/testrepo/99_-_parent_issue/__main__.md
		- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
			parent body
	"#,
	);

	// Touch parent/new-child where "parent" matches "99_-_parent_issue" by substring
	let (status, stdout, stderr) = ctx.touch("testowner/testrepo/parent/new-child").run();

	// Should succeed - parent matched, new-child is a create request
	assert!(status.success(), "Expected success, got stderr: {stderr}");
	assert!(
		stdout.contains("Created pending sub-issue: new-child"),
		"Expected sub-issue creation, stdout: {stdout}, stderr: {stderr}"
	);

	// Verify the sub-issue was created in the right place
	assert!(ctx.data_exists("issues/testowner/testrepo/99_-_parent_issue/new-child.md"));
}

/// Test that touch mode correctly handles paths where middle segments match flat files.
///
/// When `ancestry` matches flat file `99_-_ancestry_resolve_for_ind.md`:
/// - User wants to create a sub-issue under #99
/// - The flat file format is our internal problem, not the user's
/// - We match #99 as the parent, then create "check_works" as a sub-issue
/// - The Sink (Local) converts the flat file to directory format automatically
#[test]
fn test_touch_path_with_more_segments_after_flat_file_match() {
	let ctx = TestContext::new(
		r#"
		//- /data/issues/testowner/testrepo/99_-_ancestry_resolve_for_ind.md
		- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
			body content here
	"#,
	);

	// Touch ancestry/check_works where "ancestry" matches flat file "99_-_ancestry_resolve_for_ind.md"
	// Should create "check_works" as sub-issue under #99, converting flat file to directory.
	let (status, stdout, stderr) = ctx.touch("testowner/testrepo/ancestry/check_works").run();

	eprintln!("{stdout:?}");
	eprintln!("{stderr:?}");
	assert!(status.success(), "Expected success, got stderr: {stderr}");

	// Verify: flat file converted to directory, sub-issue created inside
	insta::assert_snapshot!(snapshot_issues_dir(&ctx), @r#"
	//- /testowner/testrepo/.meta.json
	{
	  "virtual_project": false,
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "99": {
	      "timestamps": {
	        "title": null,
	        "description": null,
	        "labels": null,
	        "state": null,
	        "comments": []
	      }
	    },
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
	//- /testowner/testrepo/99_-_ancestry_resolve_for_ind.md
	- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
		body content here
		
	//- /testowner/testrepo/99_-_ancestry_resolve_for_ind/check_works.md
	- [ ] check_works <!-- local: -->
	"#);
}
