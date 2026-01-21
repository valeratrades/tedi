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
		//- /data/issues/testowner/testrepo/.meta.json
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
		    }
		  }
		}
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
		//- /data/issues/testowner/testrepo/.meta.json
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
				}
			}
		}
		//- /data/issues/testowner/testrepo/99_-_ancestry_resolve_for_ind.md
		- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
			body content here
	"#,
	);

	let new_issue_contents = "new issue contents";
	let (status, stdout, stderr) = ctx.touch("testowner/testrepo/ancestry/check_works").edit_contents(new_issue_contents).run();

	eprintln!("{stdout:?}");
	eprintln!("{stderr:?}");

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
			}
		}
	}
	//- /testowner/testrepo/99_-_ancestry_resolve_for_ind.md
	- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
		body content here
	"#);

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
	    "100": {
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
	//- /testowner/testrepo/99_-_ancestry_resolve_for_ind/100_-_check_works.md
	- [ ] check_works <!-- @mock_user https://github.com/testowner/testrepo/issues/100 -->
	//- /testowner/testrepo/99_-_ancestry_resolve_for_ind/__main__.md
	- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
		body content here
	"#);
}

/// Test that touching a new sub-issue but making no edits does NOT create the issue.
/// The issue should only be created when the user actually saves changes.
#[test]
fn test_touch_new_subissue_no_edits_does_not_create() {
	let ctx = TestContext::new(
		r#"
		//- /data/issues/testowner/testrepo/.meta.json
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
				}
			}
		}
		//- /data/issues/testowner/testrepo/99_-_parent_issue.md
		- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
			parent body
	"#,
	);

	// Touch a new sub-issue path but don't make any edits (just close editor)
	let (status, _stdout, stderr) = ctx.touch("testowner/testrepo/parent/new_child").run();

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
			}
		}
	}
	//- /testowner/testrepo/99_-_parent_issue.md
	- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
		parent body
	"#);

	// Should succeed (editor opened and closed)
	assert!(status.success(), "Expected success, got stderr: {stderr}");

	// Verify: no changes - parent still flat file, no sub-issue created
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
	    "100": {
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
	//- /testowner/testrepo/99_-_parent_issue/100_-_new_child.md
	- [ ] new_child <!-- @mock_user https://github.com/testowner/testrepo/issues/100 -->
	//- /testowner/testrepo/99_-_parent_issue/__main__.md
	- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
		parent body
	"#);
}
