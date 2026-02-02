//! Integration tests for touch mode (--touch flag).
//!
//! Tests the regex-based path matching for opening/creating issues.

use v_fixtures::FixtureRenderer;

use crate::{
	common::{FixtureIssuesExt, TestContext},
	render_fixture,
};

/// Test that touch mode matches issues by substring regex.
/// Path: owner/repo/partial_title should match 99_-_full_title.md
#[test]
fn test_touch_matches_issue_by_substring() {
	let ctx = TestContext::build(
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

	let out = ctx.open_touch("testowner/testrepo/ancestry").run();

	// Should succeed and find the existing issue
	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
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
	let ctx = TestContext::build(
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
		//- /data/issues/testowner/testrepo/99_-_parent.md
		- [ ] parent <!-- @mock_user https://github.com/testowner/testrepo/issues/99-->
			_
	"#,
	);

	let out = ctx.open_touch("testowner/testrepo/parent/child").ghost_edit().run();

	// Verify: flat file converted to directory, sub-issue created inside
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
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
	//- /testowner/testrepo/99_-_parent/100_-_child.md
	- [ ] child <!-- @mock_user https://github.com/testowner/testrepo/issues/100 -->
	//- /testowner/testrepo/99_-_parent/__main__.md
	- [ ] parent <!-- @mock_user https://github.com/testowner/testrepo/issues/99-->
		_
	"#);

	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
}

/// Test that touching a new sub-issue but making no edits does NOT create the issue.
/// The issue should only be created when the user actually saves changes.
#[test]
fn test_touch_new_subissue_no_edits_does_not_create() {
	let ctx = TestContext::build(
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
	let out = ctx.open_touch("testowner/testrepo/parent/new_child").run();

	// Verify: no changes - parent still flat file, no sub-issue created
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"
	//- /testowner/testrepo/99_-_parent_issue.md
	- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
		parent body
	");

	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
}

/// Test that nested issues work when the parent directory uses title-only naming (not synced to git).
///
/// Offline creation should work without requiring git sync first.
#[test]
fn test_nested_issue_under_unsynced_parent_offline() {
	// Set up a parent issue with title-only naming (no git number - just "Parent_Issue.md")
	let ctx = TestContext::build(
		r#"
		//- /data/issues/o/r/.meta.json
		{
			"virtual_project": false,
			"next_virtual_issue_number": 0,
			"issues": {}
		}
		//- /data/issues/o/r/Parent_Issue.md
		- [ ] Parent Issue <!-- @mock_user -->
			parent body
	"#,
	);

	// Create a child under the unsynced parent (title-only naming) while offline
	let out = ctx.open_touch("o/r/Parent/child").args(&["--offline"]).ghost_edit().run();

	// Verify: parent converted to directory, child created as pending
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
	//- /o/r/.meta.json
	{
		"virtual_project": false,
		"next_virtual_issue_number": 0,
		"issues": {}
	}
	//- /o/r/Parent_Issue/__main__.md
	- [ ] Parent Issue <!-- @mock_user -->
		parent body
		
	//- /o/r/Parent_Issue/child.md
	- [ ] child <!-- pending -->
	"#);

	assert!(out.status.success(), "Should succeed opening child under unsynced parent. stderr: {}", out.stderr);
}

/// Test that online sync of child under unsynced parent syncs parent first.
#[test]
fn test_nested_issue_under_unsynced_parent_online() {
	// Set up a parent issue with title-only naming (no git number)
	let ctx = TestContext::build(
		r#"
		//- /data/issues/o/r/.meta.json
		{
			"virtual_project": false,
			"next_virtual_issue_number": 0,
			"issues": {}
		}
		//- /data/issues/o/r/Parent_Issue.md
		- [ ] Parent Issue <!-- @mock_user -->
			parent body
	"#,
	);

	// Create a child while online - should sync the parent first, then the child
	let out = ctx.open_touch("o/r/Parent/child").ghost_edit().run();

	// Verify: parent synced (#1), child synced (#2), proper nesting
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
	    },
	    "2": {
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
	//- /o/r/1_-_Parent_Issue/2_-_child.md
	- [ ] child <!-- @mock_user https://github.com/o/r/issues/2 -->
	//- /o/r/1_-_Parent_Issue/__main__.md
	- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			parent body
	"#);

	assert!(out.status.success(), "Should succeed syncing child under unsynced parent. stderr: {}", out.stderr);
}
