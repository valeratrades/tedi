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

	// Touch with partial match "ancestry" should find the issue
	let out = ctx.open_touch("testowner/testrepo/ancestry").run();

	// Should succeed and find the existing issue
	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
	assert!(out.stdout.contains("Found existing issue"), "Expected to find existing issue, stdout: {}", out.stdout);
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
		//- /data/issues/testowner/testrepo/99_-_ancestry_resolve_for_ind.md
		- [ ] ancestry resolve for ind <!--https://github.com/testowner/testrepo/issues/99-->
			body content here
	"#,
	);

	let new_issue_contents = "new issue contents";
	let out = ctx.open_touch("testowner/testrepo/ancestry/check_works").edit_contents(new_issue_contents).run();

	eprintln!("{:?}", out.stdout);
	eprintln!("{:?}", out.stderr);

	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);

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
	    }
	  }
	}
	//- /testowner/testrepo/99_-_parent_issue.md
	- [ ] parent issue <!--https://github.com/testowner/testrepo/issues/99-->
		parent body
	"#);

	assert!(out.status.success(), "Expected success, got stderr: {}", out.stderr);
}

/// Test that nested issues work when the parent directory uses title-only naming (not synced to git).
///
/// This should work without requiring git sync first.
#[cfg(not(true))] //TODO!!!!!: \
#[tokio::test]
async fn test_nested_issue_under_unsynced_parent() {
	let ctx = TestContext::build("");

	//DO: --touch the parent with --offline

	// Create parent issue directory with title-only name (not synced to git)
	let pending_parent = parse(
		r"
- [ ] Parent Issue
	pending parent
	",
	);
	ctx.local(&pending_parent, None).await;

	ctx.run(&["--offline", "TODO"]);

	//// Create child issue under the unsynced parent
	//let child_path = "issues/owner/repo/my_project/task.md";
	//let child_content = "- [ ] Task <!-- @user https://github.com/owner/repo/issues/new -->\n\tA task under the unsynced parent.\n";
	//ctx.write(child_path, child_content);

	// Construct absolute path to the child file
	let child_file_path = ctx.data_dir().join(child_path);

	// Open the child issue by absolute path
	let out = ctx.run(["--mock", "--offline", "open", "--touch", child_file_path.to_str().unwrap()]);

	// Should succeed
	assert!(out.status.success(), "Should succeed opening child under unsynced parent. stderr: {}", out.stderr);

	// Verify the file structure is preserved
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().skip_meta(), &out), @"");
}
