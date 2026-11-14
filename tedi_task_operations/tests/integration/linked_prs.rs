//! Pull requests that reference an issue are read with it and persisted beside it: remote-derived, never edited.

use v_fixtures::FixtureRenderer;

use crate::common::{FixtureIssuesExt, Seed, TestContext, parse_virtual, render_fixture};

#[tokio::test]
async fn linked_prs_persist_with_the_issue() {
	let ctx = TestContext::build();
	let vi = parse_virtual("- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	let issue = ctx.consensus(&vi, Some(Seed::new(0))).await;
	ctx.remote(&vi, Some(Seed::new(0)));
	ctx.remote_pr(&issue, "https://github.com/o/r/pull/7", "draft");
	ctx.remote_pr(&issue, "https://github.com/o/r/pull/8", "merged");

	let out = ctx.open_issue(&issue).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap(), &out), @r#"
	//- /o/r/.meta.json
	{
	  "next_virtual_issue_number": 0,
	  "issues": {
	    "1": {
	      "user": "mock_user",
	      "timestamps": {
	        "title": "2001-09-11T08:58:19Z",
	        "description": "2001-09-11T10:16:54Z",
	        "labels": "2001-09-11T00:00:00Z",
	        "state": "2001-09-11T03:00:35Z",
	        "assignees": "2001-09-11T06:01:10Z",
	        "comments": []
	      },
	      "prs": [
	        {
	          "url": "https://github.com/o/r/pull/7",
	          "state": "draft"
	        },
	        {
	          "url": "https://github.com/o/r/pull/8",
	          "state": "merged"
	        }
	      ]
	    }
	  }
	}
	//- /o/r/1_-_Task.md
	- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	"#);
}
