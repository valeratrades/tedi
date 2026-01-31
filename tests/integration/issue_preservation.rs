//! Integration tests for issue content preservation through edit/sync cycles.
//!
//! Tests that nested issues, blockers, and other content survive the
//! parse -> edit -> serialize -> sync cycle intact.

use tedi::Issue;
use v_fixtures::FixtureRenderer;

use crate::{
	common::{
		FixtureIssuesExt, TestContext,
		are_you_sure::{UnsafePathExt, read_issue_file},
		git::GitExt,
	},
	render_fixture,
};

fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}

#[tokio::test]
async fn test_comments_with_ids_sync_correctly() {
	let ctx = TestContext::build("");

	// Issue with a comment that has an ID
	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tbody text\n\
		 \n\
		 \t<!-- @mock_user https://github.com/o/r/issues/1#issuecomment-12345 -->\n\
		 \tThis is my comment\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).args(&["--force"]).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// This should NOT fail with "comment X not found in consensus"
	assert!(out.status.success(), "sync failed: {}", out.stderr);
}

#[tokio::test]
async fn test_nested_issues_preserved_through_sync() {
	let ctx = TestContext::build("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body b\n\
		 \n\
		 \t- [ ] c <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\tnested body c\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	// With the new model, children are stored in separate files in the parent's directory
	let path = ctx.resolve_issue_path(&issue);
	let parent_dir = path.parent().unwrap();
	let child_b_path = parent_dir.join("2_-_b.md");
	let child_c_path = parent_dir.join("3_-_c.md");

	let child_b_content = read_issue_file(&child_b_path);
	let child_c_content = read_issue_file(&child_c_path);

	assert!(child_b_content.contains("nested body b"), "nested issue b body lost");
	assert!(child_c_content.contains("nested body c"), "nested issue c body lost");
}

#[tokio::test]
async fn test_mixed_open_closed_nested_issues_preserved() {
	let ctx = TestContext::build("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\topen nested body\n\
		 \n\
		 \t- [x] c <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t<!--omitted {{{always-->\n\
		 \t\tclosed nested body\n\
		 \t\t<!--,}}}-->\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	// With the new model, children are stored in separate files
	let path = ctx.resolve_issue_path(&issue);
	let parent_dir = path.parent().unwrap();
	let child_b_path = parent_dir.join("2_-_b.md");
	let child_c_path = parent_dir.join("3_-_c.md.bak"); // closed issue has .bak suffix

	let child_b_content = read_issue_file(&child_b_path);
	assert!(child_b_content.contains("open nested body"), "open nested issue body lost");

	let child_c_content = read_issue_file(&child_c_path);
	assert!(child_c_content.contains("- [x] c"), "closed nested issue state lost");
}

#[tokio::test]
async fn test_blockers_preserved_through_sync() {
	let ctx = TestContext::build("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- first blocker\n\
		 \t- second blocker\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	let path = ctx.resolve_issue_path(&issue);
	let final_content = read_issue_file(&path);
	assert!(final_content.contains("# Blockers"), "blockers section lost");
	assert!(final_content.contains("first blocker"), "first blocker lost");
	assert!(final_content.contains("second blocker"), "second blocker lost");
}

#[tokio::test]
async fn test_blockers_added_during_edit_preserved() {
	let ctx = TestContext::build("");

	// Initial state: no blockers
	let initial_issue = parse("- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlorem ipsum\n");

	ctx.consensus(&initial_issue, None).await;
	ctx.remote(&initial_issue, None);

	// User adds blockers during edit
	let edited_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- new blocker added\n",
	);

	let out = ctx.open_issue(&initial_issue).edit(&edited_issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	let path = ctx.resolve_issue_path(&initial_issue);
	let final_content = read_issue_file(&path);
	assert!(final_content.contains("# Blockers"), "blockers section not preserved");
	assert!(final_content.contains("new blocker added"), "added blocker lost");
}

#[tokio::test]
async fn test_blockers_with_headers_preserved() {
	let ctx = TestContext::build("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t# phase 1\n\
		 \t- task alpha\n\
		 \t- task beta\n\
		 \n\
		 \t# phase 2\n\
		 \t- task gamma\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	let path = ctx.resolve_issue_path(&issue);
	let final_content = read_issue_file(&path);
	assert!(final_content.contains("# phase 1"), "phase 1 header lost");
	assert!(final_content.contains("# phase 2"), "phase 2 header lost");
	assert!(final_content.contains("task alpha"), "task alpha lost");
	assert!(final_content.contains("task gamma"), "task gamma lost");
}

#[tokio::test]
async fn test_nested_issues_and_blockers_together() {
	let ctx = TestContext::build("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- blocker one\n\
		 \t- blocker two\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body\n",
	);

	ctx.consensus(&issue, None).await;
	ctx.remote(&issue, None);

	let out = ctx.open_issue(&issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	// File is in directory format (path points to __main__.md)
	let path = ctx.resolve_issue_path(&issue);
	let final_content = read_issue_file(&path);
	assert!(final_content.contains("# Blockers"), "blockers section lost");
	assert!(final_content.contains("blocker one"), "blocker one lost");

	// With the new model, nested issue is in a separate file
	let child_path = path.parent().unwrap().join("2_-_b.md");
	let child_content = read_issue_file(&child_path);
	assert!(child_content.contains("nested body"), "nested issue body lost");
}

#[tokio::test]
async fn test_closing_nested_issue_creates_bak_file() {
	let ctx = TestContext::build("");

	// Start with open nested issue
	let initial_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body content\n",
	);

	ctx.consensus(&initial_issue, None).await;
	ctx.remote(&initial_issue, None);

	// User closes nested issue during edit
	let edited_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [x] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body content\n",
	);

	let out = ctx.open_issue(&initial_issue).edit(&edited_issue).run();
	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "stderr: {}", out.stderr);

	insta::assert_snapshot!(render_fixture(FixtureRenderer::try_new(&ctx).unwrap().redact_timestamps(&[20]), &out), @r#"
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
	        [REDACTED - non-deterministic timestamp]
	        "comments": []
	      }
	    }
	  }
	}
	//- /o/r/1_-_a/2_-_b.md.bak
	- [x] b <!-- @mock_user https://github.com/o/r/issues/2 -->
			nested body content
	//- /o/r/1_-_a/__main__.md
	- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->
			lorem ipsum
	"#);

	// With the new model, closed child is in a separate .bak file
	let path = ctx.resolve_issue_path(&initial_issue);
	let closed_child_path = path.parent().unwrap().join("2_-_b.md.bak");
	assert!(closed_child_path.exists(), "closed nested issue should have .bak file");

	let child_content = read_issue_file(&closed_child_path);
	assert!(child_content.contains("- [x] b"), "nested issue not marked closed");
	assert!(child_content.contains("nested body content"), "child body should be preserved");
}
