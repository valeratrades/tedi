//! `tedi pull`: every issue of the named orgs and repos lands in the store, through the same merge as
//! any other sync; a repo's cursor keeps the next pull to what changed since.

use crate::common::{Seed, TestContext, issue_tree, parse_virtual};

#[tokio::test]
async fn pull_materializes_every_issue_of_an_org() {
	let ctx = TestContext::build();
	ctx.remote(
		&parse_virtual("- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n\n  - [ ] Child <!-- @mock_user https://github.com/o/r/issues/2 -->\n    child body\n"),
		Some(Seed::new(0)),
	);
	ctx.remote(&parse_virtual("- [x] Done one <!-- @mock_user https://github.com/o/r/issues/3 -->\n  body\n"), Some(Seed::new(0)));
	ctx.remote_in(
		("o", "other").into(),
		&parse_virtual("- [ ] Elsewhere <!-- @mock_user https://github.com/o/other/issues/1 -->\n  body\n"),
		Some(Seed::new(0)),
	);

	let out = ctx.run(&["--mock", "pull", "o"]);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(issue_tree(&ctx), @"
	o/other/1_-_Elsewhere.md
	o/r/1_-_Parent/2_-_Child.md
	o/r/1_-_Parent/__main__.md
	o/r/3_-_Done_one.md.bak
	");
}

#[tokio::test]
async fn pull_updates_a_stored_issue_and_advances_the_cursor() {
	let ctx = TestContext::build();
	ctx.consensus(
		&parse_virtual("- [ ] Old title <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(-100)),
	)
	.await;
	ctx.remote(
		&parse_virtual("- [ ] New title <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(100)),
	);

	let out = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(issue_tree(&ctx), @"o/r/1_-_New_title.md");

	let again = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(again.status.success(), "stderr: {}", again.stderr);
	let calls = ctx.github_calls();
	let second = calls.iter().rposition(|c| c.starts_with("list_repo_issues")).expect("each pull lists");
	assert!(calls[second].contains("Some("), "the second pull starts from the first one's cursor: {calls:?}");
	assert!(!calls[second..].iter().any(|c| c.starts_with("fetch_issue")), "nothing changed since: {calls:?}");
}
