//! `tedi pull`: every issue of the named orgs and repos lands in the store, through the same merge as
//! any other sync; a repo's cursor keeps the next pull to what changed since.

use crate::common::{Seed, TestContext, issue_tree, parse_virtual};

const ENV_MOCK_RATE_LIMITED_AFTER_PAGES: &str = concat!("tedi", "_MOCK_RATE_LIMITED_AFTER_PAGES");

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

/// GitHub's budget is per call: a pull reads a page of whole issues per call, children included.
#[tokio::test]
async fn a_pull_reads_whole_issues_a_page_at_a_time() {
	let ctx = TestContext::build();
	ctx.remote(
		&parse_virtual("- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n\n  - [ ] Child <!-- @mock_user https://github.com/o/r/issues/2 -->\n    child body\n"),
		Some(Seed::new(0)),
	);
	ctx.remote(&parse_virtual("- [ ] Third <!-- @mock_user https://github.com/o/r/issues/3 -->\n  body\n"), Some(Seed::new(0)));

	let out = ctx.run(&["--mock", "pull", "o/r"]);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(ctx.github_calls().join("\n"), @r#"
	fetch_authenticated_user()
	list_labels(o/r)
	list_assignable(o/r)
	list_repo_issues(o/r, None, None)
	list_repo_issues(o/r, None, Some("2"))
	list_milestones
	list_repo_prs(o/r, None)
	"#);
}

/// Running out of budget mid-pull fails it, keeping what landed; the next pull starts from there.
#[tokio::test]
async fn a_pull_cut_short_resumes_after_the_last_page_it_stored() {
	let ctx = TestContext::build();
	for (n, seed) in [(1, -60), (2, -30), (3, 0)] {
		ctx.remote(
			&parse_virtual(&format!("- [ ] Issue {n} <!-- @mock_user https://github.com/o/r/issues/{n} -->\n  body\n")),
			Some(Seed::new(seed)),
		);
	}

	let cut = ctx.run_with_env(&["--mock", "pull", "o/r"], &[(ENV_MOCK_RATE_LIMITED_AFTER_PAGES, "1")]);
	assert!(!cut.status.success(), "a pull that didn't store everything fails. stdout: {}", cut.stdout);
	insta::assert_snapshot!(issue_tree(&ctx), @"
	o/r/1_-_Issue_1.md
	o/r/3_-_Issue_3.md
	");

	let resumed = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(resumed.status.success(), "stderr: {}", resumed.stderr);
	insta::assert_snapshot!(issue_tree(&ctx), @"
	o/r/1_-_Issue_1.md
	o/r/2_-_Issue_2.md
	o/r/3_-_Issue_3.md
	");
	let lists: Vec<String> = ctx.github_calls().into_iter().filter(|c| c.starts_with("list_repo_issues")).collect();
	insta::assert_snapshot!(lists.join("\n"), @r#"
	list_repo_issues(o/r, None, None)
	list_repo_issues(o/r, None, Some("2"))
	list_repo_issues(o/r, Some(2001-09-11T14:19:09Z), None)
	"#);
}

#[tokio::test]
async fn a_pull_into_a_store_that_cant_commit_fails() {
	let ctx = TestContext::build();
	ctx.remote(&parse_virtual("- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"), Some(Seed::new(0)));
	let git_dir = ctx.xdg.data_dir().join("issues/.git");
	if git_dir.exists() {
		std::fs::remove_dir_all(&git_dir).unwrap();
	}

	let out = ctx.run(&["--mock", "pull", "o/r"]);

	assert!(!out.status.success(), "nothing reached consensus. stdout: {}", out.stdout);
}
