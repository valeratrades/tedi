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

/// A title is anything GitHub takes: one with nothing a file name can hold, or longer than one may be.
#[tokio::test]
async fn pull_stores_an_issue_whatever_its_title() {
	let ctx = TestContext::build();
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), "[board]\nviews = []\nworkflows = { kanban = \"preset:kanban\" }\n").unwrap();
	let long = "я".repeat(256);
	ctx.remote(
		&parse_virtual("- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n\n  - [ ] . <!-- @mock_user https://github.com/o/r/issues/2 -->\n    child body\n"),
		None,
	);
	ctx.remote(&parse_virtual("- [x] \") { <!-- @mock_user https://github.com/o/r/issues/3 -->\n  body\n"), None);
	ctx.remote(&parse_virtual(&format!("- [x] {long} <!-- @mock_user https://github.com/o/r/issues/4 -->\n  body\n")), None);

	let mut trees = Vec::new();
	for body in ["was", "is"] {
		for n in 1..=4 {
			ctx.set_remote_body(("o", "r").into(), n, body);
		}
		let out = ctx.run(&["--mock", "pull", "o/r"]);
		assert!(out.status.success(), "stderr: {}", out.stderr);
		trees.push(issue_tree(&ctx));
	}

	assert_eq!(trees[0], trees[1], "a pull over what is stored finds it");
	insta::assert_snapshot!(trees[0].replace(&"я".repeat(112), "<я×112>"), @"
	o/r/1_-_Parent/2_-_untitled.md
	o/r/1_-_Parent/__main__.md
	o/r/3_-_untitled.md.bak
	o/r/4_-_<я×112>.md.bak
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

	let first = ctx.github_calls().len();
	let again = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(again.status.success(), "stderr: {}", again.stderr);
	insta::assert_snapshot!(ctx.github_calls()[first..].join("\n"), @"
	fetch_authenticated_user()
	repo_head(o/r)
	");
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
	repo_head(o/r)
	list_repo_issues(o/r, None, None)
	list_repo_issues(o/r, None, Some("2"))
	"#);
}

/// Most of an owner's repos are quiet most of the time: its listing is all a pull reads of those.
#[tokio::test]
async fn a_repo_unchanged_since_it_was_stored_is_not_listed() {
	let ctx = TestContext::build();
	ctx.remote(&parse_virtual("- [ ] Quiet <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"), Some(Seed::new(0)));
	let pull = || {
		let out = ctx.run(&["--mock", "pull", "o"]);
		assert!(out.status.success(), "stderr: {}", out.stderr);
	};
	pull();
	let first = ctx.github_calls().len();
	pull();
	let second = ctx.github_calls().len();
	ctx.remote_in(
		("o", "r").into(),
		&parse_virtual("- [ ] New <!-- @mock_user https://github.com/o/r/issues/2 -->\n  body\n"),
		Some(Seed::new(50)),
	);
	pull();

	let calls = ctx.github_calls();
	insta::assert_snapshot!(format!("{}\n---\n{}\n---\n{}", calls[first..second].join("\n"), calls[second..].join("\n"), issue_tree(&ctx)), @"
	fetch_authenticated_user()
	list_owner_repos(o)
	---
	fetch_authenticated_user()
	list_owner_repos(o)
	list_repo_issues(o/r, Some(2001-09-11T10:16:54Z), None)
	---
	o/r/1_-_Quiet.md
	o/r/2_-_New.md
	");
}

/// Nothing more of an owner is asked for once its budget is spent.
#[tokio::test]
async fn a_spent_budget_is_not_asked_of_again() {
	let ctx = TestContext::build();
	for repo in ["a", "b", "c"] {
		ctx.remote_in(
			("o", repo).into(),
			&parse_virtual(&format!("- [ ] Task <!-- @mock_user https://github.com/o/{repo}/issues/1 -->\n  body\n")),
			Some(Seed::new(0)),
		);
	}

	let out = ctx.run_with_env(&["--mock", "pull", "o"], &[(ENV_MOCK_RATE_LIMITED_AFTER_PAGES, "1")]);

	assert!(!out.status.success(), "a pull that didn't store everything fails. stdout: {}", out.stdout);
	assert!(out.stderr.contains("o/b, o/c"), "all that is behind is named: {}", out.stderr);
	let lists = ctx.github_calls().into_iter().filter(|c| c.starts_with("list_repo_issues")).collect::<Vec<_>>();
	insta::assert_snapshot!(lists.join("\n"), @"
	list_repo_issues(o/a, None, None)
	list_repo_issues(o/b, None, None)
	");
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
	let snap_before = issue_tree(&ctx);

	let resumed = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(resumed.status.success(), "stderr: {}", resumed.stderr);
	let snap_resumed = issue_tree(&ctx);
	let lists: Vec<String> = ctx.github_calls().into_iter().filter(|c| c.starts_with("list_repo_issues")).collect();
	insta::assert_snapshot!(format!("{snap_before}\n---\n{snap_resumed}\n---\n{}", lists.join("\n")), @r#"
	o/r/1_-_Issue_1.md
	o/r/3_-_Issue_3.md
	---
	o/r/1_-_Issue_1.md
	o/r/2_-_Issue_2.md
	o/r/3_-_Issue_3.md
	---
	list_repo_issues(o/r, None, None)
	list_repo_issues(o/r, None, Some("2"))
	list_repo_issues(o/r, Some(2001-09-11T14:19:09Z), None)
	"#);
}

/// What was stored before creation was read gets its date from the next pull, though nothing of it changed since.
#[tokio::test]
async fn a_pull_dates_what_was_stored_undated() {
	let ctx = TestContext::build();
	for n in [1, 2] {
		ctx.remote(
			&parse_virtual(&format!("- [ ] Issue {n} <!-- @mock_user https://github.com/o/r/issues/{n} -->\n  body\n")),
			Some(Seed::new(n)),
		);
	}
	let pull = || {
		let out = ctx.run(&["--mock", "pull", "o/r"]);
		assert!(out.status.success(), "stderr: {}", out.stderr);
	};
	let meta_path = ctx.xdg.data_dir().join("issues/o/r/.meta.json");
	let meta = || -> serde_json::Value { serde_json::from_str(&std::fs::read_to_string(&meta_path).unwrap()).unwrap() };
	pull();
	let dated = meta();
	assert!(dated["issues"]["1"]["created"].is_string(), "{dated}");

	let mut undated = dated.clone();
	undated.as_object_mut().unwrap().remove("dated").expect("a whole pull dates all it stores");
	for n in ["1", "2"] {
		undated["issues"][n].as_object_mut().unwrap().remove("created").expect("pulled");
	}
	std::fs::write(&meta_path, undated.to_string()).unwrap();
	pull();
	assert_eq!(meta(), dated);

	pull();
	let lists = ctx.github_calls().into_iter().filter(|c| c.starts_with("list_repo_issues(o/r, None, None")).count();
	assert_eq!(lists, 2, "the first pull, and the one that dated: never again");
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

/// What a pulled milestone contains comes with it, and the issues those host: of repos nobody named, by link.
#[test]
fn a_pulled_milestone_brings_what_it_contains() {
	let ctx = TestContext::build();
	let task = |title: &str| parse_virtual(&format!("- [ ] {title} <!-- @mock_user https://github.com/q/data/issues/5 -->\n  body\n"));
	ctx.remote_in(("q", "data").into(), &task("Was"), Some(Seed::new(-100)));
	ctx.remote_milestone("o/r", 1, "v1", "- https://github.com/p/tilers/milestone/1\n- https://github.com/hidden/x/milestone/1", &[]);
	ctx.remote_milestone("p/tilers", 1, "0.5", "- https://github.com/q/data/milestone/1", &[]);
	ctx.remote_milestone("q/data", 1, "1.0", "grid", &[5]);

	let out = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stderr.contains("https://github.com/hidden/x/milestone/1 was not pulled"), "told: {}", out.stderr);
	let snap_first = issue_tree(&ctx);

	ctx.set_remote_title(("q", "data").into(), 5, "Is");
	let out = ctx.run(&["--mock", "pull", "o/r"]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
	let milestones = std::fs::read_to_string(ctx.xdg.data_dir().join("issues/__milestones__/o/r/1_-_v1.md")).unwrap();
	insta::assert_snapshot!(format!("{snap_first}\n---\n{}\n---\n{milestones}", issue_tree(&ctx)), @"
	q/data/5_-_Was.md
	---
	q/data/5_-_Is.md
	---
	- https://github.com/p/tilers/milestone/1
	- https://github.com/hidden/x/milestone/1
	");
}
