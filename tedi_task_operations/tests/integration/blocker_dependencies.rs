//! `# Blockers` ⇄ Github's own `blocked by` links: every issue a blocker names is linked there, and a
//! link made on Github alone joins the blockers. Issues on either end of a link land locally.

use tedi_task_operations::{
	IssueLink, RepoInfo,
	local::{FsReader, Local},
};

use crate::common::{
	Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

fn link(owner: &str, repo: &str, number: u64) -> IssueLink {
	IssueLink::in_project(RepoInfo::new(owner, repo), number)
}

fn issue(owner: &str, repo: &str, number: u64, title: &str, blockers: &str) -> tedi_task_operations::VirtualIssue {
	let section = match blockers {
		"" => String::new(),
		b => format!("\n  # Blockers\n{}", b.lines().map(|l| format!("  {l}\n")).collect::<String>()),
	};
	parse_virtual(&format!("- [ ] {title} <!-- @mock_user https://github.com/{owner}/{repo}/issues/{number} -->\n  body\n{section}"))
}

fn dependency_calls(ctx: &TestContext) -> String {
	ctx.github_calls().into_iter().filter(|c| c.contains("blocked_by(")).collect::<Vec<_>>().join("\n")
}

fn stored(owner: &str, repo: &str, number: u64) -> Option<std::path::PathBuf> {
	Local::find_by_number(RepoInfo::new(owner, repo), number, FsReader)
}

#[tokio::test]
async fn test_named_blockers_are_linked_on_github() {
	let ctx = TestContext::build();
	let base = ctx.consensus(&issue("o", "r", 1, "Target", "- step"), Some(Seed::new(0))).await;
	ctx.remote(&issue("o", "r", 1, "Target", "- step"), Some(Seed::new(0)));
	ctx.remote(&issue("o", "r", 2, "Same repo", ""), None);
	ctx.remote_in(RepoInfo::new("o", "other"), &issue("o", "other", 5, "Same owner", ""), None);
	ctx.remote_in(RepoInfo::new("q", "x"), &issue("q", "x", 7, "Other owner", ""), None);

	let out = ctx
		.open_issue(&base)
		.edit(&issue("o", "r", 1, "Target", "- other#5\n- q/x#7\n- step\n  - #2\n- plain text #9"))
		.run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(dependency_calls(&ctx), @"
	add_blocked_by(o/r#1, blocker=o/other#5)
	add_blocked_by(o/r#1, blocker=q/x#7)
	add_blocked_by(o/r#1, blocker=o/r#2)
	");
}

#[tokio::test]
async fn test_github_only_link_lands_atop_blockers() {
	let ctx = TestContext::build();
	let base = ctx.consensus(&issue("o", "r", 1, "Target", "- step one\n- step two"), Some(Seed::new(0))).await;
	ctx.remote(&issue("o", "r", 1, "Target", "- step one\n- step two"), Some(Seed::new(0)));
	ctx.remote(&issue("o", "r", 2, "Same repo", ""), None);
	ctx.remote_in(RepoInfo::new("o", "other"), &issue("o", "other", 3, "Same owner", ""), None);
	ctx.remote_in(RepoInfo::new("q", "x"), &issue("q", "x", 4, "Other owner", ""), None);
	for blocker in [link("o", "r", 2), link("o", "other", 3), link("q", "x", 4)] {
		ctx.remote_blocked_by(&link("o", "r", 1), &blocker, Some(Seed::new(50)));
	}

	let out = ctx.open_issue(&base).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(read_issue_file(&ctx.resolve_issue_path(&base)), @"
	- [ ] Target <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body

	  # Blockers
	  - q/x#4
	  - other#3
	  - #2
	  - step one
	  - step two
	");
	insta::assert_snapshot!(dependency_calls(&ctx), @"");
}

#[tokio::test]
async fn test_dropped_blocker_is_unlinked_on_github() {
	let ctx = TestContext::build();
	let base = ctx.consensus(&issue("o", "r", 1, "Target", "- #2\n- step"), Some(Seed::new(0))).await;
	ctx.remote(&issue("o", "r", 1, "Target", "- #2\n- step"), Some(Seed::new(0)));
	ctx.remote(&issue("o", "r", 2, "Same repo", ""), None);
	ctx.remote_blocked_by(&link("o", "r", 1), &link("o", "r", 2), Some(Seed::new(-50)));

	let out = ctx.open_issue(&base).edit(&issue("o", "r", 1, "Target", "- step")).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(dependency_calls(&ctx), @"remove_blocked_by(o/r#1, blocker=o/r#2)");
}

/// Both ends of a link are pulled in: what blocks us, and what we block. A sub-issue comes with
/// its whole tree, so it lands where it lives.
#[tokio::test]
async fn test_linked_issues_missing_locally_are_pulled_from_their_root() {
	let ctx = TestContext::build();
	let base = ctx.consensus(&issue("o", "r", 1, "Target", "- other#5"), Some(Seed::new(0))).await;
	ctx.remote(&issue("o", "r", 1, "Target", "- other#5"), Some(Seed::new(0)));
	ctx.remote_in(
		RepoInfo::new("o", "other"),
		&parse_virtual(
			"- [ ] Root <!-- @mock_user https://github.com/o/other/issues/4 -->\n  root body\n\n  - [ ] Leaf <!--sub @mock_user https://github.com/o/other/issues/5 -->\n    leaf body\n",
		),
		None,
	);
	ctx.remote_in(RepoInfo::new("q", "x"), &issue("q", "x", 9, "Downstream", "- tail"), None);
	ctx.remote_blocked_by(&link("o", "r", 1), &link("o", "other", 5), None);
	ctx.remote_blocked_by(&link("q", "x", 9), &link("o", "r", 1), None);

	let out = ctx.open_issue(&base).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	let leaf = stored("o", "other", 5).expect("blocker pulled");
	assert!(stored("o", "other", 4).is_some_and(|root| leaf.starts_with(root.parent().unwrap())), "{}", leaf.display());
	insta::assert_snapshot!(read_issue_file(&stored("q", "x", 9).expect("blocked issue pulled")), @"
	- [ ] Downstream <!-- @mock_user https://github.com/q/x/issues/9 -->
	  body

	  # Blockers
	  - o/r#1
	  - tail
	");
}

/// A tree already stored here is synced to pick up the linked sub-issue, never overwritten.
#[tokio::test]
async fn test_linked_sub_issue_new_under_a_stored_tree_is_synced_in() {
	let ctx = TestContext::build();
	let base = ctx.consensus(&issue("o", "r", 1, "Target", "- other#5"), Some(Seed::new(0))).await;
	ctx.remote(&issue("o", "r", 1, "Target", "- other#5"), Some(Seed::new(0)));
	let root = "- [ ] Root <!-- @mock_user https://github.com/o/other/issues/4 -->\n  root body\n";
	ctx.consensus_in(RepoInfo::new("o", "other"), &parse_virtual(root), Some(Seed::new(0))).await;
	ctx.remote_in(
		RepoInfo::new("o", "other"),
		&parse_virtual(&format!("{root}\n  - [ ] Leaf <!--sub @mock_user https://github.com/o/other/issues/5 -->\n    leaf body\n")),
		Some(Seed::new(0)),
	);

	let out = ctx.open_issue(&base).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	let leaf = stored("o", "other", 5).expect("blocker synced in");
	assert!(stored("o", "other", 4).is_some_and(|root| leaf.starts_with(root.parent().unwrap())), "{}", leaf.display());
}
