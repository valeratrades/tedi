//! Assignees are one more field of the four-way merge, written as `@login` in the title line's tag group.

use crate::common::{
	Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

#[tokio::test]
async fn remote_assignees_land_in_the_title_line() {
	let ctx = TestContext::build();
	let vi = parse_virtual("- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	let issue = ctx.consensus(&vi, Some(Seed::new(-100))).await;
	ctx.remote(
		&parse_virtual("- [ ] (bug, @alice) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(100)),
	);

	let out = ctx.open_issue(&issue).run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(read_issue_file(&ctx.resolve_issue_path(&issue)), @"
	- [ ] (bug, @alice) Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	");
}

#[tokio::test]
async fn local_assignees_push_as_one_replacing_set() {
	let ctx = TestContext::build();
	let vi = parse_virtual("- [ ] (@alice) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n");
	let issue = ctx.consensus(&vi, Some(Seed::new(5))).await;
	ctx.remote(&vi, Some(Seed::new(5)));

	let out = ctx
		.open_issue(&issue)
		.edit(&parse_virtual("- [ ] (@bob, @carol) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"))
		.run();

	assert!(out.status.success(), "stderr: {}", out.stderr);
	let pushed: Vec<String> = ctx.github_calls().into_iter().filter(|c| c.starts_with("set_assignees")).collect();
	insta::assert_snapshot!(pushed.join("\n"), @r#"set_assignees(o, r, 1, ["bob", "carol"])"#);
}

/// Both sides reassigned since consensus: neither knew of the other.
#[tokio::test]
async fn assignees_changed_on_both_sides_conflict() {
	let ctx = TestContext::build();
	ctx.consensus(
		&parse_virtual("- [ ] (@alice) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(-50)),
	)
	.await;
	let local = ctx
		.local(
			&parse_virtual("- [ ] (@bob) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
			Some(Seed::new(10)),
		)
		.await;
	ctx.remote(
		&parse_virtual("- [ ] (@carol) Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(20)),
	);

	let out = ctx.open_issue(&local).run();

	assert!(!out.status.success(), "a two-sided reassignment must not resolve silently. stdout: {}", out.stdout);
}
