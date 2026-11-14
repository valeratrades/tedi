//! A clash is data: each field both sides changed since consensus, held pending per issue.

use crate::common::{Seed, TestContext, parse_virtual};

async fn clash_on_one(ctx: &TestContext) -> tedi_task_operations::Issue {
	ctx.consensus(
		&parse_virtual("- [ ] One <!-- @mock_user https://github.com/o/r/issues/1 -->\n  consensus body\n"),
		Some(Seed::new(-100)),
	)
	.await;
	let local = ctx
		.local(
			&parse_virtual("- [ ] One, locally <!-- @mock_user https://github.com/o/r/issues/1 -->\n  local body\n"),
			Some(Seed::new(100)),
		)
		.await;
	ctx.remote(
		&parse_virtual("- [ ] One, remotely <!-- @mock_user https://github.com/o/r/issues/1 -->\n  remote body\n"),
		Some(Seed::new(100)),
	);
	local
}

#[tokio::test]
async fn a_clash_names_each_field_both_sides_changed() {
	let ctx = TestContext::build();
	let local = clash_on_one(&ctx).await;

	let out = ctx.open_issue(&local).run();

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	for field in ["title", "body"] {
		assert!(out.stderr.contains(&format!("{field}:")), "{field} is not named: {}", out.stderr);
	}
	assert!(std::fs::read_to_string(ctx.conflict_file(("o", "r").into(), 1)).unwrap().contains("<<<<<<<"));
}

#[tokio::test]
async fn a_clash_blocks_only_its_own_issue() {
	let ctx = TestContext::build();
	let local = clash_on_one(&ctx).await;
	let two = parse_virtual("- [ ] Two <!-- @mock_user https://github.com/o/r/issues/2 -->\n  body\n");
	let bystander = ctx.consensus(&two, Some(Seed::new(0))).await;
	ctx.remote(&two, Some(Seed::new(0)));

	assert!(!ctx.open_issue(&local).run().status.success());
	let out = ctx.open_issue(&bystander).run();

	assert!(out.status.success(), "o/r#2 was blocked by o/r#1's clash. stderr: {}", out.stderr);
}
