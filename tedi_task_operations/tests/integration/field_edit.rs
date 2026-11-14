//! A field edit carries the value it was made against: landing on a moved-on issue is a conflict, never a revert.

use crate::common::{
	RunOutput, Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

const URL: &str = "https://github.com/o/r/issues/1";
const ISSUE: &str = "- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n";

/// `ISSUE` agreed on everywhere, then GitHub holds `remote`; the edit `field: seen → new` runs.
async fn check(ctx: &TestContext, remote: &str, field: &str, seen: &str, new: &str) -> (RunOutput, String) {
	let issue = ctx.consensus(&parse_virtual(ISSUE), Some(Seed::new(-100))).await;
	ctx.remote(&parse_virtual(remote), Some(Seed::new(100)));
	ctx.remote_labels("o", "r", &["s:selected", "t:bug"]);
	let out = ctx.run(&["--mock", "set", URL, field, new, "--seen", seen]);
	(out, read_issue_file(&ctx.resolve_issue_path(&issue)))
}

#[tokio::test]
async fn an_edit_against_the_current_value_lands() {
	let ctx = TestContext::build();
	let (out, stored) = check(&ctx, ISSUE, "title", "Task", "Renamed").await;

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(stored, @"
	- [ ] Renamed <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	");
	assert!(ctx.github_calls().iter().any(|c| c == "update_issue_title(o, r, 1, Renamed)"), "{:?}", ctx.github_calls());
}

#[tokio::test]
async fn an_edit_against_a_stale_value_is_a_conflict_and_writes_nothing() {
	let ctx = TestContext::build();
	let (out, stored) = check(&ctx, &ISSUE.replace("Task", "Moved on"), "title", "Task", "Renamed").await;

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("title:"), "the clashing field is named: {}", out.stderr);
	insta::assert_snapshot!(stored, @"
	- [ ] Moved on <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	");
	assert!(!ctx.conflict_file(("o", "r").into(), 1).exists(), "a stale edit blocks nobody: nothing diverged in the store");
}

/// Dropping a card on Selected is a labels edit; it lands `s:selected`, and selecting takes the issue.
#[tokio::test]
async fn a_card_dropped_on_selected_lands_the_pin() {
	let ctx = TestContext::build();
	let (out, stored) = check(&ctx, ISSUE, "labels", "", "s:selected").await;

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(stored, @"
	- [ ] (s:selected, @mock_user) Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	");
}

#[tokio::test]
async fn a_close_lands_as_the_state() {
	let ctx = TestContext::build();
	let (out, stored) = check(&ctx, ISSUE, "state", " ", "x").await;

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(stored, @"
	- [x] Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	");
}
