//! `tedi view`: a query over the store opened as a task view; edits to the issues it expands sync.

use std::{cell::RefCell, process::Command, rc::Rc};

use crate::{
	common::{
		Seed, TestContext,
		are_you_sure::{UnsafePathExt, read_issue_file},
		get_binary_path, parse_virtual,
	},
	sprints::EditFn,
};

const BUG: &str = "- [ ] (t:bug, s:selected) Fix it <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n";
const CHORE: &str = "- [ ] (t:chore) Tidy <!-- @mock_user https://github.com/o/r/issues/2 -->\n  chore body\n";

#[tokio::test]
async fn a_view_shows_what_matches_and_its_edits_sync() {
	let ctx = TestContext::build();
	let bug = ctx.consensus(&parse_virtual(BUG), Some(Seed::new(0))).await;
	let chore = ctx.consensus(&parse_virtual(CHORE), Some(Seed::new(0))).await;
	ctx.remote(&parse_virtual(BUG), Some(Seed::new(0)));
	ctx.remote(&parse_virtual(CHORE), Some(Seed::new(0)));
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), "[board]\narchive_after = \"1w\"\nviews = []\n").unwrap();

	let shown = Rc::new(RefCell::new(String::new()));
	let seen = shown.clone();
	let mut cmd = Command::new(get_binary_path());
	cmd.args(["--mock", "view", "stage:selected", "--group", "stage"]);
	cmd.env(concat!("tedi", "_MOCK_STATE"), &ctx.mock_state_path);
	let out = ctx.drive_editor_session(
		cmd,
		Some(Box::new(move |tmp: &std::path::Path| {
			let buffer = std::fs::read_to_string(tmp).unwrap();
			*seen.borrow_mut() = buffer.clone();
			std::fs::write(tmp, buffer.replacen("  body", "  edited body", 1)).unwrap();
		}) as EditFn),
	);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(shown.borrow(), @"
	# selected

	- [ ] (t:bug, s:selected) Fix it <!-- @mock_user https://github.com/o/r/issues/1 --> <!--{{{3-->
	  body
	  <!--}}}3-->
	");
	assert!(read_issue_file(&ctx.resolve_issue_path(&bug)).contains("edited body"));
	assert_eq!(read_issue_file(&ctx.resolve_issue_path(&chore)), CHORE);
}
