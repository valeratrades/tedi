//! An edit's labels are checked before anything reaches GitHub: one value per single-valued field of
//! the scheme, and only labels the repo has.

use crate::common::{
	Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

const ISSUE: &str = "- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n";

#[track_caller]
fn check(repo_labels: &[&str], edited_tags: &str, pushed: Option<&str>) {
	let rt = tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
	rt.block_on(async {
		let ctx = TestContext::build();
		let issue = ctx.consensus(&parse_virtual(ISSUE), Some(Seed::new(5))).await;
		ctx.remote(&parse_virtual(ISSUE), Some(Seed::new(5)));
		ctx.remote_labels("o", "r", repo_labels);

		let out = ctx
			.open_issue(&issue)
			.edit(&parse_virtual(&ISSUE.replacen("- [ ] ", &format!("- [ ] ({edited_tags}) "), 1)))
			.run();

		let set_labels: Vec<String> = ctx.github_calls().into_iter().filter(|c| c.starts_with("set_labels")).collect();
		match pushed {
			Some(pushed) => {
				assert!(out.status.success(), "stderr: {}", out.stderr);
				assert_eq!(set_labels, [pushed]);
			}
			None => {
				assert!(!out.status.success(), "stdout: {}", out.stdout);
				assert_eq!(set_labels, Vec::<String>::new());
				assert_eq!(read_issue_file(&ctx.resolve_issue_path(&issue)), ISSUE, "the stored issue is untouched");
			}
		}
	});
}

#[test]
fn labels_the_repo_has_push() {
	check(&["t:bug", "i:5", "c:ci"], "t:bug, i:5, c:ci", Some(r#"set_labels(o, r, 1, ["t:bug", "i:5", "c:ci"])"#));
}

#[test]
fn two_values_of_a_single_valued_field_are_rejected() {
	check(&["t:bug", "t:chore"], "t:bug, t:chore", None);
}

#[test]
fn a_label_the_repo_lacks_is_rejected() {
	check(&["t:bug"], "t:bug, c:cq", None);
}

/// Selecting or starting an unassigned issue takes it: the editor becomes its assignee.
#[test]
fn pinning_an_unassigned_issue_takes_it() {
	let rt = tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap();
	rt.block_on(async {
		let ctx = TestContext::build();
		let issue = ctx.consensus(&parse_virtual(ISSUE), Some(Seed::new(5))).await;
		ctx.remote(&parse_virtual(ISSUE), Some(Seed::new(5)));
		ctx.remote_labels("o", "r", &["s:wip"]);

		let out = ctx.open_issue(&issue).edit(&parse_virtual(&ISSUE.replacen("- [ ] ", "- [ ] (s:wip) ", 1))).run();

		assert!(out.status.success(), "stderr: {}", out.stderr);
		insta::assert_snapshot!(read_issue_file(&ctx.resolve_issue_path(&issue)), @"
		- [ ] (s:wip, @mock_user) Task <!-- @mock_user https://github.com/o/r/issues/1 -->
		  body
		");
	});
}
