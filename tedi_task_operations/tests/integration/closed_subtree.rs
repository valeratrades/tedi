//! A closed issue never has an open descendant: an edit producing one is rejected whole.

use tedi_task_operations::{
	RepoInfo,
	local::{FsReader, Local},
};

use crate::common::{TestContext, parse_virtual};

const TREE: &str = "- [ ] Parent task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  parent body\n\n  - [ ] Open child <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n    child body\n\n    - [ ] Open grandchild <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n      grandchild body\n";

fn local_path(number: u64) -> std::path::PathBuf {
	Local::find_by_number(RepoInfo::new("o", "r"), number, FsReader).expect("stored locally")
}

#[tokio::test]
async fn test_closing_parent_over_open_child_is_rejected() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");
	let vi = parse_virtual(TREE);
	let issue = ctx.consensus(&vi, None).await;
	ctx.remote(&vi, None);

	let (vpath, paused) = ctx.open_issue(&issue).break_to_edit();
	let before = std::fs::read_to_string(&vpath).unwrap();
	std::fs::write(&vpath, before.replacen("- [ ] Parent task", "- [x] Parent task", 1)).unwrap();

	let out = paused.resume();
	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("rejected-changes.md"), "stderr: {}", out.stderr);
	assert!(out.stderr.contains("Parent task") && out.stderr.contains("Open child"), "stderr: {}", out.stderr);
	assert_eq!(std::fs::read_to_string(&vpath).unwrap(), before);
	assert!(std::fs::read_to_string(local_path(2)).unwrap().contains("- [ ] Open child"));
	assert!(!ctx.github_calls().iter().any(|c| c.starts_with("update_issue")), "{:?}", ctx.github_calls());
}

#[tokio::test]
async fn test_closing_parent_with_its_children_in_one_save() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");
	let vi = parse_virtual(&TREE.replace("- [ ] Open grandchild", "- [x] Open grandchild"));
	let issue = ctx.consensus(&vi, None).await;
	ctx.remote(&vi, None);

	let (vpath, paused) = ctx.open_issue(&issue).break_to_edit();
	let before = std::fs::read_to_string(&vpath).unwrap();
	std::fs::write(
		&vpath,
		before
			.replacen("- [ ] Parent task", "- [x] Parent task", 1)
			.replacen("- [ ] [Open child]", "- [x] [Open child]", 1),
	)
	.unwrap();

	let out = paused.resume();
	assert!(out.status.success(), "stderr: {}", out.stderr);
	let child = local_path(2);
	assert!(child.to_string_lossy().ends_with(".bak"), "{}", child.display());
	assert!(std::fs::read_to_string(&child).unwrap().contains("- [x] Open child"));
	let calls = ctx.github_calls();
	for n in [1, 2] {
		assert!(calls.iter().any(|c| c == &format!("update_issue_state(o, r, {n}, closed)")), "#{n}: {calls:?}");
	}
}

/// The grandchild isn't in the edited buffer, only in the tree it was loaded with.
#[tokio::test]
async fn test_closing_child_over_open_grandchild_is_rejected() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");
	let vi = parse_virtual(TREE);
	let issue = ctx.consensus(&vi, None).await;
	ctx.remote(&vi, None);

	let (vpath, paused) = ctx.open_issue(&issue).break_to_edit();
	let before = std::fs::read_to_string(&vpath).unwrap();
	assert!(!before.contains("Open grandchild"), "{before}");
	std::fs::write(&vpath, before.replacen("- [ ] [Open child]", "- [x] [Open child]", 1)).unwrap();

	let out = paused.resume();
	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("Open grandchild"), "stderr: {}", out.stderr);
	assert_eq!(std::fs::read_to_string(&vpath).unwrap(), before);
}
