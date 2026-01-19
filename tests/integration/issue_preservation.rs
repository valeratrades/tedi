//! Integration tests for issue content preservation through edit/sync cycles.
//!
//! Tests that nested issues, blockers, and other content survive the
//! parse -> edit -> serialize -> sync cycle intact.

use std::path::Path;

use todo::Issue;

use crate::common::{TestContext, git::GitExt};

fn parse(content: &str) -> Issue {
	Issue::parse_virtual(content, Path::new("test.md")).expect("failed to parse test issue")
}

#[test]
fn test_comments_with_ids_sync_correctly() {
	let ctx = TestContext::new("");

	// Issue with a comment that has an ID
	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tbody text\n\
		 \n\
		 \t<!-- @mock_user https://github.com/o/r/issues/1#issuecomment-12345 -->\n\
		 \tThis is my comment\n",
	);

	ctx.consensus(&issue);
	ctx.remote(&issue);

	let path = ctx.issue_path(&issue);
	let (status, stdout, stderr) = ctx.open(&path).args(&["--force"]).run();
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	// This should NOT fail with "comment X not found in consensus"
	assert!(status.success(), "sync failed: {stderr}");
}

#[test]
fn test_nested_issues_preserved_through_sync() {
	let ctx = TestContext::new("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body b\n\
		 \n\
		 \t- [ ] c <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\tnested body c\n",
	);

	let path = ctx.consensus(&issue);
	ctx.remote(&issue);

	let (status, stdout, stderr) = ctx.run_open(&path);
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	// With the new model, children are stored in separate files in the parent's directory
	let parent_dir = path.parent().unwrap();
	let child_b_path = parent_dir.join("2_-_b.md");
	let child_c_path = parent_dir.join("3_-_c.md");

	let child_b_content = std::fs::read_to_string(&child_b_path).expect("child b file should exist");
	let child_c_content = std::fs::read_to_string(&child_c_path).expect("child c file should exist");

	assert!(child_b_content.contains("nested body b"), "nested issue b body lost");
	assert!(child_c_content.contains("nested body c"), "nested issue c body lost");
}

#[test]
fn test_mixed_open_closed_nested_issues_preserved() {
	let ctx = TestContext::new("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\topen nested body\n\
		 \n\
		 \t- [x] c <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\
		 \t\t<!--omitted {{{always-->\n\
		 \t\tclosed nested body\n\
		 \t\t<!--,}}}-->\n",
	);

	let path = ctx.consensus(&issue);
	ctx.remote(&issue);

	let (status, stdout, stderr) = ctx.run_open(&path);
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	// With the new model, children are stored in separate files
	let parent_dir = path.parent().unwrap();
	let child_b_path = parent_dir.join("2_-_b.md");
	let child_c_path = parent_dir.join("3_-_c.md.bak"); // closed issue has .bak suffix

	let child_b_content = std::fs::read_to_string(&child_b_path).expect("child b file should exist");
	assert!(child_b_content.contains("open nested body"), "open nested issue body lost");

	let child_c_content = std::fs::read_to_string(&child_c_path).expect("child c file should exist");
	assert!(child_c_content.contains("- [x] c"), "closed nested issue state lost");
}

#[test]
fn test_blockers_preserved_through_sync() {
	let ctx = TestContext::new("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- first blocker\n\
		 \t- second blocker\n",
	);

	let path = ctx.consensus(&issue);
	ctx.remote(&issue);

	let (status, stdout, stderr) = ctx.run_open(&path);
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	let final_content = std::fs::read_to_string(&path).unwrap();
	assert!(final_content.contains("# Blockers"), "blockers section lost");
	assert!(final_content.contains("first blocker"), "first blocker lost");
	assert!(final_content.contains("second blocker"), "second blocker lost");
}

#[test]
fn test_blockers_added_during_edit_preserved() {
	let ctx = TestContext::new("");

	// Initial state: no blockers
	let initial_issue = parse("- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tlorem ipsum\n");

	let path = ctx.consensus(&initial_issue);
	ctx.remote(&initial_issue);

	// User adds blockers during edit
	let edited_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- new blocker added\n",
	);

	let (status, stdout, stderr) = ctx.open(&path).edit(&edited_issue).run();
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	let final_content = std::fs::read_to_string(&path).unwrap();
	assert!(final_content.contains("# Blockers"), "blockers section not preserved");
	assert!(final_content.contains("new blocker added"), "added blocker lost");
}

#[test]
fn test_blockers_with_headers_preserved() {
	let ctx = TestContext::new("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t# phase 1\n\
		 \t- task alpha\n\
		 \t- task beta\n\
		 \n\
		 \t# phase 2\n\
		 \t- task gamma\n",
	);

	let path = ctx.consensus(&issue);
	ctx.remote(&issue);

	let (status, stdout, stderr) = ctx.run_open(&path);
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	let final_content = std::fs::read_to_string(&path).unwrap();
	assert!(final_content.contains("# phase 1"), "phase 1 header lost");
	assert!(final_content.contains("# phase 2"), "phase 2 header lost");
	assert!(final_content.contains("task alpha"), "task alpha lost");
	assert!(final_content.contains("task gamma"), "task gamma lost");
}

#[test]
fn test_nested_issues_and_blockers_together() {
	let ctx = TestContext::new("");

	let issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t# Blockers\n\
		 \t- blocker one\n\
		 \t- blocker two\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body\n",
	);

	// Use the path returned by consensus (which is in directory format for issues with children)
	let path = ctx.consensus(&issue);
	ctx.remote(&issue);

	let (status, stdout, stderr) = ctx.run_open(&path);
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	// File is in directory format (path points to __main__.md)
	let final_content = std::fs::read_to_string(&path).unwrap();
	assert!(final_content.contains("# Blockers"), "blockers section lost");
	assert!(final_content.contains("blocker one"), "blocker one lost");

	// With the new model, nested issue is in a separate file
	let child_path = path.parent().unwrap().join("2_-_b.md");
	let child_content = std::fs::read_to_string(&child_path).expect("child file should exist");
	assert!(child_content.contains("nested body"), "nested issue body lost");
}

#[test]
fn test_closing_nested_issue_creates_bak_file() {
	let ctx = TestContext::new("");

	// Start with open nested issue
	let initial_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [ ] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body content\n",
	);

	// Use the path returned by consensus (which is in directory format for issues with children)
	let path = ctx.consensus(&initial_issue);
	ctx.remote(&initial_issue);

	// User closes nested issue during edit
	let edited_issue = parse(
		"- [ ] a <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tlorem ipsum\n\
		 \n\
		 \t- [x] b <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t\tnested body content\n",
	);

	let (status, stdout, stderr) = ctx.open(&path).edit(&edited_issue).run();
	eprintln!("stdout: {stdout}\nstderr: {stderr}");

	assert!(status.success(), "stderr: {stderr}");

	// With the new model, closed child is in a separate .bak file
	let closed_child_path = path.parent().unwrap().join("2_-_b.md.bak");
	assert!(closed_child_path.exists(), "closed nested issue should have .bak file");

	let child_content = std::fs::read_to_string(&closed_child_path).unwrap();
	assert!(child_content.contains("- [x] b"), "nested issue not marked closed");
	assert!(child_content.contains("nested body content"), "child body should be preserved");
}
