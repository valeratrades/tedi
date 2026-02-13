//! Integration tests for blocker commands in integrated mode (issue files).
//!
//! These tests verify that `blocker add` and `blocker pop` work correctly
//! when operating on issue files (integrated mode) rather than standalone blocker files.

use std::path::Path;

use crate::common::{
	TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	git::GitExt,
	parse_virtual,
};

fn revolver_json(issue_path: &Path) -> String {
	revolver_json_multi(&[issue_path], 0, None)
}

fn revolver_json_multi(entries: &[&Path], current_index: usize, previous: Option<&Path>) -> String {
	serde_json::json!({
		"entries": entries,
		"current_index": current_index,
		"previous": previous
	})
	.to_string()
}

#[tokio::test]
async fn test_blocker_add_in_integrated_mode() {
	let ctx = TestContext::build("");

	// Create issue with existing blockers section
	let vi = parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- First task\n",
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json(&issue_path));

	// Run blocker add in integrated mode (no --individual-files flag)
	let out = ctx.run(&["--offline", "blocker", "add", "New task from CLI"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "blocker add should succeed in integrated mode. stderr: {}", out.stderr);

	// new blocker added, existing preserved
	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			Body text.
		
		# Blockers
		- First task
		- New task from CLI
	");
}

#[tokio::test]
async fn test_blocker_pop_in_integrated_mode() {
	let ctx = TestContext::build("");

	// Create issue with multiple blockers
	let vi = parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- First task\n\
		 \t- Second task\n\
		 \t- Third task\n",
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json(&issue_path));

	// Run blocker pop in integrated mode (no --individual-files flag)
	let out = ctx.run(&["--offline", "blocker", "pop"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "blocker pop should succeed in integrated mode. stderr: {}", out.stderr);

	// Third task popped, First and Second remain
	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			Body text.
		
		# Blockers
		- First task
		- Second task
	");
}

#[tokio::test]
async fn test_blocker_add_creates_blockers_section_if_missing() {
	let ctx = TestContext::build("");

	// Create issue WITHOUT blockers section
	let vi = parse_virtual("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tBody text without blockers section.\n");

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json(&issue_path));

	// Run blocker add in integrated mode
	let out = ctx.run(&["--offline", "blocker", "add", "New task"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "blocker add should succeed even without existing blockers section. stderr: {}", out.stderr);

	// blockers section created with new task
	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			Body text without blockers section.
		
		# Blockers
		- New task
	");
}

#[tokio::test]
async fn test_blocker_add_urgent_without_blocker_file_set() {
	// Regression test: `blocker add --urgent` should work even without a blocker file set.
	// The urgent file is owner-independent and doesn't need blocker file context.
	// Previously this errored with "No blocker file set. Use `todo blocker set <pattern>` first."
	let ctx = TestContext::build("");

	// NO blocker file set - this is the key part of the test

	// Run blocker add --urgent
	let out = ctx.run(&["--offline", "blocker", "add", "--urgent", "manage through 'urgent' until tool is working"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(
		out.status.success() && out.stdout.contains("urgent"),
		"Should succeed and confirm urgent add. stdout: {}, stderr: {}",
		out.stdout,
		out.stderr
	);
}

#[tokio::test]
async fn test_blocker_add_with_nested_context() {
	let ctx = TestContext::build("");

	// Create issue with blockers section containing nested items
	let vi = parse_virtual(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- Phase 1\n\
		 \t\t- Setup task\n\
		 \t- Phase 2\n\
		 \t\t- Implementation task\n",
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json(&issue_path));

	// Run blocker add in integrated mode
	let out = ctx.run(&["--offline", "blocker", "add", "New sub-task"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "blocker add should succeed. stderr: {}", out.stderr);

	// new sub-task added under Phase 2
	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
			Body text.
		
		# Blockers
		- Phase 1
			- Setup task
		- Phase 2
			- Implementation task
			- New sub-task
	");
}

#[tokio::test]
async fn test_blocker_toggle_cycles_between_entries() {
	let ctx = TestContext::build("");

	// Create two issues
	let vi1 = parse_virtual(
		"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \t# Blockers\n\
		 \t- task A\n",
	);
	let vi2 = parse_virtual(
		"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t# Blockers\n\
		 \t- task B\n",
	);

	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let path1 = ctx.resolve_issue_path(&issue1);
	let path2 = ctx.resolve_issue_path(&issue2);

	// Set up revolver with two entries, pointing at first
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json_multi(&[&path1, &path2], 0, None));

	// Current should show task A
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "Before toggle, current should be task A. stdout: {}", out.stdout);

	// Toggle should switch to issue B
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success(), "toggle should succeed. stderr: {}", out.stderr);
	assert!(
		out.stdout.contains("Issue B") || out.stdout.contains("task B"),
		"toggle should switch to B. stdout: {}",
		out.stdout
	);

	// Current should now show task B
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task B"), "After toggle, current should be task B. stdout: {}", out.stdout);

	// Toggle again should wrap back to issue A
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success(), "second toggle should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "After second toggle, should be back to task A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_toggle_single_entry_pulls_previous() {
	let ctx = TestContext::build("");

	let vi1 = parse_virtual(
		"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \t# Blockers\n\
		 \t- task A\n",
	);
	let vi2 = parse_virtual(
		"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t# Blockers\n\
		 \t- task B\n",
	);

	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let path1 = ctx.resolve_issue_path(&issue1);
	let path2 = ctx.resolve_issue_path(&issue2);

	// Single entry with previous set (simulates: was on B, did `set` to A)
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json_multi(&[&path1], 0, Some(&path2)));

	// Toggle should pull previous (B) into revolver and switch to it
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success(), "toggle should succeed. stderr: {}", out.stderr);
	assert!(
		out.stdout.contains("Issue B") || out.stdout.contains("task B"),
		"toggle should switch to B. stdout: {}",
		out.stdout
	);

	// Now we have a 2-slot revolver, toggle back to A
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success(), "second toggle should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "After second toggle, should be back to A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_toggle_single_entry_no_previous_errors() {
	let ctx = TestContext::build("");

	let vi = parse_virtual(
		"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \t# Blockers\n\
		 \t- task A\n",
	);

	let issue = ctx.local(&vi, None).await;
	let path = ctx.resolve_issue_path(&issue);

	// Single entry, no previous
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json(&path));

	// Toggle should fail
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(!out.status.success(), "toggle with no previous should fail. stdout: {}", out.stdout);
	assert!(out.stderr.contains("set-more"), "error should mention set-more. stderr: {}", out.stderr);
}

#[tokio::test]
async fn test_blocker_add_works_after_toggle() {
	let ctx = TestContext::build("");

	let vi1 = parse_virtual(
		"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \t# Blockers\n\
		 \t- task A\n",
	);
	let vi2 = parse_virtual(
		"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\
		 \t# Blockers\n\
		 \t- task B\n",
	);

	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let path1 = ctx.resolve_issue_path(&issue1);
	let path2 = ctx.resolve_issue_path(&issue2);

	// Start on A, toggle to B
	ctx.xdg.write_cache("blocker_revolver.json", &revolver_json_multi(&[&path1, &path2], 0, None));
	ctx.run(&["--offline", "blocker", "toggle"]);

	// Add a blocker - should go into Issue B (the now-current one)
	let out = ctx.run(&["--offline", "blocker", "add", "new task on B"]);
	assert!(out.status.success(), "add should succeed after toggle. stderr: {}", out.stderr);

	// Verify it went into issue B
	insta::assert_snapshot!(read_issue_file(&path2), @"
	- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->
		
		# Blockers
		- task B
		- new task on B
	");

	// Issue A should be untouched
	insta::assert_snapshot!(read_issue_file(&path1), @"
	- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->
		
		# Blockers
		- task A
	");
}
