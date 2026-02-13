//! Integration tests for blocker commands in integrated mode (issue files).
//!
//! These tests verify that `blocker add` and `blocker pop` work correctly
//! when operating on issue files (integrated mode) rather than standalone blocker files.

use crate::common::{
	TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	git::GitExt,
	parse_virtual,
};

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
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

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
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

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
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

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
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

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
