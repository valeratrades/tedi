//! Integration tests for blocker commands in integrated mode (issue files).
//!
//! These tests verify that `blocker add` and `blocker pop` work correctly
//! when operating on issue files (integrated mode) rather than standalone blocker files.

use tedi::Issue;

use crate::common::{
	TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	git::GitExt,
};

fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}

#[tokio::test]
async fn test_blocker_add_in_integrated_mode() {
	let ctx = TestContext::build("");

	// Create issue with existing blockers section
	let issue = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- First task\n",
	);

	// Set up: local issue file exists
	ctx.local_legacy(&issue, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode (no --individual-files flag)
	let out = ctx.run(&["--offline", "blocker", "add", "New task from CLI"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// The add command should succeed
	assert!(out.status.success(), "blocker add should succeed in integrated mode. stderr: {}", out.stderr);

	// Verify the blocker was added to the issue file
	let content = read_issue_file(&issue_path);
	assert!(content.contains("New task from CLI"), "New blocker should be added to issue file. Got: {content}");
	assert!(content.contains("First task"), "Existing blockers should be preserved. Got: {content}");
}

#[tokio::test]
async fn test_blocker_pop_in_integrated_mode() {
	let ctx = TestContext::build("");

	// Create issue with multiple blockers
	let issue = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- First task\n\
		 \t- Second task\n\
		 \t- Third task\n",
	);

	// Set up: local issue file exists
	ctx.local_legacy(&issue, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker pop in integrated mode (no --individual-files flag)
	let out = ctx.run(&["--offline", "blocker", "pop"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// The pop command should succeed
	assert!(out.status.success(), "blocker pop should succeed in integrated mode. stderr: {}", out.stderr);

	// Should show what was popped
	assert!(
		out.stdout.contains("Popped") || out.stdout.contains("Third task"),
		"Should show popped task. stdout: {}",
		out.stdout
	);

	// Verify the blocker was removed from the issue file
	let content = read_issue_file(&issue_path);
	assert!(!content.contains("Third task"), "Third task should be removed. Got: {content}");
	assert!(content.contains("First task"), "First task should remain. Got: {content}");
	assert!(content.contains("Second task"), "Second task should remain. Got: {content}");
}

#[tokio::test]
async fn test_blocker_add_creates_blockers_section_if_missing() {
	let ctx = TestContext::build("");

	// Create issue WITHOUT blockers section
	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tBody text without blockers section.\n");

	// Set up: local issue file exists
	ctx.local_legacy(&issue, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode
	let out = ctx.run(&["--offline", "blocker", "add", "New task"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// The add command should succeed
	assert!(out.status.success(), "blocker add should succeed even without existing blockers section. stderr: {}", out.stderr);

	// Verify the blockers section was created with the new task
	let content = read_issue_file(&issue_path);
	assert!(content.contains("# Blockers"), "Blockers section should be created. Got: {content}");
	assert!(content.contains("New task"), "New blocker should be added. Got: {content}");
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

	// The add command should succeed without a blocker file set
	assert!(out.status.success(), "blocker add --urgent should succeed without blocker file set. stderr: {}", out.stderr);

	// Verify the blocker was added to an urgent file
	assert!(
		out.stdout.contains("Added to urgent") || out.stdout.contains("urgent"),
		"Should confirm urgent add. stdout: {}",
		out.stdout
	);
}

#[tokio::test]
async fn test_blocker_add_with_header_context() {
	let ctx = TestContext::build("");

	// Create issue with blockers section containing headers
	let issue = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t# Phase 1\n\
		 \t- Setup task\n\
		 \t# Phase 2\n\
		 \t- Implementation task\n",
	);

	// Set up: local issue file exists
	ctx.local_legacy(&issue, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode
	let out = ctx.run(&["--offline", "blocker", "add", "New sub-task"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	// The add command should succeed
	assert!(out.status.success(), "blocker add should succeed. stderr: {}", out.stderr);

	// Verify the blocker was added (should be under Phase 2, the last header with items)
	let content = read_issue_file(&issue_path);
	assert!(content.contains("New sub-task"), "New blocker should be added. Got: {content}");
}
