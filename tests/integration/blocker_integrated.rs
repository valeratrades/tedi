//! Integration tests for blocker commands in integrated mode (issue files).
//!
//! These tests verify that `blocker add` and `blocker pop` work correctly
//! when operating on issue files (integrated mode) rather than standalone blocker files.

use std::path::Path;

use todo::Issue;

use crate::common::{TestContext, git::GitExt};

//TODO!!!: use Issue::parse_virtual directly (once Path is no longer required)
fn parse(content: &str) -> Issue {
	Issue::parse_virtual(content, Path::new("test.md")).expect("failed to parse test issue")
}

#[test]
fn test_blocker_add_in_integrated_mode() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Create issue with existing blockers section
	let issue = parse(
		"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tBody text.\n\
		 \n\
		 \t# Blockers\n\
		 \t- First task\n",
	);

	// Set up: local issue file exists
	let issue_path = ctx.local(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode (no --individual-files flag)
	let (status, stdout, stderr) = ctx.run(&["--offline", "blocker", "add", "New task from CLI"]);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// The add command should succeed
	assert!(status.success(), "blocker add should succeed in integrated mode. stderr: {stderr}");

	// Verify the blocker was added to the issue file
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("New task from CLI"), "New blocker should be added to issue file. Got: {content}");
	assert!(content.contains("First task"), "Existing blockers should be preserved. Got: {content}");
}

#[test]
fn test_blocker_pop_in_integrated_mode() {
	let ctx = TestContext::new("");
	ctx.init_git();

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
	let issue_path = ctx.local(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker pop in integrated mode (no --individual-files flag)
	let (status, stdout, stderr) = ctx.run(&["--offline", "blocker", "pop"]);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// The pop command should succeed
	assert!(status.success(), "blocker pop should succeed in integrated mode. stderr: {stderr}");

	// Should show what was popped
	assert!(stdout.contains("Popped") || stdout.contains("Third task"), "Should show popped task. stdout: {stdout}");

	// Verify the blocker was removed from the issue file
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(!content.contains("Third task"), "Third task should be removed. Got: {content}");
	assert!(content.contains("First task"), "First task should remain. Got: {content}");
	assert!(content.contains("Second task"), "Second task should remain. Got: {content}");
}

#[test]
fn test_blocker_add_creates_blockers_section_if_missing() {
	let ctx = TestContext::new("");
	ctx.init_git();

	// Create issue WITHOUT blockers section
	let issue = parse("- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\tBody text without blockers section.\n");

	// Set up: local issue file exists
	let issue_path = ctx.local(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode
	let (status, stdout, stderr) = ctx.run(&["--offline", "blocker", "add", "New task"]);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// The add command should succeed
	assert!(status.success(), "blocker add should succeed even without existing blockers section. stderr: {stderr}");

	// Verify the blockers section was created with the new task
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("# Blockers"), "Blockers section should be created. Got: {content}");
	assert!(content.contains("New task"), "New blocker should be added. Got: {content}");
}

#[test]
fn test_blocker_add_with_header_context() {
	let ctx = TestContext::new("");
	ctx.init_git();

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
	let issue_path = ctx.local(&issue);

	// Set this issue as the current blocker issue
	ctx.xdg.write_cache("current_blocker_issue.txt", issue_path.to_str().unwrap());

	// Run blocker add in integrated mode
	let (status, stdout, stderr) = ctx.run(&["--offline", "blocker", "add", "New sub-task"]);

	eprintln!("stdout: {stdout}");
	eprintln!("stderr: {stderr}");

	// The add command should succeed
	assert!(status.success(), "blocker add should succeed. stderr: {stderr}");

	// Verify the blocker was added (should be under Phase 2, the last header with items)
	let content = std::fs::read_to_string(&issue_path).unwrap();
	assert!(content.contains("New sub-task"), "New blocker should be added. Got: {content}");
}
