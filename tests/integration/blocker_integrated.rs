//! Integration tests for blocker commands in integrated mode (issue files).
//!
//! These tests verify that `blocker add` and `blocker pop` work correctly
//! when operating on issue files (integrated mode) rather than standalone blocker files.

use crate::common::{
	TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

/// Build a MilestoneBlockerCache JSON from embedded issue title lines.
/// `titles` is a list of `(title, user, url)` tuples.
fn milestone_cache_json(titles: &[(&str, &str, &str)], current_index: usize) -> String {
	milestone_cache_json_with_refs(titles, current_index, &[])
}

/// Build a MilestoneBlockerCache JSON with ref_targets annotations.
/// `ref_targets` is a list of `(source_url, target_url)` tuples.
fn milestone_cache_json_with_refs(titles: &[(&str, &str, &str)], current_index: usize, ref_targets: &[(&str, &str)]) -> String {
	let description: String = titles
		.iter()
		.enumerate()
		.map(|(i, (title, user, url))| {
			let sep = if i + 1 < titles.len() { "\n\n" } else { "" };
			format!("- [ ] {title} <!-- @{user} {url} -->{sep}")
		})
		.collect();
	let refs: serde_json::Map<String, serde_json::Value> = ref_targets.iter().map(|(src, tgt)| (src.to_string(), serde_json::Value::String(tgt.to_string()))).collect();
	serde_json::json!({
		"current_index": current_index,
		"milestone_description": description,
		"ref_targets": refs
	})
	.to_string()
}

#[tokio::test]
async fn test_blocker_pop_in_integrated_mode() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create issue with multiple blockers
	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

  # Blockers
  - First task
  - Second task
  - Third task
"#,
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue via milestone cache
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(&[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")], 0),
	);

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
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create issue WITHOUT blockers section
	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	Body text without blockers section.
"#,
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue via milestone cache
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(&[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")], 0),
	);

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
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

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
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create issue with blockers section containing nested items
	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
description

  # Blockers
  - Phase 1
    - Setup task
  - Phase 2
    - Implementation task
"#,
	);

	// Set up: local issue file exists
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	// Set this issue as the current blocker issue via milestone cache
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(&[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")], 0),
	);

	// Run blocker add in integrated mode
	let out = ctx.run(&["--offline", "blocker", "add", "New sub-task"]);

	eprintln!("stdout: {}", out.stdout);
	eprintln!("stderr: {}", out.stderr);

	assert!(out.status.success(), "blocker add should succeed. stderr: {}", out.stderr);

	// new sub-task added under Phase 2
	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    description

	  # Blockers
	  - Phase 1
	    - Setup task
	  - Phase 2
	    - Implementation task
	    - New sub-task
	");
}

#[tokio::test]
async fn test_blocker_move_up_cycles_between_entries() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create two issues
	let vi1 = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - task A
"#,
	);
	let vi2 = parse_virtual(
		r#"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->

  # Blockers
  - task B
"#,
	);

	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let _path1 = ctx.resolve_issue_path(&issue1);
	let _path2 = ctx.resolve_issue_path(&issue2);

	// Set up milestone cache with two embedded issues, pointing at first
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(
			&[
				("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
				("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
			],
			0,
		),
	);

	// Current should show task A
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "Before move, current should be task A. stdout: {}", out.stdout);

	// Move up should switch to issue B
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success(), "move up should succeed. stderr: {}", out.stderr);
	assert!(
		out.stdout.contains("Issue B") || out.stdout.contains("task B"),
		"move up should switch to B. stdout: {}",
		out.stdout
	);

	// Current should now show task B
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task B"), "After move up, current should be task B. stdout: {}", out.stdout);

	// Move up again should wrap back to issue A
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success(), "second move up should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "After second move up, should be back to task A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_move_up_with_three_entries_cycles() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - task A
"#,
	);
	let vi2 = parse_virtual(
		r#"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->

  # Blockers
  - task B
"#,
	);
	let vi3 = parse_virtual(
		r#"- [ ] Issue C <!-- @mock_user https://github.com/o/r/issues/3 -->

  # Blockers
  - task C
"#,
	);

	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;
	ctx.local(&vi3, None).await;

	// Set up milestone cache with three embedded issues, pointing at first
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(
			&[
				("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
				("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
				("Issue C", "mock_user", "https://github.com/o/r/issues/3"),
			],
			0,
		),
	);

	// Move up A→B
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success(), "move up should succeed. stderr: {}", out.stderr);
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task B"), "After first move up, should be B. stdout: {}", out.stdout);

	// Move up B→C
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success());
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task C"), "After second move up, should be C. stdout: {}", out.stdout);

	// Move up C→A (wrap)
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success());
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "After third move up, should wrap to A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_move_single_entry_errors() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - task A
"#,
	);

	ctx.local(&vi, None).await;

	// Single entry in milestone cache
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(&[("Issue A", "mock_user", "https://github.com/o/r/issues/1")], 0),
	);

	// Move up should fail with single entry
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(!out.status.success(), "move with single entry should fail. stdout: {}", out.stdout);
	assert!(out.stderr.contains("Only one issue"), "error should mention single issue. stderr: {}", out.stderr);
}

#[tokio::test]
async fn test_blocker_add_works_after_move() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - task A
"#,
	);
	let vi2 = parse_virtual(
		r#"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->

  # Blockers
  - task B
"#,
	);

	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let path1 = ctx.resolve_issue_path(&issue1);
	let path2 = ctx.resolve_issue_path(&issue2);

	// Start on A, move up to B
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json(
			&[
				("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
				("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
			],
			0,
		),
	);
	ctx.run(&["--offline", "blocker", "move", "up"]);

	// Add a blocker - should go into Issue B (the now-current one)
	let out = ctx.run(&["--offline", "blocker", "add", "new task on B"]);
	assert!(out.status.success(), "add should succeed after move. stderr: {}", out.stderr);

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

#[tokio::test]
async fn test_blocker_move_skips_ref_annotated_issues() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create three issues: A (real task), B (delegates to C via ref), C (real task)
	let vi1 = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - task A
"#,
	);
	let vi2 = parse_virtual(
		r#"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->

  # Blockers
  - o/r#3
"#,
	);
	let vi3 = parse_virtual(
		r#"- [ ] Issue C <!-- @mock_user https://github.com/o/r/issues/3 -->

  # Blockers
  - task C
"#,
	);

	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;
	ctx.local(&vi3, None).await;

	// Set up milestone cache with ref_targets: B points at C
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json_with_refs(
			&[
				("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
				("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
				("Issue C", "mock_user", "https://github.com/o/r/issues/3"),
			],
			0,
			&[("https://github.com/o/r/issues/2", "https://github.com/o/r/issues/3")],
		),
	);

	// Starting at A (index 0), move up should skip B (ref-annotated) and land on C
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(out.status.success(), "move up should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task C"), "Should skip B and land on C. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_move_all_refs_errors() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create two issues, both with ref blockers
	let vi1 = parse_virtual(
		r#"- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->

  # Blockers
  - o/r#2
"#,
	);
	let vi2 = parse_virtual(
		r#"- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->

  # Blockers
  - o/r#1
"#,
	);

	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;

	// Both issues are ref-annotated
	ctx.xdg.write_cache(
		"milestone_blockers.json",
		&milestone_cache_json_with_refs(
			&[
				("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
				("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
			],
			0,
			&[
				("https://github.com/o/r/issues/1", "https://github.com/o/r/issues/2"),
				("https://github.com/o/r/issues/2", "https://github.com/o/r/issues/1"),
			],
		),
	);

	// Move should error: all issues have refs
	let out = ctx.run(&["--offline", "blocker", "move", "up"]);
	assert!(!out.status.success(), "move should fail when all issues have refs. stdout: {}", out.stdout);
	assert!(
		out.stderr.contains("All issues") || out.stderr.contains("Nothing to stop at"),
		"error should mention all refs. stderr: {}",
		out.stderr
	);
}
