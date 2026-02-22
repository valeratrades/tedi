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
	let description: String = titles
		.iter()
		.enumerate()
		.map(|(i, (title, user, url))| {
			let sep = if i + 1 < titles.len() { "\n\n" } else { "" };
			format!("- [ ] {title} <!-- @{user} {url} -->{sep}")
		})
		.collect();
	serde_json::json!({
		"current_index": current_index,
		"milestone_description": description
	})
	.to_string()
}

#[tokio::test]
async fn test_blocker_add_in_integrated_mode() {
	let ctx = TestContext::build("");

	// Create issue with existing blockers section
	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

  # Blockers
  - First task
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
	let ctx = TestContext::build("");

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
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

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
async fn test_blocker_toggle_with_three_entries_cycles() {
	let ctx = TestContext::build("");

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

	// Toggle A→B
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success(), "toggle should succeed. stderr: {}", out.stderr);
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task B"), "After first toggle, should be B. stdout: {}", out.stdout);

	// Toggle B→C
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success());
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task C"), "After second toggle, should be C. stdout: {}", out.stdout);

	// Toggle C→A (wrap)
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(out.status.success());
	let out = ctx.run(&["--offline", "blocker", "current"]);
	assert!(out.stdout.contains("task A"), "After third toggle, should wrap to A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_blocker_toggle_single_entry_errors() {
	let ctx = TestContext::build("");

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

	// Toggle should fail with single entry
	let out = ctx.run(&["--offline", "blocker", "toggle"]);
	assert!(!out.status.success(), "toggle with single entry should fail. stdout: {}", out.stdout);
	assert!(out.stderr.contains("Only one issue"), "error should mention single issue. stderr: {}", out.stderr);
}

#[tokio::test]
async fn test_blocker_add_works_after_toggle() {
	let ctx = TestContext::build("");

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

	// Start on A, toggle to B
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
