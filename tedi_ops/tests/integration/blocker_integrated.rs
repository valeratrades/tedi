//! Integration tests for the `sprints selected` blocker operations and `sprints select`
//! rotation over the active sprint's issues.

use crate::common::{
	TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

/// Build a `sprints_selection.json` whose active (normal) sprint embeds `titles`.
/// `titles` is a list of `(title, user, url)`; `selected` optionally pins the selection.
fn selection_cache_json(titles: &[(&str, &str, &str)], selected: Option<&str>) -> String {
	let content: String = titles
		.iter()
		.enumerate()
		.map(|(i, (title, user, url))| {
			let sep = if i + 1 < titles.len() { "\n\n" } else { "" };
			format!("- [ ] {title} <!-- @{user} {url} -->{sep}")
		})
		.collect();
	let mut selections = serde_json::Map::new();
	if let Some(sel) = selected {
		selections.insert("1d".to_string(), serde_json::Value::String(sel.to_string()));
	}
	serde_json::json!({
		"selections": selections,
		"normal": { "key": "1d", "content": content }
	})
	.to_string()
}

fn write_selection(ctx: &TestContext, titles: &[(&str, &str, &str)]) {
	ctx.xdg.write_cache("sprints_selection.json", &selection_cache_json(titles, None));
}

/// rust-analyzer-style cursor encoding: `$0` inserted into `content` at the position the
/// mock editor reported on stderr, so cursor placement is part of the snapshot.
fn with_cursor(content: &str, stderr: &str) -> String {
	let pos = stderr.lines().find_map(|l| l.strip_prefix("[mock] position: ")).expect("mock editor must report a position");
	let (line, col) = pos.split_once(':').expect("position format is line:col");
	let (line, col): (usize, usize) = (line.parse().unwrap(), col.parse().unwrap());

	let mut lines: Vec<String> = content.lines().map(str::to_string).collect();
	while lines.len() < line {
		lines.push(String::new()); // a position past EOF renders as the line the editor would create
	}
	let target = &mut lines[line - 1];
	let byte = target.char_indices().nth(col - 1).map(|(i, _)| i).unwrap_or(target.len());
	target.insert_str(byte, "$0");
	lines.join("\n") + "\n"
}

#[tokio::test]
async fn test_selected_open_cursor_at_last_blocker() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

  # Blockers
  - First task
  - Second task
"#,
	);
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);
	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run_with_editor(&["--offline", "sprints", "selected", "open"]);
	assert!(out.status.success(), "selected open should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(with_cursor(&read_issue_file(&issue_path), &out.stderr), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    Body text.

	  # Blockers
	  - First task
	  - $0Second task
	");
}

#[tokio::test]
async fn test_selected_open_cursor_without_blockers_above_children() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

  - [ ] Sub Issue <!--sub @mock_user https://github.com/o/r/issues/2 -->
"#,
	);
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);
	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run_with_editor(&["--offline", "sprints", "selected", "open"]);
	assert!(out.status.success(), "selected open should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(with_cursor(&read_issue_file(&issue_path), &out.stderr), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    Body text.
	$0
	  - [ ] [Sub Issue](./2_-_Sub_Issue.md) <!-- @mock_user https://github.com/o/r/issues/2 -->
	");
}

#[tokio::test]
async fn test_selected_open_cursor_without_blockers_below_body() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.
"#,
	);
	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);
	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run_with_editor(&["--offline", "sprints", "selected", "open"]);
	assert!(out.status.success(), "selected open should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(with_cursor(&read_issue_file(&issue_path), &out.stderr), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    Body text.
	$0
	");
}

#[tokio::test]
async fn test_selected_pop() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->

  Body text.

  # Blockers
  - First task
  - Second task
  - Third task
"#,
	);

	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run(&["--offline", "sprints", "selected", "pop"]);
	eprintln!("stdout: {}\nstderr: {}", out.stdout, out.stderr);
	assert!(out.status.success(), "selected pop should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    Body text.

	  # Blockers
	  - First task
	  - Second task
	");
}

#[tokio::test]
async fn test_selected_add_creates_blockers_section_if_missing() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		r#"- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	Body text without blockers section.
"#,
	);

	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run(&["--offline", "sprints", "selected", "add", "New task"]);
	eprintln!("stdout: {}\nstderr: {}", out.stdout, out.stderr);
	assert!(out.status.success(), "selected add should succeed. stderr: {}", out.stderr);

	insta::assert_snapshot!(read_issue_file(&issue_path), @"
	- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->
	    Body text without blockers section.

	  # Blockers
	  - New task
	");
}

#[tokio::test]
async fn test_selected_add_with_nested_context() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

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

	let issue = ctx.local(&vi, None).await;
	let issue_path = ctx.resolve_issue_path(&issue);

	write_selection(&ctx, &[("Test Issue", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run(&["--offline", "sprints", "selected", "add", "New sub-task"]);
	eprintln!("stdout: {}\nstderr: {}", out.stdout, out.stderr);
	assert!(out.status.success(), "selected add should succeed. stderr: {}", out.stderr);

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
async fn test_select_next_cycles_between_entries() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task A\n");
	let vi2 = parse_virtual("- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\n  # Blockers\n  - task B\n");
	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;

	write_selection(
		&ctx,
		&[
			("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
			("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
		],
	);

	// Before any select, the top item (A) is selected.
	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task A"), "initial selection should be A. stdout: {}", out.stdout);

	let out = ctx.run(&["--offline", "sprints", "select", "--next"]);
	assert!(out.status.success(), "select --next should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task B"), "after --next, selection should be B. stdout: {}", out.stdout);

	// Wrap back to A.
	ctx.run(&["--offline", "sprints", "select", "--next"]);
	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task A"), "after wrapping, should be back to A. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_select_single_entry_errors() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task A\n");
	ctx.local(&vi, None).await;

	write_selection(&ctx, &[("Issue A", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run(&["--offline", "sprints", "select", "--next"]);
	assert!(!out.status.success(), "select with single entry should fail. stdout: {}", out.stdout);
	assert!(out.stderr.contains("Only one issue"), "error should mention single issue. stderr: {}", out.stderr);
}

#[tokio::test]
async fn test_selected_add_works_after_select() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task A\n");
	let vi2 = parse_virtual("- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\n  # Blockers\n  - task B\n");
	let issue1 = ctx.local(&vi1, None).await;
	let issue2 = ctx.local(&vi2, None).await;
	let path1 = ctx.resolve_issue_path(&issue1);
	let path2 = ctx.resolve_issue_path(&issue2);

	write_selection(
		&ctx,
		&[
			("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
			("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
		],
	);
	ctx.run(&["--offline", "sprints", "select", "--next"]);

	let out = ctx.run(&["--offline", "sprints", "selected", "add", "new task on B"]);
	assert!(out.status.success(), "add should succeed after select. stderr: {}", out.stderr);

	insta::assert_snapshot!(read_issue_file(&path2), @"
	- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->
	  # Blockers
	  - task B
	  - new task on B
	");
	insta::assert_snapshot!(read_issue_file(&path1), @"
	- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->
	  # Blockers
	  - task A
	");
}

#[tokio::test]
async fn test_select_skips_delegating_issues() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// A (real task), B (delegates to C via ref blocker), C (real task).
	let vi1 = parse_virtual("- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task A\n");
	let vi2 = parse_virtual("- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\n  # Blockers\n  - o/r#3\n");
	let vi3 = parse_virtual("- [ ] Issue C <!-- @mock_user https://github.com/o/r/issues/3 -->\n\n  # Blockers\n  - task C\n");
	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;
	ctx.local(&vi3, None).await;

	write_selection(
		&ctx,
		&[
			("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
			("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
			("Issue C", "mock_user", "https://github.com/o/r/issues/3"),
		],
	);

	// From A, --next skips B (delegates) and lands on C.
	let out = ctx.run(&["--offline", "sprints", "select", "--next"]);
	assert!(out.status.success(), "select --next should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task C"), "should skip B and land on C. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_select_all_delegating_errors() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Issue A <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - o/r#2\n");
	let vi2 = parse_virtual("- [ ] Issue B <!-- @mock_user https://github.com/o/r/issues/2 -->\n\n  # Blockers\n  - o/r#1\n");
	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;

	write_selection(
		&ctx,
		&[
			("Issue A", "mock_user", "https://github.com/o/r/issues/1"),
			("Issue B", "mock_user", "https://github.com/o/r/issues/2"),
		],
	);

	let out = ctx.run(&["--offline", "sprints", "select", "--next"]);
	assert!(!out.status.success(), "select should fail when all issues delegate. stdout: {}", out.stdout);
	assert!(
		out.stderr.contains("All issues") || out.stderr.contains("Nothing to stop at"),
		"error should mention all refs. stderr: {}",
		out.stderr
	);
}

#[tokio::test]
async fn test_select_unique_pattern_selects_directly() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Issue Alpha <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task alpha\n");
	let vi2 = parse_virtual("- [ ] Issue Beta <!-- @mock_user https://github.com/o/r/issues/2 -->\n\n  # Blockers\n  - task beta\n");
	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;

	write_selection(
		&ctx,
		&[
			("Issue Alpha", "mock_user", "https://github.com/o/r/issues/1"),
			("Issue Beta", "mock_user", "https://github.com/o/r/issues/2"),
		],
	);

	// "2" uniquely matches Beta's local path — succeeds without fzf.
	let out = ctx.run(&["--offline", "sprints", "select", "2"]);
	assert!(out.status.success(), "unique match should succeed. stderr: {}", out.stderr);

	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task beta"), "after select beta, current should be task beta. stdout: {}", out.stdout);
}

#[tokio::test]
async fn test_select_no_match_errors() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Issue Alpha <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - task alpha\n");
	ctx.local(&vi1, None).await;

	write_selection(&ctx, &[("Issue Alpha", "mock_user", "https://github.com/o/r/issues/1")]);

	let out = ctx.run(&["--offline", "sprints", "select", "zzznomatch"]);
	assert!(!out.status.success(), "no-match should fail. stdout: {}", out.stdout);
	assert!(out.stderr.contains("zzznomatch"), "error should mention the pattern. stderr: {}", out.stderr);
}

#[tokio::test]
async fn test_search_finds_matching_issue() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi1 = parse_virtual("- [ ] Fix the crash <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  # Blockers\n  - repro\n");
	let vi2 = parse_virtual("- [ ] Unrelated thing <!-- @mock_user https://github.com/o/r/issues/2 -->\n");
	ctx.local(&vi1, None).await;
	ctx.local(&vi2, None).await;

	let out = ctx.run(&["--offline", "sprints", "search", "crash"]);
	assert!(out.status.success(), "search should succeed. stderr: {}", out.stderr);
	assert!(
		out.stdout.contains("Fix the crash") || out.stdout.contains("issues/1"),
		"should surface issue 1. stdout: {}",
		out.stdout
	);
	assert!(!out.stdout.contains("Unrelated"), "should not surface unrelated issue. stdout: {}", out.stdout);
}
