//! Ranked issue search, observed through `sprints search` order and `sprints select` resolution.

use crate::common::{TestContext, parse_virtual};

async fn with_issues(issues: &[&str]) -> TestContext {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");
	for issue in issues {
		ctx.local(&parse_virtual(issue), None).await;
	}
	ctx
}

fn issue(n: u64, title: &str, body: &str) -> String {
	format!("- [ ] {title} <!-- @mock_user https://github.com/o/r/issues/{n} -->\n\n  {body}\n")
}

/// Issue numbers of the top-level items `sprints search` prints, in order; `None` on failure.
fn search(ctx: &TestContext, args: &[&str]) -> Option<Vec<u64>> {
	let out = ctx.run(&[&["--offline", "sprints", "search"], args].concat());
	if !out.status.success() {
		return None;
	}
	Some(
		out.stdout
			.lines()
			.filter(|l| l.starts_with("- ["))
			.map(|l| {
				let (_, url_tail) = l.split_once("/issues/").expect("item carries its url");
				url_tail.split(|c: char| !c.is_ascii_digit()).next().expect("split yields").parse().expect("issue number")
			})
			.collect(),
	)
}

fn sorted(mut v: Vec<u64>) -> Vec<u64> {
	v.sort_unstable();
	v
}

#[tokio::test]
async fn test_issue_outranks_child_matching_only_through_it() {
	let ctx =
		with_issues(&["- [ ] Crash reporter <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  - [ ] Collect logs <!--sub @mock_user https://github.com/o/r/issues/2 -->\n"]).await;
	assert_eq!(search(&ctx, &["crash"]), Some(vec![1, 2]));
	assert_eq!(search(&ctx, &["logs"]), Some(vec![2, 1]));
}

#[tokio::test]
async fn test_whole_word_outranks_word_prefix() {
	let ctx = with_issues(&[&issue(1, "fixtures cleanup", "x"), &issue(2, "fix_button styling", "x")]).await;
	assert_eq!(search(&ctx, &["fix"]), Some(vec![2, 1]));
	assert_eq!(search(&ctx, &["'fix'"]), Some(vec![2]));
}

/// A scatter is a typo, not a hit: it loses to any contiguous one, whatever field that sits in.
#[tokio::test]
async fn test_contiguous_body_hit_outranks_fuzzy_title_scatter() {
	let ctx = with_issues(&[&issue(1, "pay off student mortgage and loans", "x"), &issue(2, "residence permit", "the postman lost it")]).await;
	assert_eq!(search(&ctx, &["postman"]), Some(vec![2, 1]));
}

#[tokio::test]
async fn test_exclusion_scopes() {
	let ctx = with_issues(&[&issue(1, "crash on start", "x"), &issue(2, "logging", "mentions a crash"), &issue(3, "other", "x")]).await;
	assert_eq!(search(&ctx, &["--", "!crash"]).map(sorted), Some(vec![2, 3]));
	assert_eq!(search(&ctx, &["--", "!!crash"]), Some(vec![3]));
}

#[tokio::test]
async fn test_or_group() {
	let ctx = with_issues(&[&issue(1, "alpha", "x"), &issue(2, "beta", "x"), &issue(3, "gamma", "x")]).await;
	assert_eq!(search(&ctx, &["alpha | gamma"]).map(sorted), Some(vec![1, 3]));
}

#[tokio::test]
async fn test_body_is_never_fuzzy_matched() {
	let ctx = with_issues(&[&issue(1, "unrelated", "foo bar"), &issue(2, "foo bar", "x")]).await;
	assert_eq!(search(&ctx, &["fbr"]), Some(vec![2]));
	assert_eq!(search(&ctx, &["o b"]), Some(vec![2, 1]));
}

#[tokio::test]
async fn test_invalid_cli_regex_errors() {
	let ctx = with_issues(&[&issue(1, "alpha", "x")]).await;
	assert_eq!(search(&ctx, &["-ee", "("]), None);
	assert_eq!(search(&ctx, &["-ee", "al.ha"]), Some(vec![1]));
}

/// `select` resolves by the real title (punctuation the sanitized filename drops included).
#[tokio::test]
async fn test_select_resolves_by_real_title() {
	let ctx = with_issues(&[
		&issue(1, "Weird thing", "# Blockers\n  - task one"),
		&issue(2, "Fix: the (weird) crash", "# Blockers\n  - task two"),
	])
	.await;
	ctx.xdg.write_cache(
		"sprints_selection.json",
		&serde_json::json!({ "normal": { "key": "1d", "content": "- https://github.com/o/r/issues/1\n- https://github.com/o/r/issues/2\n" } }).to_string(),
	);

	let out = ctx.run(&["--offline", "sprints", "select", "-e", "(weird)"]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
	let out = ctx.run(&["--offline", "sprints", "selected", "list"]);
	assert!(out.stdout.contains("task two"), "stdout: {}", out.stdout);
}
