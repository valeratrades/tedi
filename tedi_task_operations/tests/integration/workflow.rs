//! A workflow places each issue: where it sits is tedi's own, kept beside the store. A pull moves it by
//! the workflow's rules; a move runs the hooks of the buckets it enters, landing with all their edits or none.

use std::path::PathBuf;

use tedi_core::{PrState, PullRequest};

use crate::common::{
	RunOutput, Seed, TestContext,
	are_you_sure::{UnsafePathExt, read_issue_file},
	parse_virtual,
};

const KANBAN: &str = "[board]\nviews = []\nworkflows = { kanban = \"preset:kanban\" }\n";
const ISSUE: &str = "- [ ] Task <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n";
const URL: &str = "https://github.com/o/r/issues/1";

const FLOW: &str = "intake = \"todo\"\n\n[[buckets]]\nname = \"todo\"\n\n[[buckets]]\nname = \"shipped\"\n\n[[buckets]]\nname = \"archived\"\n\n[[rules]]\nbuiltin = \"on_pr\"\nstate = \"merged\"\nto = \"shipped\"\nforward_only = true\n";

/// `days` after the seeds' base (2001-09-11T12:00:00Z), as the binary's clock reads it.
fn days_after_seeds(days: i64) -> String {
	jiff::Timestamp::from_second(1000209600 + days * 86400).unwrap().to_string()
}

fn configure(ctx: &TestContext, toml: &str) {
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), toml).unwrap();
}

fn states_file(ctx: &TestContext, repo: &str) -> PathBuf {
	ctx.xdg.data_dir().join("issues").join(repo).join(".states.json")
}

/// Where each issue of `repo` sits, as stored; nothing stored is every issue in its intake.
fn states(ctx: &TestContext, repo: &str) -> String {
	match std::fs::read_to_string(states_file(ctx, repo)) {
		Ok(held) => serde_json::from_str::<serde_json::Value>(&held).unwrap().to_string(),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => "(intake)".into(),
		Err(e) => panic!("{e}"),
	}
}

fn hold(ctx: &TestContext, repo: &str, held: serde_json::Value) {
	let path = states_file(ctx, repo);
	std::fs::create_dir_all(path.parent().unwrap()).unwrap();
	std::fs::write(path, held.to_string()).unwrap();
}

fn pull(ctx: &TestContext, now_days: i64) -> RunOutput {
	let out = ctx.run_with_env(&["--mock", "pull", "o/r"], &[(concat!("tedi", "_MOCK_NOW"), &days_after_seeds(now_days))]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
	out
}

/// Where `kanban` puts #1 on a pull: it sits at `held` (where an `s:` pin put it), GitHub holds it with
/// `checkbox` and pull requests in `prs`, closed `closed_days_ago` (`None`: no close event kept).
fn place(held: Option<&str>, checkbox: &str, prs: &[&str], closed_days_ago: Option<i64>) -> String {
	let ctx = TestContext::build();
	configure(&ctx, KANBAN);
	let line = ISSUE.replacen("[ ]", &format!("[{checkbox}]"), 1);
	let closed = checkbox != " ";
	let issue = ctx.remote(&parse_virtual(&line), (!closed || closed_days_ago.is_some()).then(|| Seed::new(0)));
	for (i, state) in prs.iter().enumerate() {
		ctx.remote_pr(&issue, &format!("https://github.com/o/r/pull/{}", 10 + i), state);
	}
	if let Some(held) = held {
		hold(&ctx, "o/r", serde_json::json!({ "kanban": { "1": held } }));
	}
	pull(&ctx, closed_days_ago.unwrap_or(0));
	states(&ctx, "o/r")
}

/// Every case the stage derivation held, now rules on a pull.
#[test]
fn kanban_places_what_the_stages_derived() {
	type Case = (Option<&'static str>, &'static str, &'static [&'static str], Option<i64>);
	let cases: [Case; 12] = [
		(None, " ", &[], None),
		(Some("icebox"), " ", &[], None),
		(Some("selected"), " ", &[], None),
		(Some("in_progress"), " ", &[], None),
		(Some("selected"), " ", &["draft"], None),
		(Some("in_progress"), " ", &["draft", "open"], None),
		(None, " ", &["closed", "merged"], None),
		(Some("in_review"), " ", &["closed"], None),
		(Some("in_progress"), "x", &["open"], Some(1)),
		(None, "-", &[], Some(1)),
		(None, "x", &[], Some(8)),
		(None, "x", &[], None),
	];
	let out: Vec<String> = cases
		.into_iter()
		.map(|(held, checkbox, prs, closed)| {
			let placed = std::thread::spawn(move || place(held, checkbox, prs, closed)).join().unwrap(); // a mock GitHub per thread
			format!("{held:?} [{checkbox}] prs {prs:?} closed {closed:?} days ago → {placed}")
		})
		.collect();
	insta::assert_snapshot!(out.join("\n"), @r#"
	None [ ] prs [] closed None days ago → (intake)
	Some("icebox") [ ] prs [] closed None days ago → {"kanban":{"1":"icebox"}}
	Some("selected") [ ] prs [] closed None days ago → {"kanban":{"1":"selected"}}
	Some("in_progress") [ ] prs [] closed None days ago → {"kanban":{"1":"in_progress"}}
	Some("selected") [ ] prs ["draft"] closed None days ago → {"kanban":{"1":"in_progress"}}
	Some("in_progress") [ ] prs ["draft", "open"] closed None days ago → {"kanban":{"1":"in_review"}}
	None [ ] prs ["closed", "merged"] closed None days ago → (intake)
	Some("in_review") [ ] prs ["closed"] closed None days ago → {"kanban":{"1":"in_progress"}}
	Some("in_progress") [x] prs ["open"] closed Some(1) days ago → {"kanban":{"1":"done"}}
	None [-] prs [] closed Some(1) days ago → {"kanban":{"1":"wont_do"}}
	None [x] prs [] closed Some(8) days ago → {"kanban":{"1":"backlog"}}
	None [x] prs [] closed None days ago → {"kanban":{"1":"backlog"}}
	"#);
}

/// Where `kanban` puts pull request #2, in `state`, closed `closed_days_ago`.
fn place_pr(state: PrState, closed_days_ago: Option<i64>) -> String {
	let ctx = TestContext::build();
	configure(&ctx, KANBAN);
	let base: jiff::Timestamp = days_after_seeds(0).parse().unwrap();
	ctx.remote_pull_request(
		("o", "r").into(),
		PullRequest {
			number: 2,
			title: "Fix".into(),
			body: String::new(),
			state,
			author: "mock_user".into(),
			assignees: Vec::new(),
			labels: Vec::new(),
			closes: Vec::new(),
			updated_at: base,
			closed_at: closed_days_ago.map(|_| base),
		},
	);
	pull(&ctx, closed_days_ago.unwrap_or(0));
	states(&ctx, "o/r")
}

#[test]
fn kanban_places_pull_requests_by_their_state() {
	let cases = [
		(PrState::Draft, None),
		(PrState::Open, None),
		(PrState::Merged, Some(1)),
		(PrState::Closed, Some(1)),
		(PrState::Merged, Some(8)),
	];
	let out: Vec<String> = cases
		.into_iter()
		.map(|(state, closed)| format!("{state} closed {closed:?} days ago → {}", std::thread::spawn(move || place_pr(state, closed)).join().unwrap()))
		.collect();
	insta::assert_snapshot!(out.join("\n"), @r#"
	draft closed None days ago → {"kanban":{"2":"in_progress"}}
	open closed None days ago → {"kanban":{"2":"in_review"}}
	merged closed Some(1) days ago → {"kanban":{"2":"done"}}
	closed closed Some(1) days ago → {"kanban":{"2":"wont_do"}}
	merged closed Some(8) days ago → {"kanban":{"2":"backlog"}}
	"#);
}

/// `ISSUE` agreed on everywhere, then GitHub holds `remote`; #1 sits at `held`, and `tedi move` sends it to `to`.
async fn move_to(ctx: &TestContext, remote: &str, held: Option<&str>, to: &str, seen: Option<&str>) -> (RunOutput, String) {
	configure(ctx, KANBAN);
	let issue = ctx.consensus(&parse_virtual(ISSUE), Some(Seed::new(-100))).await;
	ctx.remote(&parse_virtual(remote), Some(Seed::new(100)));
	if let Some(held) = held {
		hold(ctx, "o/r", serde_json::json!({ "kanban": { "1": held } }));
	}
	let mut args = vec!["--mock", "move", URL, to];
	if let Some(seen) = seen {
		args.extend(["--seen", seen]);
	}
	let out = ctx.run(&args);
	(out, read_issue_file(&ctx.resolve_issue_path(&issue)))
}

#[tokio::test]
async fn a_move_onto_selected_takes_an_unassigned_issue() {
	let ctx = TestContext::build();
	let (out, stored) = move_to(&ctx, ISSUE, None, "kanban/selected", None).await;

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(format!("{stored}{}", states(&ctx, "o/r")), @r#"
	- [ ] (@mock_user) Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	{"kanban":{"1":"selected"}}
	"#);
	assert!(ctx.github_calls().iter().any(|c| c.starts_with("set_assignees")), "{:?}", ctx.github_calls());
}

#[tokio::test]
async fn a_move_onto_done_closes() {
	let ctx = TestContext::build();
	let (out, stored) = move_to(&ctx, ISSUE, Some("in_progress"), "kanban/done", None).await;

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(format!("{stored}{}", states(&ctx, "o/r")), @r#"
	- [x] Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	{"kanban":{"1":"done"}}
	"#);
}

/// Review is the rules' alone: a person moving a card there is refused, and nothing lands.
#[tokio::test]
async fn a_rejected_move_writes_nothing() {
	let ctx = TestContext::build();
	let (out, stored) = move_to(&ctx, ISSUE, Some("selected"), "kanban/in_review", None).await;

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("a ready pull request moves it here"), "the hook's reason is shown: {}", out.stderr);
	assert_eq!(stored, ISSUE);
	assert_eq!(states(&ctx, "o/r"), r#"{"kanban":{"1":"selected"}}"#);
}

/// GitHub assigned it since: the hook's edit, made against the unassigned card it was shown, clashes,
/// and the move lands nothing — the state stays where it was.
#[tokio::test]
async fn a_move_whose_edit_clashes_lands_nothing() {
	let ctx = TestContext::build();
	let (out, stored) = move_to(&ctx, &ISSUE.replacen("- [ ] ", "- [ ] (@bob) ", 1), None, "kanban/selected", None).await;

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert!(out.stderr.contains("assignees:"), "the clashing field is named: {}", out.stderr);
	assert!(!stored.contains("mock_user)"), "{stored}");
	assert_eq!(states(&ctx, "o/r"), "(intake)");
}

#[tokio::test]
async fn a_move_made_against_a_state_since_left_is_a_conflict() {
	let ctx = TestContext::build();
	let (out, stored) = move_to(&ctx, ISSUE, Some("selected"), "kanban/done", Some("kanban/icebox")).await;

	assert!(!out.status.success(), "stdout: {}", out.stdout);
	assert_eq!(stored, ISSUE);
	assert_eq!(states(&ctx, "o/r"), r#"{"kanban":{"1":"selected"}}"#);
}

/// A team's own workflow file: a merged pull request moves its issue forward, never back past where it sits.
#[test]
fn a_rule_moving_forward_only_never_moves_back() {
	let out: Vec<String> = [None, Some("archived")]
		.into_iter()
		.map(|held| {
			std::thread::spawn(move || {
				let ctx = TestContext::build();
				let flow = ctx.xdg.inner.root.join("flow.toml");
				std::fs::write(&flow, FLOW).unwrap();
				configure(&ctx, &format!("[board]\nviews = []\nworkflows = {{ flow = {:?} }}\n", flow.display().to_string()));
				let issue = ctx.remote(&parse_virtual(ISSUE), Some(Seed::new(0)));
				ctx.remote_pr(&issue, "https://github.com/o/r/pull/10", "merged");
				if let Some(held) = held {
					hold(&ctx, "o/r", serde_json::json!({ "flow": { "1": held } }));
				}
				pull(&ctx, 0);
				format!("{held:?} → {}", states(&ctx, "o/r"))
			})
			.join()
			.unwrap()
		})
		.collect();
	insta::assert_snapshot!(out.join("\n"), @r#"
	None → {"flow":{"1":"shipped"}}
	Some("archived") → {"flow":{"1":"archived"}}
	"#);
}

/// A hook in any language: the C example, built here, loaded through the config, closes what enters `done`.
#[tokio::test]
async fn a_c_hook_buffers_an_edit_that_lands() {
	let ctx = TestContext::build();
	let dir = ctx.xdg.inner.root.join("hooks");
	std::fs::create_dir_all(&dir).unwrap();
	let hooks = concat!(env!("CARGO_MANIFEST_DIR"), "/../tedi_hooks");
	let built = std::process::Command::new("cc")
		.args(["-shared", "-fPIC", "-I", hooks, "-o"])
		.arg(dir.join("libclose.so"))
		.arg(format!("{hooks}/examples/close.c"))
		.status()
		.expect("cc is in the dev shell");
	assert!(built.success());
	std::fs::write(
		dir.join("flow.toml"),
		"intake = \"todo\"\n\n[[buckets]]\nname = \"todo\"\n\n[[buckets]]\nname = \"done\"\non_enter = [{ lib = \"libclose.so\", symbol = \"close_on_enter\" }]\n",
	)
	.unwrap();
	let issue = ctx.consensus(&parse_virtual(ISSUE), Some(Seed::new(-100))).await;
	ctx.remote(&parse_virtual(ISSUE), Some(Seed::new(100)));
	configure(
		&ctx,
		&format!("[board]\nviews = []\nworkflows = {{ flow = {:?} }}\n", dir.join("flow.toml").display().to_string()),
	);

	let out = ctx.run(&["--mock", "move", URL, "flow/done"]);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(format!("{}{}", read_issue_file(&ctx.resolve_issue_path(&issue)), states(&ctx, "o/r")), @r#"
	- [x] Task <!-- @mock_user https://github.com/o/r/issues/1 -->
	  body
	{"flow":{"1":"done"}}
	"#);
}
