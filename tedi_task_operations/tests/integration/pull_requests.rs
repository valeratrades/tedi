//! A pulled repo's pull requests are mirrored read-only and carded beside its issues: `kanban` places
//! each by its state, and `is:pr` / `is:issue` tell them apart.

use jiff::{SignedDuration, Timestamp};
use tedi_core::{IssueLink, PrState, PullRequest};
use tedi_task_operations::{
	board::{self, Query},
	workflow::{Loaded, Workflows},
};

use crate::common::{Seed, TestContext, parse_virtual};

const NOW: &str = "2026-11-14T00:00:00Z";

fn days_ago(d: i64) -> Timestamp {
	NOW.parse::<Timestamp>().unwrap() - SignedDuration::from_hours(24 * d)
}

fn pr(number: u64, title: &str, state: PrState, assignees: &[&str], closes: &[u64], updated: i64, closed: Option<i64>) -> PullRequest {
	PullRequest {
		number,
		title: title.into(),
		body: format!("how {title}"),
		state,
		author: "mock_user".into(),
		assignees: assignees.iter().map(|a| a.to_string()).collect(),
		labels: Vec::new(),
		closes: closes.iter().map(|n| IssueLink::parse(&format!("https://github.com/o/r/issues/{n}")).unwrap()).collect(),
		requested: Vec::new(),
		reviews: Default::default(),
		updated_at: days_ago(updated),
		created_at: Some(days_ago(updated + 1)),
		closed_at: closed.map(days_ago),
	}
}

fn pull(ctx: &TestContext) -> crate::common::RunOutput {
	ctx.run_with_env(&["--mock", "pull", "o/r"], &[(concat!("tedi", "_MOCK_NOW"), NOW)])
}

fn seed(ctx: &TestContext) {
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), "[board]\nviews = []\nworkflows = { kanban = \"preset:kanban\" }\n").unwrap();
	ctx.remote(
		&parse_virtual("- [ ] (@mock_user) Fix it <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(Seed::new(0)),
	);
	for pr in [
		pr(2, "Wip fix", PrState::Draft, &["mock_user"], &[1], 1, None),
		pr(3, "Ready", PrState::Open, &["mock_user"], &[], 2, None),
		pr(4, "Landed", PrState::Merged, &[], &[], 3, Some(3)),
		pr(5, "Abandoned", PrState::Closed, &["mock_user"], &[], 30, Some(30)),
		pr(6, "Docs", PrState::Open, &[], &[], 40, None),
	] {
		ctx.remote_pull_request(("o", "r").into(), pr);
	}
}

async fn shown(ctx: &TestContext, queries: &[&str]) -> String {
	ctx.set_issues_dir_override();
	let workflows: Workflows = [("kanban".to_string(), Loaded::load("preset:kanban").unwrap())].into();
	let board = board::load(&workflows).await.unwrap();
	let mut out = Vec::new();
	for q in queries {
		let hits = q.parse::<Query>().unwrap().run(&board.cards, Some("mock_user")).unwrap();
		let mut cards: Vec<String> = hits
			.iter()
			.map(|&i| {
				let c = &board.cards[i];
				let what = match &c.pr {
					Some(pr) => format!("PR {}{}", pr.state, pr.closes.iter().map(|l| format!(" closes #{}", l.number())).collect::<String>()),
					None => "issue".into(),
				};
				format!("  #{} {} [{}] {what}", c.link.number(), c.title, c.states["kanban"])
			})
			.collect();
		cards.sort();
		out.push(format!("{q}\n{}", cards.join("\n")));
	}
	out.join("\n")
}

#[tokio::test]
async fn a_pull_cards_prs_by_their_state_beside_the_issues() {
	let ctx = TestContext::build();
	seed(&ctx);

	let out = pull(&ctx);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(shown(&ctx, &["is:pr", "is:issue", "assignee:@me is:open"]).await, @"
	is:pr
	  #2 Wip fix [in_progress] PR draft closes #1
	  #3 Ready [in_review] PR open
	  #4 Landed [done] PR merged
	  #5 Abandoned [backlog] PR closed
	  #6 Docs [in_review] PR open
	is:issue
	  #1 Fix it [triage] issue
	assignee:@me is:open
	  #1 Fix it [triage] issue
	  #2 Wip fix [in_progress] PR draft closes #1
	  #3 Ready [in_review] PR open
	");
}

#[tokio::test]
async fn a_pr_changed_since_is_reread_and_the_walk_stops_at_the_last_ones_newest() {
	let ctx = TestContext::build();
	seed(&ctx);
	assert!(pull(&ctx).status.success());
	ctx.remote_pull_request(("o", "r").into(), pr(3, "Ready", PrState::Merged, &["mock_user"], &[], 0, Some(0)));

	let out = pull(&ctx);

	assert!(out.status.success(), "stderr: {}", out.stderr);
	let walks: Vec<String> = ctx.github_calls().into_iter().filter(|c| c.starts_with("list_repo_prs")).collect();
	let snap_before = walks.join("\n");
	insta::assert_snapshot!(format!("{snap_before}\n---\n{}", shown(&ctx, &["is:pr is:closed"]).await), @r#"
	list_repo_prs(o/r, None)
	list_repo_prs(o/r, Some("2"))
	list_repo_prs(o/r, Some("4"))
	list_repo_prs(o/r, None)
	list_repo_prs(o/r, Some("2"))
	---
	is:pr is:closed
	  #3 Ready [done] PR merged
	  #4 Landed [done] PR merged
	  #5 Abandoned [backlog] PR closed
	"#);
}
