//! What the board's issue tab edits past an issue's own fields: the milestones it sits in, its comments, a pull
//! request's reviewers.

use serde_json::json;
use tedi_core::PrState;

use crate::{
	common::{Seed, TestContext, parse_virtual},
	web::{BOARD, board, call, serve},
};

const ISSUE: &str = "https://github.com/o/r/issues/1";

/// `o/r#1` and `o/r#2` in the store, and what `remote` seeds pulled with them.
fn pulled(ctx: &TestContext, remote: impl FnOnce(&TestContext)) {
	for n in [1, 2] {
		ctx.remote(
			&parse_virtual(&format!("- [ ] Task {n} <!-- @mock_user https://github.com/o/r/issues/{n} -->\n  body\n")),
			Some(Seed::new(0)),
		);
	}
	remote(ctx);
	pull(ctx);
}

fn pull(ctx: &TestContext) {
	let out = ctx.run(&["--mock", "pull", "o/r", "o/s"]);
	assert!(out.status.success(), "stderr: {}", out.stderr);
}

/// `repo#number title` of each milestone the board shows `o/r#1` in.
fn milestones(port: u16) -> Vec<String> {
	let mut held: Vec<String> = board(port)["targets"]
		.as_array()
		.unwrap()
		.iter()
		.filter(|t| t["nodes"].as_array().unwrap().contains(&json!(ISSUE)))
		.map(|t| format!("{} {}", t["link"].as_str().unwrap().strip_prefix("https://github.com/").unwrap(), t["title"].as_str().unwrap()))
		.collect();
	held.sort();
	held
}

fn set(port: u16, repo: &str, seen: Option<u64>, new: Option<u64>) -> serde_json::Value {
	let link = |n: u64| format!("https://github.com/{repo}/milestone/{n}");
	let (status, body) = call(
		port,
		"POST",
		"/api/milestone",
		None,
		Some(json!({ "link": ISSUE, "repo": repo, "seen": seen.map(link), "new": new.map(link) })),
	);
	assert_eq!(status, 200, "{body}");
	serde_json::from_str(&body).unwrap()
}

#[test]
fn the_board_shows_open_milestones_an_orgs_sprints_among_them() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_milestone("o/r", 1, "v1", "ship it", &[1]);
		ctx.remote_milestone("o/r", 2, "1d", "the day\n\n# grouped\n- #1\n- https://github.com/o/.github/milestone/3", &[]);
		ctx.remote_milestone("o/.github", 3, "Bottlenecks", "- o/r#1", &[]);
	});
	let (_server, port) = serve(&ctx, BOARD, &[]);
	let targets = board(port)["targets"].clone();
	let shown: Vec<String> = targets
		.as_array()
		.unwrap()
		.iter()
		.map(|t| format!("{} {} {}", t["title"], t["description"], t["nodes"]))
		.collect();
	insta::assert_snapshot!(shown.join("\n"), @r#"
	"v1" "ship it" ["https://github.com/o/r/issues/1"]
	"1d" "the day" ["https://github.com/o/r/issues/1","https://github.com/o/r/milestone/3"]
	"Bottlenecks" "" ["https://github.com/o/r/issues/1"]
	"#);

	// what is planned is not the board's to say; what is a bottleneck is
	assert!(set(port, "o/r", Some(1), Some(2)).get("Rejected").is_some());
	assert_eq!(set(port, "o/.github", Some(3), None), "Landed");
}

#[test]
fn within_a_repo_a_milestone_is_swapped_and_its_description_left_alone() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_milestone("o/r", 1, "v1", "ship it", &[1, 2]);
		ctx.remote_milestone("o/r", 2, "v2", "ship more", &[]);
	});
	let (_server, port) = serve(&ctx, BOARD, &[]);

	assert_eq!(set(port, "o/r", Some(1), Some(2)), "Landed");

	assert_eq!(milestones(port), ["o/r/milestone/2 v2"]);
	let snap_before = ctx.remote_milestones();
	assert_eq!(set(port, "o/r", Some(2), None), "Landed");
	assert_eq!(milestones(port), [""; 0]);
	insta::assert_snapshot!(format!("{snap_before}\n---\n{}", ctx.remote_milestones()), @"
	o/r#1 v1 [2]
	ship it
	o/r#2 v2 [1]
	ship more
	---
	o/r#1 v1 [2]
	ship it
	o/r#2 v2 []
	ship more
	");
}

#[test]
fn another_repos_milestone_names_the_issue_in_its_description() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_milestone("o/r", 1, "v1", "ship it", &[1]);
		ctx.remote_in(
			("o", "s").into(),
			&parse_virtual("- [ ] Other <!-- @mock_user https://github.com/o/s/issues/7 -->\n  body\n"),
			Some(Seed::new(0)),
		);
		ctx.remote_milestone("o/s", 4, "launch", "go live", &[7]);
	});
	let (_server, port) = serve(&ctx, BOARD, &[]);

	assert_eq!(set(port, "o/s", None, Some(4)), "Landed");

	assert_eq!(milestones(port), ["o/r/milestone/1 v1", "o/s/milestone/4 launch"]);
	let snap_before = ctx.remote_milestones();
	assert_eq!(set(port, "o/s", Some(4), None), "Landed");
	assert_eq!(milestones(port), ["o/r/milestone/1 v1"]);
	insta::assert_snapshot!(format!("{snap_before}\n---\n{}", ctx.remote_milestones()), @"
	o/r#1 v1 [1]
	ship it
	o/s#4 launch [7]
	go live

	- https://github.com/o/r/issues/1
	---
	o/r#1 v1 [1]
	ship it
	o/s#4 launch [7]
	go live
	");
}

#[test]
fn an_edit_against_a_milestone_it_left_lands_nothing() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_milestone("o/r", 1, "v1", "", &[1]);
		ctx.remote_milestone("o/r", 2, "v2", "", &[]);
		ctx.remote_milestone("o/r", 3, "v3", "", &[]);
	});
	ctx.remote_milestone("o/r", 1, "v1", "", &[]);
	ctx.remote_milestone("o/r", 3, "v3", "", &[1]);
	let (_server, port) = serve(&ctx, BOARD, &[]);

	let refused = set(port, "o/r", Some(1), Some(2));
	assert!(refused["Rejected"].as_str().is_some_and(|why| why.contains("v3")), "{refused}");

	assert_eq!(milestones(port), ["o/r/milestone/3 v3"]);
	assert!(!ctx.github_calls().iter().any(|c| c.starts_with("set_issue_milestone")), "{:?}", ctx.github_calls());
}

/// A description may name what the `milestone` field held: unassigned on Github, it is neither shown nor named.
#[test]
fn an_issue_unassigned_on_github_leaves_the_milestone_naming_it() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| ctx.remote_milestone("o/r", 1, "v1", "ship\n\n- https://github.com/o/r/issues/1", &[1]));
	ctx.remote_milestone("o/r", 1, "v1", "ship\n\n- https://github.com/o/r/issues/1", &[]);
	pull(&ctx);
	let (_server, port) = serve(&ctx, BOARD, &[]);
	assert_eq!(milestones(port), [""; 0]);
	insta::assert_snapshot!(ctx.remote_milestones(), @"
	o/r#1 v1 []
	ship
	");
}

#[test]
fn a_milestone_renamed_on_github_is_stored_once() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| ctx.remote_milestone("o/r", 1, "v1", "ship", &[1]));
	ctx.remote_milestone("o/r", 1, "v1.0", "ship", &[1]);
	pull(&ctx);
	let (_server, port) = serve(&ctx, BOARD, &[]);
	assert_eq!(milestones(port), ["o/r/milestone/1 v1.0"]);
	let mut files: Vec<String> = std::fs::read_dir(ctx.xdg.data_dir().join("issues/__milestones__/o/r"))
		.unwrap()
		.map(|f| f.unwrap().file_name().into_string().unwrap())
		.collect();
	files.sort();
	assert_eq!(files, [".meta.json", "1_-_v10.md"]);
}

#[test]
fn comments_are_added_changed_and_deleted_by_their_author() {
	let ctx = TestContext::build();
	pulled(&ctx, |_| {});
	let (_server, port) = serve(&ctx, BOARD, &[]);
	let thread = || -> serde_json::Value {
		let (status, body) = call(port, "POST", "/api/thread", None, Some(json!({ "link": ISSUE })));
		assert_eq!(status, 200, "{body}");
		serde_json::from_str(&body).unwrap()
	};
	let comment = |edit: serde_json::Value| -> serde_json::Value {
		let (status, body) = call(port, "POST", "/api/comment", None, Some(json!({ "link": ISSUE, "edit": edit })));
		assert_eq!(status, 200, "{body}");
		serde_json::from_str(&body).unwrap()
	};
	assert_eq!(thread()["author"], "mock_user");

	assert_eq!(comment(json!({ "Add": { "body": "first" } })), "Landed");
	assert!(comment(json!({ "Add": { "body": "  " } }))["Rejected"].is_string());
	let said = thread()["comments"][0].clone();
	assert_eq!((&said["by"], &said["body"]), (&json!("mock_user"), &json!("first")));
	let id = &said["id"];

	assert!(
		comment(json!({ "Change": { "id": id, "seen": "else", "new": "second" } }))["Rejected"].is_string(),
		"against what it doesn't hold"
	);
	assert_eq!(comment(json!({ "Change": { "id": id, "seen": "first", "new": "second" } })), "Landed");
	assert_eq!(thread()["comments"][0]["body"], "second");
	assert_eq!(comment(json!({ "Delete": { "id": id, "seen": "second" } })), "Landed");
	assert_eq!(thread()["comments"], json!([]));
}

#[test]
fn a_review_is_requested_and_withdrawn() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_pull_request(
			("o", "r").into(),
			tedi_core::PullRequest {
				number: 5,
				title: "Fix".into(),
				body: String::new(),
				state: PrState::Open,
				author: "mock_user".into(),
				assignees: Vec::new(),
				labels: Vec::new(),
				closes: Vec::new(),
				requested: Vec::new(),
				reviews: Default::default(),
				updated_at: "2026-11-14T00:00:00Z".parse().unwrap(),
				created_at: None,
				closed_at: None,
			},
		)
	});
	let (_server, port) = serve(&ctx, BOARD, &[]);
	let requested = || -> serde_json::Value { board(port)["cards"].as_array().unwrap().iter().find(|c| c["link"].as_str().unwrap().ends_with("/5")).unwrap()["pr"]["requested"].clone() };
	let ask = |on: bool| {
		call(
			port,
			"POST",
			"/api/review",
			None,
			Some(json!({ "link": "https://github.com/o/r/issues/5", "reviewer": "alice", "on": on })),
		)
		.0
	};

	assert_eq!(ask(true), 200);
	assert_eq!(requested(), json!(["alice"]));
	assert_eq!(ask(false), 200);
	assert_eq!(requested(), json!([]));
	assert_eq!(
		ctx.github_calls().into_iter().filter(|c| c.starts_with("request_review")).collect::<Vec<_>>(),
		["request_review(o/r, 5, alice, true)", "request_review(o/r, 5, alice, false)"]
	);
}

#[test]
fn the_board_carries_who_a_repos_issues_go_to() {
	let ctx = TestContext::build();
	pulled(&ctx, |_| {});
	let (_server, port) = serve(&ctx, BOARD, &[]);
	assert_eq!(board(port)["assignable"]["o/r"], json!(["mock_user"]));
}

#[test]
fn the_board_carries_what_a_milestone_contains() {
	let ctx = TestContext::build();
	pulled(&ctx, |ctx| {
		ctx.remote_milestone("o/r", 1, "v1", "- #1\n- https://github.com/o/s/milestone/4", &[]);
		ctx.remote_milestone("o/s", 4, "launch", "- https://github.com/o/r/milestone/1", &[]);
	});
	let (_server, port) = serve(&ctx, BOARD, &[]);
	let board = board(port);
	let owned: Vec<String> = board["targets"]
		.as_array()
		.unwrap()
		.iter()
		.map(|t| format!("{} hosts {} contains {}", t["title"], t["hosted"], t["contains"]))
		.collect();
	insta::assert_snapshot!(owned.join("\n"), @r#"
	"launch" hosts [] contains ["https://github.com/o/r/milestone/1"]
	"v1" hosts ["https://github.com/o/r/issues/1"] contains ["https://github.com/o/s/milestone/4"]
	"#);
}
