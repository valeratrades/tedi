//! Accounts: each viewer behind a gate brings keys and scopes of their own. What they reach is theirs alone, and an
//! edit is made as them.

use std::{
	collections::BTreeSet,
	time::{Duration, Instant},
};

use serde_json::{Value, json};

use crate::{
	common::{Seed, TestContext, parse_virtual},
	web::{BOARD, SECRET, call, deliver, serve},
};

const TEAM: &str = "https://github.com/o/r/issues/1";
const PRIVATE: &str = "https://github.com/alice/priv/issues/1";
const GATED: [&str; 2] = ["--viewer-header", "X-Auth-User"];

fn config() -> String {
	format!("watch = [\"o\"]\ngithub_webhook_secret = \"{SECRET}\"\n\n{BOARD}")
}

/// The team's `o/r#1` and alice's `alice/priv#1` on GitHub; `ka` is alice's key, `kb` bob's, `kc` carol's, whom alice lets into her repo.
fn seed(ctx: &TestContext) {
	ctx.remote(&parse_virtual(&format!("- [ ] Team task <!-- @mock_user {TEAM} -->\n  body\n")), Some(Seed::new(0)));
	let secret = parse_virtual(&format!("- [ ] Secret plan <!-- @alice {PRIVATE} -->\n  Secret body\n"));
	ctx.remote_in(("alice", "priv").into(), &secret, Some(Seed::new(0)));
	ctx.remote_milestone("alice/priv", 1, "1d", "Secret day\n\n- #1", &[]);
	// stored with the team's own: GitHub shows the App both ends of the link
	ctx.remote_blocked_by(
		&tedi_task_operations::IssueLink::parse(TEAM).unwrap(),
		&tedi_task_operations::IssueLink::parse(PRIVATE).unwrap(),
		Some(Seed::new(0)),
	);
	ctx.remote_tokens(&[("ka", "alice", &["alice/priv"]), ("kb", "bob", &[]), ("kc", "carol", &["alice/priv"])]);
}

fn post(port: u16, user: &str, path: &str, body: Value) -> Value {
	let (status, body) = call(port, "POST", path, Some(user), Some(body));
	assert_eq!(status, 200, "{path}: {body}");
	serde_json::from_str(&body).unwrap()
}

fn bring(port: u16, user: &str, owner: &str, token: &str) {
	post(port, user, "/api/account/edit", json!({ "edit": { "Key": { "owner": owner, "token": token } } }));
}

fn scope(port: u16, user: &str, scope: &str) {
	post(port, user, "/api/account/edit", json!({ "edit": { "Scope": scope } }));
}

/// The board as `user` is served it, its lines.
fn board(port: u16, user: &str) -> String {
	let (status, body) = call(port, "GET", "/api/board", Some(user), None);
	assert_eq!(status, 200, "{body}");
	body
}

fn until(what: &str, mut holds: impl FnMut() -> bool) {
	let since = Instant::now();
	while !holds() {
		assert!(since.elapsed() < Duration::from_secs(30), "never: {what}");
		std::thread::sleep(Duration::from_millis(50));
	}
}

/// alice's scope stored and hers, served on the port.
fn with_alice(ctx: &TestContext) -> (crate::web::Server, u16) {
	let (server, port) = serve(ctx, &config(), &GATED);
	bring(port, "alice", "alice", "ka");
	scope(port, "alice", "alice");
	until("alice's repo is pulled", || board(port, "alice").contains("Secret plan"));
	post(port, "alice", "/api/refresh", json!({}));
	until("the team's repo is pulled", || board(port, "alice").contains("Team task"));
	(server, port)
}

/// Every server fn, as bob would use it to see alice's: `(path, body)`, `None` where it names nothing.
fn attempts() -> Vec<(&'static str, &'static str, Option<Value>)> {
	let link = json!(PRIVATE);
	let item = json!({ "View": "Triage" });
	vec![
		("GET", "/api/setup", None),
		("GET", "/api/board", None),
		("POST", "/api/query", Some(json!({ "asked": "" }))),
		("POST", "/api/view", Some(json!({ "named": "", "group": "repo" }))),
		("POST", "/api/refresh", Some(json!({}))),
		(
			"POST",
			"/api/edit",
			Some(json!({ "link": link, "edits": [{ "field": "title", "seen": "Secret plan", "new": "x" }] })),
		),
		("POST", "/api/move", Some(json!({ "link": link, "workflow": "kanban", "to": "selected", "seen": null }))),
		("POST", "/api/milestone", Some(json!({ "link": link, "repo": "alice/priv", "seen": null, "new": null }))),
		(
			"POST",
			"/api/milestone",
			Some(json!({ "link": TEAM, "repo": "alice/priv", "seen": null, "new": "https://github.com/alice/priv/milestone/1" })),
		),
		("POST", "/api/thread", Some(json!({ "link": link }))),
		("POST", "/api/comment", Some(json!({ "link": link, "edit": { "Add": { "body": "x" } } }))),
		("POST", "/api/review", Some(json!({ "link": link, "reviewer": "bob", "on": true }))),
		("POST", "/api/conflict", Some(json!({ "link": link }))),
		("POST", "/api/resolve", Some(json!({ "link": link, "resolution": "x" }))),
		("GET", "/api/account", None),
		("POST", "/api/account/edit", Some(json!({ "edit": { "Scope": "alice/priv" } }))),
		("GET", "/api/library", None),
		("POST", "/api/library/edit", Some(json!({ "edit": { "Workspace": null } }))),
		("GET", "/api/catalog", None),
		("POST", "/api/share", Some(json!({ "item": item, "bands": [], "scope": null }))),
		("POST", "/api/shared", Some(json!({ "link": "/l/view/alice/hers@1" }))),
		("POST", "/api/propose", Some(json!({ "item": item, "onto": { "New": "x" }, "message": "" }))),
	]
}

/// Those that name an issue, a repo or a milestone: refused outright, not answered emptily.
const REFUSED: [&str; 8] = [
	"/api/edit", "/api/move", "/api/milestone", "/api/thread", "/api/comment", "/api/review", "/api/conflict", "/api/resolve",
];

/// Sends ticks that say nothing, for as long as it is listened to.
const UNTRIED: &str = "/api/changes";

#[test]
fn what_one_account_brings_is_out_of_anothers_reach() {
	let api = include_str!("../../../tedi_web/src/api.rs");
	let declared: BTreeSet<&str> = ["#[get(\"", "#[post(\"", "#[put(\"", "#[delete(\"", "#[patch(\""]
		.iter()
		.flat_map(|open| api.split(open).skip(1).map(|rest| rest.split('"').next().unwrap()))
		.collect();
	assert!(!api.contains("#[server]"), "a server fn needs a stable path to be tried here");
	let tried: BTreeSet<&str> = attempts().into_iter().map(|(_, path, _)| path).chain([UNTRIED]).collect();
	assert_eq!(declared, tried, "each server fn is tried against another account's scope");

	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = with_alice(&ctx);
	bring(port, "bob", "bob", "kb");
	assert!(board(port, "alice").contains("Secret day"), "a milestone of the scope is a target");
	assert!(board(port, "bob").contains("Team task"), "the team's is everyone's");

	for (method, path, body) in attempts() {
		let (status, answer) = call(port, method, path, Some("bob"), body.clone());
		assert!(!answer.contains("Secret"), "{path}: {answer}");
		if REFUSED.contains(&path) {
			assert_ne!(status, 200, "{path} {body:?}: {answer}");
		}
	}
	let (_, refused) = call(port, "POST", "/api/thread", Some("bob"), Some(json!({ "link": PRIVATE })));
	let (_, unheard_of) = call(port, "POST", "/api/thread", Some("bob"), Some(json!({ "link": "https://github.com/alice/nowhere/issues/1" })));
	assert_eq!(refused.replace("priv", "nowhere"), unheard_of, "nor does a refusal tell whether it is here");

	// bob's own scope names it by now: his key lists none of it
	until("bob's scope is tried", || call(port, "GET", "/api/account", Some("bob"), None).1.contains("\"at\""));
	assert!(!board(port, "bob").contains("Secret"), "a scope reaches what its own key lists");

	assert_eq!(
		post(port, "alice", "/api/thread", json!({ "link": PRIVATE }))["comments"],
		json!([]),
		"alice reaches it all along"
	);
}

#[test]
fn a_repo_gone_from_the_keys_listing_leaves_reach() {
	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = with_alice(&ctx);
	ctx.remote_tokens(&[("ka", "alice", &[])]);
	post(port, "alice", "/api/refresh", json!({}));
	until("it leaves", || !board(port, "alice").contains("Secret"));
	let (status, _) = call(port, "POST", "/api/thread", Some("alice"), Some(json!({ "link": PRIVATE })));
	assert_ne!(status, 200);

	// given back, then the key revoked
	ctx.remote_tokens(&[("ka", "alice", &["alice/priv"])]);
	post(port, "alice", "/api/refresh", json!({}));
	until("it is back", || board(port, "alice").contains("Secret plan"));
	ctx.remote_tokens(&[]);
	post(port, "alice", "/api/refresh", json!({}));
	until("it leaves with the key", || !board(port, "alice").contains("Secret"));
	let (_, account) = call(port, "GET", "/api/account", Some("alice"), None);
	assert!(account.contains("Bad credentials"), "the key says why: {account}");
}

#[test]
fn a_key_is_verified_kept_and_never_sent_back() {
	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = serve(&ctx, &config(), &GATED);
	let bring = |owner: &str, token: &str| {
		call(
			port,
			"POST",
			"/api/account/edit",
			Some("alice"),
			Some(json!({ "edit": { "Key": { "owner": owner, "token": token } } })),
		)
	};
	assert_ne!(bring("alice", "nonsense").0, 200, "GitHub knows no such token");
	let (status, held) = bring("Alice", "ka");
	assert_eq!(status, 200, "{held}");
	let held: Value = serde_json::from_str(&held).unwrap();
	assert_eq!(held["login"], "alice", "the login is the key's, never typed");
	assert_eq!(held["keys"][0]["owner"], "alice");
	assert_ne!(bring("bob", "kb").0, 200, "another person's key");

	for path in ["/api/account", "/api/setup", "/api/library"] {
		assert!(!call(port, "GET", path, Some("alice"), None).1.contains("\"ka\""), "{path}");
	}
	let file = ctx.xdg.data_dir().join("web/accounts/alice.json");
	let mode = std::os::unix::fs::PermissionsExt::mode(&std::fs::metadata(file).unwrap().permissions());
	assert_eq!(mode & 0o777, 0o600);

	post(port, "alice", "/api/account/edit", json!({ "edit": { "DropKey": "alice" } }));
	assert_eq!(
		serde_json::from_str::<Value>(&call(port, "GET", "/api/account", Some("alice"), None).1).unwrap()["login"],
		Value::Null
	);
}

#[test]
fn a_move_is_its_viewers() {
	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = with_alice(&ctx);
	let select = |user: &str, link: &str| {
		call(
			port,
			"POST",
			"/api/move",
			Some(user),
			Some(json!({ "link": link, "workflow": "kanban", "to": "selected", "seen": null })),
		)
	};
	let (status, body) = select("bob", TEAM);
	assert_ne!(status, 200, "bob brought no key: who moves it is unknown: {body}");

	// created since the last pull
	ctx.remote_in(
		("alice", "priv").into(),
		&parse_virtual("- [ ] Fresh <!-- @alice https://github.com/alice/priv/issues/2 -->\n  body\n"),
		Some(Seed::new(0)),
	);
	ctx.remote_tokens(&[("ka", "alice", &["alice/priv"])]);
	for (link, title) in [(TEAM, "Team task"), (PRIVATE, "Secret plan"), ("https://github.com/alice/priv/issues/2", "Fresh")] {
		assert_eq!(select("alice", link), (200, "\"Landed\"".into()));
		let cards = board(port, "alice");
		let card = cards
			.lines()
			.flat_map(|l| serde_json::from_str::<Value>(l).unwrap()["Read"]["cards"].as_array().cloned().unwrap_or_default())
			.find(|c| c["title"] == title);
		let card = card.unwrap_or_else(|| panic!("{title} is stored"));
		assert_eq!((&card["states"]["kanban"], &card["assignees"]), (&json!("selected"), &json!(["alice"])));
	}
}

#[test]
fn a_comment_is_edited_by_its_authors_account_alone() {
	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = with_alice(&ctx);
	bring(port, "carol", "alice", "kc");
	scope(port, "carol", "alice/priv");
	until("carol reaches it", || board(port, "carol").contains("Secret plan"));

	assert_eq!(
		post(port, "alice", "/api/comment", json!({ "link": PRIVATE, "edit": { "Add": { "body": "from alice" } } })),
		"Landed"
	);
	let said = post(port, "carol", "/api/thread", json!({ "link": PRIVATE }))["comments"][0].clone();
	assert_eq!((&said["by"], &said["body"]), (&json!("alice"), &json!("from alice")));

	let change = |new: &str, seen: &str| json!({ "link": PRIVATE, "edit": { "Change": { "id": said["id"], "seen": seen, "new": new } } });
	let refused = post(port, "carol", "/api/comment", change("from carol", "from alice"));
	assert!(refused["Rejected"].is_string(), "{refused}");
	assert_eq!(post(port, "alice", "/api/comment", change("from alice again", "from alice")), "Landed");
	let state: Value = serde_json::from_str(&std::fs::read_to_string(&ctx.mock_state_path).unwrap()).unwrap();
	assert_eq!(state["comments"][0]["body"], "from alice again");
	assert_eq!(state["comments"][0]["owner_login"], "alice");

	// whoever acted, the bytes stored are the store's own
	let stored = |ctx: &TestContext| std::fs::read_to_string(ctx.xdg.data_dir().join("issues/alice/priv/1_-_Secret_plan.md")).unwrap();
	let mut other = TestContext::build();
	other.mock_state_path = ctx.mock_state_path.clone();
	let out = other.run(&["--mock", "pull", "alice/priv"]);
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(stored(&ctx), stored(&other));
}

#[test]
fn a_delivery_is_taken_for_an_accounts_scope() {
	let ctx = TestContext::build();
	seed(&ctx);
	let (_server, port) = with_alice(&ctx);
	assert_eq!(deliver(port, "stranger/r"), 204);
	ctx.remote_in(
		("alice", "priv").into(),
		&parse_virtual("- [ ] Fresh <!-- @alice https://github.com/alice/priv/issues/2 -->\n  body\n"),
		Some(Seed::new(0)),
	);
	ctx.remote_tokens(&[("ka", "alice", &["alice/priv"])]);
	assert_eq!(deliver(port, "alice/priv"), 202);
	until("it is pulled", || board(port, "alice").contains("Fresh"));
}
