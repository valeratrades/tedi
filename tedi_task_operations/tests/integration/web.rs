//! `tedi web`: its webhook pulls only what `watch` names (the GitHub App is public, so a stranger's install delivers signed with
//! our secret), each viewer's library is their own, and the board wears its labels' colours.

use std::{
	io::{Read as _, Write as _},
	net::TcpStream,
	process::{Child, Command, Stdio},
	time::{Duration, Instant},
};

use hmac::Mac as _;
use tedi_core::board::{Board, Part};

use crate::common::{BINARY_ENV, TestContext, get_binary_path};

pub(crate) const SECRET: &str = "webhook secret";

pub(crate) const BOARD: &str =
	"[board]\nworkflows = { kanban = \"preset:kanban\" }\nviews = [{ name = \"Triage\", group_by = \"state:kanban\", shown = [] }]\n\n[board.scopes.Main]\nrepos = [\"o/*\"]\n";

/// The team's config at `o/config`, as it stands after each commit.
const CONFIG_REPO: &str = "o/config";

pub(crate) fn deliver(port: u16, repo: &str) -> u16 {
	let body = format!(r#"{{"repository":{{"full_name":"{repo}"}}}}"#);
	let mut mac = hmac::Hmac::<sha2::Sha256>::new_from_slice(SECRET.as_bytes()).unwrap();
	mac.update(body.as_bytes());
	let signature = hex::encode(mac.finalize().into_bytes());
	let mut stream = TcpStream::connect(("127.0.0.1", port)).unwrap();
	write!(
		stream,
		"POST /webhook HTTP/1.1\r\nHost: board\r\nContent-Type: application/json\r\nX-Hub-Signature-256: sha256={signature}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
		body.len()
	)
	.unwrap();
	let mut response = String::new();
	stream.read_to_string(&mut response).unwrap();
	response.split(' ').nth(1).expect("an HTTP status line").parse().unwrap()
}

pub(crate) struct Server(Child);
impl Drop for Server {
	fn drop(&mut self) {
		self.0.kill().unwrap(); // a failed assert must not leave it serving, holding the harness' stderr open
		self.0.wait().unwrap();
	}
}

/// `tedi --mock web` over `ctx`'s store, with `config` as its config and `args` after `web`; serving once this returns.
pub(crate) fn serve(ctx: &TestContext, config: &str, args: &[&str]) -> (Server, u16) {
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), config).unwrap();
	let port = std::net::TcpListener::bind("127.0.0.1:0").unwrap().local_addr().unwrap().port();
	let mut cmd = Command::new(get_binary_path());
	cmd.args(["--mock", "web", "--addr", &format!("127.0.0.1:{port}")]).args(args);
	cmd.envs(BINARY_ENV);
	cmd.env(concat!("tedi", "_GITHUB_TOKEN"), "test_token");
	for (key, value) in ctx.xdg.env_vars() {
		cmd.env(key, value);
	}
	cmd.env(concat!("tedi", "_MOCK_STATE"), &ctx.mock_state_path);
	cmd.env(crate::common::ENV_MOCK_CALL_LOG, ctx.call_log_path());
	let mut server = Server(cmd.stdout(Stdio::null()).stderr(Stdio::inherit()).spawn().unwrap());
	let up = Instant::now();
	while TcpStream::connect(("127.0.0.1", port)).is_err() {
		assert!(server.0.try_wait().unwrap().is_none(), "`tedi web` exited before serving");
		assert!(up.elapsed() < Duration::from_secs(30), "`tedi web` never listened");
		std::thread::sleep(Duration::from_millis(20));
	}
	(server, port)
}

#[test]
fn a_delivery_for_a_repo_outside_watch_is_not_pulled() {
	let ctx = TestContext::build();
	let (server, port) = serve(&ctx, &format!("watch = [\"o\"]\ngithub_webhook_secret = \"{SECRET}\"\n\n[board]\nviews = []\n"), &[]);

	assert_eq!(deliver(port, "stranger/r"), 204);
	assert_eq!(deliver(port, "o/r"), 202);
	// the puller is serial: once the watched repo is listed, the stranger's would have been before it
	let listed = Instant::now();
	while !ctx.github_calls().iter().any(|c| c.starts_with("list_repo_issues(o/r")) {
		assert!(listed.elapsed() < Duration::from_secs(30), "the watched repo was never pulled: {:?}", ctx.github_calls());
		std::thread::sleep(Duration::from_millis(20));
	}
	drop(server);
	assert!(!ctx.github_calls().iter().any(|c| c.contains("stranger")), "{:?}", ctx.github_calls());
}

/// `(status, body)` of one request; `user` rides in `X-Auth-User`.
pub(crate) fn call(port: u16, method: &str, path: &str, user: Option<&str>, body: Option<serde_json::Value>) -> (u16, String) {
	let body = body.map(|b| b.to_string()).unwrap_or_default();
	let user = user.map(|u| format!("X-Auth-User: {u}\r\n")).unwrap_or_default();
	let mut stream = TcpStream::connect(("127.0.0.1", port)).unwrap();
	write!(
		stream,
		"{method} {path} HTTP/1.0\r\nHost: board\r\n{user}Content-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
		body.len()
	)
	.unwrap();
	let mut response = String::new();
	stream.read_to_string(&mut response).unwrap();
	let status = response.split(' ').nth(1).expect("an HTTP status line").parse().unwrap();
	let body = response.split_once("\r\n\r\n").expect("headers end").1.to_string();
	(status, body)
}

/// The board, put together of its parts.
pub(crate) fn board(port: u16) -> serde_json::Value {
	let (status, body) = call(port, "GET", "/api/board", None, None);
	assert_eq!(status, 200, "{body}");
	let parts: Vec<Part> = body.lines().map(|l| serde_json::from_str(l).unwrap()).collect();
	assert!(matches!(parts.last(), Some(Part::Rest { .. })), "{body}");
	let mut board = Board::default();
	board.extend(parts);
	serde_json::to_value(board).unwrap()
}

fn library(port: u16, user: Option<&str>) -> serde_json::Value {
	let (status, body) = call(port, "GET", "/api/library", user, None);
	assert_eq!(status, 200, "{body}");
	serde_json::from_str(&body).unwrap()
}

fn edit(port: u16, user: Option<&str>, edit: serde_json::Value) -> (u16, String) {
	call(port, "POST", "/api/library/edit", user, Some(serde_json::json!({ "edit": edit })))
}

fn lens(scope: &str, search: &str, sort: &str) -> serde_json::Value {
	serde_json::json!({ "scope": scope, "search": search, "sort": sort })
}

fn keep_workspace(name: &str, band: &str, layout: &str) -> serde_json::Value {
	serde_json::json!({ "Keep": { "Workspace": { "name": name, "workspace": { "tiles": { band: layout }, "lens": lens("", "", "rank") } } } })
}

fn tiled(workspace: &str, band: &str, layout: &str) -> serde_json::Value {
	serde_json::json!({ "Workspace": { "workspace": workspace, "part": { "Tiles": { "band": band, "layout": layout } } } })
}

fn lensed(workspace: &str, part: &str, to: &str) -> serde_json::Value {
	serde_json::json!({ "Workspace": { "workspace": workspace, "part": { part: to } } })
}

fn boxed(view: &str, part: serde_json::Value) -> serde_json::Value {
	serde_json::json!({ "View": { "view": view, "part": part } })
}

#[test]
fn a_viewer_who_never_opened_the_board_has_ws_1_selected_and_empty() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let fresh = library(port, Some("alice"));
	assert_eq!(fresh["workspace"], "ws 1");
	assert_eq!(fresh["workspaces"], serde_json::json!({ "ws 1": { "tiles": {}, "lens": lens("", "", "rank") } }));
	let first = id(&fresh, serde_json::json!({ "Workspace": "ws 1" }));

	assert_eq!(edit(port, Some("alice"), serde_json::json!("NewWorkspace")).0, 200);
	let library = library(port, Some("alice"));
	assert_eq!(library["workspace"], "ws 2");
	assert_eq!(library["workspaces"]["ws 2"], fresh["workspaces"]["ws 1"]);
	assert_eq!(id(&library, serde_json::json!({ "Workspace": "ws 1" })), first, "the id it was first read under");

	assert_eq!(edit(port, Some("alice"), serde_json::json!({ "Drop": { "shelf": "Workspaces", "name": "ws 1" } })).0, 200);
	assert_eq!(edit(port, Some("alice"), serde_json::json!("NewWorkspace")).0, 200);
	let library = self::library(port, Some("alice"));
	assert_eq!(library["workspaces"].as_object().unwrap().keys().collect::<Vec<_>>(), ["ws 1", "ws 2"], "the lowest free");
	assert_eq!(edit(port, Some("alice"), serde_json::json!({ "Drop": { "shelf": "Workspaces", "name": "ws 1" } })).0, 200);
	assert_eq!(self::library(port, Some("alice"))["workspace"], serde_json::Value::Null, "the one selected is gone: none is");
}

#[test]
fn behind_a_gate_each_viewer_keeps_their_own_library() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);

	let (status, body) = edit(port, Some("alice"), keep_workspace("focus", "Xl", "{}"));
	assert_eq!(status, 200, "{body}");
	assert_eq!(library(port, Some("alice"))["workspaces"]["focus"]["tiles"]["Xl"], "{}");
	assert!(library(port, Some("bob"))["workspaces"].get("focus").is_none());

	let (status, _) = call(port, "GET", "/api/library", None, None);
	assert_ne!(status, 200, "a request the gate didn't name a viewer on");
	let (status, _) = call(port, "GET", "/api/library", Some("../alice"), None);
	assert_ne!(status, 200, "a viewer that isn't a plain name");
}

#[test]
fn ungated_the_library_is_the_instances_github_user() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &[]);
	let (status, body) = edit(port, None, keep_workspace("focus", "Xl", "{}"));
	assert_eq!(status, 200, "{body}");
	assert_eq!(library(port, None)["workspaces"]["focus"]["tiles"]["Xl"], "{}");
	let (status, _) = call(port, "GET", "/api/library", Some("alice"), None);
	assert_eq!(status, 200, "ungated, a header names nobody");
	assert_eq!(library(port, Some("alice"))["workspaces"]["focus"]["tiles"]["Xl"], "{}");
}

#[test]
fn a_library_keeps_each_band_and_its_names_straight() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, keep_workspace("focus", "Xl", "wide")).0, 200);
	assert_eq!(edit(port, me, tiled("focus", "Sm", "narrow")).0, 200);
	assert_eq!(library(port, me)["workspaces"]["focus"]["tiles"], serde_json::json!({ "Sm": "narrow", "Xl": "wide" }));

	assert_eq!(edit(port, me, keep_workspace("other", "Xl", "x")).0, 200);
	let rename = |to: &str| serde_json::json!({ "Rename": { "shelf": "Workspaces", "from": "focus", "to": to } });
	assert_ne!(edit(port, me, rename("other")).0, 200, "renamed onto a name that's taken");

	assert_eq!(edit(port, me, serde_json::json!({ "Favorite": { "what": { "Workspace": "focus" }, "on": true } })).0, 200);
	assert_eq!(edit(port, me, serde_json::json!({ "Favorite": { "what": { "Scope": "Main" }, "on": true } })).0, 200);
	assert_ne!(
		edit(port, me, serde_json::json!({ "Favorite": { "what": { "View": "nowhere" }, "on": true } })).0,
		200,
		"no such view"
	);
	assert_eq!(edit(port, me, rename("deep")).0, 200);
	assert_eq!(
		library(port, me)["favorites"],
		serde_json::json!([{ "Scope": "Main" }, { "Workspace": "deep" }]),
		"a favorite follows its rename"
	);

	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Workspaces", "name": "deep" } })).0, 200);
	let library = library(port, me);
	assert_eq!(library["favorites"], serde_json::json!([{ "Scope": "Main" }]), "a dropped workspace takes its favorite with it");
	assert_eq!(library["workspaces"].as_object().unwrap().keys().collect::<Vec<_>>(), ["other", "ws 1"]);
	assert_ne!(edit(port, me, tiled("deep", "Xl", "late")).0, 200, "an edit of one that is gone");
}

#[test]
fn lens_parts_edited_from_two_tabs_both_stand() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, lensed("ws 1", "Scope", "repo:o/*")).0, 200);
	assert_eq!(edit(port, me, lensed("ws 1", "Search", "is:open crash")).0, 200);
	assert_eq!(edit(port, me, lensed("ws 1", "Sort", "importance,-hours")).0, 200);
	assert_eq!(edit(port, me, tiled("ws 1", "Xl", "wide")).0, 200);
	assert_eq!(
		library(port, me)["workspaces"]["ws 1"],
		serde_json::json!({ "tiles": { "Xl": "wide" }, "lens": lens("repo:o/*", "is:open crash", "importance,-hours") })
	);
	assert_ne!(edit(port, me, lensed("ws 1", "Search", "i:many")).0, 200, "a search that doesn't parse");
	assert_ne!(edit(port, me, lensed("ws 1", "Scope", "repo:o/* words")).0, 200, "a scope of words");
	assert_eq!(library(port, me)["workspaces"]["ws 1"]["lens"], lens("repo:o/*", "is:open crash", "importance,-hours"));
}

#[test]
fn an_edit_naming_one_workspace_leaves_the_other_alone() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, keep_workspace("focus", "Xl", "focus")).0, 200);
	for selected in ["focus", "ws 1"] {
		assert_eq!(edit(port, me, serde_json::json!({ "Open": selected })).0, 200);
		assert_eq!(edit(port, me, tiled("ws 1", "Xl", selected)).0, 200);
		assert_eq!(edit(port, me, lensed("ws 1", "Search", selected)).0, 200);
		let library = library(port, me);
		assert_eq!(
			library["workspaces"]["ws 1"],
			serde_json::json!({ "tiles": { "Xl": selected }, "lens": lens("", selected, "rank") })
		);
		assert_eq!(library["workspaces"]["focus"], serde_json::json!({ "tiles": { "Xl": "focus" }, "lens": lens("", "", "rank") }));
		assert_eq!(library["workspace"], selected, "an edit selects nothing");
	}
}

#[test]
fn a_team_workspace_edited_is_a_draft_the_teams_untouched() {
	let ctx = TestContext::build();
	let board = format!("{BOARD}\n[board.workspaces.Desk]\ntiles = {{ Xl = \"theirs\" }}\nlens = {{ scope = \"repo:o/*\", search = \"\", sort = \"rank\" }}\n");
	let (_server, port) = serve(&ctx, &board, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, serde_json::json!({ "Open": "Desk" })).0, 200);
	assert_eq!(edit(port, me, lensed("Desk", "Search", "is:open")).0, 200);
	assert_eq!(edit(port, me, tiled("Desk", "Xl", "mine")).0, 200);
	let drafted = library(port, me);
	assert_eq!(
		drafted["drafts"]["workspaces"],
		serde_json::json!({ "Desk": { "tiles": { "Xl": "mine" }, "lens": lens("repo:o/*", "is:open", "rank") } })
	);
	assert!(drafted["workspaces"].get("Desk").is_none());
	let (status, body) = call(port, "GET", "/api/setup", me, None);
	assert_eq!(status, 200, "{body}");
	let setup: serde_json::Value = serde_json::from_str(&body).unwrap();
	assert_eq!(
		setup["workspaces"]["Desk"],
		serde_json::json!({ "tiles": { "Xl": "theirs" }, "lens": lens("repo:o/*", "", "rank") })
	);
	assert_eq!(library(port, Some("bob"))["drafts"]["workspaces"], serde_json::json!({}));

	assert_eq!(edit(port, me, serde_json::json!({ "Revert": { "Workspace": "Desk" } })).0, 200);
	assert_eq!(library(port, me)["drafts"]["workspaces"], serde_json::json!({}));
	assert_eq!(edit(port, me, lensed("Desk", "Search", "is:open")).0, 200);
	assert_eq!(edit(port, me, lensed("Desk", "Search", "")).0, 200);
	assert_eq!(library(port, me)["drafts"]["workspaces"], serde_json::json!({}), "edited back to the team's: nothing is drafted");
}

#[test]
fn a_scope_edited_after_being_applied_changes_no_workspace() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let keep = |scope: serde_json::Value| edit(port, me, serde_json::json!({ "Keep": { "Scope": { "name": "mine", "scope": scope } } })).0;
	assert_eq!(keep(serde_json::json!({ "repos": ["o/core"], "terms": "assignee:@me" })), 200);
	assert_eq!(edit(port, me, lensed("ws 1", "Scope", "repo:o/core assignee:@me")).0, 200);
	assert_eq!(keep(serde_json::json!({ "terms": "t:bug" })), 200, "terms alone make a scope");
	let held = library(port, me);
	assert_eq!(held["scopes"]["mine"], serde_json::json!({ "terms": "t:bug" }));
	assert_eq!(held["workspaces"]["ws 1"]["lens"]["scope"], "repo:o/core assignee:@me");
	assert_ne!(keep(serde_json::json!({})), 200, "neither repos, nor scopes, nor terms");
	assert_ne!(keep(serde_json::json!({ "terms": "crash on save" })), 200, "a scope of words");
	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Scopes", "name": "mine" } })).0, 200);
	assert_eq!(library(port, me)["workspaces"]["ws 1"]["lens"]["scope"], "repo:o/core assignee:@me");
}

/// The id `of` (`{ids: {id: item}}`) gives `item`.
fn id(of: &serde_json::Value, item: serde_json::Value) -> String {
	let ids = of["ids"].as_object().expect("a map of ids").iter();
	let held: Vec<&String> = ids.filter(|(_, held)| **held == item).map(|(id, _)| id).collect();
	let [id] = held.as_slice() else { panic!("{item} has ids {held:?}") };
	id.to_string()
}

#[test]
fn an_object_keeps_its_id_through_a_rename() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let kept = |name: &str| keep_view(view(name, &[]));
	assert_eq!(edit(port, me, kept("first")).0, 200);
	let first = id(&library(port, me), serde_json::json!({ "View": "first" }));
	assert_eq!(edit(port, me, kept("first")).0, 200);
	assert_eq!(id(&library(port, me), serde_json::json!({ "View": "first" })), first, "kept again");

	assert_eq!(edit(port, me, serde_json::json!({ "Rename": { "shelf": "Views", "from": "first", "to": "second" } })).0, 200);
	assert_eq!(id(&library(port, me), serde_json::json!({ "View": "second" })), first);
	assert_eq!(edit(port, me, kept("first")).0, 200);
	assert_ne!(id(&library(port, me), serde_json::json!({ "View": "first" })), first, "another view under the old name");

	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Views", "name": "second" } })).0, 200);
	assert!(library(port, me)["ids"].get(&first).is_none(), "a dropped view's id names nothing");

	assert_eq!(edit(port, me, keep_workspace("focus", "Xl", "{}")).0, 200);
	id(&library(port, me), serde_json::json!({ "Workspace": "focus" }));
}

#[test]
fn a_library_kept_before_ids_is_given_lasting_ones() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, keep_workspace("focus", "Xl", "{}")).0, 200);
	let file = ctx.xdg.inner.root.join("data/tedi/web/alice.json");
	let mut old: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&file).unwrap()).unwrap();
	old.as_object_mut().unwrap().remove("ids");
	std::fs::write(&file, old.to_string()).unwrap();

	let given = id(&library(port, me), serde_json::json!({ "Workspace": "focus" }));
	assert_eq!(edit(port, me, serde_json::json!({ "Favorite": { "what": { "Workspace": "focus" }, "on": true } })).0, 200);
	assert_eq!(id(&library(port, me), serde_json::json!({ "Workspace": "focus" })), given);
}

#[test]
fn a_view_is_laid_out_as_a_column_layout() {
	let ctx = TestContext::build();
	let board = format!("{BOARD}\n[board.columns.team]\nby = \"state:kanban\"\ntints = {{ triage = \"blue\" }}\nhidden = [\"done\"]\n\n[board.columns.repos]\nby = \"repo\"\n");
	let (_server, port) = serve(&ctx, &board, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let mine = serde_json::json!({ "by": "state:kanban", "arrangement": { "Xl": "tabbed" }, "tints": { "triage": "red" } });
	assert_eq!(edit(port, me, serde_json::json!({ "Keep": { "Columns": { "name": "mine", "columns": mine } } })).0, 200);
	let lay = |columns: &str| serde_json::json!({ "Lay": { "view": "Triage", "columns": columns } });

	assert_eq!(edit(port, me, lay("mine")).0, 200);
	let held = library(port, me);
	assert_eq!(
		held["drafts"]["views"]["Triage"],
		serde_json::json!({ "name": "Triage", "group_by": "state:kanban", "shown": [], "arrangement": { "Xl": "tabbed" }, "tints": { "triage": "red" } })
	);
	assert_eq!(held["laid"]["Triage"], "mine");

	let (status, body) = edit(port, me, serde_json::json!({ "Rename": { "shelf": "Columns", "from": "mine", "to": "ours" } }));
	assert_eq!(status, 200, "{body}");
	assert_eq!(library(port, me)["laid"]["Triage"], "ours");

	assert_eq!(edit(port, me, lay("team")).0, 200);
	assert_eq!(
		library(port, me)["drafts"]["views"]["Triage"],
		serde_json::json!({ "name": "Triage", "group_by": "state:kanban", "shown": [], "tints": { "triage": "blue" }, "hidden": ["done"] }),
		"its boxes are the layout's, whole"
	);
	assert_ne!(edit(port, me, lay("nowhere")).0, 200);
	assert_ne!(edit(port, me, lay("repos")).0, 200, "columns of another grouping");

	assert_eq!(edit(port, me, lay("ours")).0, 200);
	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Columns", "name": "ours" } })).0, 200);
	assert_eq!(library(port, me)["laid"], serde_json::json!({}), "the layout it named is gone");
}

#[test]
fn a_views_boxes_are_drafted_a_part_at_a_time_and_kept_with_it() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let arranged = |view: &str, band: &str, layout: &str| boxed(view, serde_json::json!({ "Arrangement": { "band": band, "layout": layout } }));
	assert_eq!(edit(port, me, arranged("Triage", "Xl", "tabbed")).0, 200);
	assert_eq!(edit(port, me, arranged("Triage", "Sm", "narrow")).0, 200);
	assert_eq!(edit(port, me, boxed("Triage", serde_json::json!({ "Tints": { "triage": "red" } }))).0, 200);
	assert_eq!(edit(port, me, boxed("Triage", serde_json::json!({ "Hidden": ["done"] }))).0, 200);
	let drafted = library(port, me)["drafts"]["views"]["Triage"].clone();
	assert_eq!(
		drafted,
		serde_json::json!({
			"name": "Triage", "group_by": "state:kanban", "shown": [],
			"arrangement": { "Sm": "narrow", "Xl": "tabbed" }, "tints": { "triage": "red" }, "hidden": ["done"],
		})
	);

	let mut main = drafted;
	main["name"] = "main".into();
	let (status, body) = edit(port, me, keep_view(main.clone()));
	assert_eq!(status, 200, "{body}");
	assert_eq!(library(port, me)["views"]["main"], main);

	assert_eq!(edit(port, me, arranged("main", "Xl", "split")).0, 200);
	let held = library(port, me);
	assert_eq!(held["drafts"]["views"]["main"]["arrangement"]["Xl"], "split");
	assert_eq!(held["views"]["main"]["arrangement"]["Xl"], "tabbed", "kept as it was until saved");
	assert_eq!(held["drafts"]["views"]["Triage"]["arrangement"]["Xl"], "tabbed", "each is its own from then on");
	assert_eq!(edit(port, me, keep_view(held["drafts"]["views"]["main"].clone())).0, 200);
	assert!(library(port, me)["drafts"]["views"].get("main").is_none(), "saved: nothing is drafted");

	assert_eq!(edit(port, me, boxed("main", serde_json::json!({ "GroupBy": "repo" }))).0, 200);
	assert_eq!(
		library(port, me)["drafts"]["views"]["main"],
		serde_json::json!({ "name": "main", "group_by": "repo", "shown": [] }),
		"other groups: no box of these is kept"
	);
	assert_eq!(edit(port, me, serde_json::json!({ "Revert": { "View": "main" } })).0, 200);
	assert!(library(port, me)["drafts"]["views"].get("main").is_none());
	assert_ne!(edit(port, me, arranged("nowhere", "Xl", "x")).0, 200);
}

#[test]
fn the_board_carries_its_repos_label_colors_as_of_the_last_pull() {
	let ctx = TestContext::build();
	ctx.remote(
		&crate::common::parse_virtual("- [ ] (t:bug, wontfix) Fix it <!-- @mock_user https://github.com/o/r/issues/1 -->\n  body\n"),
		Some(crate::common::Seed::new(0)),
	);
	ctx.remote_labels("o", "r", &["t:bug", "wontfix"]);
	assert!(ctx.run(&["--mock", "pull", "o/r"]).status.success());
	ctx.remote_labels("o", "r", &["t:bug", "won't fix"]);
	assert!(ctx.run(&["--mock", "pull", "o/r"]).status.success());

	let (_server, port) = serve(&ctx, BOARD, &[]);
	assert_eq!(board(port)["colors"], serde_json::json!({ "o/r": { "t:bug": "ededed", "won't fix": "ededed" } }));
}

fn team_file(triage_hides: &str) -> String {
	format!(
		"# the team's board\n\n[[board.views]]\nname = \"Triage\"\ngroup_by = \"state:kanban\"\nshown = []\nhidden = [{triage_hides}]\n\n[[board.views]]\nname = \"Repos\"\ngroup_by = \"repo\"\nshown = []\n"
	)
}

/// The board as deployed from `team_file(triage_hides)`, its history at `CONFIG_REPO`.
fn sourced(triage_hides: &str) -> String {
	let views = team_file(triage_hides).replace("# the team's board\n\n", "");
	format!("[board]\nworkflows = {{ kanban = \"preset:kanban\" }}\nsource = {{ repo = \"{CONFIG_REPO}\", path = \"board.toml\", branch = \"main\" }}\n\n{views}")
}

fn view(name: &str, hidden: &[&str]) -> serde_json::Value {
	let mut view = serde_json::json!({ "name": name, "group_by": "state:kanban", "shown": [] });
	if !hidden.is_empty() {
		view["hidden"] = hidden.into();
	}
	view
}

fn keep_view(view: serde_json::Value) -> serde_json::Value {
	serde_json::json!({ "Keep": { "View": view } })
}

fn catalog(port: u16, user: &str) -> serde_json::Value {
	let (status, body) = call(port, "GET", "/api/catalog", Some(user), None);
	assert_eq!(status, 200, "{body}");
	serde_json::from_str(&body).unwrap()
}

fn entry(catalog: &serde_json::Value, owner: serde_json::Value, item: serde_json::Value) -> &serde_json::Value {
	catalog["entries"]
		.as_array()
		.unwrap()
		.iter()
		.find(|e| e["owner"] == owner && e["item"] == item)
		.unwrap_or_else(|| panic!("no {owner} {item} in {catalog}"))
}

#[test]
fn a_fork_counts_what_each_side_changed_since() {
	let ctx = TestContext::build();
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("\"done\""));
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "carol", &team_file("\"done\", \"wont_do\""));
	let gate = ["--viewer-header", "X-Auth-User"];
	let running = sourced("\"done\", \"wont_do\"");
	let (server, port) = serve(&ctx, &running, &gate);
	let me = Some("alice");
	let triage = serde_json::json!({ "View": "Triage" });
	let mine = serde_json::json!({ "View": "Triage (mine)" });

	let (status, body) = edit(port, me, serde_json::json!({ "Fork": { "from": { "Team": triage }, "name": "Triage (mine)" } }));
	assert_eq!(status, 200, "{body}");
	assert_eq!(
		library(port, me)["forks"],
		serde_json::json!([{ "item": mine, "upstream": { "owner": "Team", "name": "Triage", "version": 2, "ahead": 0 } }])
	);
	let team = catalog(port, "alice");
	let team = entry(&team, serde_json::json!("Team"), triage.clone());
	assert_eq!(team["version"], 2);
	assert_eq!(team["history"].as_array().unwrap().iter().map(|v| v["author"].clone()).collect::<Vec<_>>(), ["bob", "carol"]);
	assert_eq!(team["history"][1]["changes"], serde_json::json!([{ "sign": "Added", "text": "hidden: wont_do" }]));
	assert_eq!(team["forks"], serde_json::json!([{ "viewer": "alice", "name": "Triage (mine)", "ahead": 0, "behind": 0 }]));

	let narrowed = view("Triage (mine)", &["done"]);
	assert_eq!(edit(port, me, keep_view(narrowed.clone())).0, 200);
	assert_eq!(edit(port, me, keep_view(narrowed)).0, 200, "saved unchanged");
	assert_eq!(library(port, me)["forks"][0]["upstream"]["ahead"], 1, "only a save that changed it counts");

	drop(server);
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("\"backlog\""));
	let (_server, port) = serve(&ctx, &running, &gate);
	let catalog = catalog(port, "alice");
	assert_eq!(entry(&catalog, serde_json::json!({ "Viewer": "alice" }), mine.clone())["behind"], 1);
	assert_eq!(entry(&catalog, serde_json::json!("Team"), triage)["history"].as_array().unwrap().len(), 3);
	assert_ne!(edit(port, me, serde_json::json!({ "Pull": mine })).0, 200, "pulling would drop the save it is ahead by");
}

#[test]
fn a_past_commit_with_what_a_view_no_longer_holds_still_reads_as_a_version() {
	let ctx = TestContext::build();
	let past = |query: &str| {
		format!(
			"[[board.views]]\nname = \"Triage\"\nquery = \"{query}\"\nlayout = \"board\"\ngroup_by = \"state:kanban\"\nsort = \"importance\"\nshown = []\n\n[board.columns.notion]\nby = \"state:kanban\"\nlayout = {{ Xl = \"tabbed\" }}\ntints = {{ triage = \"orange\" }}\n"
		)
	};
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &past("is:open"));
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "carol", &past("is:open t:bug"));
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("\"done\""));
	let (_server, port) = serve(&ctx, &sourced("\"done\""), &["--viewer-header", "X-Auth-User"]);
	let catalog = catalog(port, "alice");
	let triage = entry(&catalog, serde_json::json!("Team"), serde_json::json!({ "View": "Triage" }));
	assert_eq!(
		triage["history"]
			.as_array()
			.unwrap()
			.iter()
			.map(|v| (v["author"].clone(), v["changes"].clone()))
			.collect::<Vec<_>>(),
		[
			("bob".into(), serde_json::json!([])),
			("bob".into(), serde_json::json!([{ "sign": "Added", "text": "hidden: done" }]))
		],
		"carol's changed nothing a view holds"
	);
	assert_eq!(triage["version"], 2);
}

#[test]
fn a_share_link_pins_what_was_shared() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let (alice, bob) = (Some("alice"), Some("bob"));
	let desk = r#"{"version":2,"grid":{"cells":[{"group":{"id":0,"tabs":["view:Mine"],"active":0},"x":0,"y":0,"w":48,"h":24,"desired_w":null,"desired_x":null,"desired_h":null,"rank":0}],"state":"Settled","next_group_id":1}}"#;
	let mut mine = view("Mine", &["done"]);
	mine["tints"] = serde_json::json!({ "triage": "red" });
	mine["arrangement"] = serde_json::json!({ "Xl": "tabbed" });
	assert_eq!(edit(port, alice, keep_view(mine.clone())).0, 200);
	assert_eq!(edit(port, alice, keep_workspace("desk", "Xl", desk)).0, 200);
	assert_eq!(edit(port, alice, lensed("desk", "Scope", "repo:o/*")).0, 200);
	assert_eq!(edit(port, alice, lensed("desk", "Search", "is:open")).0, 200);
	let held = serde_json::json!({ "tiles": { "Xl": desk }, "lens": lens("repo:o/*", "is:open", "rank") });

	let share = |user| {
		let (status, body) = call(port, "POST", "/api/share", user, Some(serde_json::json!({ "item": { "Workspace": "desk" }, "bands": ["Xl"] })));
		assert_eq!(status, 200, "{body}");
		serde_json::from_str::<String>(&body).unwrap()
	};
	let link = share(alice);
	assert_eq!(link, "/l/ws/alice/desk@1?bands=xl");
	assert_eq!(edit(port, alice, keep_view(view("Mine", &[]))).0, 200);
	assert_eq!(edit(port, alice, lensed("desk", "Search", "is:closed")).0, 200);

	let (status, body) = call(port, "POST", "/api/shared", bob, Some(serde_json::json!({ "link": link })));
	assert_eq!(status, 200, "{body}");
	let shared: serde_json::Value = serde_json::from_str(&body).unwrap();
	assert_eq!(shared["construction"], serde_json::json!({ "Workspace": held }), "its lens with its tiles");
	assert_eq!(
		shared["carried"],
		serde_json::json!({ "Mine": [1, mine] }),
		"the view as it stood when the link was copied, its boxes with it"
	);
	assert_eq!(library(port, bob)["received"], serde_json::json!([link]));

	let (status, body) = edit(port, bob, serde_json::json!({ "Fork": { "from": { "Link": link }, "name": "desk" } }));
	assert_eq!(status, 200, "{body}");
	let forked = library(port, bob);
	assert_eq!(forked["workspaces"]["desk"], held);
	assert_eq!(forked["views"]["Mine"], mine);
	assert_eq!(
		forked["forks"],
		serde_json::json!([
			{ "item": { "View": "Mine" }, "upstream": { "owner": { "Viewer": "alice" }, "name": "Mine", "version": 1, "ahead": 0 } },
			{ "item": { "Workspace": "desk" }, "upstream": { "owner": { "Viewer": "alice" }, "name": "desk", "version": 1, "ahead": 0 } },
		])
	);

	assert_eq!(share(alice), "/l/ws/alice/desk@2?bands=xl", "its lens and the view it carries changed since");
	assert_eq!(
		entry(&catalog(port, "bob"), serde_json::json!({ "Viewer": "bob" }), serde_json::json!({ "View": "Mine" }))["behind"],
		1
	);
}

#[test]
fn a_proposal_is_a_pull_request_replacing_only_that_object() {
	let ctx = TestContext::build();
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("\"done\""));
	let (_server, port) = serve(&ctx, &sourced("\"done\""), &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(
		edit(port, me, serde_json::json!({ "Fork": { "from": { "Team": { "View": "Triage" } }, "name": "Triage (mine)" } })).0,
		200
	);
	assert_eq!(edit(port, me, keep_view(view("Triage (mine)", &["done", "wont_do"]))).0, 200);

	let (status, body) = call(
		port,
		"POST",
		"/api/propose",
		me,
		Some(serde_json::json!({ "item": { "View": "Triage (mine)" }, "onto": { "Update": "Triage" }, "message": "fewer columns" })),
	);
	assert_eq!(status, 200, "{body}");
	assert_eq!(serde_json::from_str::<String>(&body).unwrap(), format!("https://github.com/{CONFIG_REPO}/pull/1"));
	let [proposal] = ctx.remote_proposals().try_into().unwrap();
	assert_eq!((proposal.base.as_str(), proposal.path.as_str()), ("main", "board.toml"));
	assert_eq!(proposal.content, team_file("\"done\", \"wont_do\""));
	assert!(proposal.body.starts_with("fewer columns\n\n```diff\n+ hidden: wont_do\n```"), "{}", proposal.body);
}

#[test]
fn a_viewers_scope_nests_in_any_scope_by_its_path() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let keep = |name: &str| edit(port, me, serde_json::json!({ "Keep": { "Scope": { "name": name, "scope": { "repos": ["o/core"] } } } })).0;
	assert_eq!(keep("Main / core"), 200, "inside the team's Main");
	assert_ne!(keep("Nowhere / core"), 200, "inside nothing");
	assert_ne!(keep("Main"), 200, "the team's own path");
	assert_eq!(keep("Main / core / deep"), 200, "inside the viewer's own");

	let (status, body) = edit(port, me, serde_json::json!({ "Rename": { "shelf": "Scopes", "from": "Main / core", "to": "core" } }));
	assert_eq!(status, 200, "{body}");
	let held = self::library(port, me);
	assert_eq!(
		held["scopes"].as_object().unwrap().keys().collect::<Vec<_>>(),
		["core", "core / deep"],
		"what it held moves with it"
	);

	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Scopes", "name": "core" } })).0, 200);
	let library = self::library(port, me);
	assert_eq!(library["scopes"], serde_json::json!({}));
}

/// A lens sits in a point of the chain or in terms, told apart by the text alone.
#[test]
fn a_point_of_the_chain_is_a_scope() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, &format!("orgs.Main = [\"o\"]\n{BOARD}"), &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let (status, setup) = call(port, "GET", "/api/setup", me, None);
	assert_eq!(status, 200, "{setup}");
	assert_eq!(serde_json::from_str::<serde_json::Value>(&setup).unwrap()["orgs"], serde_json::json!({ "main": ["o"] }));

	library(port, me);
	let sit = |scope: &str| edit(port, me, lensed("ws 1", "Scope", scope)).0;
	let held = || library(port, me)["workspaces"]["ws 1"]["lens"]["scope"].as_str().unwrap().to_string();
	for point in [
		"https://github.com/o",
		"https://github.com/o/r",
		"https://github.com/o/r/milestone/1",
		"https://github.com/O/r/issues/1",
	] {
		assert_eq!(sit(point), 200, "{point}");
		assert_eq!(held(), point.to_lowercase());
	}
	let long = format!("https://github.com/{}", "o".repeat(40));
	for bad in [
		"https://github.com/",
		"https://github.com/o/r/pull/1",
		"https://github.com/o/r/issues/x",
		"https://github.com/o is:open",
		&long,
	] {
		assert_ne!(sit(bad), 200, "{bad}");
	}
	assert_eq!(held(), "https://github.com/o/r/issues/1");
	assert_eq!(sit("repo:o/*"), 200, "terms still read as they are");
}

#[test]
fn a_config_whose_orgs_nest_deeper_is_refused() {
	let ctx = TestContext::build();
	std::fs::write(ctx.xdg.inner.root.join("config/tedi.toml"), format!("orgs.a = [\"b\"]\norgs.b = [\"c\"]\n{BOARD}")).unwrap();
	let out = ctx.run(&["--mock", "web", "--addr", "127.0.0.1:0"]);
	assert!(!out.status.success(), "{}", out.stdout);
	let why = "orgs nest one level";
	assert!(out.stderr.contains(why) || out.stdout.contains(why), "stdout: {}, stderr: {}", out.stdout, out.stderr);
}
