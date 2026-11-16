//! `tedi web`: its webhook pulls only what `watch` names (the GitHub App is public, so a stranger's install delivers signed with
//! our secret), each viewer's library is their own, and the board wears its labels' colours.

use std::{
	io::{Read as _, Write as _},
	net::TcpStream,
	process::{Child, Command, Stdio},
	time::{Duration, Instant},
};

use hmac::Mac as _;

use crate::common::{BINARY_ENV, TestContext, get_binary_path};

const SECRET: &str = "webhook secret";

fn deliver(port: u16, repo: &str) -> u16 {
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

struct Server(Child);
impl Drop for Server {
	fn drop(&mut self) {
		self.0.kill().unwrap(); // a failed assert must not leave it serving, holding the harness' stderr open
		self.0.wait().unwrap();
	}
}

/// `tedi --mock web` over `ctx`'s store, with `config` as its config and `args` after `web`; serving once this returns.
fn serve(ctx: &TestContext, config: &str, args: &[&str]) -> (Server, u16) {
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
	let (server, port) = serve(
		&ctx,
		&format!("watch = [\"o\"]\ngithub_webhook_secret = \"{SECRET}\"\n\n[board]\narchive_after = \"1w\"\nviews = []\n"),
		&[],
	);

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

const BOARD: &str = "[board]\narchive_after = \"1w\"\nviews = [{ name = \"Triage\", query = \"is:open\", layout = \"board\", group_by = \"stage\", sort = \"rank\", shown = [] }]\n\n[board.scopes.Main]\nrepos = [\"o/*\"]\n";

/// `(status, body)` of one request; `user` rides in `X-Auth-User`.
fn call(port: u16, method: &str, path: &str, user: Option<&str>, body: Option<serde_json::Value>) -> (u16, String) {
	let body = body.map(|b| b.to_string()).unwrap_or_default();
	let user = user.map(|u| format!("X-Auth-User: {u}\r\n")).unwrap_or_default();
	let mut stream = TcpStream::connect(("127.0.0.1", port)).unwrap();
	write!(
		stream,
		"{method} {path} HTTP/1.1\r\nHost: board\r\n{user}Content-Type: application/json\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
		body.len()
	)
	.unwrap();
	let mut response = String::new();
	stream.read_to_string(&mut response).unwrap();
	let status = response.split(' ').nth(1).expect("an HTTP status line").parse().unwrap();
	let body = response.split_once("\r\n\r\n").expect("headers end").1.to_string();
	(status, body)
}

fn library(port: u16, user: Option<&str>) -> serde_json::Value {
	let (status, body) = call(port, "GET", "/api/library", user, None);
	assert_eq!(status, 200, "{body}");
	serde_json::from_str(&body).unwrap()
}

fn edit(port: u16, user: Option<&str>, edit: serde_json::Value) -> (u16, String) {
	call(port, "POST", "/api/library/edit", user, Some(serde_json::json!({ "edit": edit })))
}

fn keep_workspace(name: &str, band: &str, layout: &str) -> serde_json::Value {
	serde_json::json!({ "Keep": { "Workspace": { "name": name, "band": band, "layout": layout } } })
}

#[test]
fn behind_a_gate_each_viewer_keeps_their_own_library() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);

	let (status, body) = edit(port, Some("alice"), keep_workspace("focus", "Xl", "{}"));
	assert_eq!(status, 200, "{body}");
	assert_eq!(library(port, Some("alice"))["workspaces"]["focus"]["Xl"], "{}");
	assert_eq!(library(port, Some("bob"))["workspaces"], serde_json::json!({}));

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
	assert_eq!(library(port, None)["workspaces"]["focus"]["Xl"], "{}");
	let (status, _) = call(port, "GET", "/api/library", Some("alice"), None);
	assert_eq!(status, 200, "ungated, a header names nobody");
	assert_eq!(library(port, Some("alice"))["workspaces"]["focus"]["Xl"], "{}");
}

#[test]
fn a_library_keeps_each_band_and_its_names_straight() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(edit(port, me, keep_workspace("focus", "Xl", "wide")).0, 200);
	assert_eq!(edit(port, me, keep_workspace("focus", "Sm", "narrow")).0, 200);
	assert_eq!(library(port, me)["workspaces"]["focus"], serde_json::json!({ "Sm": "narrow", "Xl": "wide" }));

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
	assert_eq!(library["workspaces"], serde_json::json!({ "other": { "Xl": "x" } }));
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
	let (status, body) = call(port, "GET", "/api/board", None, None);
	assert_eq!(status, 200, "{body}");
	let board: serde_json::Value = serde_json::from_str(&body).unwrap();
	assert_eq!(board["colors"], serde_json::json!({ "o/r": { "t:bug": "ededed", "won't fix": "ededed" } }));
}
