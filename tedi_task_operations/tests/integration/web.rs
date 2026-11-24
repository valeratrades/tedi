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

const BOARD: &str = "[board]\nworkflows = { kanban = \"preset:kanban\" }\nviews = [{ name = \"Triage\", query = \"is:open\", layout = \"board\", group_by = \"state:kanban\", sort = \"rank\", shown = [] }]\n\n[board.scopes.Main]\nrepos = [\"o/*\"]\n";

/// The team's config at `o/config`, as it stands after each commit.
const CONFIG_REPO: &str = "o/config";

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
	serde_json::json!({ "Keep": { "Workspace": { "name": name, "tiles": { band: layout } } } })
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
fn a_view_kept_under_a_new_name_keeps_its_columns() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	let arranged = |band: &str, layout: &str| serde_json::json!({ "Arranged": { "dock": { "Columns": { "view": "Triage", "by": "state:kanban" } }, "band": band, "layout": layout } });
	assert_eq!(edit(port, me, arranged("Xl", "tabbed")).0, 200);
	assert_eq!(edit(port, me, arranged("Sm", "narrow")).0, 200);
	let tints = serde_json::json!({ "triage": "red" });
	assert_eq!(edit(port, me, serde_json::json!({ "Tinted": { "view": "Triage", "by": "state:kanban", "tints": tints } })).0, 200);

	assert_eq!(edit(port, me, serde_json::json!({ "Locked": { "view": "Triage", "locked": true } })).0, 200);

	let (status, body) = edit(
		port,
		me,
		serde_json::json!({ "Keep": { "Filter": { "view": view("main", "is:open"), "arranged_as": "Triage" } } }),
	);
	assert_eq!(status, 200, "{body}");
	let on_screen = &library(port, me)["on_screen"];
	assert_eq!(on_screen["columns"]["main"]["state:kanban"], serde_json::json!({ "Sm": "narrow", "Xl": "tabbed" }));
	assert_eq!(on_screen["tints"]["main"]["state:kanban"], tints);
	assert_eq!(on_screen["locked"], serde_json::json!(["Triage", "main"]));

	assert_eq!(
		edit(
			port,
			me,
			serde_json::json!({ "Arranged": { "dock": { "Columns": { "view": "main", "by": "state:kanban" } }, "band": "Xl", "layout": "split" } })
		)
		.0,
		200
	);
	assert_eq!(
		library(port, me)["on_screen"]["columns"]["Triage"]["state:kanban"]["Xl"],
		"tabbed",
		"each is its own from then on"
	);
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

fn team_file(triage_sort: &str) -> String {
	format!(
		"# the team's board\n\n[[board.views]]\nname = \"Triage\"\nquery = \"is:open\"\nlayout = \"board\"\ngroup_by = \"state:kanban\"\nsort = \"{triage_sort}\"\nshown = []\n\n[[board.views]]\nname = \"PRs\"\nquery = \"is:pr\"\nlayout = \"list\"\ngroup_by = \"state:kanban\"\nsort = \"rank\"\nshown = []\n"
	)
}

/// The board as deployed from `team_file(triage_sort)`, its history at `CONFIG_REPO`.
fn sourced(triage_sort: &str) -> String {
	let views = team_file(triage_sort).replace("# the team's board\n\n", "");
	format!("[board]\nworkflows = {{ kanban = \"preset:kanban\" }}\nsource = {{ repo = \"{CONFIG_REPO}\", path = \"board.toml\", branch = \"main\" }}\n\n{views}")
}

fn view(name: &str, query: &str) -> serde_json::Value {
	serde_json::json!({ "name": name, "query": query, "layout": "board", "group_by": "state:kanban", "sort": "importance", "shown": [] })
}

fn catalog(port: u16, user: &str) -> serde_json::Value {
	let (status, body) = call(port, "GET", "/api/catalog", Some(user), None);
	assert_eq!(status, 200, "{body}");
	serde_json::from_str(&body).unwrap()
}

fn entry<'a>(catalog: &'a serde_json::Value, owner: serde_json::Value, item: serde_json::Value) -> &'a serde_json::Value {
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
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("rank"));
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "carol", &team_file("importance"));
	let gate = ["--viewer-header", "X-Auth-User"];
	let (server, port) = serve(&ctx, &sourced("importance"), &gate);
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
	assert_eq!(team["history"][1]["changes"], serde_json::json!([{ "sign": "Changed", "text": "sort rank → importance" }]));
	assert_eq!(team["forks"], serde_json::json!([{ "viewer": "alice", "name": "Triage (mine)", "ahead": 0, "behind": 0 }]));

	let narrowed = view("Triage (mine)", "is:open repo:o/r");
	assert_eq!(
		edit(port, me, serde_json::json!({ "Keep": { "Filter": { "view": narrowed, "arranged_as": "Triage (mine)" } } })).0,
		200
	);
	assert_eq!(
		edit(port, me, serde_json::json!({ "Keep": { "Filter": { "view": narrowed, "arranged_as": "Triage (mine)" } } })).0,
		200,
		"saved unchanged"
	);
	assert_eq!(library(port, me)["forks"][0]["upstream"]["ahead"], 1, "only a save that changed it counts");

	drop(server);
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("hours"));
	let (_server, port) = serve(&ctx, &sourced("importance"), &gate);
	let catalog = catalog(port, "alice");
	assert_eq!(entry(&catalog, serde_json::json!({ "Viewer": "alice" }), mine.clone())["behind"], 1);
	assert_eq!(entry(&catalog, serde_json::json!("Team"), triage)["history"].as_array().unwrap().len(), 3);
	assert_ne!(edit(port, me, serde_json::json!({ "Pull": mine })).0, 200, "pulling would drop the save it is ahead by");
}

#[test]
fn a_share_link_pins_what_was_shared() {
	let ctx = TestContext::build();
	let (_server, port) = serve(&ctx, BOARD, &["--viewer-header", "X-Auth-User"]);
	let (alice, bob) = (Some("alice"), Some("bob"));
	let desk = r#"{"version":2,"grid":{"cells":[{"group":{"id":0,"tabs":["view:Mine"],"active":0},"x":0,"y":0,"w":48,"h":24,"desired_w":null,"desired_x":null,"desired_h":null,"rank":0}],"state":"Settled","next_group_id":1}}"#;
	assert_eq!(
		edit(
			port,
			alice,
			serde_json::json!({ "Keep": { "Filter": { "view": view("Mine", "is:open"), "arranged_as": "Mine" } } })
		)
		.0,
		200
	);
	assert_eq!(edit(port, alice, keep_workspace("desk", "Xl", desk)).0, 200);

	let share = |user| {
		let (status, body) = call(
			port,
			"POST",
			"/api/share",
			user,
			Some(serde_json::json!({ "item": { "Workspace": "desk" }, "bands": ["Xl"], "scope": "Main" })),
		);
		assert_eq!(status, 200, "{body}");
		serde_json::from_str::<String>(&body).unwrap()
	};
	let link = share(alice);
	assert_eq!(link, "/l/ws/alice/desk@1?bands=xl&scope=Main");
	assert_eq!(
		edit(
			port,
			alice,
			serde_json::json!({ "Keep": { "Filter": { "view": view("Mine", "is:closed"), "arranged_as": "Mine" } } })
		)
		.0,
		200
	);

	let (status, body) = call(port, "POST", "/api/shared", bob, Some(serde_json::json!({ "link": link })));
	assert_eq!(status, 200, "{body}");
	let shared: serde_json::Value = serde_json::from_str(&body).unwrap();
	assert_eq!(shared["construction"], serde_json::json!({ "Workspace": { "Xl": desk } }));
	assert_eq!(
		shared["carried"],
		serde_json::json!({ "Mine": [1, view("Mine", "is:open")] }),
		"the view as it stood when the link was copied"
	);
	assert_eq!(library(port, bob)["received"], serde_json::json!([link]));

	let (status, body) = edit(port, bob, serde_json::json!({ "Fork": { "from": { "Link": link }, "name": "desk" } }));
	assert_eq!(status, 200, "{body}");
	let forked = library(port, bob);
	assert_eq!(forked["workspaces"]["desk"], serde_json::json!({ "Xl": desk }));
	assert_eq!(forked["filters"]["Mine"], view("Mine", "is:open"));
	assert_eq!(
		forked["forks"],
		serde_json::json!([
			{ "item": { "View": "Mine" }, "upstream": { "owner": { "Viewer": "alice" }, "name": "Mine", "version": 1, "ahead": 0 } },
			{ "item": { "Workspace": "desk" }, "upstream": { "owner": { "Viewer": "alice" }, "name": "desk", "version": 1, "ahead": 0 } },
		])
	);

	assert_eq!(share(alice), "/l/ws/alice/desk@2?bands=xl&scope=Main", "the view it carries changed since");
	assert_eq!(
		entry(&catalog(port, "bob"), serde_json::json!({ "Viewer": "bob" }), serde_json::json!({ "View": "Mine" }))["behind"],
		1
	);
}

#[test]
fn a_proposal_is_a_pull_request_replacing_only_that_object() {
	let ctx = TestContext::build();
	ctx.remote_file_commit(CONFIG_REPO, "board.toml", "bob", &team_file("importance"));
	let (_server, port) = serve(&ctx, &sourced("importance"), &["--viewer-header", "X-Auth-User"]);
	let me = Some("alice");
	assert_eq!(
		edit(port, me, serde_json::json!({ "Fork": { "from": { "Team": { "View": "Triage" } }, "name": "Triage (mine)" } })).0,
		200
	);
	assert_eq!(
		edit(
			port,
			me,
			serde_json::json!({ "Keep": { "Filter": { "view": view("Triage (mine)", "is:open repo:o/r"), "arranged_as": "Triage (mine)" } } })
		)
		.0,
		200
	);

	let (status, body) = call(
		port,
		"POST",
		"/api/propose",
		me,
		Some(serde_json::json!({ "item": { "View": "Triage (mine)" }, "onto": { "Update": "Triage" }, "message": "narrow it" })),
	);
	assert_eq!(status, 200, "{body}");
	assert_eq!(serde_json::from_str::<String>(&body).unwrap(), format!("https://github.com/{CONFIG_REPO}/pull/1"));
	let [proposal] = ctx.remote_proposals().try_into().unwrap();
	assert_eq!((proposal.base.as_str(), proposal.path.as_str()), ("main", "board.toml"));
	assert_eq!(
		proposal.content,
		team_file("importance").replace("query = \"is:open\"\nlayout = \"board\"", "query = \"is:open repo:o/r\"\nlayout = \"board\"")
	);
	assert!(proposal.body.starts_with("narrow it\n\n```diff\n+ repo:o/r\n```"), "{}", proposal.body);
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
	assert_eq!(edit(port, me, serde_json::json!({ "Scope": "Main / core / deep" })).0, 200);

	let (status, body) = edit(port, me, serde_json::json!({ "Rename": { "shelf": "Scopes", "from": "Main / core", "to": "core" } }));
	assert_eq!(status, 200, "{body}");
	let held = self::library(port, me);
	assert_eq!(
		held["scopes"].as_object().unwrap().keys().collect::<Vec<_>>(),
		["core", "core / deep"],
		"what it held moves with it"
	);
	assert_eq!(held["scope"], "core / deep");

	assert_eq!(edit(port, me, serde_json::json!({ "Drop": { "shelf": "Scopes", "name": "core" } })).0, 200);
	let library = self::library(port, me);
	assert_eq!(library["scopes"], serde_json::json!({}));
	assert_eq!(library["scope"], serde_json::Value::Null);
}
