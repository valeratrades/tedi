//! `selected.md`: the kanban's started work as a text beside a planning sprint. Its sections are the home's buckets; a save
//! is sent there as moves, and what is stored here is the home's answer and what differs from it.

use std::path::Path;

use serde_json::{Value, json};

use crate::{
	common::{RunOutput, Seed, TestContext, get_binary_path, parse_virtual},
	sprints::EditFn,
	web::{BOARD, Server, call, serve},
};

/// One GitHub, the home's store, and the one `tedi` edits at.
struct World {
	home: TestContext,
	me: TestContext,
	server: Option<Server>,
	port: u16,
}

fn link(n: u64) -> String {
	format!("https://github.com/mock_user/app/issues/{n}")
}

/// `mock_user/app`'s #1 selected, #2 in progress, #3 in triage, all assigned; #4 is nobody's. `1d` holds #1, and Bottlenecks #3.
fn world() -> World {
	let mut home = TestContext::build();
	let me = TestContext::build();
	for (n, assigned) in [(1, "(@mock_user) "), (2, "(@mock_user) "), (3, "(@mock_user) "), (4, "")] {
		let issue = parse_virtual(&format!("- [ ] {assigned}Task {n} <!-- @mock_user {} -->\n  body {n}\n", link(n)));
		me.remote_in(("mock_user", "app").into(), &issue, Some(Seed::new(0)));
	}
	me.remote_labels("mock_user", "other", &[]);
	me.remote_labels("mock_user", "todos", &[]);
	me.remote_milestone("mock_user/todos", 1, "1d", &format!("- {}", link(1)), &[]);
	me.remote_milestone("mock_user/todos", 2, "Bottlenecks", &format!("- {}", link(3)), &[]);
	let mut state: Value = serde_json::from_str(&std::fs::read_to_string(&me.mock_state_path).unwrap()).unwrap();
	state["milestones"][0]["due_on"] = "2099-01-01T00:00:00Z".into();
	std::fs::write(&me.mock_state_path, state.to_string()).unwrap();

	home.mock_state_path = me.mock_state_path.clone();
	let config = format!("watch = [\"mock_user\"]\n\n{BOARD}");
	std::fs::write(home.xdg.inner.root.join("config/tedi.toml"), &config).unwrap();
	let pulled = home.run(&["--mock", "pull", "mock_user"]);
	assert!(pulled.status.success(), "{}", pulled.stderr);
	let held = json!({ "kanban": { "1": "selected", "2": "in_progress" } });
	std::fs::write(home.xdg.data_dir().join("issues/mock_user/app/.states.json"), held.to_string()).unwrap();
	let (server, port) = serve(&home, &config, &[]);

	let world = World {
		home,
		me,
		server: Some(server),
		port,
	};
	world.configure(&format!("http://127.0.0.1:{port}"));
	world
}

impl World {
	fn configure(&self, home: &str) {
		self.me.xdg.write_config(
			"config.toml",
			&format!("github_token = \"test_token\"\nhome = \"{home}\"\n\n[milestones]\nurl = \"mock_user/todos\"\nselected = \"kanban/selected\"\n"),
		);
	}

	/// `tedi {args} sprints edit 1d`, `edit` made in the directory its tabs opened in.
	fn edit(&self, args: &[&str], edit: impl FnOnce(&Path) + 'static) -> RunOutput {
		let mut cmd = std::process::Command::new(get_binary_path());
		cmd.arg("--mock").args(args).args(["sprints", "edit", "1d"]);
		cmd.env(concat!("tedi", "_MOCK_STATE"), &self.me.mock_state_path);
		let edit = Box::new(move |first: &Path| edit(first.parent().unwrap())) as EditFn;
		self.me.drive_editor_session(cmd, Some(edit))
	}

	/// The same, `selected.md` rewritten.
	fn rewrite(&self, args: &[&str], to: impl FnOnce(String) -> String + 'static) -> RunOutput {
		self.edit(args, |dir| {
			let path = dir.join("selected.md");
			let now = std::fs::read_to_string(&path).unwrap();
			std::fs::write(path, to(now)).unwrap();
		})
	}

	fn stored(&self) -> String {
		std::fs::read_to_string(self.me.xdg.data_dir().join("issues/__milestones__/mock_user/todos/selected.md")).unwrap()
	}

	/// `repo#n state` of every issue, as the home has it.
	fn held(&self) -> Vec<String> {
		let (status, body) = call(self.port, "POST", "/api/query", None, Some(json!({ "asked": "is:issue" })));
		assert_eq!(status, 200, "{body}");
		let answer: Value = serde_json::from_str(&body).unwrap();
		let mut held: Vec<String> = answer["hits"]
			.as_array()
			.unwrap()
			.iter()
			.map(|h| {
				let path: Vec<&str> = h["link"].as_str().unwrap().rsplit('/').collect();
				format!("{}#{} {}", path[2], path[0], h["states"]["kanban"].as_str().unwrap())
			})
			.collect();
		held.sort();
		held
	}

	fn move_there(port: u16, n: u64, to: &str) {
		let body = json!({ "link": link(n), "workflow": "kanban", "to": to, "seen": null });
		assert_eq!(call(port, "POST", "/api/move", None, Some(body)), (200, "\"Landed\"".into()));
	}
}

fn lines(text: &str) -> String {
	text.replace("https://github.com/mock_user/", "")
}

/// Each line as a bare link under its header: what a block collapses to.
fn bare(sections: &[(&str, &[u64])]) -> String {
	let section = |(header, issues): &(&str, &[u64])| {
		let lines: String = issues.iter().map(|n| format!("- {}\n", link(*n))).collect();
		if header.is_empty() { lines } else { format!("\n# {header}\n{lines}") }
	};
	sections.iter().map(section).collect()
}

#[test]
fn it_opens_beside_a_planning_sprint_and_its_bottlenecks() {
	let w = world();
	let seen = std::sync::Arc::new(std::sync::Mutex::new(String::new()));
	let told = seen.clone();
	let out = w.edit(&[], move |dir| {
		let mut names: Vec<String> = std::fs::read_dir(dir).unwrap().map(|e| e.unwrap().file_name().into_string().unwrap()).collect();
		names.sort();
		*told.lock().unwrap() = format!("{}\n---\n{}", names.join(" "), std::fs::read_to_string(dir.join("selected.md")).unwrap());
	});
	assert!(out.status.success(), "{}", out.stderr);
	insta::assert_snapshot!(format!("{}\n---\n{}", lines(&seen.lock().unwrap()), lines(&w.stored())), @"
	1d.md Bottlenecks.md selected.md
	---
	- [ ] (@mock_user) Task 1 <!-- @mock_user app/issues/1 --> <!--{{{3-->
	  body 1
	  <!--}}}1-->

	# in_progress

	- [ ] (@mock_user) Task 2 <!-- @mock_user app/issues/2 --> <!--{{{3-->
	  body 2
	  <!--}}}1-->

	# triage

	- [ ] (@mock_user) Task 3 <!-- @mock_user app/issues/3 --> <!--{{{3-->
	  body 3
	  <!--}}}1-->
	---
	- app/issues/1

	# in_progress

	- app/issues/2

	# triage

	- app/issues/3
	");
}

#[test]
fn a_line_under_another_header_moves_there() {
	let w = world();
	let out = w.rewrite(&[], |_| bare(&[("", &[2]), ("in_progress", &[1]), ("icebox", &[3])]));
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held(), ["app#1 in_progress", "app#2 selected", "app#3 icebox", "app#4 triage"]);
	assert_eq!(
		w.stored(),
		format!("{}\n", tedi_core::TaskView::parse(&bare(&[("", &[2]), ("in_progress", &[1]), ("icebox", &[3])])).serialize())
	);
}

#[test]
fn a_line_gone_goes_to_the_intake_and_one_there_already_stays() {
	let w = world();
	let out = w.rewrite(&[], |_| bare(&[("in_progress", &[2])]));
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held(), ["app#1 triage", "app#2 in_progress", "app#3 triage", "app#4 triage"]);
	assert!(out.stdout.contains("changes nothing") && out.stdout.contains(&link(3)), "{}", out.stdout);
	assert!(w.stored().contains(&link(3)), "it is where the home has it: {}", w.stored());
}

#[test]
fn a_link_added_is_moved_from_wherever_it_sits() {
	let w = world();
	let out = w.rewrite(&[], |now| format!("- mock_user/app#4\n{now}"));
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held()[3], "app#4 selected");
	assert!(w.stored().split("# in_progress").next().unwrap().contains(&link(4)), "{}", w.stored());
	let local = std::fs::read_to_string(w.me.xdg.data_dir().join("issues/mock_user/app/4_-_Task_4.md")).unwrap();
	assert!(local.contains("@mock_user) Task 4"), "what entering it edits is pulled: {local}");
}

#[test]
fn a_text_line_becomes_an_issue_once_online() {
	let w = world();
	assert!(w.edit(&[], |_| {}).status.success());

	let out = w.rewrite(&["--offline"], |now| format!("- [app] Write it down\n\n- [ ] And file it\n\n{now}"));
	assert!(out.status.success(), "{}", out.stderr);
	let offline = lines(&w.stored());
	assert_eq!(w.held().len(), 4);

	let out = w.edit(&[], |_| {});
	assert!(out.status.success(), "{}", out.stderr);
	insta::assert_snapshot!(format!("{offline}\n---\n{}\n---\n{}", lines(&w.stored()), w.held().join("\n")), @r"
	- \[app] Write it down

	- [ ] And file it

	- [ ] app/issues/1

	# in_progress

	- app/issues/2

	# triage

	- app/issues/3

	---
	- todos/issues/1
	- app/issues/5
	- app/issues/1

	# in_progress

	- app/issues/2

	# triage

	- app/issues/3

	---
	app#1 selected
	app#2 in_progress
	app#3 triage
	app#4 triage
	app#5 selected
	todos#1 selected
	");
}

#[test]
fn closed_in_a_buffer_is_no_removal() {
	let w = world();
	let out = w.edit(&[], |dir| {
		let sprint = std::fs::read_to_string(dir.join("1d.md")).unwrap();
		std::fs::write(dir.join("1d.md"), sprint.replace("- [ ] (@mock_user) Task 1", "- [x] (@mock_user) Task 1")).unwrap();
		std::fs::write(dir.join("selected.md"), bare(&[("in_progress", &[2]), ("triage", &[3])])).unwrap();
	});
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held()[0], "app#1 selected", "closing is the home's rule, on its pull");
	let (_, body) = call(w.port, "POST", "/api/refresh", None, Some(json!({})));
	assert_eq!(body, "null");
	let since = std::time::Instant::now();
	while w.held()[0] == "app#1 selected" {
		assert!(since.elapsed() < std::time::Duration::from_secs(30), "{:?}", w.held());
		std::thread::sleep(std::time::Duration::from_millis(50));
	}
}

#[test]
fn a_moved_issue_is_placed_again_where_it_went() {
	let w = world();
	let out = w.rewrite(&[], |now| {
		let (title, rest) = now.split_once(" <!--").unwrap();
		format!("{title} !mv other <!--{rest}")
	});
	assert!(out.status.success(), "{}", out.stderr);
	assert!(w.held().contains(&"other#1 selected".to_string()), "{:?}", w.held());
	assert!(!w.held().contains(&"app#1 triage".to_string()), "{:?}", w.held());
	assert!(w.stored().contains("mock_user/other/issues/1"), "{}", w.stored());
	assert!(w.me.stored_milestone("mock_user/todos", 1).contains("mock_user/other/issues/1"), "the sprint follows");
}

#[rstest::rstest]
#[case::milestone("- https://github.com/mock_user/todos/milestone/2\n")]
#[case::header("# someday\n- mock_user/app#4\n")]
#[case::twice("# in_review\n- mock_user/app#1\n")]
fn what_is_no_placement_is_not_saved(#[case] line: &'static str) {
	let w = world();
	let out = w.edit(&[], move |dir| {
		let add = |name: &str, line: &str| {
			let now = std::fs::read_to_string(dir.join(name)).unwrap();
			std::fs::write(dir.join(name), format!("{now}\n\n{line}")).unwrap();
		};
		add("selected.md", line);
		add("1d.md", "- mock_user/app#2\n");
	});
	assert!(!out.status.success(), "{}", out.stdout);
	assert_eq!(w.held(), ["app#1 selected", "app#2 in_progress", "app#3 triage", "app#4 triage"]);
	assert!(w.me.stored_milestone("mock_user/todos", 1).contains(&link(2)), "the sprint is");
}

#[test]
fn a_move_of_a_card_that_left_is_refused_and_the_rest_lands() {
	let w = world();
	let port = w.port;
	let out = w.rewrite(&[], move |_| {
		World::move_there(port, 1, "in_progress");
		bare(&[("in_progress", &[2]), ("icebox", &[1, 3])])
	});
	assert!(out.status.success(), "{}", out.stderr);
	assert!(out.stderr.contains("stays where the home has it"), "{}", out.stderr);
	assert_eq!(w.held(), ["app#1 in_progress", "app#2 in_progress", "app#3 icebox", "app#4 triage"]);
	insta::assert_snapshot!(lines(&w.stored()), @"
	# in_progress

	- app/issues/2
	- app/issues/1

	# icebox

	- app/issues/3
	");
}

#[test]
fn an_offline_edit_is_replayed_by_the_next_online_one() {
	let w = world();
	assert!(w.edit(&[], |_| {}).status.success());
	let out = w.rewrite(&["--offline"], |_| bare(&[("icebox", &[1]), ("in_progress", &[2]), ("triage", &[3])]));
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held()[0], "app#1 selected", "nothing left yet");
	assert!(w.stored().contains("# icebox"), "{}", w.stored());

	World::move_there(w.port, 2, "selected");
	let out = w.edit(&[], |_| {});
	assert!(out.status.success(), "{}", out.stderr);
	assert_eq!(w.held(), ["app#1 icebox", "app#2 selected", "app#3 triage", "app#4 triage"]);
	insta::assert_snapshot!(lines(&w.stored()), @"
	- app/issues/2

	# icebox

	- app/issues/1

	# triage

	- app/issues/3
	");
}

#[test]
fn one_of_its_two_files_absent_is_never_fetched() {
	let w = world();
	assert!(w.edit(&[], |_| {}).status.success());
	std::fs::write(w.me.xdg.data_dir().join("issues/__milestones__/mock_user/todos/selected.md"), "").unwrap();
	std::fs::remove_file(w.me.xdg.data_dir().join("issues/__milestones__/mock_user/todos/.selected.json")).unwrap();
	assert!(w.edit(&[], |_| {}).status.success());
	assert_eq!(w.held(), ["app#1 selected", "app#2 in_progress", "app#3 triage", "app#4 triage"]);
	assert!(w.stored().contains(&link(1)), "{}", w.stored());
}

#[test]
fn an_issue_open_in_two_tabs_is_edited_from_either() {
	let w = world();
	let out = w.rewrite(&[], |now| now.replace("body 1", "body 1, reworded"));
	assert!(out.status.success(), "{}", out.stderr);
	let local = || std::fs::read_to_string(w.me.xdg.data_dir().join("issues/mock_user/app/1_-_Task_1.md")).unwrap();
	assert!(local().contains("reworded"), "{}", local());
	let out = w.edit(&[], |dir| {
		let sprint = std::fs::read_to_string(dir.join("1d.md")).unwrap();
		std::fs::write(dir.join("1d.md"), sprint.replace("reworded", "twice")).unwrap();
	});
	assert!(out.status.success(), "{}", out.stderr);
	assert!(local().contains("body 1, twice"), "{}", local());
}

#[test]
fn an_unreachable_home_costs_the_tab_alone() {
	let mut w = world();
	assert!(w.edit(&[], |_| {}).status.success());
	drop(w.server.take());
	let out = w.edit(&[], |dir| {
		let sprint = std::fs::read_to_string(dir.join("1d.md")).unwrap();
		std::fs::write(dir.join("1d.md"), format!("{sprint}\n\n- mock_user/app#2\n")).unwrap();
		std::fs::write(dir.join("selected.md"), bare(&[("in_progress", &[1, 2]), ("triage", &[3])])).unwrap();
	});
	assert!(out.status.success(), "{}", out.stderr);
	assert!(w.me.stored_milestone("mock_user/todos", 1).contains(&link(2)));

	let (server, port) = serve(&w.home, &format!("watch = [\"mock_user\"]\n\n{BOARD}"), &[]);
	(w.server, w.port) = (Some(server), port);
	w.configure(&format!("http://127.0.0.1:{port}"));
	assert!(w.edit(&[], |_| {}).status.success());
	assert_eq!(w.held()[0], "app#1 in_progress", "sent once it is back");
}

#[test]
fn a_lapsed_session_says_to_sign_in_again() {
	use std::io::{Read as _, Write as _};
	let w = world();
	let gate = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
	w.configure(&format!("http://{}", gate.local_addr().unwrap()));
	let refusing = std::thread::spawn(move || {
		let (mut stream, _) = gate.accept().unwrap();
		_ = stream.read(&mut [0; 4096]).unwrap();
		stream.write_all(b"HTTP/1.1 401 Unauthorized\r\nContent-Length: 0\r\nConnection: close\r\n\r\n").unwrap();
	});
	let out = w.edit(&[], |dir| assert!(!dir.join("selected.md").exists()));
	refusing.join().unwrap();
	assert!(out.status.success(), "{}", out.stderr);
	assert!(out.stderr.contains("tedi login"), "{}", out.stderr);
}

#[test]
fn held_elsewhere_the_sprint_opens_alone() {
	let w = world();
	std::fs::create_dir_all(w.me.xdg.cache_dir()).unwrap();
	let held = std::fs::File::create(w.me.xdg.cache_dir().join("milestones_selected.lock")).unwrap();
	held.lock().unwrap();
	let out = w.edit(&[], |dir| assert!(!dir.join("selected.md").exists()));
	assert!(out.status.success(), "{}", out.stderr);
	assert!(out.stderr.contains("already being edited by"), "{}", out.stderr);
}

/// With a home, no workflow is held here: a move and a view are its answers.
#[test]
fn a_move_and_a_view_are_the_homes() {
	let w = world();
	assert!(w.edit(&[], |_| {}).status.success());
	let moved = w.me.run(&["--mock", "move", &link(4), "kanban/selected", "--seen", "kanban/triage"]);
	assert!(moved.status.success(), "{}", moved.stderr);
	assert_eq!(w.held()[3], "app#4 selected");
	assert!(!w.me.run(&["--mock", "move", &link(4), "kanban/done", "--seen", "kanban/triage"]).status.success());
	assert!(!w.me.xdg.data_dir().join("issues/mock_user/app/.states.json").exists());

	let viewed = w.me.xdg.inner.root.join("view.md");
	let mut cmd = std::process::Command::new(get_binary_path());
	cmd.args(["--mock", "view", "state:kanban/selected", "--group", "state:kanban"]);
	cmd.env(concat!("tedi", "_MOCK_STATE"), &w.me.mock_state_path);
	let copy = viewed.clone();
	let out = w.me.drive_editor_session(cmd, Some(Box::new(move |tmp: &Path| _ = std::fs::copy(tmp, copy).unwrap())));
	assert!(out.status.success(), "{}", out.stderr);
	let view = std::fs::read_to_string(viewed).unwrap();
	assert!(
		view.starts_with("# selected\n") && view.contains("Task 1") && view.contains("Task 4") && !view.contains("Task 2"),
		"{view}"
	);
}
