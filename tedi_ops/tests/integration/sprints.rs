//! Integration tests for milestone embedded issue operations.
//!
//! Tests the full `milestones edit` cycle: expand shorthand refs from local issue files,
//! present to editor, sync changes back. Uses `--mock` to skip GitHub API.

use std::{
	io::Write,
	path::{Path, PathBuf},
	process::Command,
};

use crate::common::{RunOutput, Seed, TestContext, drain_pipe, get_binary_path, parse_virtual, set_nonblocking};

const ENV_MOCK_MILESTONE: &str = concat!("tedi", "_MOCK_MILESTONE");
const ENV_MOCK_PIPE: &str = concat!("tedi", "_MOCK_PIPE");

type EditFn = Box<dyn FnOnce(&Path)>;

impl TestContext {
	/// Run `milestones edit` in mock mode against a milestone file.
	///
	/// Writes `milestone_content` to a file, runs the binary with `--mock milestones edit 1d`.
	/// The editor mock (pipe) fires immediately — so the expanded content is what gets "saved".
	/// Returns the RunOutput + the resulting milestone file content.
	fn milestone_edit_no_changes(&self, milestone_content: &str) -> (RunOutput, String) {
		self.milestone_edit_impl(milestone_content, &["--mock", "sprints", "edit", "1d"], None)
	}

	/// Run `milestones edit --offline` in mock mode, modifying the temp file before signaling the pipe.
	fn milestone_edit_with_changes(&self, milestone_content: &str, edit_fn: impl FnOnce(&Path) + 'static) -> (RunOutput, String) {
		self.milestone_edit_impl(milestone_content, &["--mock", "--offline", "sprints", "edit", "1d"], Some(Box::new(edit_fn) as EditFn))
	}

	fn milestone_edit_impl(&self, milestone_content: &str, args: &[&str], edit_fn: Option<EditFn>) -> (RunOutput, String) {
		self.set_issues_dir_override();
		let milestone_path = self.xdg.inner.root.join("mock_milestone.md");
		std::fs::write(&milestone_path, milestone_content).unwrap();

		let mut cmd = Command::new(get_binary_path());
		cmd.args(args);
		cmd.env(ENV_MOCK_MILESTONE, &milestone_path);
		let out = self.drive_editor_session(cmd, edit_fn);
		let result = std::fs::read_to_string(&milestone_path).unwrap_or_default();
		(out, result)
	}

	/// Run `sprints edit urgent --offline`, modifying the temp file before signaling the pipe.
	/// `--mock` pins `current_user` to `mock_user` so materialized virtual issues land deterministically.
	fn urgent_edit(&self, edit_fn: impl FnOnce(&Path) + 'static) -> RunOutput {
		self.set_issues_dir_override();
		let mut cmd = Command::new(get_binary_path());
		cmd.args(["--mock", "--offline", "sprints", "edit", "urgent", "--offline"]);
		self.drive_editor_session(cmd, Some(Box::new(edit_fn) as EditFn))
	}

	/// Spawn the binary with the mock-editor pipe attached, apply `edit_fn` to the tmp file
	/// once the binary is parked in the editor, then signal the pipe and collect output.
	fn drive_editor_session(&self, mut cmd: Command, edit_fn: Option<EditFn>) -> RunOutput {
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env(concat!("tedi", "__GITHUB_TOKEN"), "test_token");
		cmd.env(ENV_MOCK_PIPE, &self.pipe_path);
		for (key, value) in self.xdg.env_vars() {
			cmd.env(key, value);
		}
		cmd.stdout(std::process::Stdio::piped());
		cmd.stderr(std::process::Stdio::piped());

		let mut child = cmd.spawn().unwrap();
		let mut stdout = child.stdout.take().unwrap();
		let mut stderr = child.stderr.take().unwrap();
		set_nonblocking(&stdout);
		set_nonblocking(&stderr);

		let pipe_path = self.pipe_path.clone();
		let mut stdout_buf = Vec::new();
		let mut stderr_buf = Vec::new();
		let mut edit_fn = edit_fn;
		let mut signaled = false;

		while child.try_wait().unwrap().is_none() {
			drain_pipe(&mut stdout, &mut stdout_buf);
			drain_pipe(&mut stderr, &mut stderr_buf);

			if !signaled {
				std::thread::sleep(std::time::Duration::from_millis(100));
				let stderr_so_far = String::from_utf8_lossy(&stderr_buf);

				// Apply edit once the binary has written the tmp file and is waiting on pipe
				if edit_fn.is_some()
					&& stderr_so_far.contains("Waiting for signal on pipe")
					&& let Some(tmp_path) = parse_tmp_path(&stderr_so_far)
				{
					(edit_fn.take().unwrap())(&tmp_path);
				}

				// Signal the pipe (no-edit case signals immediately; edit case signals after editing)
				if edit_fn.is_none() {
					#[cfg(unix)]
					{
						use std::os::unix::fs::OpenOptionsExt;
						if let Ok(mut pipe) = std::fs::OpenOptions::new().write(true).custom_flags(libc::O_NONBLOCK).open(&pipe_path)
							&& pipe.write_all(b"x").is_ok()
						{
							signaled = true;
						}
					}
				}
			}
			std::thread::sleep(std::time::Duration::from_millis(10));
		}

		drain_pipe(&mut stdout, &mut stdout_buf);
		drain_pipe(&mut stderr, &mut stderr_buf);
		child.wait().unwrap();

		RunOutput {
			status: child.try_wait().unwrap().unwrap(),
			stdout: String::from_utf8_lossy(&stdout_buf).into_owned(),
			stderr: String::from_utf8_lossy(&stderr_buf).into_owned(),
		}
	}
}

/// Extract the milestone tmp_path from stderr output.
/// Looks for `[milestone] tmp_path: <path>`.
fn parse_tmp_path(stderr: &str) -> Option<PathBuf> {
	for line in stderr.lines() {
		if let Some(rest) = line.strip_prefix("[milestone] tmp_path: ") {
			return Some(PathBuf::from(rest.trim()));
		}
	}
	None
}

/// Shorthand ref at root level gets expanded to full embedded issue view.
#[tokio::test]
async fn test_expand_shorthand_ref() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual(
		"- [ ] My Issue <!-- @mock_user https://github.com/o/r/issues/10 -->\n\
		 \tbody\n\
		 \n\
		 \t# Blockers\n\
		 \t- task alpha\n\
		 \t- task beta\n",
	);
	ctx.local(&vi, Some(Seed::new(0))).await;

	let (out, result) = ctx.milestone_edit_no_changes("# Sprint\n\n- o/r#10\n\nFooter");

	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("No changes"), "expected 'No changes' in stdout: {}", out.stdout);
	insta::assert_snapshot!(result, @"
	# Sprint

	- o/r#10

	Footer
	");
}

/// Child issue (nested under a parent dir) can be found and expanded.
#[tokio::test]
async fn test_expand_child_issue() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Create parent issue (dir-format, has children)
	let parent_vi = parse_virtual(
		"- [ ] Parent Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\
		 \tparent body\n\
		 \n\
		 \t## Sub-issues\n\
		 \t- [ ] Child Issue <!-- @mock_user https://github.com/o/r/issues/2 -->\n",
	);
	ctx.local(&parent_vi, Some(Seed::new(0))).await;

	// The milestone references the child issue
	let (out, result) = ctx.milestone_edit_no_changes("- o/r#2");

	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("No changes"), "expected 'No changes', got stdout: {}\nstderr: {}", out.stdout, out.stderr);
	insta::assert_snapshot!(result, @"- o/r#2");
}

/// Adding blockers to an issue that has none should sync them to the issue file.
#[tokio::test]
async fn test_milestone_edit_adds_blockers() {
	use crate::common::are_you_sure::read_issue_file;

	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	// Issue starts with NO blockers
	let vi = parse_virtual("- [ ] Empty Issue <!-- @mock_user https://github.com/o/r/issues/50 -->\n\tjust a body\n");
	ctx.local(&vi, Some(Seed::new(0))).await;

	let (out, result_milestone) = ctx.milestone_edit_with_changes("- o/r#50", |tmp_path| {
		let content = std::fs::read_to_string(tmp_path).unwrap();
		let new_content = content.trim_end().to_string() + "\n\t# Blockers\n\t- todo\n";
		std::fs::write(tmp_path, new_content).unwrap();
	});

	assert!(out.status.success(), "milestones edit should succeed. stderr: {}", out.stderr);

	// The milestone description should be collapsed to a bare link in a list
	insta::assert_snapshot!(result_milestone, @"- https://github.com/o/r/issues/50");

	// The issue file should now have the blockers
	ctx.set_issues_dir_override();
	let issue_path = tedi_ops::local::Local::find_by_number(tedi_ops::RepoInfo::new("o", "r"), 50, tedi_ops::local::FsReader).expect("issue #50 should still exist");
	let issue_content = read_issue_file(&issue_path);
	insta::assert_snapshot!(issue_content, @"
	- [ ] Empty Issue <!-- @mock_user https://github.com/o/r/issues/50 -->
	  just a body

	  # Blockers
	  - todo
	");
}

/// `sprints edit urgent`: adding issues and plain-text items persists to
/// `$XDG_DATA_HOME/tedi/issues/urgent.md`, and the file survives selection polling.
#[tokio::test]
async fn test_urgent_edit_persists() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [ ] My Issue <!-- @mock_user https://github.com/o/r/issues/10 -->\n\tbody\n");
	ctx.local(&vi, Some(Seed::new(0))).await;

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- equilibre people research\n  - linkedin token\n- o/r#10\n- pay for Tokyo server\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("Updated urgent sprint"), "stdout: {}", out.stdout);

	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"
	- equilibre people research
	  - linkedin token
	- https://github.com/o/r/issues/10
	- pay for Tokyo server
	");

	// eww polls `sprints selected current` every 0.5s — it must never eat the urgent file
	let poll = ctx.run(&["--offline", "sprints", "selected", "current"]);
	assert!(ctx.xdg.data_exists("issues/urgent.md"), "urgent.md was deleted by selection polling");
	assert!(poll.status.success(), "stderr: {}", poll.stderr);
}

/// A text-only urgent file (no issue links at all) must survive selection polling —
/// regression test for the auto-clear wiping freshly saved urgent content.
#[tokio::test]
async fn test_urgent_text_only_survives_selection_polling() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- pay for Tokyo server\n- extend tba -u\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);

	let _ = ctx.run(&["--offline", "sprints", "selected", "current"]);
	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"
	- pay for Tokyo server
	- extend tba -u
	");
}

/// Once every issue in urgent is closed, selection polling prunes the closed links
/// but keeps plain-text items.
#[tokio::test]
async fn test_urgent_prunes_closed_issues() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [x] Done Issue <!-- @mock_user https://github.com/o/r/issues/60 -->\n\tbody\n");
	ctx.local(&vi, Some(Seed::new(0))).await;
	ctx.xdg.write_data("issues/urgent.md", "- https://github.com/o/r/issues/60\n- pay for Tokyo server\n");

	let _ = ctx.run(&["--offline", "sprints", "selected", "current"]);
	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"- pay for Tokyo server");
}

/// An urgent file holding nothing but closed issues gets deleted by selection polling.
#[tokio::test]
async fn test_urgent_deleted_when_all_issues_closed() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [x] Done Issue <!-- @mock_user https://github.com/o/r/issues/60 -->\n\tbody\n");
	ctx.local(&vi, Some(Seed::new(0))).await;
	ctx.xdg.write_data("issues/urgent.md", "- https://github.com/o/r/issues/60\n");

	let _ = ctx.run(&["--offline", "sprints", "selected", "current"]);
	assert!(!ctx.xdg.data_exists("issues/urgent.md"), "fully-closed urgent file should be deleted");
}

/// While an edit session holds the urgent lock, selection polling must not clean up the file.
#[tokio::test]
async fn test_urgent_cleanup_deferred_during_edit() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [x] Done Issue <!-- @mock_user https://github.com/o/r/issues/60 -->\n\tbody\n");
	ctx.local(&vi, Some(Seed::new(0))).await;
	ctx.xdg.write_data("issues/urgent.md", "- https://github.com/o/r/issues/60\n");

	let envs: Vec<(&'static str, PathBuf)> = ctx.xdg.env_vars();
	let urgent_host = ctx.xdg.data_dir().join("issues/urgent.md");
	let out = ctx.urgent_edit(move |tmp_path| {
		// the binary is parked in the editor holding the lock — polling now would have
		// deleted the fully-closed urgent file if cleanup didn't defer to the lock
		let mut cmd = Command::new(get_binary_path());
		cmd.args(["--offline", "sprints", "selected", "current"]);
		cmd.env("__IS_INTEGRATION_TEST", "1");
		for (key, value) in &envs {
			cmd.env(key, value);
		}
		cmd.output().unwrap();
		assert!(urgent_host.exists(), "urgent.md was cleaned up mid-edit");

		std::fs::write(tmp_path, "- kept through the edit\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);
	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"- kept through the edit");
}

/// A new `- [ ] task` in the urgent list becomes a numbered local virtual issue,
/// and the urgent list stores its link.
#[tokio::test]
async fn test_urgent_new_task_materializes_virtual_issue() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- [ ] some new task\n  # Blockers\n  - first step\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("Created virtual issue mock_user/virtual#1"), "stdout: {}", out.stdout);

	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"- https://github.com/mock_user/virtual/issues/1");

	ctx.set_issues_dir_override();
	let path = tedi_ops::local::Local::find_by_number(tedi_ops::RepoInfo::new("mock_user", "virtual"), 1, tedi_ops::local::FsReader).expect("materialized issue must be findable by number");
	insta::assert_snapshot!(std::fs::read_to_string(&path).unwrap(), @"
	- [ ] some new task <!-- virtual https://github.com/mock_user/virtual/issues/1 -->
	  # Blockers
	  - first step
	");

	let meta: serde_json::Value = serde_json::from_str(&ctx.xdg.read_data("issues/mock_user/virtual/.meta.json")).unwrap();
	assert_eq!(meta["virtual_project"], serde_json::json!(true));
	assert_eq!(meta["next_virtual_issue_number"], serde_json::json!(2));

	// selection plumbing: the materialized issue's blocker is the current selection
	let poll = ctx.run(&["--offline", "sprints", "selected", "current"]);
	assert!(poll.status.success(), "stderr: {}", poll.stderr);
	assert!(poll.stdout.contains("first step"), "stdout: {}", poll.stdout);
}

/// A materialized virtual issue re-expands on the next urgent edit, and blocker
/// edits sync back to its file.
#[tokio::test]
async fn test_urgent_virtual_issue_reexpands_and_syncs_blockers() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- [ ] some new task\n  # Blockers\n  - first step\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);

	let out = ctx.urgent_edit(|tmp_path| {
		let content = std::fs::read_to_string(tmp_path).unwrap();
		assert!(
			content.contains("<!-- virtual https://github.com/mock_user/virtual/issues/1 -->"),
			"second edit must open on the expanded virtual issue, got:\n{content}"
		);
		std::fs::write(tmp_path, content.trim_end().to_string() + "\n  - second step\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);

	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"- https://github.com/mock_user/virtual/issues/1");

	ctx.set_issues_dir_override();
	let path = tedi_ops::local::Local::find_by_number(tedi_ops::RepoInfo::new("mock_user", "virtual"), 1, tedi_ops::local::FsReader).expect("issue #1 should still exist");
	insta::assert_snapshot!(std::fs::read_to_string(&path).unwrap(), @"
	- [ ] some new task <!-- virtual https://github.com/mock_user/virtual/issues/1 -->
	  # Blockers
	  - first step
	  - second step
	");
}

/// A new `- [ ] task` added to a milestone materializes the same way.
#[tokio::test]
async fn test_milestone_edit_materializes_virtual_issue() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let (out, result_milestone) = ctx.milestone_edit_with_changes("# Sprint", |tmp_path| {
		let content = std::fs::read_to_string(tmp_path).unwrap();
		std::fs::write(tmp_path, content.trim_end().to_string() + "\n\n- [ ] milestone task\n  # Blockers\n  - step one\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);

	insta::assert_snapshot!(result_milestone, @"
	# Sprint

	- https://github.com/mock_user/virtual/issues/1
	");

	ctx.set_issues_dir_override();
	let path = tedi_ops::local::Local::find_by_number(tedi_ops::RepoInfo::new("mock_user", "virtual"), 1, tedi_ops::local::FsReader).expect("materialized issue must be findable by number");
	insta::assert_snapshot!(std::fs::read_to_string(&path).unwrap(), @"
	- [ ] milestone task <!-- virtual https://github.com/mock_user/virtual/issues/1 -->
	  # Blockers
	  - step one
	");
}

/// Category headers (with an issue ref among children) and single-word childless
/// items stay plain text — no virtual project gets created for them.
#[tokio::test]
async fn test_urgent_edit_skips_category_headers_and_half_typed() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let vi = parse_virtual("- [ ] My Issue <!-- @mock_user https://github.com/o/r/issues/10 -->\n\tbody\n");
	ctx.local(&vi, Some(Seed::new(0))).await;

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- [ ] discretionary_engine\n  - [ ] o/r#10\n- [ ] halftyped\n").unwrap();
	});
	assert!(out.status.success(), "stderr: {}", out.stderr);

	assert!(!ctx.xdg.data_exists("issues/mock_user/virtual"), "no virtual project should be created for skipped items");
	insta::assert_snapshot!(ctx.xdg.read_data("issues/urgent.md"), @"
	- [ ] discretionary_engine
	  - https://github.com/o/r/issues/10

	- [ ] halftyped
	");
}

/// Urgent is local-only: milestone refs are rejected and nothing is saved.
#[tokio::test]
async fn test_urgent_edit_rejects_milestone_refs() {
	let ctx = TestContext::build_with_preexisting_state_unsafe("");

	let out = ctx.urgent_edit(|tmp_path| {
		std::fs::write(tmp_path, "- https://github.com/o/r/milestone/3\n").unwrap();
	});
	assert!(!out.status.success(), "expected failure, stdout: {}", out.stdout);
	assert!(out.stderr.contains("cannot contain milestone refs"), "stderr: {}", out.stderr);
	assert!(!ctx.xdg.data_exists("issues/urgent.md"), "rejected edit must not be saved");
}
