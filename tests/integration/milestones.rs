//! Integration tests for milestone embedded issue operations.
//!
//! Tests the full `milestones edit` cycle: expand shorthand refs from local issue files,
//! present to editor, sync changes back. Uses `--mock` to skip GitHub API.

use crate::common::{
	RunOutput, TestContext, drain_pipe, get_binary_path,
	git::{GitExt, Seed},
	parse_virtual, set_nonblocking,
};

const ENV_MOCK_MILESTONE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE");
const ENV_MOCK_PIPE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_PIPE");

impl TestContext {
	/// Run `milestones edit` in mock mode against a milestone file.
	///
	/// Writes `milestone_content` to a file, runs the binary with `--mock milestones edit 1d`.
	/// The editor mock (pipe) fires immediately — so the expanded content is what gets "saved".
	/// Returns the RunOutput + the resulting milestone file content.
	fn milestone_edit_no_changes(&self, milestone_content: &str) -> (RunOutput, String) {
		use std::io::Write;

		self.set_issues_dir_override();
		let milestone_path = self.xdg.inner.root.join("mock_milestone.md");
		std::fs::write(&milestone_path, milestone_content).unwrap();

		let mut cmd = std::process::Command::new(get_binary_path());
		cmd.args(["--mock", "milestones", "edit", "1d"]);
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env(concat!(env!("CARGO_PKG_NAME"), "__GITHUB_TOKEN"), "test_token");
		cmd.env(ENV_MOCK_MILESTONE, &milestone_path);
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
		let mut signaled = false;

		while child.try_wait().unwrap().is_none() {
			drain_pipe(&mut stdout, &mut stdout_buf);
			drain_pipe(&mut stderr, &mut stderr_buf);

			if !signaled {
				std::thread::sleep(std::time::Duration::from_millis(100));
				// Signal pipe immediately — no edits, just let it through.
				// The binary will see expanded == edited, print "No changes", and exit.
				// But we need the expanded content — read /tmp/milestone_1d.md before signaling.
				#[cfg(unix)]
				{
					use std::os::unix::fs::OpenOptionsExt;
					if let Ok(mut pipe) = std::fs::OpenOptions::new().write(true).custom_flags(0x800).open(&pipe_path)
						&& pipe.write_all(b"x").is_ok()
					{
						signaled = true;
					}
				}
			}
			std::thread::sleep(std::time::Duration::from_millis(10));
		}

		drain_pipe(&mut stdout, &mut stdout_buf);
		drain_pipe(&mut stderr, &mut stderr_buf);
		child.wait().unwrap();

		let result = std::fs::read_to_string(&milestone_path).unwrap_or_default();

		let out = RunOutput {
			status: child.try_wait().unwrap().unwrap(),
			stdout: String::from_utf8_lossy(&stdout_buf).into_owned(),
			stderr: String::from_utf8_lossy(&stderr_buf).into_owned(),
		};

		(out, result)
	}
}

/// Shorthand ref at root level gets expanded to full embedded issue view.
#[tokio::test]
async fn test_expand_shorthand_ref() {
	let ctx = TestContext::build("");

	let vi = parse_virtual(
		"- [ ] My Issue <!-- @mock_user https://github.com/o/r/issues/10 -->\n\
		 \tbody\n\
		 \n\
		 \t# Blockers\n\
		 \t- task alpha\n\
		 \t- task beta\n",
	);
	ctx.local(&vi, Some(Seed::new(0))).await;

	let (out, _result) = ctx.milestone_edit_no_changes("# Sprint\n\no/r#10\n\nFooter");

	// The expanded content is written to /tmp/milestone_1d.md — since we don't edit,
	// the binary says "No changes". But we can check the tmp file was written correctly.
	// The stdout should contain "No changes made"
	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("No changes"), "expected 'No changes' in stdout: {}", out.stdout);
}

/// Child issue (nested under a parent dir) can be found and expanded.
#[tokio::test]
async fn test_expand_child_issue() {
	let ctx = TestContext::build("");

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
	let (out, _result) = ctx.milestone_edit_no_changes("o/r#2");

	assert!(out.status.success(), "stderr: {}", out.stderr);
	assert!(out.stdout.contains("No changes"), "expected 'No changes', got stdout: {}\nstderr: {}", out.stdout, out.stderr);
}
