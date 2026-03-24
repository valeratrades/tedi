//! Integration tests for milestone embedded issue operations.
//!
//! Tests the full `milestones edit` cycle: expand shorthand refs from local issue files,
//! present to editor, sync changes back. Uses `--mock` to skip GitHub API.

use std::{io::Write, path::PathBuf, process::Command};

use crate::common::{RunOutput, Seed, TestContext, drain_pipe, get_binary_path, parse_virtual, set_nonblocking};

const ENV_MOCK_MILESTONE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_MILESTONE");
const ENV_MOCK_PIPE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_PIPE");

type EditFn = Box<dyn FnOnce(&std::path::Path)>;

impl TestContext {
	/// Run `milestones edit` in mock mode against a milestone file.
	///
	/// Writes `milestone_content` to a file, runs the binary with `--mock milestones edit 1d`.
	/// The editor mock (pipe) fires immediately — so the expanded content is what gets "saved".
	/// Returns the RunOutput + the resulting milestone file content.
	fn milestone_edit_no_changes(&self, milestone_content: &str) -> (RunOutput, String) {
		self.milestone_edit_impl(milestone_content, &["--mock", "milestones", "edit", "1d"], None)
	}

	/// Run `milestones edit --offline` in mock mode, modifying the temp file before signaling the pipe.
	fn milestone_edit_with_changes(&self, milestone_content: &str, edit_fn: impl FnOnce(&std::path::Path) + 'static) -> (RunOutput, String) {
		self.milestone_edit_impl(milestone_content, &["--mock", "--offline", "milestones", "edit", "1d"], Some(Box::new(edit_fn) as EditFn))
	}

	fn milestone_edit_impl(&self, milestone_content: &str, args: &[&str], edit_fn: Option<EditFn>) -> (RunOutput, String) {
		self.set_issues_dir_override();
		let milestone_path = self.xdg.inner.root.join("mock_milestone.md");
		std::fs::write(&milestone_path, milestone_content).unwrap();

		let mut cmd = Command::new(get_binary_path());
		cmd.args(args);
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

		let result = std::fs::read_to_string(&milestone_path).unwrap_or_default();

		let out = RunOutput {
			status: child.try_wait().unwrap().unwrap(),
			stdout: String::from_utf8_lossy(&stdout_buf).into_owned(),
			stderr: String::from_utf8_lossy(&stderr_buf).into_owned(),
		};

		(out, result)
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
	let issue_path = tedi::local::Local::find_by_number(tedi::RepoInfo::new("o", "r"), 50, tedi::local::FsReader).expect("issue #50 should still exist");
	let issue_content = read_issue_file(&issue_path);
	insta::assert_snapshot!(issue_content, @"
	- [ ] Empty Issue <!-- @mock_user https://github.com/o/r/issues/50 -->
	  just a body

	  # Blockers
	  - todo
	");
}
