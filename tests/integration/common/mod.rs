//! Shared test infrastructure for integration tests.
//!
//! Provides `TestContext` - a unified test context that handles:
//! - XDG directory setup with proper environment variables
//! - Running commands against the compiled binary
//! - Mock state management for Github API simulation
//! - Named pipe communication for editor simulation
//!
//! # Example
//!
//! ```ignore
//! let ctx = TestContext::new(r#"
//!     //- /data/blockers/test.md
//!     - task 1
//! "#);
//!
//! let (status, stdout, stderr) = ctx.run(&["blocker", "list"]);
//! assert!(status.success());
//! ```

pub mod git;

use std::{
	io::Write,
	path::{Path, PathBuf},
	process::{Command, ExitStatus},
	sync::OnceLock,
};

use v_fixtures::{Fixture, fs_standards::xdg::Xdg};

static BINARY_COMPILED: OnceLock<()> = OnceLock::new();

/// Compile the binary before running any tests
pub fn ensure_binary_compiled() {
	BINARY_COMPILED.get_or_init(|| {
		let status = Command::new("cargo").arg("build").status().expect("Failed to execute cargo build");

		if !status.success() {
			panic!("Failed to build binary");
		}
	});
}

fn get_binary_path() -> PathBuf {
	ensure_binary_compiled();

	let mut path = std::env::current_exe().unwrap();
	path.pop(); // Remove test binary name
	path.pop(); // Remove 'deps'
	path.push(env!("CARGO_PKG_NAME"));
	path
}

/// Unified test context for integration tests.
///
/// Combines functionality from the old `TodoTestContext` and `SyncTestContext`.
/// Handles XDG directory setup, command execution, and optional mock state.
pub struct TestContext {
	/// The Xdg wrapper managing temp directories
	pub xdg: Xdg,
	/// Path to mock Github state file (for sync tests)
	pub mock_state_path: PathBuf,
	/// Path to named pipe for editor simulation (for sync tests)
	pub pipe_path: PathBuf,
}

impl TestContext {
	/// Create a new test context from a fixture string.
	///
	/// Files in the fixture should use XDG category prefixes:
	/// - `/data/blockers/test.md` → `XDG_DATA_HOME/todo/blockers/test.md`
	/// - `/cache/current.txt` → `XDG_CACHE_HOME/todo/current.txt`
	/// - `/state/db.json` → `XDG_STATE_HOME/todo/db.json`
	///
	/// # Example
	///
	/// ```ignore
	/// let ctx = TestContext::new(r#"
	///     //- /data/blockers/test.md
	///     # Project
	///     - task 1
	/// "#);
	/// ```
	pub fn new(fixture_str: &str) -> Self {
		let fixture = Fixture::parse(fixture_str);
		let xdg = Xdg::new(fixture.write_to_tempdir(), env!("CARGO_PKG_NAME"));

		let mock_state_path = xdg.inner.root.join("mock_state.json");
		let pipe_path = xdg.inner.create_pipe("editor_pipe");

		Self { xdg, mock_state_path, pipe_path }
	}

	/// Run a command with proper XDG environment.
	///
	/// Returns (exit_status, stdout, stderr) for easy assertions.
	pub fn run(&self, args: &[&str]) -> (ExitStatus, String, String) {
		let mut cmd = Command::new(get_binary_path());
		cmd.args(args);
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env("TODO__GITHUB_TOKEN", "test_token");
		for (key, value) in self.xdg.env_vars() {
			cmd.env(key, value);
		}
		let output = cmd.output().unwrap();
		(
			output.status,
			String::from_utf8_lossy(&output.stdout).into_owned(),
			String::from_utf8_lossy(&output.stderr).into_owned(),
		)
	}

	/// Run `open` command with mock Github state and editor pipe.
	///
	/// Returns (exit_status, stdout, stderr).
	pub fn run_open(&self, issue_path: &Path) -> (ExitStatus, String, String) {
		self.open(issue_path).run()
	}

	/// Create an OpenBuilder for running the `open` command with various options.
	pub fn open<'a>(&'a self, issue_path: &'a Path) -> OpenBuilder<'a> {
		OpenBuilder {
			ctx: self,
			issue_path,
			extra_args: Vec::new(),
			edit_to: None,
		}
	}

	/// Read a file from the data directory.
	pub fn read(&self, relative_path: &str) -> String {
		self.xdg.read_data(relative_path.trim_start_matches('/'))
	}

	/// Write a file to the data directory.
	pub fn write(&self, relative_path: &str, content: &str) {
		self.xdg.write_data(relative_path.trim_start_matches('/'), content);
	}

	/// Check if a file exists in the data directory.
	pub fn data_exists(&self, relative_path: &str) -> bool {
		self.xdg.data_exists(relative_path.trim_start_matches('/'))
	}

	/// Read the current project from cache.
	pub fn read_current_project(&self) -> Option<String> {
		if self.xdg.cache_exists("current_project.txt") {
			Some(self.xdg.read_cache("current_project.txt"))
		} else {
			None
		}
	}

	/// Read a blocker file (path relative to blockers directory).
	pub fn read_blocker(&self, blocker_relative_path: &str) -> String {
		self.xdg.read_data(&format!("blockers/{blocker_relative_path}"))
	}

	/// Check if a blocker file exists.
	pub fn blocker_exists(&self, blocker_relative_path: &str) -> bool {
		self.xdg.data_exists(&format!("blockers/{blocker_relative_path}"))
	}

	/// Get the data directory path.
	pub fn data_dir(&self) -> PathBuf {
		self.xdg.data_dir()
	}

	/// Set up mock Github to return an issue.
	///
	/// The issue parameter should be a serde_json::Value representing the mock state.
	pub fn setup_mock_state(&self, state: &serde_json::Value) {
		std::fs::write(&self.mock_state_path, serde_json::to_string_pretty(state).unwrap()).unwrap();
	}

	/// Create an OpenUrlBuilder for running the `open` command with a Github URL.
	pub fn open_url(&self, owner: &str, repo: &str, number: u64) -> OpenUrlBuilder<'_> {
		let url = format!("https://github.com/{owner}/{repo}/issues/{number}");
		OpenUrlBuilder {
			ctx: self,
			url,
			extra_args: Vec::new(),
			edit_at_path: None,
		}
	}
}

/// Builder for running the `open` command with various options.
pub struct OpenBuilder<'a> {
	ctx: &'a TestContext,
	issue_path: &'a Path,
	extra_args: Vec<&'a str>,
	edit_to: Option<todo::Issue>,
}
impl<'a> OpenBuilder<'a> {
	/// Add extra CLI arguments.
	pub fn args(mut self, args: &[&'a str]) -> Self {
		self.extra_args.extend(args);
		self
	}

	/// Edit the file to this issue while "editor is open".
	pub fn edit(mut self, issue: &todo::Issue) -> Self {
		self.edit_to = Some(issue.clone());
		self
	}

	/// Run the command and return (exit_status, stdout, stderr).
	pub fn run(self) -> (ExitStatus, String, String) {
		let mut cmd = Command::new(get_binary_path());
		cmd.arg("--mock").arg("open");
		cmd.args(&self.extra_args);
		cmd.arg(self.issue_path.to_str().unwrap());
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env("TODO__GITHUB_TOKEN", "test_token");
		for (key, value) in self.ctx.xdg.env_vars() {
			cmd.env(key, value);
		}
		cmd.env("TODO_MOCK_STATE", &self.ctx.mock_state_path);
		cmd.env("TODO_MOCK_PIPE", &self.ctx.pipe_path);
		cmd.stdout(std::process::Stdio::piped());
		cmd.stderr(std::process::Stdio::piped());

		let mut child = cmd.spawn().unwrap();

		// Poll for process completion, signaling pipe when it's waiting
		let pipe_path = self.ctx.pipe_path.clone();
		let issue_path = self.issue_path.to_path_buf();
		let edit_to = self.edit_to.clone();
		let mut signaled = false;

		loop {
			// Check if process has exited
			match child.try_wait().unwrap() {
				Some(_status) => break,
				None => {
					// Process still running
					if !signaled {
						// Give process time to reach pipe wait
						std::thread::sleep(std::time::Duration::from_millis(100));

						// Edit the file while "editor is open" if requested
						// Use serialize_virtual since that's what the user sees/edits (full tree with children)
						if let Some(issue) = &edit_to {
							std::fs::write(&issue_path, issue.serialize_virtual()).unwrap();
						}

						// Try to signal the pipe (use nix O_NONBLOCK to avoid blocking)
						// Only mark as signaled if we successfully wrote to the pipe.
						// The pipe open will fail if no reader is waiting yet.
						#[cfg(unix)]
						{
							use std::os::unix::fs::OpenOptionsExt;
							if let Ok(mut pipe) = std::fs::OpenOptions::new().write(true).custom_flags(0x800).open(&pipe_path) {
								if pipe.write_all(b"x").is_ok() {
									signaled = true;
								}
							}
						}
					}
					std::thread::sleep(std::time::Duration::from_millis(10));
				}
			}
		}

		let output = child.wait_with_output().unwrap();
		(
			output.status,
			String::from_utf8_lossy(&output.stdout).into_owned(),
			String::from_utf8_lossy(&output.stderr).into_owned(),
		)
	}
}

/// Builder for running the `open` command with a URL (remote source).
pub struct OpenUrlBuilder<'a> {
	ctx: &'a TestContext,
	url: String,
	extra_args: Vec<&'a str>,
	edit_at_path: Option<(PathBuf, todo::Issue)>,
}

impl<'a> OpenUrlBuilder<'a> {
	/// Add extra CLI arguments.
	pub fn args(mut self, args: &[&'a str]) -> Self {
		self.extra_args.extend(args);
		self
	}

	/// Edit the file at the specified path while "editor is open".
	/// Use this when you know the path the issue will be stored at.
	pub fn edit_at(mut self, path: &Path, issue: &todo::Issue) -> Self {
		self.edit_at_path = Some((path.to_path_buf(), issue.clone()));
		self
	}

	/// Run the command and return (exit_status, stdout, stderr).
	pub fn run(self) -> (ExitStatus, String, String) {
		let mut cmd = Command::new(get_binary_path());
		cmd.arg("--mock").arg("open");
		cmd.args(&self.extra_args);
		cmd.arg(&self.url);
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env("TODO__GITHUB_TOKEN", "test_token");
		for (key, value) in self.ctx.xdg.env_vars() {
			cmd.env(key, value);
		}
		cmd.env("TODO_MOCK_STATE", &self.ctx.mock_state_path);
		cmd.env("TODO_MOCK_PIPE", &self.ctx.pipe_path);
		cmd.stdout(std::process::Stdio::piped());
		cmd.stderr(std::process::Stdio::piped());

		let mut child = cmd.spawn().unwrap();

		// Poll for process completion, signaling pipe when it's waiting
		let pipe_path = self.ctx.pipe_path.clone();
		let edit_at_path = self.edit_at_path.clone();
		let mut signaled = false;

		loop {
			match child.try_wait().unwrap() {
				Some(_status) => break,
				None => {
					if !signaled {
						std::thread::sleep(std::time::Duration::from_millis(100));

						// Edit the file while "editor is open" if requested
						// Use serialize_virtual since that's what the user sees/edits (full tree with children)
						if let Some((path, issue)) = &edit_at_path {
							std::fs::write(path, issue.serialize_virtual()).unwrap();
						}

						// Try to signal the pipe (use O_NONBLOCK to avoid blocking)
						// Only mark as signaled if we successfully wrote to the pipe.
						// The pipe open will fail if no reader is waiting yet.
						#[cfg(unix)]
						{
							use std::os::unix::fs::OpenOptionsExt;
							if let Ok(mut pipe) = std::fs::OpenOptions::new().write(true).custom_flags(0x800).open(&pipe_path) {
								if pipe.write_all(b"x").is_ok() {
									signaled = true;
								}
							}
						}
					}
					std::thread::sleep(std::time::Duration::from_millis(10));
				}
			}
		}

		let output = child.wait_with_output().unwrap();
		(
			output.status,
			String::from_utf8_lossy(&output.stdout).into_owned(),
			String::from_utf8_lossy(&output.stderr).into_owned(),
		)
	}
}
