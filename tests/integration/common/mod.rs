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
//! let out = ctx.run(&["blocker", "list"]);
//! assert!(out.status.success());
//! ```

pub mod git;
/// Compile the binary before running any tests
pub fn ensure_binary_compiled() {
	BINARY_COMPILED.get_or_init(|| {
		let status = Command::new("cargo").arg("build").status().expect("Failed to execute cargo build");

		if !status.success() {
			panic!("Failed to build binary");
		}
	});
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
	pub fn run(&self, args: &[&str]) -> RunOutput {
		let mut cmd = Command::new(get_binary_path());
		cmd.args(args);
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env(ENV_GITHUB_TOKEN, "test_token");
		for (key, value) in self.xdg.env_vars() {
			cmd.env(key, value);
		}
		let output = cmd.output().unwrap();
		RunOutput {
			status: output.status,
			stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
			stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
		}
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
		OpenUrlBuilder::with_url(self, url)
	}

	/// Create an OpenUrlBuilder for running the `open --touch` command.
	pub fn touch(&self, pattern: &str) -> OpenUrlBuilder<'_> {
		OpenUrlBuilder::with_touch(self, pattern.to_string())
	}

	/// Render a fixture with optional error output if the command failed.
	pub fn render_fixture(&self, renderer: FixtureRenderer<'_>, output: &RunOutput) -> String {
		let mut result = renderer.render();
		if !output.status.success() {
			result.push_str(&format!("\n\nBINARY FAILED\nstdout:\n{}\nstderr:\n{}", output.stdout, output.stderr));
		}
		result
	}
}

/// Builder for running the `open` command with various options.
pub struct OpenBuilder<'a> {
	ctx: &'a TestContext,
	issue_path: &'a Path,
	extra_args: Vec<&'a str>,
	edit_to: Option<tedi::Issue>,
}
impl<'a> OpenBuilder<'a> {
	/// Add extra CLI arguments.
	pub fn args(mut self, args: &[&'a str]) -> Self {
		self.extra_args.extend(args);
		self
	}

	/// Edit the file to this issue while "editor is open".
	pub fn edit(mut self, issue: &tedi::Issue) -> Self {
		self.edit_to = Some(issue.clone());
		self
	}

	/// Run the command and return RunOutput.
	pub fn run(self) -> RunOutput {
		let mut cmd = Command::new(get_binary_path());
		cmd.arg("--mock").arg("open");
		cmd.args(&self.extra_args);
		cmd.arg(self.issue_path.to_str().unwrap());
		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env(ENV_GITHUB_TOKEN, "test_token");
		for (key, value) in self.ctx.xdg.env_vars() {
			cmd.env(key, value);
		}
		cmd.env(ENV_MOCK_STATE, &self.ctx.mock_state_path);
		cmd.env(ENV_MOCK_PIPE, &self.ctx.pipe_path);
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
							let content = issue.serialize_virtual();
							eprintln!("[test:OpenBuilder] submitting user input // writing to {issue_path:?}:\n{content}");
							std::fs::write(&issue_path, content).unwrap();
						}

						// Try to signal the pipe (use nix O_NONBLOCK to avoid blocking)
						// Only mark as signaled if we successfully wrote to the pipe.
						// The pipe open will fail if no reader is waiting yet.
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
			}
		}

		let output = child.wait_with_output().unwrap();
		RunOutput {
			status: output.status,
			stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
			stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
		}
	}
}

/// Builder for running the `open` command with a URL or touch pattern.
pub struct OpenUrlBuilder<'a> {
	ctx: &'a TestContext,
	target: OpenTarget,
	extra_args: Vec<&'a str>,
	edit_op: Option<EditOperation>,
}
impl<'a> OpenUrlBuilder<'a> {
	/// Create a builder for opening by URL.
	fn with_url(ctx: &'a TestContext, url: String) -> Self {
		Self {
			ctx,
			target: OpenTarget::Url(url),
			extra_args: Vec::new(),
			edit_op: None,
		}
	}

	/// Create a builder for opening by touch pattern.
	fn with_touch(ctx: &'a TestContext, pattern: String) -> Self {
		Self {
			ctx,
			target: OpenTarget::Touch(pattern),
			extra_args: Vec::new(),
			edit_op: None,
		}
	}

	/// Add extra CLI arguments.
	pub fn args(mut self, args: &[&'a str]) -> Self {
		self.extra_args.extend(args);
		self
	}

	/// Operates on the issue that is opened for the user in virtual format.
	/// Sets the virtual serialization of the provided issue over the temp file.
	/// The issue's identity determines the virtual edit path.
	pub fn edit(mut self, issue: &Issue) -> Self {
		self.edit_op = Some(EditOperation::FullIssue(issue.clone()));
		self
	}

	/// Operates on the issue that is opened for the user in virtual format in a temp file.
	/// Updates its **contents** (body) to the provided String and submits.
	/// NB: don't submit the issue header at the top - just contents without any indentation.
	pub fn edit_contents<T: AsRef<str>>(mut self, new_issue_body: T) -> Self {
		self.edit_op = Some(EditOperation::ContentsOnly(new_issue_body.as_ref().to_string()));
		self
	}

	/// Edit the file at the specified path while "editor is open".
	/// Use this when you know the path the issue will be stored at.
	/// NB: be very careful when using - the proper interface for submitting generic user-like edits
	/// is [edit](Self::edit) or [edit_contents](Self::edit_contents). Operating on the filesystem
	/// directly is ill-advised and should only be used in tests that specifically want to see
	/// reaction to underlying filesystem changes.
	pub fn unsafe_edit_source_file(mut self, path: &Path, issue: &tedi::Issue) -> Self {
		self.edit_op = Some(EditOperation::SourceFile(path.to_path_buf(), issue.clone()));
		self
	}

	/// Run the command and return RunOutput.
	pub fn run(self) -> RunOutput {
		let mut cmd = Command::new(get_binary_path());
		cmd.arg("--mock").arg("open");
		cmd.args(&self.extra_args);

		match &self.target {
			OpenTarget::Url(url) => {
				cmd.arg(url);
			}
			OpenTarget::Touch(pattern) => {
				cmd.arg("--touch").arg(pattern);
			}
		}

		cmd.env("__IS_INTEGRATION_TEST", "1");
		cmd.env(ENV_GITHUB_TOKEN, "test_token");
		for (key, value) in self.ctx.xdg.env_vars() {
			cmd.env(key, value);
		}
		cmd.env(ENV_MOCK_STATE, &self.ctx.mock_state_path);
		cmd.env(ENV_MOCK_PIPE, &self.ctx.pipe_path);
		cmd.stdout(std::process::Stdio::piped());
		cmd.stderr(std::process::Stdio::piped());

		let mut child = cmd.spawn().unwrap();

		// Poll for process completion, signaling pipe when it's waiting
		let pipe_path = self.ctx.pipe_path.clone();
		let edit_op = self.edit_op.clone();
		let mut signaled = false;

		loop {
			match child.try_wait().unwrap() {
				Some(_status) => break,
				None => {
					if !signaled {
						std::thread::sleep(std::time::Duration::from_millis(100));

						// Edit the file while "editor is open" if requested
						match &edit_op {
							Some(EditOperation::FullIssue(issue)) => {
								// Write to the virtual edit path computed from the issue
								let vpath = tedi::local::Local::virtual_edit_path(issue);
								if let Some(parent) = vpath.parent() {
									std::fs::create_dir_all(parent).unwrap();
								}
								std::fs::write(&vpath, issue.serialize_virtual()).unwrap();
							}
							Some(EditOperation::ContentsOnly(new_body)) => {
								// Find the virtual edit file, read it, replace body, write back
								let virtual_edit_base = PathBuf::from("/tmp").join(env!("CARGO_PKG_NAME"));
								if let Some(vpath) = find_virtual_edit_file(&virtual_edit_base) {
									let content = std::fs::read_to_string(&vpath).unwrap();
									let new_content = replace_issue_body(&content, new_body);
									std::fs::write(&vpath, new_content).unwrap();
								}
							}
							Some(EditOperation::SourceFile(path, issue)) => {
								// Write directly to the source file (unsafe mode)
								std::fs::write(path, issue.serialize_virtual()).unwrap();
							}
							None => {}
						}

						// Try to signal the pipe (use O_NONBLOCK to avoid blocking)
						// Only mark as signaled if we successfully wrote to the pipe.
						// The pipe open will fail if no reader is waiting yet.
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
			}
		}

		let output = child.wait_with_output().unwrap();
		RunOutput {
			status: output.status,
			stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
			stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
		}
	}
}

/// Output from running a command.
pub struct RunOutput {
	pub status: ExitStatus,
	pub stdout: String,
	pub stderr: String,
}
mod snapshot;

use std::{
	io::Write,
	path::{Path, PathBuf},
	process::{Command, ExitStatus},
	sync::OnceLock,
};

pub use snapshot::FixtureIssuesExt;
use tedi::Issue;
use v_fixtures::{Fixture, FixtureRenderer, fs_standards::xdg::Xdg};

/// Environment variable names derived from package name
const ENV_GITHUB_TOKEN: &str = concat!(env!("CARGO_PKG_NAME"), "__GITHUB_TOKEN");
const ENV_MOCK_STATE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_STATE");
const ENV_MOCK_PIPE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_PIPE");

static BINARY_COMPILED: OnceLock<()> = OnceLock::new();

fn get_binary_path() -> PathBuf {
	ensure_binary_compiled();

	let mut path = std::env::current_exe().unwrap();
	path.pop(); // Remove test binary name
	path.pop(); // Remove 'deps'
	path.push(env!("CARGO_PKG_NAME"));
	path
}

/// Type of edit operation for the builder.
#[derive(Clone)]
enum EditOperation {
	/// Edit using a full Issue (writes serialize_virtual)
	FullIssue(Issue),
	/// Edit just the contents/body (preserves header, replaces body)
	ContentsOnly(String),
	/// Edit the source file directly (unsafe, for testing filesystem behavior)
	SourceFile(PathBuf, Issue),
}

/// The target for opening an issue (URL or touch pattern).
#[derive(Clone)]
enum OpenTarget {
	/// Open by Github URL
	Url(String),
	/// Open by touch pattern (--touch flag)
	Touch(String),
}

/// Find the most recently modified .md file under the virtual edit base path.
fn find_virtual_edit_file(base: &Path) -> Option<PathBuf> {
	if !base.exists() {
		return None;
	}

	let mut best: Option<(PathBuf, std::time::SystemTime)> = None;

	fn walk(dir: &Path, best: &mut Option<(PathBuf, std::time::SystemTime)>) {
		if let Ok(entries) = std::fs::read_dir(dir) {
			for entry in entries.flatten() {
				let path = entry.path();
				if path.is_dir() {
					walk(&path, best);
				} else if path.extension().is_some_and(|e| e == "md")
					&& let Ok(meta) = path.metadata()
					&& let Ok(mtime) = meta.modified()
					&& best.as_ref().map(|(_, t)| mtime > *t).unwrap_or(true)
				{
					*best = Some((path, mtime));
				}
			}
		}
	}

	walk(base, &mut best);
	best.map(|(p, _)| p)
}

/// Replace the body content in a virtual-format issue file.
/// Keeps the title line (first line starting with `- [`), replaces everything after it.
fn replace_issue_body(content: &str, new_body: &str) -> String {
	let mut lines = content.lines();

	// Keep the title line
	let title_line = lines.next().expect("content should not be empty");

	// Build new content: title line + indented new body
	let mut result = String::from(title_line);
	result.push('\n');

	// Indent the new body content (virtual format uses single tab indent for body)
	for line in new_body.lines() {
		if line.is_empty() {
			result.push('\n');
		} else {
			result.push('\t');
			result.push_str(line);
			result.push('\n');
		}
	}

	result
}
