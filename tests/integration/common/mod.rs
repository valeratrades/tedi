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
//! let ctx = TestContext::build("");
//!
//! // Or with fixtures:
//! let ctx = TestContext::build(r#"
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
	/// Create a new test context from a fixture string with git initialized.
	///
	/// Files in the fixture should use XDG category prefixes:
	/// - `/data/blockers/test.md` → `XDG_DATA_HOME/todo/blockers/test.md`
	/// - `/cache/current.txt` → `XDG_CACHE_HOME/todo/current.txt`
	/// - `/state/db.json` → `XDG_STATE_HOME/todo/db.json`
	///
	/// # Example
	///
	/// ```ignore
	/// let ctx = TestContext::build(r#"
	///     //- /data/blockers/test.md
	///     # Project
	///     - task 1
	/// "#);
	/// ```
	pub fn build(fixture_str: &str) -> Self {
		use git::GitExt;

		let fixture = Fixture::parse(fixture_str);
		let xdg = Xdg::new(fixture.write_to_tempdir(), env!("CARGO_PKG_NAME"));

		let mock_state_path = xdg.inner.root.join("mock_state.json");
		let pipe_path = xdg.inner.create_pipe("editor_pipe");

		// Set overrides so all library calls use our temp dir
		tedi::mocks::set_issues_dir(xdg.data_dir().join("issues"));

		let ctx = Self { xdg, mock_state_path, pipe_path };
		ctx.init_git();
		ctx
	}

	/// Run a command with proper XDG environment.
	///
	/// Used in cases where we don't have to simulate additional user input. If you're testing `open` command, you probably want to run it through [open builder](Self::open)
	pub fn run(&self, args: &[&str]) -> RunOutput {
		//if !args.contains("--mock") {
		//	args = ["--mock"].join(args);
		//}
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

	/// Create an OpenBuilder for running the `open` command with an Issue reference.
	///
	/// Takes an `&Issue` and uses `IssueIndex::from(issue).to_string()` as the selector
	/// pattern passed to the CLI. This is the correct way to identify issues without
	/// relying on absolute paths.
	pub fn open_issue<'a>(&'a self, issue: &'a Issue) -> OpenBuilder<'a> {
		OpenBuilder {
			ctx: self,
			target: BuilderTarget::Issue(issue),
			extra_args: Vec::new(),
			edit_op: None,
			ghost_edit: false,
		}
	}

	/// Create an OpenBuilder for running the `open` command with a Github URL.
	pub fn open_url(&self, repo_info: tedi::RepoInfo, number: u64) -> OpenBuilder<'_> {
		let url = format!("https://github.com/{}/{}/issues/{number}", repo_info.owner(), repo_info.repo());
		OpenBuilder {
			ctx: self,
			target: BuilderTarget::Url(url),
			extra_args: Vec::new(),
			edit_op: None,
			ghost_edit: false,
		}
	}

	/// Create an OpenBuilder for running the `open --touch` command.
	pub fn open_touch(&self, pattern: &str) -> OpenBuilder<'_> {
		OpenBuilder {
			ctx: self,
			target: BuilderTarget::Touch(pattern.to_string()),
			extra_args: Vec::new(),
			edit_op: None,
			ghost_edit: false,
		}
	}

	/// Set up mock Github to return an issue.
	///
	/// The issue parameter should be a serde_json::Value representing the mock state.
	fn setup_mock_state(&self, state: &serde_json::Value) {
		std::fs::write(&self.mock_state_path, serde_json::to_string_pretty(state).unwrap()).unwrap();
	}
}

/// Builder for running the `open` command with various options.
pub struct OpenBuilder<'a> {
	ctx: &'a TestContext,
	target: BuilderTarget<'a>,
	extra_args: Vec<&'a str>,
	edit_op: Option<EditOperation>,
	ghost_edit: bool,
}
impl<'a> OpenBuilder<'a> {
	/// Add extra CLI arguments.
	pub fn args(mut self, args: &[&'a str]) -> Self {
		self.extra_args.extend(args);
		self
	}

	/// Edit the file to this issue while "editor is open".
	pub fn edit(mut self, issue: &Issue) -> Self {
		self.edit_op = Some(EditOperation::FullIssue(Box::new(issue.clone())));
		self
	}

	/// Operates on the issue that is opened for the user in virtual format in a temp file.
	/// Updates its **contents** (body) to the provided String and submits.
	/// NB: don't submit the issue header at the top - just contents without any indentation.
	pub fn edit_contents<T: AsRef<str>>(mut self, new_issue_body: T) -> Self {
		self.edit_op = Some(EditOperation::ContentsOnly(new_issue_body.as_ref().to_string()));
		self
	}

	/// Skip editor and pretend edit was made. Syncs the issue without user interaction.
	pub fn ghost_edit(mut self) -> Self {
		self.ghost_edit = true;
		self
	}

	/// Pause execution when the virtual file is ready for editing.
	///
	/// Returns `(virtual_file_path, continuation)`. The test can:
	/// 1. Read the virtual file at the returned path
	/// 2. Modify it as needed
	/// 3. Call `.resume()` on the continuation to signal completion and get the result
	pub fn break_to_edit(self) -> (PathBuf, PausedEdit) {
		self.ctx.set_issues_dir_override();

		let (global_args, subcommand_args): (Vec<&str>, Vec<&str>) = self.extra_args.into_iter().partition(|arg| GLOBAL_FLAGS.iter().any(|f| arg.starts_with(f)));

		let mut cmd = Command::new(get_binary_path());
		cmd.arg("--mock");
		cmd.args(&global_args);
		cmd.arg("open");
		cmd.args(&subcommand_args);

		match &self.target {
			BuilderTarget::Issue(issue) => {
				let issue_path = tedi::local::LocalPath::from(*issue)
					.resolve_parent(tedi::local::FsReader)
					.expect("failed to resolve issue parent path")
					.search()
					.expect("failed to find issue file")
					.path();
				cmd.arg(&issue_path);
			}
			BuilderTarget::Url(url) => {
				cmd.arg(url);
			}
			BuilderTarget::Touch(pattern) => {
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
		let stdout = child.stdout.take().unwrap();
		let stderr = child.stderr.take().unwrap();
		set_nonblocking(&stdout);
		set_nonblocking(&stderr);

		let pipe_path = self.ctx.pipe_path.clone();
		let virtual_edit_base = self.ctx.xdg.inner.root.clone();

		// Wait for virtual file to appear
		let vpath = loop {
			std::thread::sleep(std::time::Duration::from_millis(50));
			if let Some(vpath) = find_virtual_edit_file(&virtual_edit_base) {
				break vpath;
			}
			if child.try_wait().unwrap().is_some() {
				panic!("Process exited before creating virtual file");
			}
		};

		(vpath, PausedEdit { child, stdout, stderr, pipe_path })
	}

	/// Run the command and return RunOutput.
	pub fn run(self) -> RunOutput {
		self.ctx.set_issues_dir_override();

		// Separate global flags from subcommand flags
		let (global_args, subcommand_args): (Vec<&str>, Vec<&str>) = self.extra_args.into_iter().partition(|arg| GLOBAL_FLAGS.iter().any(|f| arg.starts_with(f)));

		let mut cmd = Command::new(get_binary_path());

		// Global flags come first (before subcommand)
		if self.ghost_edit {
			cmd.arg("--mock=ghost-edit");
		} else {
			cmd.arg("--mock");
		}
		cmd.args(&global_args);

		// Then the subcommand
		cmd.arg("open");

		// Then subcommand-specific flags
		cmd.args(&subcommand_args);

		// Then the target
		match &self.target {
			BuilderTarget::Issue(issue) => {
				let issue_path = tedi::local::LocalPath::from(*issue)
					.resolve_parent(tedi::local::FsReader)
					.expect("failed to resolve issue parent path")
					.search()
					.expect("failed to find issue file")
					.path();
				cmd.arg(&issue_path);
			}
			BuilderTarget::Url(url) => {
				cmd.arg(url);
			}
			BuilderTarget::Touch(pattern) => {
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

		// Take ownership of stdout/stderr to drain them and prevent pipe buffer deadlock
		let mut stdout = child.stdout.take().unwrap();
		let mut stderr = child.stderr.take().unwrap();
		set_nonblocking(&stdout);
		set_nonblocking(&stderr);
		let mut stdout_buf = Vec::new();
		let mut stderr_buf = Vec::new();

		// Poll for process completion, signaling pipe when it's waiting
		let pipe_path = self.ctx.pipe_path.clone();
		let edit_op = self.edit_op.clone();
		let mut signaled = false;

		while child.try_wait().unwrap().is_none() {
			// Drain pipes to prevent deadlock from full pipe buffers
			drain_pipe(&mut stdout, &mut stdout_buf);
			drain_pipe(&mut stderr, &mut stderr_buf);

			// Process still running
			if !signaled {
				// Give process time to reach pipe wait
				std::thread::sleep(std::time::Duration::from_millis(100));

				// Edit the file while "editor is open" if requested
				match &edit_op {
					Some(EditOperation::FullIssue(issue)) => {
						// Write to the virtual edit path computed from the issue
						let vpath = tedi::local::Local::virtual_edit_path(issue);
						let content = issue.serialize_virtual();
						eprintln!("[test:OpenBuilder] submitting user input // writing to {vpath:?}:\n{content}");
						std::fs::write(&vpath, content).unwrap();
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
					None => {}
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

		// Final drain after process exits
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

pub fn parse(content: &str) -> Issue {
	Issue::deserialize_virtual(content).expect("failed to parse test issue")
}
/// Render a fixture with optional error output if the command failed.
pub fn render_fixture(renderer: FixtureRenderer<'_>, output: &RunOutput) -> String {
	let result = renderer.always_show_filepath().render();

	// will only see it if snapshot failed. //Q: how much overhead this has though?
	let s = format!("\n\nBINARY FAILED\nstdout:\n{}\nstderr:\n{}", output.stdout, output.stderr);
	eprintln!("{s}");

	result
}
/// Output from running a command.
pub struct RunOutput {
	pub status: ExitStatus,
	pub stdout: String,
	pub stderr: String,
}

/// Handle for a paused edit operation. Call `.resume()` to continue execution.
pub struct PausedEdit {
	child: std::process::Child,
	stdout: std::process::ChildStdout,
	stderr: std::process::ChildStderr,
	pipe_path: PathBuf,
}

impl PausedEdit {
	/// Resume execution after modifying the virtual file.
	pub fn resume(mut self) -> RunOutput {
		let mut stdout_buf = Vec::new();
		let mut stderr_buf = Vec::new();

		#[cfg(unix)]
		{
			use std::os::unix::fs::OpenOptionsExt;
			let mut pipe = std::fs::OpenOptions::new()
				.write(true)
				.custom_flags(0x800) // O_NONBLOCK
				.open(&self.pipe_path)
				.expect("failed to open pipe");
			pipe.write_all(b"x").expect("failed to signal pipe");
		}

		while self.child.try_wait().unwrap().is_none() {
			drain_pipe(&mut self.stdout, &mut stdout_buf);
			drain_pipe(&mut self.stderr, &mut stderr_buf);
			std::thread::sleep(std::time::Duration::from_millis(10));
		}

		drain_pipe(&mut self.stdout, &mut stdout_buf);
		drain_pipe(&mut self.stderr, &mut stderr_buf);

		self.child.wait().unwrap();
		RunOutput {
			status: self.child.try_wait().unwrap().unwrap(),
			stdout: String::from_utf8_lossy(&stdout_buf).into_owned(),
			stderr: String::from_utf8_lossy(&stderr_buf).into_owned(),
		}
	}
}

/// Unsafe filesystem operations for tests that genuinely need path-based access.
///
/// **DO NOT USE** unless you are testing filesystem edge cases specifically.
/// Normal tests should use `ctx.open_issue(&issue)` with the proper `Issue` type.
///
/// To use: `use crate::common::are_you_sure::UnsafePathExt;`
pub mod are_you_sure {
	use std::path::{Path, PathBuf};

	use tedi::local::{FsReader, LocalPath};

	use super::TestContext;

	/// Extension trait for unsafe path-based operations.
	///
	/// These methods bypass the proper IssueIndex-based addressing and work
	/// directly with filesystem paths. Only use for tests that specifically
	/// need to verify filesystem behavior or edge cases.
	pub trait UnsafePathExt {
		/// Get the flat format path for an issue: `{number}_-_{title}.md`
		///
		/// **Unsafe**: bypasses proper issue addressing. Use only for filesystem tests.
		fn flat_issue_path(&self, repo_info: tedi::RepoInfo, number: u64, title: &str) -> PathBuf;

		/// Get the directory format path for an issue: `{number}_-_{title}/__main__.md`
		///
		/// **Unsafe**: bypasses proper issue addressing. Use only for filesystem tests.
		fn dir_issue_path(&self, repo_info: tedi::RepoInfo, number: u64, title: &str) -> PathBuf;

		/// Resolve an issue's actual filesystem path after it's been written.
		///
		/// **Unsafe**: uses filesystem search. Prefer working with Issue directly.
		fn resolve_issue_path(&self, issue: &tedi::Issue) -> PathBuf;
	}

	impl UnsafePathExt for TestContext {
		fn flat_issue_path(&self, repo_info: tedi::RepoInfo, number: u64, title: &str) -> PathBuf {
			let sanitized = title.replace(' ', "_");
			self.xdg.data_dir().join(format!("issues/{}/{}/{number}_-_{sanitized}.md", repo_info.owner(), repo_info.repo()))
		}

		fn dir_issue_path(&self, repo_info: tedi::RepoInfo, number: u64, title: &str) -> PathBuf {
			let sanitized = title.replace(' ', "_");
			self.xdg
				.data_dir()
				.join(format!("issues/{}/{}/{number}_-_{sanitized}/__main__.md", repo_info.owner(), repo_info.repo()))
		}

		fn resolve_issue_path(&self, issue: &tedi::Issue) -> PathBuf {
			self.set_issues_dir_override();
			LocalPath::from(issue).resolve_parent(FsReader).unwrap().search().unwrap().path()
		}
	}

	/// Read an issue file's contents directly from the filesystem.
	///
	/// **Unsafe**: bypasses proper issue loading. Use only for filesystem verification tests.
	pub fn read_issue_file(path: &Path) -> String {
		std::fs::read_to_string(path).expect("failed to read issue file")
	}

	/// Write content directly to a filesystem path.
	///
	/// **Unsafe**: bypasses virtual edit path. Use only for tests checking filesystem edge cases.
	pub fn write_to_path(path: &Path, content: &str) {
		if let Some(parent) = path.parent() {
			std::fs::create_dir_all(parent).expect("failed to create parent dirs");
		}
		std::fs::write(path, content).expect("failed to write file");
	}
}
/// Global CLI flags that must appear before the subcommand.
const GLOBAL_FLAGS: &[&str] = &["--offline", "--mock", "-v", "--verbose", "-q", "--quiet"];
/// Environment variable names derived from package name
const ENV_GITHUB_TOKEN: &str = concat!(env!("CARGO_PKG_NAME"), "__GITHUB_TOKEN");
const ENV_MOCK_STATE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_STATE");
const ENV_MOCK_PIPE: &str = concat!(env!("CARGO_PKG_NAME"), "_MOCK_PIPE");
/// What target the OpenBuilder opens.
enum BuilderTarget<'a> {
	/// Open by issue reference (derives path from issue)
	Issue(&'a Issue),
	/// Open by Github URL
	Url(String),
	/// Open by touch pattern (--touch flag)
	Touch(String),
}

mod snapshot;

use std::{
	io::{Read, Write},
	os::fd::AsRawFd,
	path::{Path, PathBuf},
	process::{Command, ExitStatus},
	sync::OnceLock,
};

/// Set a file descriptor to non-blocking mode.
fn set_nonblocking<F: AsRawFd>(f: &F) {
	//SAFETY: don't care if log is overwritten
	unsafe {
		let fd = f.as_raw_fd();
		let flags = libc::fcntl(fd, libc::F_GETFL);
		libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
	}
}

/// Drain available data from a non-blocking pipe into a buffer.
fn drain_pipe<R: Read>(pipe: &mut R, buf: &mut Vec<u8>) {
	let mut tmp = [0u8; 4096];
	//LOOP: we're just draining, so if fs is functioning, we will see the `Ok(0)`
	loop {
		match pipe.read(&mut tmp) {
			Ok(0) => break,
			Ok(n) => buf.extend_from_slice(&tmp[..n]),
			Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => break,
			Err(e) => panic!("pipe read error: {e}"),
		}
	}
}

pub use snapshot::FixtureIssuesExt;
use tedi::Issue;
use v_fixtures::{Fixture, FixtureRenderer, fs_standards::xdg::Xdg};

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
	FullIssue(Box<Issue>),
	/// Edit just the contents/body (preserves header, replaces body)
	ContentsOnly(String),
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
