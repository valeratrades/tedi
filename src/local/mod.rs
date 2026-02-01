//! Local filesystem storage for issues.
//!
//! This module handles all local storage concerns.
//!
//! The local storage structure:
//! ```text
//! XDG_DATA_HOME/todo/issues/{owner}/{repo}/
//!   .meta.json                           # Project + per-issue metadata
//!   {number}_-_{title}.md                # Flat issue file
//!   {number}_-_{title}/                  # Directory format (when has children)
//!     __main__.md                        # The issue content
//!     {child_number}_-_{child_title}.md  # Child issues
//! ```
//!
//! All public access goes through methods on the `Local` type.

pub mod conflict;
pub mod consensus;
/// Error type for local issue loading operations.
#[derive(Debug, thiserror::Error, derive_more::From)]
pub enum LocalError {
	/// Path resolution or IO error.
	#[error(transparent)]
	Io(LocalPathError),

	/// Failed to parse issue content.
	#[error("failed to parse issue file: {path}")]
	ParseError {
		path: PathBuf,
		#[source]
		source: Box<crate::ParseError>,
	},

	//Q: LocalPathError also contains ReaderError. Seems suboptimal, - wonder if I can restructure somehow to remove this proprietor level ambiguity
	/// Reader operation failed.
	#[error(transparent)]
	Reader(ReaderError),

	/// Git operation failed (for consensus reads).
	//TODO: check if it covers cases that ReaderError doesn't when it comes to git operations. If not, we should nuke this and rely on `Io` which already references LocalPathError (which nests ReaderError)
	#[error("git operation failed: {message}")]
	GitError { message: String },

	/// Unresolved merge conflict blocks operation.
	#[error(transparent)]
	ConflictBlocked(conflict::ConflictBlockedError),
}

/// Error type for consensus sink operations.
#[derive(Debug, miette::Diagnostic, derive_more::Display, thiserror::Error)]
pub enum ConsensusSinkError {
	#[display("failed to write issue files: {_0}")]
	#[diagnostic(code(tedi::consensus::write))]
	Write(color_eyre::Report),

	#[display("git add failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_add))]
	GitAdd(String),

	#[display("git status failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_status))]
	GitStatus(String),

	#[display("git commit failed: {_0}")]
	#[diagnostic(code(tedi::consensus::git_commit))]
	GitCommit(String),

	#[display("files rejected by .gitignore:\n{_0}")]
	#[diagnostic(code(tedi::consensus::gitignore), help("Check your .gitignore rules or remove the conflicting patterns"))]
	GitIgnoreRejection(String),

	#[display("invalid data directory path (not valid UTF-8)")]
	#[diagnostic(code(tedi::consensus::invalid_path))]
	InvalidDataDir,

	#[display("{_0}")]
	#[diagnostic(code(tedi::consensus::io))]
	Io(#[from] std::io::Error),
}
/// Source for loading issues from local storage.
///
/// Combines a `LocalPath` (for path computation) with a reader (for reading from fs or git).
#[derive(Clone, Debug)]
pub struct LocalIssueSource<R: LocalReader> {
	pub local_path: LocalPath,
	pub reader: R,
}
impl<R: LocalReader> LocalIssueSource<R> {
	pub fn new(local_path: LocalPath, reader: R) -> Self {
		Self { local_path, reader }
	}

	/// Create a child source with the same reader.
	pub fn child(&self, child_index: IssueIndex) -> Self {
		Self {
			local_path: LocalPath::new(child_index),
			reader: self.reader,
		}
	}

	/// Get the IssueIndex from the underlying LocalPath.
	pub fn index(&self) -> &IssueIndex {
		&self.local_path.index
	}
}
impl LocalIssueSource<FsReader> {
	/// Create a source for reading from filesystem (submitted state).
	pub fn submitted(local_path: LocalPath) -> Self {
		Self::new(local_path, FsReader)
	}

	/// Create a source from a filesystem path by extracting the IssueIndex.
	///
	/// This extracts the parent_index from the path, then constructs the full index
	/// by adding the target issue's selector.
	pub fn from_path(path: &Path) -> Result<Self, color_eyre::Report> {
		let index = Local::extract_index_from_path(path)?;
		Ok(Self::new(LocalPath::new(index), FsReader))
	}
}
impl LocalIssueSource<GitReader> {
	/// Create a source for reading from git HEAD (consensus state).
	pub fn consensus(local_path: LocalPath) -> Self {
		Self::new(local_path, GitReader)
	}
}

/// Type for local filesystem operations.
///
/// All custom logic for local issue storage is consolidated as methods on this type.
pub enum Local {}
impl Local {
	/// The filename used for the main issue file when it has a directory for sub-issues.
	pub const MAIN_ISSUE_FILENAME: &'static str = "__main__";

	/// Get the virtual edit path for an issue.
	///
	/// Returns a path in `{base}/{index}.md` where:
	/// - In tests: base is derived from the mock issues_dir's parent (the XDG data dir)
	/// - In production: base is `/tmp/{CARGO_PKG_NAME}`
	///
	/// The index is the Display representation of the issue's full index (e.g. `owner/repo/123/456`).
	/// This path is used when opening an editor for the user to edit the issue.
	pub fn virtual_edit_path(issue: &crate::Issue) -> PathBuf {
		// In tests, MockIssuesDir is set. Derive virtual edit path from the same temp dir.
		// In subprocesses spawned by tests, MockIssuesDir won't be set but XDG env vars will be,
		// so issues_dir() will return the test's temp dir, and we derive from its parent.
		let base: PathBuf = if crate::mocks::MockIssuesDir::get().is_some() || std::env::var("__IS_INTEGRATION_TEST").is_ok() {
			// Either thread_local is set (in-process test code) or we're a subprocess of a test
			Self::issues_dir().parent().unwrap().parent().unwrap().parent().unwrap().to_path_buf()
		} else {
			PathBuf::from("/tmp").join(env!("CARGO_PKG_NAME"))
		};
		let index_path = issue.full_index().to_string();
		let vpath = base.join(format!("{index_path}.md"));
		if let Some(parent) = vpath.parent() {
			std::fs::create_dir_all(parent).unwrap();
		}
		vpath
	}

	/// Returns the base directory for issue storage: XDG_DATA_HOME/todo/issues/
	///
	/// If a mock override is set (via `mocks::set_issues_dir`), returns that instead.
	/// This allows tests to isolate their filesystem state per-thread.
	pub fn issues_dir() -> PathBuf {
		if let Some(override_dir) = crate::mocks::MockIssuesDir::get() {
			return override_dir;
		}
		v_utils::xdg_data_dir!("issues")
	}

	/// Parse a single issue node from filesystem content.
	///
	/// This parses one issue file (without loading children from separate files).
	/// Children field will be empty - they're loaded separately via LazyIssue.
	#[deprecated(
		note = "needs to be reimplemented. Should take `Reader` implementor for reading contents, and `file_path` is excessive since we already have IssueIndex, - at most pass LocalPathResolved here"
	)]
	fn parse_single_node(content: &str, index: IssueIndex, file_path: &Path) -> Result<Issue, LocalError> {
		let mut issue = Issue::parse_virtual(content, index).map_err(|e| LocalError::ParseError {
			path: file_path.to_path_buf(),
			source: Box::new(e),
		})?;
		// Clear any inline children (filesystem format stores them in separate files)
		issue.children.clear();
		Ok(issue)
	}

	/// Get the project directory path (where .meta.json lives).
	/// Structure: issues/{owner}/{repo}/
	pub fn project_dir(owner: &str, repo: &str) -> PathBuf {
		Self::issues_dir().join(owner).join(repo)
	}

	/// Sanitize a title for use in filenames.
	/// Converts spaces to underscores and removes special characters.
	pub fn sanitize_title(title: &str) -> String {
		title
			.chars()
			.map(|c| {
				if c.is_alphanumeric() || c == '-' || c == '_' {
					c
				} else if c == ' ' {
					'_'
				} else {
					'\0'
				}
			})
			.filter(|&c| c != '\0')
			.collect::<String>()
			.trim_matches('_')
			.to_string()
	}

	/// Format an issue filename from number and title.
	/// Format: {number}_-_{sanitized_title}.md or just {sanitized_title}.md if no number
	/// Adds .bak suffix for closed issues.
	fn format_issue_filename(issue_number: Option<u64>, title: &str, closed: bool) -> String {
		let sanitized = Self::sanitize_title(title);
		let base = match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}.md"),
			Some(num) => format!("{num}_-_{sanitized}.md"),
			None if sanitized.is_empty() => "untitled.md".to_string(),
			None => format!("{sanitized}.md"),
		};
		if closed { format!("{base}.bak") } else { base }
	}

	/// Get the directory name for an issue (used when it has sub-issues).
	/// Format: {number}_-_{sanitized_title}
	//TODO: start adding `.bak` for closed
	pub fn issue_dir_name(issue_number: Option<u64>, title: &str) -> String {
		let sanitized = Self::sanitize_title(title);
		match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}"),
			Some(num) => format!("{num}_-_{sanitized}"),
			None if sanitized.is_empty() => "untitled".to_string(),
			None => sanitized,
		}
	}

	/// Get the path for the main issue file when stored inside a directory.
	/// Format: {dir}/__main__.md[.bak]
	pub fn main_file_path(issue_dir: &Path, closed: bool) -> PathBuf {
		let filename = if closed {
			format!("{}.md.bak", Self::MAIN_ISSUE_FILENAME)
		} else {
			format!("{}.md", Self::MAIN_ISSUE_FILENAME)
		};
		issue_dir.join(filename)
	}

	/// Find the main file inside an issue directory (checks both open and closed).
	pub fn main_file_in_dir(issue_dir: &Path) -> Option<PathBuf> {
		let main_file = Self::main_file_path(issue_dir, false);
		if main_file.exists() {
			return Some(main_file);
		}
		let main_bak = Self::main_file_path(issue_dir, true);
		if main_bak.exists() {
			return Some(main_bak);
		}
		None
	}

	/// Get the most recently modified issue file.
	#[deprecated(note = "rewrite to use LocalPathResolved::matching_subpaths")]
	pub fn most_recent_issue_file() -> Result<Option<PathBuf>> {
		fn collect_issue_files(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
			for entry in std::fs::read_dir(dir)? {
				let entry = entry?;
				let path = entry.path();
				if path.is_dir() {
					collect_issue_files(&path, files)?;
				} else if path.is_file()
					&& let Some(name) = path.file_name().and_then(|n| n.to_str())
					&& (name.ends_with(".md") || name.ends_with(".md.bak"))
				{
					files.push(path);
				}
			}
			Ok(())
		}

		let issues_base = Self::issues_dir();
		if !issues_base.exists() {
			return Ok(None);
		}

		let mut files = Vec::new();
		collect_issue_files(&issues_base, &mut files)?;

		// Sort by modification time (most recent first) and return the first
		files.sort_by(|a, b| {
			let a_time = std::fs::metadata(a).and_then(|m| m.modified()).ok();
			let b_time = std::fs::metadata(b).and_then(|m| m.modified()).ok();
			b_time.cmp(&a_time)
		});

		Ok(files.into_iter().next())
	}

	/// Parse an IssueSelector from a filename or directory name.
	/// Format: `{number}_-_{title}[.md[.bak]]` or just `{title}[.md[.bak]]` for pending issues.
	///
	/// Returns `GitId` if an issue number is found, `Title` for title-only .md files.
	/// Returns `None` for non-issue files (directories without numbers, non-.md files).
	pub fn parse_issue_selector_from_name(name: &str) -> Option<IssueSelector> {
		// Strip file extensions if present
		let base = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap_or(name);

		// Try to extract issue number
		if let Some(sep_pos) = base.find("_-_") {
			if let Ok(num) = base[..sep_pos].parse::<u64>() {
				return Some(IssueSelector::GitId(num));
			}
		} else if let Ok(num) = base.parse::<u64>() {
			return Some(IssueSelector::GitId(num));
		}

		// No number - return Title for both .md files and directories (pending/unsynced issues)
		IssueSelector::try_title(base)
	}

	/// Extract the full IssueIndex from an issue file path (including the issue itself).
	///
	/// Unlike `extract_parent_index`, this includes the target issue's selector.
	pub fn extract_index_from_path(path: &Path) -> Result<IssueIndex> {
		let issues_base = Self::issues_dir();

		let rel_path = path.strip_prefix(&issues_base).map_err(|_| eyre!("Issue file is not in issues directory: {path:?}"))?;

		let components: Vec<_> = rel_path.components().collect();
		if components.len() < 3 {
			bail!("Path too short to extract issue: {path:?}");
		}

		let owner = components[0].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract owner from path: {path:?}"))?;
		let repo = components[1].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract repo from path: {path:?}"))?;

		// Check if this is a directory format file (__main__.md or __main__.md.bak)
		let filename = components
			.last()
			.expect("components verified to have at least 3 elements")
			.as_os_str()
			.to_str()
			.ok_or_else(|| eyre!("Could not convert filename to str: {path:?}"))?;
		let is_dir_format = filename.starts_with(Self::MAIN_ISSUE_FILENAME);

		// Collect all issue selectors including the target issue
		let mut selectors = Vec::new();

		// For directory format: path is owner/repo/dir1/dir2/.../issue_dir/__main__.md
		//   - Loop processes dir1..issue_dir (index 2 to len-2 exclusive)
		//   - Then add issue_dir (index len-2) separately
		// For flat format: path is owner/repo/dir1/dir2/.../issue.md
		//   - Loop processes dir1.. (index 2 to len-1 exclusive)
		//   - Then add issue from filename
		let dir_end = if is_dir_format {
			components.len() - 2 // Stop before the issue directory (will be added separately)
		} else {
			components.len() - 1 // Stop before the filename
		};

		// Process ancestor issue directories (skip owner and repo)
		for component in &components[2..dir_end] {
			let name = component.as_os_str().to_str().ok_or_else(|| eyre!("Invalid path component: {component:?}"))?;

			// Skip if this looks like a file (has .md extension)
			if name.ends_with(".md") || name.ends_with(".md.bak") {
				continue;
			}

			// Extract issue number from directory name
			if let Some(selector) = Self::parse_issue_selector_from_name(name) {
				selectors.push(selector);
			}
		}

		// Add the target issue selector
		if is_dir_format {
			// The directory just before __main__.md is the issue
			let dir_name = components[components.len() - 2].as_os_str().to_str().ok_or_else(|| eyre!("Invalid directory component"))?;
			if let Some(selector) = Self::parse_issue_selector_from_name(dir_name) {
				selectors.push(selector);
			}
		} else {
			// The filename is the issue (e.g., 123_-_title.md)
			if let Some(selector) = Self::parse_issue_selector_from_name(filename) {
				selectors.push(selector);
			}
		}

		Ok(IssueIndex::with_index(owner, repo, selectors))
	}

	/// Interactive issue file selection using fzf.
	/// Collects all issue files and presents them in fzf for selection.
	pub fn fzf_issue(initial_query: &str, exact: ExactMatchLevel) -> Result<PathBuf> {
		use std::{
			io::Write,
			process::{Command, Stdio},
		};

		fn collect_issue_files(dir: &Path, files: &mut Vec<PathBuf>) -> std::io::Result<()> {
			for entry in std::fs::read_dir(dir)? {
				let entry = entry?;
				let path = entry.path();
				if path.is_dir() {
					collect_issue_files(&path, files)?;
				} else if path.is_file()
					&& let Some(name) = path.file_name().and_then(|n| n.to_str())
					&& (name.ends_with(".md") || name.ends_with(".md.bak"))
				{
					files.push(path);
				}
			}
			Ok(())
		}

		let issues_base = Self::issues_dir();
		if !issues_base.exists() {
			bail!("No issue files found. Use a Github URL to fetch an issue first.");
		}

		let mut files = Vec::new();
		collect_issue_files(&issues_base, &mut files)?;

		if files.is_empty() {
			bail!("No issue files found. Use a Github URL to fetch an issue first.");
		}

		// Sort by modification time (most recent first)
		files.sort_by(|a, b| {
			let a_time = std::fs::metadata(a).and_then(|m| m.modified()).ok();
			let b_time = std::fs::metadata(b).and_then(|m| m.modified()).ok();
			b_time.cmp(&a_time)
		});

		let file_list: Vec<String> = files
			.iter()
			.filter_map(|p| p.strip_prefix(&issues_base).ok().map(|rel| rel.to_string_lossy().to_string()))
			.collect();

		let (filtered_list, fzf_query): (Vec<&String>, String) = match exact {
			ExactMatchLevel::Fuzzy | ExactMatchLevel::ExactTerms => (file_list.iter().collect(), initial_query.to_string()),
			ExactMatchLevel::RegexSubstring =>
				if initial_query.is_empty() {
					(file_list.iter().collect(), String::new())
				} else {
					let re = Regex::new(initial_query).map_err(|e| eyre!("Invalid regex pattern: {e}"))?;
					let filtered: Vec<&String> = file_list.iter().filter(|f| re.is_match(f)).collect();
					(filtered, String::new())
				},
			ExactMatchLevel::RegexLine =>
				if initial_query.is_empty() {
					(file_list.iter().collect(), String::new())
				} else {
					let pattern = {
						let has_start = initial_query.starts_with('^');
						let has_end = initial_query.ends_with('$');
						match (has_start, has_end) {
							(true, true) => initial_query.to_string(),
							(true, false) => format!("{initial_query}$"),
							(false, true) => format!("^{initial_query}"),
							(false, false) => format!("^{initial_query}$"),
						}
					};
					let re = Regex::new(&pattern).map_err(|e| eyre!("Invalid regex pattern: {e}"))?;
					let filtered: Vec<&String> = file_list.iter().filter(|f| re.is_match(f)).collect();
					(filtered, String::new())
				},
		};

		let mut cmd = Command::new("fzf");

		cmd.arg("--query").arg(&fzf_query);

		if matches!(exact, ExactMatchLevel::ExactTerms) {
			cmd.arg("--exact");
		}

		cmd.arg("--select-1")
			.arg("--preview")
			.arg("cat {}")
			.arg("--preview-window")
			.arg("right:50%:wrap")
			.current_dir(&issues_base)
			.stdin(Stdio::piped())
			.stdout(Stdio::piped());

		let mut child = cmd.spawn()?;

		if let Some(stdin) = child.stdin.as_mut() {
			for file in &filtered_list {
				writeln!(stdin, "{file}")?;
			}
		}

		let output = child.wait_with_output()?;

		if output.status.success() {
			let selected = String::from_utf8_lossy(&output.stdout).trim().to_string();
			if !selected.is_empty() {
				return Ok(issues_base.join(selected));
			}
		}

		bail!("No issue selected")
	}

	/// Check if a project is virtual (has no Github remote)
	pub fn is_virtual_project(repo_info: crate::RepoInfo) -> bool {
		Self::load_project_meta(repo_info.owner(), repo_info.repo()).virtual_project //TODO: update all occurences of `owner, repo` to just `repo_info` everywhere
	}

	/// Ensure a virtual project exists (creates if needed).
	pub fn ensure_virtual_project(owner: &str, repo: &str) -> Result<ProjectMeta> {
		let meta_path = Self::project_meta_path(owner, repo);
		if meta_path.exists() {
			let project_meta = Self::load_project_meta(owner, repo);
			if !project_meta.virtual_project {
				bail!("Project {owner}/{repo} exists but is not a virtual project");
			}
			Ok(project_meta)
		} else {
			let project_meta = ProjectMeta {
				virtual_project: true,
				next_virtual_issue_number: 1,
				issues: std::collections::BTreeMap::new(),
			};
			Self::save_project_meta(owner, repo, &project_meta)?;
			Ok(project_meta)
		}
	}

	/// Allocate the next issue number for a virtual project.
	pub fn allocate_virtual_issue_number(owner: &str, repo: &str) -> Result<u64> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		if !project_meta.virtual_project {
			bail!("Cannot allocate virtual issue number for non-virtual project {owner}/{repo}");
		}

		if project_meta.next_virtual_issue_number == 0 {
			project_meta.next_virtual_issue_number = 1;
		}

		let issue_number = project_meta.next_virtual_issue_number;
		project_meta.next_virtual_issue_number += 1;
		Self::save_project_meta(owner, repo, &project_meta)?;

		Ok(issue_number)
	}

	/// Get the metadata file path for a project
	fn project_meta_path(owner: &str, repo: &str) -> PathBuf {
		Self::project_dir(owner, repo).join(".meta.json")
	}

	/// Load project metadata, creating empty if not exists
	#[deprecated(
		note = "move `load_project_meta_from_reader` logic in here, force callers to use RepoInfo and pass FsReader themselves // 'deprecated' as in this interface, not function itself"
	)]
	pub fn load_project_meta(owner: &str, repo: &str) -> ProjectMeta {
		Self::load_project_meta_from_reader(RepoInfo::new(owner, repo), &FsReader)
	}

	/// Save project metadata
	fn save_project_meta(owner: &str, repo: &str, meta: &ProjectMeta) -> Result<()> {
		let meta_path = Self::project_meta_path(owner, repo);
		if let Some(parent) = meta_path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let content = serde_json::to_string_pretty(meta)?;
		std::fs::write(&meta_path, content)?;
		Ok(())
	}

	/// Load project metadata using the provided reader.
	#[instrument(skip(reader))]
	pub(crate) fn load_project_meta_from_reader<R: LocalReader>(repo_info: RepoInfo, reader: &R) -> ProjectMeta {
		let meta_path = Self::project_meta_path(repo_info.owner(), repo_info.repo());

		match reader.read_content(&meta_path) {
			Ok(c) => match serde_json::from_str(&c) {
				Ok(meta) => meta,
				Err(e) => panic!("corrupted project metadata at {}: {e}", meta_path.display()),
			},
			Err(e) if e.is_not_found() => ProjectMeta::default(),
			Err(e) => panic!("failed to read project metadata at {}: {e}", meta_path.display()),
		}
	}

	/// Load metadata for a specific issue using the provided reader.
	pub(crate) fn load_issue_meta_from_reader<R: LocalReader>(repo_info: RepoInfo, issue_number: u64, reader: &R) -> Option<IssueMeta> {
		let project_meta = Self::load_project_meta_from_reader(repo_info, reader);
		project_meta.issues.get(&issue_number).cloned()
	}

	/// Save metadata for a specific issue to the project's .meta.json.
	#[instrument]
	fn save_issue_meta(owner: &str, repo: &str, issue_number: u64, meta: &IssueMeta) -> Result<()> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		project_meta.issues.insert(issue_number, meta.clone());
		Self::save_project_meta(owner, repo, &project_meta)
	}

	/// Remove metadata for a specific issue from the project's .meta.json.
	#[instrument]
	fn remove_issue_meta(owner: &str, repo: &str, issue_number: u64) -> Result<()> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		if project_meta.issues.remove(&issue_number).is_some() {
			Self::save_project_meta(owner, repo, &project_meta)?;
		}
		Ok(())
	}
}

/// Exact match level for fzf queries.
#[derive(Clone, Copy, Debug, Default)]
pub enum ExactMatchLevel {
	#[default]
	Fuzzy,
	ExactTerms,
	RegexSubstring,
	RegexLine,
}
/// Project-level metadata file.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProjectMeta {
	#[serde(default)]
	pub virtual_project: bool,
	#[serde(default)]
	pub next_virtual_issue_number: u64,
	#[serde(default, skip_serializing_if = "std::collections::BTreeMap::is_empty")]
	pub issues: std::collections::BTreeMap<u64, IssueMeta>,
}
/// Per-issue metadata stored in .meta.json.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct IssueMeta {
	/// Timestamps for individual field changes.
	#[serde(default)]
	pub timestamps: crate::IssueTimestamps,
}
/// Marker type for loading from consensus (git HEAD).
/// Note: This is different from `impl_sink::Consensus` which is for writing.
pub enum LocalConsensus {}
mod reader;

pub use reader::{FsReader, GitReader, LocalReader, ReaderError};

mod local_path {
	use std::collections::VecDeque;

	use miette::SourceSpan;
	use tracing_error::SpanTrace;

	use super::*;

	/// Error type for LocalPath operations.
	///
	/// Contains the error kind (for matching), pre-rendered miette diagnostic,
	/// and a SpanTrace captured at error creation time.
	#[derive(Debug, thiserror::Error)]
	#[error("{rendered}\n\n{spantrace}")]
	pub struct LocalPathError {
		pub kind: LocalPathErrorKind,
		rendered: String,
		spantrace: SpanTrace,
	}

	impl LocalPathError {
		fn from_diagnostic(kind: LocalPathErrorKind, diag: LocalPathDiagnostic) -> Self {
			let rendered = format!("{:?}", miette::Report::new(diag));
			Self {
				kind,
				rendered,
				spantrace: SpanTrace::capture(),
			}
		}

		pub fn missing_parent(selector: IssueSelector, searched_path: PathBuf) -> Self {
			Self::from_diagnostic(LocalPathErrorKind::MissingParent, LocalPathDiagnostic::MissingParent { selector, searched_path })
		}

		pub fn not_found(selector: IssueSelector, searched_path: PathBuf) -> Self {
			Self::from_diagnostic(LocalPathErrorKind::NotFound, LocalPathDiagnostic::NotFound { selector, searched_path })
		}

		pub fn not_unique(selector: IssueSelector, searched_path: PathBuf, matching_paths: Vec<PathBuf>) -> Self {
			// Build source code display showing all matching paths
			let paths_display = matching_paths.iter().map(|p| p.display().to_string()).collect::<Vec<_>>().join("\n");
			let first_path_len = matching_paths.first().map(|p| p.display().to_string().len()).unwrap_or(0);

			Self::from_diagnostic(
				LocalPathErrorKind::NotUnique,
				LocalPathDiagnostic::NotUnique {
					selector,
					searched_path,
					paths_source: miette::NamedSource::new("matching files", paths_display),
					span: (0, first_path_len).into(),
					matching_paths,
				},
			)
		}

		pub fn reader(selector: IssueSelector, source: ReaderError) -> Self {
			Self::from_diagnostic(LocalPathErrorKind::Reader, LocalPathDiagnostic::Reader { selector, source })
		}
	}

	/// The kind of local path error (for pattern matching).
	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum LocalPathErrorKind {
		MissingParent,
		NotFound,
		NotUnique,
		Reader,
	}

	/// Internal miette diagnostic for nice error rendering with source highlighting.
	#[derive(Debug, miette::Diagnostic, thiserror::Error)]
	enum LocalPathDiagnostic {
		/// Parent issue not found at expected location.
		#[error("parent issue {selector:?} not found")]
		#[diagnostic(
			code(tedi::local::missing_parent),
			help("Fetch the parent issue first.\n  Searched in: {}", searched_path.display())
		)]
		MissingParent { selector: IssueSelector, searched_path: PathBuf },

		/// Issue file not found (search failed).
		#[error("issue file {selector:?} not found")]
		#[diagnostic(
			code(tedi::local::not_found),
			help("Searched in: {}", searched_path.display())
		)]
		NotFound { selector: IssueSelector, searched_path: PathBuf },

		/// Multiple files match the selector.
		#[error("multiple files match {selector:?}")]
		#[diagnostic(code(tedi::local::not_unique), help("Specify a more precise selector to disambiguate"))]
		NotUnique {
			selector: IssueSelector,
			searched_path: PathBuf,
			#[source_code]
			paths_source: miette::NamedSource<String>,
			#[label("conflicts with other matches below")]
			span: SourceSpan,
			matching_paths: Vec<PathBuf>,
		},

		/// Reader operation failed.
		#[error("while resolving {selector:?}")]
		#[diagnostic(code(tedi::local::reader))]
		Reader {
			selector: IssueSelector,
			#[source]
			source: ReaderError,
		},
	}

	/// Result of finding an entry matching a selector.
	#[derive(Clone, Debug)]
	pub(crate) enum FoundEntry {
		/// Found a directory with this name.
		Dir(String),
		/// Found a flat file with this full filename (including extension).
		File(String),
	}

	/// Find all entries (dirs or files) matching a selector in a directory.
	#[instrument(skip(reader), fields(parent = %parent.display()))]
	//TODO!!!!!!: rewrite the entire thing without pointless distinction between SoundEntry::Dir and FoundEntry::File, - just check once when deciding the whether the final one should be nested into /__main__.md and that's it
	pub(crate) fn find_all_entries_by_selector<R: LocalReader>(reader: &R, parent: &Path, selector: &IssueSelector) -> Result<Vec<FoundEntry>, ReaderError> {
		let entries = reader.list_dir(parent)?;

		let mut results = Vec::new();
		for name in &entries {
			/// Check if an entry name matches the selector.
			fn entry_matches_selector<R: LocalReader>(reader: &R, parent: &Path, name: &str, selector: &IssueSelector) -> Result<Option<FoundEntry>, ReaderError> {
				let entry_path = parent.join(name);
				let is_dir = reader.is_dir(&entry_path)?;

				match selector {
					IssueSelector::GitId(issue_number) => {
						let prefix = format!("{issue_number}_-_");

						if name.starts_with(&prefix) {
							match is_dir {
								true => return Ok(Some(FoundEntry::Dir(name.to_string()))),
								false => return Ok(Some(FoundEntry::File(name.to_string()))),
							}
						}
						Ok(None)
					}
					IssueSelector::Title(title) => {
						let sanitized = Local::sanitize_title(title.as_str());

						if name.contains(&sanitized) {
							match is_dir {
								true => return Ok(Some(FoundEntry::Dir(name.to_string()))),
								false => return Ok(Some(FoundEntry::File(name.to_string()))),
							}
						}
						Ok(None)
					}
				}
			}
			if let Some(entry) = entry_matches_selector(reader, parent, name, selector)? {
				results.push(entry);
			}
		}
		Ok(results)
	}

	/// On-demand path construction for local issue storage.
	///
	/// Use the builder pattern:
	/// - Reading: `local_path.resolve_parent(reader)?.search()?`
	/// - Writing: `local_path.resolve_parent(reader)?.deterministic(title, closed, has_children)`
	///
	/// r[local.path-reader-only]
	#[derive(Clone, Debug)]
	pub struct LocalPath {
		pub(crate) index: IssueIndex,
	}

	impl LocalPath {
		pub fn new(index: IssueIndex) -> Self {
			Self { index }
		}

		/// Resolve parent directory and prepare for final path resolution.
		///
		/// Consumes self and returns a `LocalPathResolved` that can be used to:
		/// - `search()`: find existing file matching the final selector
		/// - `deterministic(title, closed, has_children)`: construct target path for writes
		#[tracing::instrument(skip(reader))]
		pub fn resolve_parent<R: LocalReader>(self, reader: R) -> Result<LocalPathResolved<R>, LocalPathError> {
			let mut path = Local::project_dir(self.index.owner(), self.index.repo());

			let selectors = self.index.index();
			let (parent_selectors, remaining) = if selectors.is_empty() {
				(&[][..], VecDeque::new())
			} else {
				(&selectors[..selectors.len() - 1], VecDeque::from(vec![selectors.last().cloned().unwrap()]))
			};

			for selector in parent_selectors {
				let all_matches = find_all_entries_by_selector(&reader, &path, selector).map_err(|source| LocalPathError::reader(*selector, source))?;
				path = match all_matches.len() {
					0 => return Err(LocalPathError::missing_parent(*selector, path.clone())),
					n => {
						let matching_paths: Vec<PathBuf> = all_matches
							.iter()
							.map(|entry| {
								path.join(match entry {
									FoundEntry::Dir(name) => format!("{name}/"),
									//Q: don't like that this loses informatio; am I sure this is fine?
									FoundEntry::File(name) => format!("{}/", name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap()), //HACK: don't like that we're assuming to know how to work with extensions at this level
								})
							})
							.collect();
						match n {
							1 => matching_paths.into_iter().take(1).collect(),
							_ => return Err(LocalPathError::not_unique(*selector, path.clone(), matching_paths)),
						}
					}
				};
			}

			Ok(LocalPathResolved {
				resolved_path: path,
				unresolved_selector_nodes: remaining,
				reader,
			})
		}
	}

	impl From<IssueIndex> for LocalPath {
		fn from(index: IssueIndex) -> Self {
			Self::new(index)
		}
	}

	impl From<&Issue> for LocalPath {
		fn from(issue: &Issue) -> Self {
			Self::new(IssueIndex::from(issue))
		}
	}

	/// Resolved parent directory, ready for final path resolution.
	///
	/// Created by `LocalPath::resolve_parent()`. Use:
	/// - `search()` to find an existing file
	///	- `deterministic(...)` to construct a target path for writes
	///	Then call `path()` or `issue_dir()` to get the final `PathBuf`.
	#[derive(Clone, Debug)]
	pub struct LocalPathResolved<R: LocalReader> {
		resolved_path: PathBuf,
		unresolved_selector_nodes: VecDeque<IssueSelector>,
		reader: R,
	}

	impl<R: LocalReader> LocalPathResolved<R> {
		/// Search for an existing file matching the next selector.
		///
		/// Checks both flat and directory formats, open and closed states.
		/// Consumes one selector from the queue.
		///
		/// # Errors
		/// - `NotFound` if no matching entry exists
		/// - `NotUnique` if multiple entries match the selector
		/// - `Reader` if filesystem/git operations fail
		#[tracing::instrument(skip(self), fields(resolved_path = %self.resolved_path.display(), selector = ?self.unresolved_selector_nodes.front()))]
		pub fn search(mut self) -> Result<Self, LocalPathError> {
			let selector = self.unresolved_selector_nodes.pop_front().expect("Cannot search with empty selectors");
			let all_matches = find_all_entries_by_selector(&self.reader, &self.resolved_path, &selector).map_err(|source| LocalPathError::reader(selector, source))?;

			match all_matches.len() {
				0 => return Err(LocalPathError::not_found(selector, self.resolved_path.clone())),
				1 => {}
				_ => {
					let matching_paths = all_matches
						.iter()
						.map(|e| {
							self.resolved_path.join(match e {
								FoundEntry::Dir(name) | FoundEntry::File(name) => name,
							})
						})
						.collect();
					return Err(LocalPathError::not_unique(selector, self.resolved_path.clone(), matching_paths));
				}
			}

			let map_err = |source| LocalPathError::reader(selector, source);

			match &all_matches[0] {
				FoundEntry::Dir(name) => {
					let dir_path = self.resolved_path.join(name);
					let main_open = Local::main_file_path(&dir_path, false);
					if self.reader.exists(&main_open).map_err(map_err)? {
						self.resolved_path = main_open;
					} else {
						let main_closed = Local::main_file_path(&dir_path, true);
						if self.reader.exists(&main_closed).map_err(map_err)? {
							self.resolved_path = main_closed;
						} else {
							return Err(LocalPathError::not_found(selector, dir_path));
						}
					}
				}
				FoundEntry::File(name) => {
					self.resolved_path = self.resolved_path.join(name);
				}
			};
			Ok(self)
		}

		/// Construct the deterministic target path for writing.
		///
		/// Based on:
		/// - `title`: the issue title (used in filename)
		/// - `closed`: whether the issue is closed (affects .md vs .md.bak extension)
		/// - `has_children`: whether stored in directory format (affects __main__.md vs flat file)
		///
		/// Consumes one selector from the queue.
		/// Note: This does NOT create any directories. Use Sink to write.
		pub fn deterministic(mut self, title: &str, closed: bool, has_children: bool) -> Self {
			let selector = self
				.unresolved_selector_nodes
				.pop_front()
				.expect("implementation error, - this should only be called when we know it's not empty");

			let git_id = match selector {
				IssueSelector::GitId(n) => Some(n),
				_ => None,
			};
			if has_children {
				let dir_name = Local::issue_dir_name(git_id, title);
				let issue_dir = self.resolved_path.join(&dir_name);
				self.resolved_path = Local::main_file_path(&issue_dir, closed);
			} else {
				let filename = Local::format_issue_filename(git_id, title, closed);
				self.resolved_path = self.resolved_path.join(filename);
			};
			self
		}

		/// Get the resolved file path. Consumes self.
		///
		/// Panics if selectors queue is not empty.
		pub fn path(self) -> PathBuf {
			assert!(
				self.unresolved_selector_nodes.is_empty(),
				"path() called with remaining selectors: {:?}",
				self.unresolved_selector_nodes
			);
			self.resolved_path
		}

		/// Get the resolved issue directory (if issue is in directory format). Consumes self.
		///
		/// Returns `Some(dir)` if the issue is in directory format, `None` for flat files.
		/// Panics if selectors queue is not empty or if reader operations fail (indicates corrupted state).
		#[tracing::instrument(skip(self), fields(resolved_path = %self.resolved_path.display()))]
		pub fn issue_dir(self) -> Option<PathBuf> {
			assert!(
				self.unresolved_selector_nodes.is_empty(),
				"issue_dir() called with remaining selectors: {:?}",
				self.unresolved_selector_nodes
			);
			// These unwraps are intentional: if we successfully resolved the path via search(),
			// is_dir() failing indicates filesystem corruption - panic is appropriate.
			if self.reader.is_dir(&self.resolved_path).unwrap() {
				Some(self.resolved_path)
			} else {
				// Only return parent if this is a __main__.md file (directory format).
				// For flat files like `1_-_Title.md`, there is no issue directory.
				let is_main_file = self
					.resolved_path
					.file_name()
					.map(|n| n.to_string_lossy().starts_with(Local::MAIN_ISSUE_FILENAME))
					.unwrap_or(false);
				if is_main_file { self.resolved_path.parent().map(|p| p.to_path_buf()) } else { None }
			}
		}

		/// Get all paths matching the next selector (same logic as `search`).
		///
		/// Returns empty Vec if no matches.
		/// Returns file paths for flat files, `__main__.md` paths for directories.
		///
		/// Does not consume the selector from the queue.
		#[tracing::instrument(skip(self), fields(resolved_path = %self.resolved_path.display(), selector = ?self.unresolved_selector_nodes.front()))]
		pub fn matching_subpaths(&self) -> Result<Vec<PathBuf>, LocalPathError> {
			let Some(selector) = self.unresolved_selector_nodes.front() else {
				tracing::trace!("no selector in queue, returning empty");
				return Ok(Vec::new());
			};

			let map_err = |source| LocalPathError::reader(*selector, source);

			let all_matches = find_all_entries_by_selector(&self.reader, &self.resolved_path, selector).map_err(map_err)?;
			tracing::debug!(?all_matches, "find_all_entries_by_selector returned");

			let mut results = Vec::new();
			for entry in all_matches {
				match entry {
					FoundEntry::Dir(name) => {
						let dir_path = self.resolved_path.join(&name);
						let main_open = Local::main_file_path(&dir_path, false);
						if self.reader.exists(&main_open).map_err(map_err)? {
							tracing::trace!(path = %main_open.display(), "found open __main__.md in dir");
							results.push(main_open);
						} else {
							let main_closed = Local::main_file_path(&dir_path, true);
							if self.reader.exists(&main_closed).map_err(map_err)? {
								tracing::trace!(path = %main_closed.display(), "found closed __main__.md.bak in dir");
								results.push(main_closed);
							} else {
								tracing::warn!(dir = %dir_path.display(), "directory exists but has no __main__.md - skipping");
							}
						}
					}
					FoundEntry::File(name) => {
						let file_path = self.resolved_path.join(&name);
						tracing::trace!(path = %file_path.display(), "found flat file");
						results.push(file_path);
					}
				}
			}
			tracing::debug!(?results, "matching_subpaths returning");
			Ok(results)
		}
	}
}
pub use local_path::{LocalPath, LocalPathError, LocalPathErrorKind, LocalPathResolved};

impl<R: LocalReader> From<LocalPath> for LocalIssueSource<R>
where
	R: Default,
{
	fn from(local_path: LocalPath) -> Self {
		Self::new(local_path, R::default())
	}
}

impl<R: LocalReader> From<IssueIndex> for LocalIssueSource<R>
where
	R: Default,
{
	fn from(index: IssueIndex) -> Self {
		Self::new(LocalPath::new(index), R::default())
	}
}

mod impl_sink;

use std::path::{Path, PathBuf};

pub use impl_sink::{Consensus, Submitted};
//==============================================================================
// Error Types
//==============================================================================
use regex::Regex;
use serde::{Deserialize, Serialize};
use v_utils::prelude::*;

use crate::{Issue, IssueIndex, IssueSelector, RepoInfo};

//==============================================================================
// Local - The interface for local issue storage
//==============================================================================

//NB: the reason we expose methods through Local, - is to shortcut the possibility of having them appear without clear reference to what part of logic owns them.
// Note that conventionally, same effect is achieved by deciding to not export methods out of a module, but use them as `mod::method`, which would translate to almost tht exactly the same eg `local::issue_dir()`
// Difference is: when using AI, I can't control how the methods are exported. This tag solves it.

//==============================================================================
// Types
//==============================================================================

impl TryFrom<u8> for ExactMatchLevel {
	type Error = &'static str;

	fn try_from(count: u8) -> Result<Self, Self::Error> {
		match count {
			0 => Ok(Self::Fuzzy),
			1 => Ok(Self::ExactTerms),
			2 => Ok(Self::RegexSubstring),
			3 => Ok(Self::RegexLine),
			_ => Err("--exact / -e can be specified at most 3 times"),
		}
	}
}

//==============================================================================
// LazyIssue Implementation for LocalIssueSource<FsReader>
//==============================================================================

impl crate::LazyIssue<Local> for Issue {
	type Error = LocalError;
	type Source = LocalIssueSource<FsReader>;

	#[tracing::instrument(skip_all)]
	async fn parent_index(source: &Self::Source) -> Result<Option<crate::IssueIndex>, Self::Error> {
		// The source's IssueIndex IS the issue's index; parent_index is derived from it
		let index = source.index();
		Ok(index.parent())
	}

	#[tracing::instrument(skip_all)]
	async fn identity(&mut self, source: Self::Source) -> Result<crate::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let index = *source.index();

		if self.contents.title.is_empty() {
			let file_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.path();
			let content = source.reader.read_content(&file_path)?;
			let parsed = Local::parse_single_node(&content, index, &file_path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta_from_reader(index.repo_info(), issue_number, &source.reader)
			&& let Some(linked) = self.identity.mut_linked_issue_meta()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	#[tracing::instrument(skip(self, source))]
	async fn contents(&mut self, source: Self::Source) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let index = *source.index();
		let file_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.path();
		let content = source.reader.read_content(&file_path)?;
		let parsed = Local::parse_single_node(&content, index, &file_path)?;
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		Ok(self.contents.clone())
	}

	#[tracing::instrument(skip_all)]
	async fn children(&mut self, source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let dir_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.issue_dir();
		let Some(dir_path) = dir_path else {
			return Ok(Vec::new()); // Flat file, no children
		};

		let entries = source.reader.list_dir(&dir_path)?;
		let mut children = Vec::new();
		let this_index = *source.index();

		for name in entries {
			if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				continue;
			}

			let child_selector = match Local::parse_issue_selector_from_name(&name) {
				Some(sel) => sel,
				None => continue,
			};

			let child_index = this_index.child(child_selector);
			let child_source = source.child(child_index);

			let child = <Issue as crate::LazyIssue<Local>>::load(child_source).await?;
			children.push(child);
		}

		children.sort_by_key(|issue| issue.git_id().unwrap_or(0)); //IGNORED_ERROR: pending issues (no number) sort first

		self.children = children.clone();
		Ok(children)
	}

	#[tracing::instrument]
	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		// Check for unresolved conflicts (only for FsReader/Submitted, and only for root loads)
		conflict::check_conflict(source.index().owner())?;

		let parent_index = <Self as crate::LazyIssue<Local>>::parent_index(&source).await?.unwrap();
		let mut issue = Issue::empty_local(parent_index);
		<Self as crate::LazyIssue<Local>>::identity(&mut issue, source.clone()).await?;
		<Self as crate::LazyIssue<Local>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Self as crate::LazyIssue<Local>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}

//==============================================================================
// LazyIssue Implementation for LocalIssueSource<GitReader> (Consensus loading)
//==============================================================================

impl crate::LazyIssue<LocalConsensus> for Issue {
	type Error = LocalError;
	type Source = LocalIssueSource<GitReader>;

	async fn parent_index(source: &Self::Source) -> Result<Option<crate::IssueIndex>, Self::Error> {
		let index = source.index();
		Ok(index.parent())
	}

	#[instrument]
	async fn identity(&mut self, source: Self::Source) -> Result<crate::IssueIdentity, Self::Error> {
		tracing::debug!(is_linked = self.identity.is_linked(), "LazyIssue<LocalConsensus>::identity");
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let index = *source.index();
		tracing::debug!(?index, title_empty = self.contents.title.is_empty(), "LazyIssue<LocalConsensus>::identity check title");

		if self.contents.title.is_empty() {
			let search_result = source.local_path.clone().resolve_parent(source.reader).and_then(|r| r.search());
			tracing::debug!(?search_result, "LazyIssue<LocalConsensus>::identity search");
			let file_path = search_result?.path();
			let content = source.reader.read_content(&file_path)?;
			let parsed = Local::parse_single_node(&content, index, &file_path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta_from_reader(index.repo_info(), issue_number, &source.reader)
			&& let Some(linked) = self.identity.mut_linked_issue_meta()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	#[instrument]
	async fn contents(&mut self, source: Self::Source) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let index = *source.index();
		let file_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.path();
		let content = source.reader.read_content(&file_path)?;
		let parsed = Local::parse_single_node(&content, index, &file_path)?;
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		Ok(self.contents.clone())
	}

	#[instrument]
	async fn children(&mut self, source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let dir_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.issue_dir();
		let Some(dir_path) = dir_path else {
			return Ok(Vec::new()); // Flat file, no children
		};

		let entries = source.reader.list_dir(&dir_path)?;
		let mut children = Vec::new();
		let this_index = *source.index();

		for name in entries {
			if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				continue;
			}

			let child_selector = match Local::parse_issue_selector_from_name(&name) {
				Some(sel) => sel,
				None => continue,
			};

			let child_index = this_index.child(child_selector);
			let child_source = source.child(child_index);

			let child = <Issue as crate::LazyIssue<LocalConsensus>>::load(child_source).await?;
			children.push(child);
		}

		children.sort_by_key(|issue| issue.git_id().unwrap_or(0)); //IGNORED_ERROR: pending issues (no number) sort first

		self.children = children.clone();
		Ok(children)
	}

	#[instrument]
	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		// No conflict check for consensus loading (we're reading committed state)
		let parent_index = <Self as crate::LazyIssue<LocalConsensus>>::parent_index(&source).await?.unwrap();
		let mut issue = Issue::empty_local(parent_index);
		<Self as crate::LazyIssue<LocalConsensus>>::identity(&mut issue, source.clone()).await?;
		<Self as crate::LazyIssue<LocalConsensus>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Self as crate::LazyIssue<LocalConsensus>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}
