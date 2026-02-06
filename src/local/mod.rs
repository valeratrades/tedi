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

#![allow(unused_assignments)] // miette's derive macro triggers false positives

use HashMap;

pub mod conflict;
pub mod consensus;
/// Error type for local issue loading operations.
#[derive(Debug, thiserror::Error, derive_more::From)]
pub enum LocalError {
	/// Path resolution or IO error.
	#[error(transparent)]
	Io(LocalPathError),

	/// Failed to parse issue content.
	#[error(transparent)]
	Parse(crate::ParseError),

	/// Issue composition error.
	#[error(transparent)]
	Issue(crate::IssueError),

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

	/// Required executable not found.
	#[error("`{executable}` not found in PATH (required for {operation})")]
	MissingExecutable { executable: &'static str, operation: &'static str },

	/// Path extraction failed.
	#[error("failed to extract issue index from path: {0}")]
	#[from(skip)]
	PathExtraction(String),

	#[error(transparent)]
	Other(Report),
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
/// Use `build()` to construct with validation of required tools.
#[derive(Clone, Debug)]
pub struct LocalIssueSource<R: LocalReader> {
	pub local_path: LocalPath,
	pub reader: R,
}
impl<R: LocalReader> LocalIssueSource<R> {
	fn new(local_path: LocalPath, reader: R) -> Self {
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
	/// Build a source for reading from filesystem (submitted state).
	///
	/// Checks:
	/// - `fd` executable is available (for file searches)
	/// - No unresolved merge conflicts exist
	pub async fn build(local_path: LocalPath) -> Result<Self, LocalError> {
		// Check for fd
		if std::process::Command::new("fd").arg("--version").output().is_err() {
			return Err(LocalError::MissingExecutable {
				executable: "fd",
				operation: "local filesystem operations",
			});
		}

		// Check for unresolved conflicts (resolves if user already fixed markers)
		if let Some(conflict_file) = conflict::check_for_existing_conflict(local_path.index.into()).await.map_err(|e| LocalError::Other(e))? {
			return Err(ConflictBlockedError { conflict_file }.into());
		}

		Ok(Self::new(local_path, FsReader))
	}

	/// Build a source from a filesystem path by extracting the IssueIndex.
	///
	/// This extracts the parent_index from the path, then constructs the full index
	/// by adding the target issue's selector.
	pub async fn build_from_path(path: &Path) -> Result<Self, LocalError> {
		let index = Local::extract_index_from_path(path).map_err(|e| LocalError::PathExtraction(e.to_string()))?;
		Self::build(LocalPath::new(index)).await
	}
}
impl LocalIssueSource<GitReader> {
	/// Build a source for reading from git HEAD (consensus state).
	///
	/// Checks that `git` executable is available.
	pub fn build(local_path: LocalPath) -> Result<Self, LocalError> {
		// Check for git
		if std::process::Command::new("git").arg("--version").output().is_err() {
			return Err(LocalError::MissingExecutable {
				executable: "git",
				operation: "consensus state operations",
			});
		}

		Ok(Self::new(local_path, GitReader))
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

	/// Get the project directory path (where .meta.json lives).
	/// Structure: issues/{owner}/{repo}/
	pub fn project_dir(repo_info: RepoInfo) -> PathBuf {
		Self::issues_dir().join(repo_info.owner()).join(repo_info.repo())
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

	/// Format: {number}_-_{sanitized_title} or just {sanitized_title} if no number
	fn __issue_base_name(issue_number: Option<u64>, sanitized: &str) -> String {
		match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}"),
			Some(num) => format!("{num}_-_{sanitized}"),
			None if sanitized.is_empty() => "untitled".to_string(),
			None => sanitized.to_string(),
		}
	}

	/// Format an issue filename from number and title.
	/// Format: {number}_-_{sanitized_title}.md[.bak]
	fn format_issue_filename(issue_number: Option<u64>, title: &str, closed: bool) -> String {
		let sanitized = Self::sanitize_title(title);
		let base = format!("{}.md", Self::__issue_base_name(issue_number, &sanitized));
		if closed { format!("{base}.bak") } else { base }
	}

	/// Get the directory name for an issue (used when it has sub-issues).
	/// Format: {number}_-_{sanitized_title}[.bak]
	pub fn issue_dir_name(issue_number: Option<u64>, title: &str, closed: bool) -> String {
		let sanitized = Self::sanitize_title(title);
		let base = Self::__issue_base_name(issue_number, &sanitized);
		if closed { format!("{base}.bak") } else { base }
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

		Ok(IssueIndex::with_index(RepoInfo::new(owner, repo), selectors))
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
	pub fn is_virtual_project(repo_info: RepoInfo) -> bool {
		Self::load_project_meta(repo_info).virtual_project
	}

	/// Ensure a virtual project exists (creates if needed).
	pub fn ensure_virtual_project(repo_info: RepoInfo) -> Result<ProjectMeta> {
		let meta_path = Self::project_meta_path(repo_info);
		if meta_path.exists() {
			let project_meta = Self::load_project_meta(repo_info);
			if !project_meta.virtual_project {
				bail!("Project {}/{} exists but is not a virtual project", repo_info.owner(), repo_info.repo());
			}
			Ok(project_meta)
		} else {
			let project_meta = ProjectMeta {
				virtual_project: true,
				next_virtual_issue_number: 1,
				issues: std::collections::BTreeMap::new(),
			};
			Self::save_project_meta(repo_info, &project_meta)?;
			Ok(project_meta)
		}
	}

	/// Allocate the next issue number for a virtual project.
	pub fn allocate_virtual_issue_number(repo_info: RepoInfo) -> Result<u64> {
		let mut project_meta = Self::load_project_meta(repo_info);
		if !project_meta.virtual_project {
			bail!("Cannot allocate virtual issue number for non-virtual project {}/{}", repo_info.owner(), repo_info.repo());
		}

		if project_meta.next_virtual_issue_number == 0 {
			project_meta.next_virtual_issue_number = 1;
		}

		let issue_number = project_meta.next_virtual_issue_number;
		project_meta.next_virtual_issue_number += 1;
		Self::save_project_meta(repo_info, &project_meta)?;

		Ok(issue_number)
	}

	/// Get the metadata file path for a project
	fn project_meta_path(repo_info: RepoInfo) -> PathBuf {
		Self::project_dir(repo_info).join(".meta.json")
	}

	/// Load project metadata, creating empty if not exists
	pub fn load_project_meta(repo_info: RepoInfo) -> ProjectMeta {
		Self::load_project_meta_from_reader(repo_info, &FsReader)
	}

	/// Save project metadata
	fn save_project_meta(repo_info: RepoInfo, meta: &ProjectMeta) -> Result<()> {
		let meta_path = Self::project_meta_path(repo_info);
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
		let meta_path = Self::project_meta_path(repo_info);

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
	pub fn save_issue_meta(repo_info: RepoInfo, issue_number: u64, meta: &IssueMeta) -> Result<()> {
		let mut project_meta = Self::load_project_meta(repo_info);
		project_meta.issues.insert(issue_number, meta.clone());
		Self::save_project_meta(repo_info, &project_meta)
	}

	/// Remove metadata for a specific issue from the project's .meta.json.
	#[instrument]
	fn remove_issue_meta(repo_info: RepoInfo, issue_number: u64) -> Result<()> {
		let mut project_meta = Self::load_project_meta(repo_info);
		if project_meta.issues.remove(&issue_number).is_some() {
			Self::save_project_meta(repo_info, &project_meta)?;
		}
		Ok(())
	}

	/// Reconstruct a `HollowIssue` from project metadata and filesystem tree.
	///
	/// Traverses the directory tree for the issue at `idx`, parses child filenames
	/// into `IssueSelector`s, and for each `GitId` child looks up its `IssueMeta`
	/// in `ProjectMeta` to build the `LinkedIssueMeta`.
	pub fn read_hollow_from_project_meta(idx: IssueIndex) -> Result<crate::HollowIssue, LocalError> {
		let repo_info = idx.repo_info();
		let project_meta = Self::load_project_meta(repo_info);

		let parent_is_git_id = idx.index().last().map_or(false, |s| matches!(s, IssueSelector::GitId(_)));

		// Build this node's remote from project_meta
		let remote = if let Some(IssueSelector::GitId(n)) = idx.index().last() {
			let issue_meta = project_meta.issues.get(n);
			let user = issue_meta.and_then(|m| m.user.clone()).unwrap_or_default();
			let timestamps = issue_meta.map(|m| m.timestamps.clone()).unwrap_or_default();
			let link = crate::IssueLink::parse(&format!("https://github.com/{}/{}/issues/{n}", repo_info.owner(), repo_info.repo())).expect("constructed link must be valid");
			Some(Box::new(crate::LinkedIssueMeta::new(user, link, timestamps)))
		} else {
			None
		};

		// Resolve the issue directory to find children
		let local_path = LocalPath::new(idx);
		let issue_dir = match local_path.resolve_parent(FsReader) {
			Ok(resolved) => resolved.search().ok().and_then(|r| r.issue_dir()),
			Err(_) => None,
		};

		let mut children = HashMap::new();
		if let Some(dir_path) = issue_dir {
			let entries = FsReader.list_dir(&dir_path)?;
			for name in entries {
				if name.starts_with(Self::MAIN_ISSUE_FILENAME) {
					continue;
				}
				let Some(child_selector) = Self::parse_issue_selector_from_name(&name) else {
					continue;
				};

				// If child is GitId but parent isn't, that's an error
				if matches!(child_selector, IssueSelector::GitId(_)) && !parent_is_git_id {
					return Err(LocalError::Other(eyre!("child issue {child_selector:?} has GitId but parent {:?} does not", idx.index().last())));
				}

				let child_idx = idx.child(child_selector);
				let child_hollow = Self::read_hollow_from_project_meta(child_idx)?;
				children.insert(child_selector, child_hollow);
			}
		}

		Ok(crate::HollowIssue::new(remote, children))
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
	/// User who created the issue.
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub user: Option<String>,
	/// Timestamps for individual field changes.
	#[serde(default)]
	pub timestamps: crate::IssueTimestamps,
}
mod reader;

pub use reader::{FsReader, GitReader, LocalReader, ReaderError, ReaderErrorKind};

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

		pub fn parent_is_flat(selector: IssueSelector, parent_file: PathBuf, source: ReaderError) -> Self {
			let path_str = parent_file.display().to_string();
			let last_component = parent_file.file_name().map(|s| s.to_string_lossy().to_string()).unwrap_or_default();
			let span_start = path_str.len().saturating_sub(last_component.len());
			let span_len = last_component.len();

			Self::from_diagnostic(
				LocalPathErrorKind::ParentIsFlat,
				LocalPathDiagnostic::ParentIsFlat {
					selector,
					path_source: miette::NamedSource::new("parent path", path_str),
					span: (span_start, span_len).into(),
					source,
				},
			)
		}
	}

	/// The kind of local path error (for pattern matching).
	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum LocalPathErrorKind {
		MissingParent,
		NotFound,
		NotUnique,
		/// Attempted to search for children under a flat file (not a directory).
		ParentIsFlat,
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

		/// Attempted to search for children under a flat file.
		#[error("cannot search for {selector:?} - parent is a flat file, not a directory")]
		#[diagnostic(
			code(tedi::local::parent_is_flat),
			help("The parent issue exists as a flat file. It will be converted to directory format when a sub-issue is added.")
		)]
		ParentIsFlat {
			selector: IssueSelector,
			#[source_code]
			path_source: miette::NamedSource<String>,
			#[label("this is a flat file, cannot contain children")]
			span: SourceSpan,
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

				let matches = match selector {
					IssueSelector::GitId(issue_number) => {
						let prefix = format!("{issue_number}_-_");
						name.starts_with(&prefix)
					}
					IssueSelector::Title(title) => {
						let sanitized = Local::sanitize_title(title.as_str());
						name.contains(&sanitized)
					}
					IssueSelector::Regex(pattern) => {
						let base = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap_or(name);
						regex::Regex::new(pattern.as_str())
							.map(|re| re.is_match(base))
							.unwrap_or_else(|_| base.contains(pattern.as_str()))
					}
				};

				if matches {
					return Ok(Some(if is_dir { FoundEntry::Dir(name.to_string()) } else { FoundEntry::File(name.to_string()) }));
				}
				Ok(None)
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
			let mut path = Local::project_dir(self.index.repo_info());

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
									FoundEntry::File(name) => format!("{}/", name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap()),
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
		/// - `ParentIsFlat` if parent path is a flat file (not a directory)
		/// - `Reader` if other filesystem/git operations fail
		#[tracing::instrument(skip(self), fields(resolved_path = %self.resolved_path.display(), selector = ?self.unresolved_selector_nodes.front()))]
		pub fn search(mut self) -> Result<Self, LocalPathError> {
			let selector = self.unresolved_selector_nodes.pop_front().expect("Cannot search with empty selectors");
			let all_matches = find_all_entries_by_selector(&self.reader, &self.resolved_path, &selector).map_err(|source| {
				// Check if the resolved_path is actually a flat file (parent was matched as file, not dir)
				// The resolved_path looks like "foo/99_-_parent/" but actual file is "foo/99_-_parent.md"
				let path_str = self.resolved_path.to_string_lossy();
				let potential_flat_file = if let Some(stripped) = path_str.strip_suffix('/') {
					PathBuf::from(format!("{stripped}.md"))
				} else {
					PathBuf::from(format!("{path_str}.md"))
				};

				if source.kind == super::ReaderErrorKind::NotFound && self.reader.exists(&potential_flat_file).unwrap_or(false) {
					LocalPathError::parent_is_flat(selector, potential_flat_file, source)
				} else if source.kind == super::ReaderErrorKind::NotADirectory {
					LocalPathError::parent_is_flat(selector, self.resolved_path.clone(), source)
				} else {
					LocalPathError::reader(selector, source)
				}
			})?;

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
				let dir_name = Local::issue_dir_name(git_id, title, closed);
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

mod fs_sink;

use std::path::{Path, PathBuf};

pub use consensus::Consensus;
pub use fs_sink::LocalFs;
//==============================================================================
// Error Types
//==============================================================================
use regex::Regex;
use serde::{Deserialize, Serialize};
use v_utils::prelude::*;

use crate::{Issue, IssueIndex, IssueSelector, RepoInfo, local::conflict::ConflictBlockedError};

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
// LazyIssue Implementation for LocalIssueSource<R: LocalReader>
//==============================================================================

impl<R: LocalReader> crate::LazyIssue<LocalIssueSource<R>> for Issue {
	type Error = LocalError;

	#[tracing::instrument(skip_all)]
	async fn parent_index(source: &LocalIssueSource<R>) -> Result<Option<crate::IssueIndex>, Self::Error> {
		let index = source.index();
		Ok(index.parent())
	}

	#[tracing::instrument(skip_all)]
	async fn identity(&mut self, source: LocalIssueSource<R>) -> Result<crate::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let index = *source.index();

		if self.contents.title.is_empty() {
			let file_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.path();
			let content = source.reader.read_content(&file_path)?;
			let parsed = crate::VirtualIssue::parse(&content, file_path)?;
			self.contents = parsed.contents;
			self.identity.parent_index = index.parent().unwrap_or_else(|| IssueIndex::repo_only(index.repo_info()));
			self.identity.is_virtual = Local::is_virtual_project(index.repo_info());
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
	async fn contents(&mut self, source: LocalIssueSource<R>) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let index = *source.index();
		let file_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.path();
		let content = source.reader.read_content(&file_path)?;
		let parsed = crate::VirtualIssue::parse(&content, file_path)?;
		self.contents = parsed.contents;
		self.identity.parent_index = index.parent().unwrap_or_else(|| IssueIndex::repo_only(index.repo_info()));
		self.identity.is_virtual = Local::is_virtual_project(index.repo_info());

		Ok(self.contents.clone())
	}

	#[tracing::instrument(skip_all)]
	async fn children(&mut self, source: LocalIssueSource<R>) -> Result<HashMap<IssueSelector, Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let dir_path = source.local_path.clone().resolve_parent(source.reader)?.search()?.issue_dir();
		let Some(dir_path) = dir_path else {
			return Ok(HashMap::new()); // Flat file, no children
		};

		let entries = source.reader.list_dir(&dir_path)?;
		let mut children = HashMap::new();
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

			let child = Issue::load(child_source).await?;
			children.insert(child.selector(), child);
		}

		self.children = children.clone();
		Ok(children)
	}

	// Uses default load() impl from LazyIssue trait
}
