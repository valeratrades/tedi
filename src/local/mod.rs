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
#[derive(Debug, miette::Diagnostic, thiserror::Error, derive_more::From)]
pub enum LocalError {
	/// File not found at the expected path.
	#[error("issue file not found: {path}")]
	FileNotFound { path: PathBuf },

	/// Failed to read file contents.
	#[error("failed to read file: {path}")]
	ReadError {
		path: PathBuf,
		#[source]
		source: std::io::Error,
	},

	/// Failed to parse issue content.
	#[error("failed to parse issue file: {path}")]
	ParseError {
		path: PathBuf,
		#[source]
		source: Box<crate::ParseError>,
	},

	/// Git operation failed (for consensus reads).
	#[error("git operation failed: {message}")]
	GitError { message: String },

	/// Unresolved merge conflict blocks operation.
	#[error(transparent)]
	#[diagnostic(transparent)]
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

//==============================================================================
// LocalReader - Abstraction for reading from different sources
//==============================================================================

/// Trait for reading content from different sources (filesystem or git).
///
/// Separates the read abstraction from path computation.
pub trait LocalReader: Clone {
	/// Read file content at the given path.
	fn read_content(&self, path: &Path) -> Option<String>;
	/// List directory entries at the given path.
	fn list_dir(&self, path: &Path) -> Vec<String>;
	/// Check if the path is a directory.
	fn is_dir(&self, path: &Path) -> bool;
	/// Check if the path exists.
	fn exists(&self, path: &Path) -> bool;
}

/// Reader that reads from the filesystem (submitted/current state).
#[derive(Clone, Copy, Debug, Default)]
pub struct FsReader;

impl LocalReader for FsReader {
	fn read_content(&self, path: &Path) -> Option<String> {
		std::fs::read_to_string(path).ok()
	}

	fn list_dir(&self, path: &Path) -> Vec<String> {
		let Ok(entries) = std::fs::read_dir(path) else {
			return Vec::new();
		};
		entries.flatten().filter_map(|e| e.file_name().to_str().map(|s| s.to_string())).collect()
	}

	fn is_dir(&self, path: &Path) -> bool {
		path.is_dir()
	}

	fn exists(&self, path: &Path) -> bool {
		path.exists()
	}
}

/// Reader that reads from git HEAD (consensus state).
#[derive(Clone, Copy, Debug, Default)]
pub struct GitReader;

impl LocalReader for GitReader {
	fn read_content(&self, path: &Path) -> Option<String> {
		Local::read_git_content(path)
	}

	fn list_dir(&self, path: &Path) -> Vec<String> {
		Local::list_git_entries(path)
	}

	fn is_dir(&self, path: &Path) -> bool {
		Local::is_git_dir(path)
	}

	fn exists(&self, path: &Path) -> bool {
		self.read_content(path).is_some() || self.is_dir(path)
	}
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
			reader: self.reader.clone(),
		}
	}

	/// Get the IssueIndex from the underlying LocalPath.
	pub fn index(&self) -> &IssueIndex {
		self.local_path.index()
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

/// On-demand path construction for local issue storage.
///
/// r[local.sink-only-mutation]
/// This type computes paths but does NOT create directories or files.
/// Only `Sink<Submitted>` and `Sink<Consensus>` may mutate the filesystem.
#[derive(Clone, Debug)]
pub struct LocalPath {
	index: IssueIndex,
	/// Cached directory names for ancestors (computed lazily on first path request).
	/// None means not yet computed.
	ancestor_dir_names: Option<Vec<String>>,
}
impl LocalPath {
	pub fn new(index: IssueIndex) -> Self {
		Self { index, ancestor_dir_names: None }
	}

	pub fn index(&self) -> &IssueIndex {
		&self.index
	}

	/// Compute the file path for this issue.
	///
	/// Returns the path where the issue file should be located based on:
	/// - `closed`: whether the issue is closed (affects .md vs .md.bak extension)
	/// - `has_children`: whether stored in directory format (affects __main__.md vs flat file)
	/// - `title`: the issue title (used in filename)
	///
	/// Note: This does NOT create any directories. Use Sink to write.
	pub fn file_path(&mut self, title: &str, closed: bool, has_children: bool) -> Result<PathBuf, color_eyre::Report> {
		// Clone index data before mutable borrow for caching
		let owner = self.index.owner().to_string();
		let repo = self.index.repo().to_string();
		let issue_number = self.index.issue_number();

		let ancestor_dir_names = self.resolve_ancestor_dir_names()?;

		let mut path = Local::project_dir(&owner, &repo);
		for dir_name in ancestor_dir_names {
			path = path.join(dir_name);
		}

		if has_children {
			let dir_name = Local::issue_dir_name(issue_number, title);
			path = path.join(dir_name);
			Ok(Local::main_file_path(&path, closed))
		} else {
			let filename = Local::format_issue_filename(issue_number, title, closed);
			Ok(path.join(filename))
		}
	}

	/// Compute the directory path for this issue (where children would live).
	pub fn dir_path(&mut self, title: &str) -> Result<PathBuf, color_eyre::Report> {
		// Clone index data before mutable borrow for caching
		let owner = self.index.owner().to_string();
		let repo = self.index.repo().to_string();
		let issue_number = self.index.issue_number();

		let ancestor_dir_names = self.resolve_ancestor_dir_names()?;

		let mut path = Local::project_dir(&owner, &repo);
		for dir_name in ancestor_dir_names {
			path = path.join(dir_name);
		}

		let dir_name = Local::issue_dir_name(issue_number, title);
		Ok(path.join(dir_name))
	}

	/// Find the actual file path for this issue using the reader.
	///
	/// Searches for the issue file by number, checking both flat and directory formats,
	/// and both open and closed states. Uses the provided reader to check existence.
	pub fn find_file_path<R: LocalReader>(&mut self, reader: &R) -> Result<PathBuf, LocalError> {
		let repo_info = self.index.repo_info();
		let num_path = self.index.num_path();

		Local::find_issue_file_by_num_path_with_reader(repo_info, &num_path, reader).ok_or_else(|| {
			// Construct a descriptive path for the error
			let base_path = Local::project_dir(repo_info.owner(), repo_info.repo());
			let num_path_str = num_path.iter().map(|n| n.to_string()).collect::<Vec<_>>().join("/");
			LocalError::FileNotFound { path: base_path.join(num_path_str) }
		})
	}

	/// Find the directory path for this issue if it exists (has children).
	///
	/// Returns `Some(path)` if the issue is stored in directory format, `None` otherwise.
	pub fn find_dir_path<R: LocalReader>(&mut self, reader: &R) -> Option<PathBuf> {
		let repo_info = self.index.repo_info();
		let num_path = self.index.num_path();

		Local::find_issue_dir_by_num_path_with_reader(repo_info, &num_path, reader)
	}

	/// Resolve ancestor directory names by traversing filesystem.
	/// Caches result for subsequent calls.
	pub(crate) fn resolve_ancestor_dir_names(&mut self) -> Result<&Vec<String>, color_eyre::Report> {
		if self.ancestor_dir_names.is_none() {
			let names = self.compute_ancestor_dir_names()?;
			self.ancestor_dir_names = Some(names);
		}
		Ok(self.ancestor_dir_names.as_ref().unwrap())
	}

	/// Find directory names for each ancestor by traversing filesystem.
	fn compute_ancestor_dir_names(&self) -> Result<Vec<String>, color_eyre::Report> {
		let mut path = Local::project_dir(self.index.owner(), self.index.repo());

		if !path.exists() {
			color_eyre::eyre::bail!("Project directory does not exist: {}", path.display());
		}

		let parent_nums = self.index.parent_nums();
		let mut result = Vec::with_capacity(parent_nums.len());

		for issue_number in parent_nums {
			let dir_name = Local::find_issue_dir_name_by_number(&path, issue_number)
				.ok_or_else(|| color_eyre::eyre::eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

			path = path.join(&dir_name);
			result.push(dir_name);
		}

		Ok(result)
	}

	/// Ensure parent directories exist, converting flat files to directory format as needed.
	///
	/// This is called by Sink before writing an issue file and by `modify_and_sync_issue`
	/// before opening the editor. It handles:
	/// 1. Creating the project directory if needed
	/// 2. Converting any parent flat files to directory format
	///
	/// Returns the ancestor directory names after ensuring they exist as directories.
	pub fn ensure_parent_dirs(&mut self) -> Result<Vec<String>, color_eyre::Report> {
		let mut path = Local::project_dir(self.index.owner(), self.index.repo());

		if !path.exists() {
			std::fs::create_dir_all(&path)?;
		}

		let parent_nums = self.index.parent_nums();
		let mut result = Vec::with_capacity(parent_nums.len());

		for issue_number in parent_nums {
			let dir_name = Local::find_issue_dir_name_by_number(&path, issue_number)
				.ok_or_else(|| color_eyre::eyre::eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

			let parent_dir_path = path.join(&dir_name);

			// If parent exists as flat file, convert to directory
			if !parent_dir_path.is_dir() {
				let flat_open = path.join(format!("{dir_name}.md"));
				let flat_closed = path.join(format!("{dir_name}.md.bak"));

				std::fs::create_dir_all(&parent_dir_path)?;

				// Move flat file to __main__.md inside the directory
				if flat_closed.exists() {
					let main_path = Local::main_file_path(&parent_dir_path, true);
					std::fs::rename(&flat_closed, &main_path)?;
				} else if flat_open.exists() {
					let main_path = Local::main_file_path(&parent_dir_path, false);
					std::fs::rename(&flat_open, &main_path)?;
				}
			}

			path = parent_dir_path;
			result.push(dir_name);
		}

		// Cache the result
		self.ancestor_dir_names = Some(result.clone());
		Ok(result)
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
	/// Returns a path in `/tmp/{CARGO_PKG_NAME}/{owner/repo/lineage...}/{fname}.md` where:
	/// - lineage is the parent issue numbers joined by `/`
	/// - fname is the proper issue filename based on number and title
	///
	/// This path is used when opening an editor for the user to edit the issue.
	pub fn virtual_edit_path(issue: &crate::Issue) -> PathBuf {
		let mut path = PathBuf::from("/tmp").join(env!("CARGO_PKG_NAME"));

		// Add owner/repo
		path = path.join(issue.identity.owner()).join(issue.identity.repo());

		// Add lineage components (parent issue numbers)
		for parent_num in issue.identity.lineage() {
			path = path.join(parent_num.to_string());
		}

		// Add the issue filename
		let fname = Self::format_issue_filename(issue.number(), &issue.contents.title, false);
		path.join(fname)
	}

	/// Returns the base directory for issue storage: XDG_DATA_HOME/todo/issues/
	pub fn issues_dir() -> PathBuf {
		v_utils::xdg_data_dir!("issues")
	}

	/// Read file content from git HEAD.
	pub(crate) fn read_git_content(file_path: &Path) -> Option<String> {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let data_dir_str = data_dir.to_str()?;
		let rel_path = file_path.strip_prefix(&data_dir).ok()?;
		let rel_path_str = rel_path.to_str()?;

		// Check if git is initialized
		let git_check = Command::new("git").args(["-C", data_dir_str, "rev-parse", "--git-dir"]).output().ok()?;
		if !git_check.status.success() {
			return None;
		}

		// Check if file is tracked
		let ls_output = Command::new("git").args(["-C", data_dir_str, "ls-files", rel_path_str]).output().ok()?;
		if !ls_output.status.success() || ls_output.stdout.is_empty() {
			return None;
		}

		// Read from HEAD
		let output = Command::new("git").args(["-C", data_dir_str, "show", &format!("HEAD:./{rel_path_str}")]).output().ok()?;

		if !output.status.success() {
			return None;
		}

		String::from_utf8(output.stdout).ok()
	}

	/// List directory entries from git HEAD.
	pub(crate) fn list_git_entries(dir: &Path) -> Vec<String> {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let Some(data_dir_str) = data_dir.to_str() else {
			return Vec::new();
		};
		let Some(rel_dir) = dir.strip_prefix(&data_dir).ok() else {
			return Vec::new();
		};
		let Some(rel_dir_str) = rel_dir.to_str() else {
			return Vec::new();
		};

		let output = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "--name-only", "HEAD", &format!("{rel_dir_str}/")])
			.output();

		let Ok(output) = output else {
			return Vec::new();
		};
		if !output.status.success() {
			return Vec::new();
		}

		let prefix = format!("{rel_dir_str}/");
		std::str::from_utf8(&output.stdout)
			.expect("git output must be valid UTF-8")
			.lines()
			.filter_map(|line| line.strip_prefix(&prefix))
			.map(|s| s.to_string())
			.collect()
	}

	/// Check if a path is a directory in git.
	pub(crate) fn is_git_dir(dir: &Path) -> bool {
		use std::process::Command;

		let data_dir = Self::issues_dir();
		let Some(data_dir_str) = data_dir.to_str() else {
			return false;
		};
		let Some(rel_dir) = dir.strip_prefix(&data_dir).ok() else {
			return false;
		};
		let Some(rel_dir_str) = rel_dir.to_str() else {
			return false;
		};

		let check = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "HEAD", &format!("{rel_dir_str}/")])
			.output()
			.expect("failed to execute git command");
		check.status.success() && !check.stdout.is_empty()
	}

	/// Parse a single issue node from filesystem content.
	///
	/// This parses one issue file (without loading children from separate files).
	/// Children field will be empty - they're loaded separately via LazyIssue.
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
	pub fn issue_dir_name(issue_number: Option<u64>, title: &str) -> String {
		let sanitized = Self::sanitize_title(title);
		match issue_number {
			Some(num) if sanitized.is_empty() => format!("{num}"),
			Some(num) => format!("{num}_-_{sanitized}"),
			None if sanitized.is_empty() => "untitled".to_string(),
			None => sanitized,
		}
	}

	/// Get the path for an issue file from an Issue.
	/// Uses the issue's identity to resolve the path.
	///
	/// Checks if the issue exists in directory format on disk (has sub-issues) and returns
	/// the appropriate path (`__main__.md` for directory format, flat file otherwise).
	///
	/// # Errors
	/// Returns an error if ancestor directories can't be resolved.
	pub fn issue_file_path(issue: &Issue) -> Result<PathBuf> {
		let repo_info = issue.identity.repo_info();
		let lineage = issue.identity.lineage();
		let ancestor_dir_names = Self::build_ancestor_dir_names_from_lineage(repo_info, &lineage)?;
		let closed = issue.contents.state.is_closed();

		// Check if issue exists in directory format on disk
		let issue_dir = Self::issue_dir_path_from_dir_names(repo_info.owner(), repo_info.repo(), issue.number(), &issue.contents.title, &ancestor_dir_names);
		if issue_dir.is_dir() {
			return Ok(Self::main_file_path(&issue_dir, closed));
		}

		// Flat file format
		let mut path = Self::project_dir(repo_info.owner(), repo_info.repo());
		for dir_name in &ancestor_dir_names {
			path = path.join(dir_name);
		}
		let filename = Self::format_issue_filename(issue.number(), &issue.contents.title, closed);
		Ok(path.join(filename))
	}

	/// Get the path to the issue directory using ancestor directory names directly.
	pub fn issue_dir_path_from_dir_names(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestor_dir_names: &[String]) -> PathBuf {
		let mut path = Self::project_dir(owner, repo);

		for dir_name in ancestor_dir_names {
			path = path.join(dir_name);
		}

		path.join(Self::issue_dir_name(issue_number, title))
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

	/// Find the actual file path for an issue, checking both flat and directory formats.
	pub fn find_issue_file(owner: &str, repo: &str, issue_number: Option<u64>, title: &str, ancestor_dir_names: &[String]) -> Option<PathBuf> {
		// Build base path: project_dir / ancestor_dir_names...
		let mut base_path = Self::project_dir(owner, repo);
		for dir_name in ancestor_dir_names {
			base_path = base_path.join(dir_name);
		}

		// Try flat format first (both open and closed)
		let flat_path = base_path.join(Self::format_issue_filename(issue_number, title, false));
		if flat_path.exists() {
			return Some(flat_path);
		}

		let flat_closed_path = base_path.join(Self::format_issue_filename(issue_number, title, true));
		if flat_closed_path.exists() {
			return Some(flat_closed_path);
		}

		// Try directory format
		let issue_dir = Self::issue_dir_path_from_dir_names(owner, repo, issue_number, title, ancestor_dir_names);
		if issue_dir.is_dir() {
			let main_path = Self::main_file_path(&issue_dir, false);
			if main_path.exists() {
				return Some(main_path);
			}

			let main_closed_path = Self::main_file_path(&issue_dir, true);
			if main_closed_path.exists() {
				return Some(main_closed_path);
			}
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

	/// Build ancestor directory names by traversing the filesystem for a lineage.
	/// Finds issues by number (either flat file or directory) and returns the directory names.
	pub fn build_ancestor_dir_names_from_lineage(repo_info: RepoInfo, lineage: &[u64]) -> Result<Vec<String>> {
		let mut path = Self::project_dir(repo_info.owner(), repo_info.repo());

		if !path.exists() {
			bail!("Project directory does not exist: {}", path.display());
		}

		let mut result = Vec::with_capacity(lineage.len());

		for &issue_number in lineage {
			let dir_name = Self::find_issue_dir_name_by_number(&path, issue_number)
				.ok_or_else(|| eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

			path = path.join(&dir_name);
			result.push(dir_name);
		}

		Ok(result)
	}

	/// Find the issue file by repo info and full number path.
	/// The num_path includes all ancestors plus the target issue's own number.
	pub fn find_issue_file_by_num_path(repo_info: RepoInfo, num_path: &[u64]) -> Option<PathBuf> {
		let mut path = Self::project_dir(repo_info.owner(), repo_info.repo());

		if !path.exists() {
			return None;
		}

		if num_path.is_empty() {
			return None;
		}

		// Navigate to parent directories
		for &issue_number in &num_path[..num_path.len() - 1] {
			let dir = Self::find_issue_dir_by_number(&path, issue_number)?;
			path = dir;
		}

		// Find the target issue (last in num_path)
		let target_number = *num_path.last().unwrap();

		// Check for directory format first (issue with children)
		if let Some(dir) = Self::find_issue_dir_by_number(&path, target_number) {
			let main_file = Self::main_file_path(&dir, false);
			if main_file.exists() {
				return Some(main_file);
			}
			let main_bak = Self::main_file_path(&dir, true);
			if main_bak.exists() {
				return Some(main_bak);
			}
		}

		// Check for flat file format
		let entries = std::fs::read_dir(&path).ok()?;
		let prefix_with_sep = format!("{target_number}_-_");
		let exact_match = format!("{target_number}.md");
		let exact_match_bak = format!("{target_number}.md.bak");

		for entry in entries.flatten() {
			let entry_path = entry.path();
			if !entry_path.is_file() {
				continue;
			}

			let Some(name) = entry_path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if name.starts_with(&prefix_with_sep) && (name.ends_with(".md") || name.ends_with(".md.bak")) {
				return Some(entry_path);
			}
			if name == exact_match || name == exact_match_bak {
				return Some(entry_path);
			}
		}

		None
	}

	/// Find an issue directory by its number prefix.
	fn find_issue_dir_by_number(parent: &Path, issue_number: u64) -> Option<PathBuf> {
		let entries = std::fs::read_dir(parent).ok()?;

		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match = format!("{issue_number}");

		for entry in entries.flatten() {
			let path = entry.path();
			if !path.is_dir() {
				continue;
			}

			let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if name.starts_with(&prefix_with_sep) || name == exact_match {
				return Some(path);
			}
		}

		None
	}

	/// Find an issue by number (flat file or directory) and return the directory name it would use.
	/// For flat file `99_-_title.md` returns `99_-_title`.
	/// For directory `99_-_title/` returns `99_-_title`.
	fn find_issue_dir_name_by_number(parent: &Path, issue_number: u64) -> Option<String> {
		let entries = std::fs::read_dir(parent).ok()?;

		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match_dir = format!("{issue_number}");

		for entry in entries.flatten() {
			let path = entry.path();
			let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
				continue;
			};

			if path.is_dir() {
				if name.starts_with(&prefix_with_sep) || name == exact_match_dir {
					return Some(name.to_string());
				}
			} else if path.is_file()
				&& let Some(base) = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md"))
				&& (base.starts_with(&prefix_with_sep) || base == exact_match_dir)
			{
				return Some(base.to_string());
			}
		}

		None
	}

	/// Find the issue file by repo info and full number path, using the provided reader.
	pub(crate) fn find_issue_file_by_num_path_with_reader<R: LocalReader>(repo_info: RepoInfo, num_path: &[u64], reader: &R) -> Option<PathBuf> {
		let mut path = Self::project_dir(repo_info.owner(), repo_info.repo());

		if !reader.exists(&path) {
			return None;
		}

		if num_path.is_empty() {
			return None;
		}

		// Navigate to parent directories
		for &issue_number in &num_path[..num_path.len() - 1] {
			let dir = Self::find_issue_dir_by_number_with_reader(&path, issue_number, reader)?;
			path = dir;
		}

		// Find the target issue (last in num_path)
		let target_number = *num_path.last().unwrap();

		// Check for directory format first (issue with children)
		if let Some(dir) = Self::find_issue_dir_by_number_with_reader(&path, target_number, reader) {
			let main_file = Self::main_file_path(&dir, false);
			if reader.exists(&main_file) {
				return Some(main_file);
			}
			let main_bak = Self::main_file_path(&dir, true);
			if reader.exists(&main_bak) {
				return Some(main_bak);
			}
		}

		// Check for flat file format
		let entries = reader.list_dir(&path);
		let prefix_with_sep = format!("{target_number}_-_");
		let exact_match = format!("{target_number}.md");
		let exact_match_bak = format!("{target_number}.md.bak");

		for name in entries {
			let entry_path = path.join(&name);

			// Skip directories (already checked above)
			if reader.is_dir(&entry_path) {
				continue;
			}

			if name.starts_with(&prefix_with_sep) && (name.ends_with(".md") || name.ends_with(".md.bak")) {
				return Some(entry_path);
			}
			if name == exact_match || name == exact_match_bak {
				return Some(entry_path);
			}
		}

		None
	}

	/// Find an issue directory by its number prefix, using the provided reader.
	fn find_issue_dir_by_number_with_reader<R: LocalReader>(parent: &Path, issue_number: u64, reader: &R) -> Option<PathBuf> {
		let entries = reader.list_dir(parent);

		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match = format!("{issue_number}");

		for name in entries {
			let path = parent.join(&name);

			if !reader.is_dir(&path) {
				continue;
			}

			if name.starts_with(&prefix_with_sep) || name == exact_match {
				return Some(path);
			}
		}

		None
	}

	/// Find the issue directory by repo info and full number path, using the provided reader.
	/// Returns `Some(path)` if the issue exists in directory format (has children).
	pub(crate) fn find_issue_dir_by_num_path_with_reader<R: LocalReader>(repo_info: RepoInfo, num_path: &[u64], reader: &R) -> Option<PathBuf> {
		let mut path = Self::project_dir(repo_info.owner(), repo_info.repo());

		if !reader.exists(&path) {
			return None;
		}

		if num_path.is_empty() {
			return None;
		}

		// Navigate through all directories in num_path
		for &issue_number in num_path {
			let dir = Self::find_issue_dir_by_number_with_reader(&path, issue_number, reader)?;
			path = dir;
		}

		// Check that the final path is indeed a directory (has children)
		if reader.is_dir(&path) { Some(path) } else { None }
	}

	/// Parse an IssueSelector from a filename or directory name.
	/// Format: `{number}_-_{title}[.md[.bak]]` or just `{number}[.md[.bak]]`
	pub(crate) fn parse_issue_selector_from_name(name: &str) -> Option<IssueSelector> {
		// Strip file extensions if present
		let base = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap_or(name);

		// Extract issue number
		if let Some(sep_pos) = base.find("_-_") {
			base[..sep_pos].parse::<u64>().ok().map(IssueSelector::GitId)
		} else {
			base.parse::<u64>().ok().map(IssueSelector::GitId)
		}
	}

	/// Extract full ancestry (owner/repo/lineage) from an issue file path.
	///
	/// Path structure: `issues/{owner}/{repo}/{number}_-_{title}/.../file.md`
	/// The lineage contains PARENT issue numbers from the path components between repo and the target file.
	///
	/// For directory format (`__main__.md`), the immediately containing directory is the issue itself,
	/// not a parent, so it's excluded from lineage.
	/// Extract the parent's IssueIndex from an issue file path.
	///
	/// For root issues, returns `IssueIndex::repo_only(owner, repo)`.
	/// For child issues, returns the parent's full IssueIndex.
	pub fn extract_parent_index(path: &Path) -> Result<IssueIndex> {
		let issues_base = Self::issues_dir();

		let rel_path = path.strip_prefix(&issues_base).map_err(|_| eyre!("Issue file is not in issues directory: {path:?}"))?;

		let components: Vec<_> = rel_path.components().collect();
		if components.len() < 2 {
			bail!("Path too short to extract owner/repo: {path:?}");
		}

		let owner = components[0].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract owner from path: {path:?}"))?;
		let repo = components[1].as_os_str().to_str().ok_or_else(|| eyre!("Could not extract repo from path: {path:?}"))?;

		// Check if this is a directory format file (__main__.md or __main__.md.bak)
		let filename = components
			.last()
			.expect("components verified to have at least 2 elements")
			.as_os_str()
			.to_str()
			.ok_or_else(|| eyre!("Could not convert filename to str: {path:?}"))?;
		let is_dir_format = filename.starts_with(Self::MAIN_ISSUE_FILENAME);

		// Everything between repo and the final component is a potential parent issue directory
		// Format: {number}_-_{title} or just {number}
		// For directory format, exclude the last directory (that's the issue itself, not a parent)
		let mut parent_selectors = Vec::new();
		let end_offset = if is_dir_format { 2 } else { 1 }; // Skip filename + issue dir for dir format
		for component in &components[2..components.len().saturating_sub(end_offset)] {
			let name = component.as_os_str().to_str().ok_or_else(|| eyre!("Invalid path component: {component:?}"))?;

			// Skip if this looks like a file (has .md extension)
			if name.ends_with(".md") || name.ends_with(".md.bak") {
				continue;
			}

			// Extract issue number from directory name
			let issue_number = if let Some(sep_pos) = name.find("_-_") {
				name[..sep_pos].parse::<u64>().map_err(|_| eyre!("Invalid issue directory format: {name}"))?
			} else {
				name.parse::<u64>().map_err(|_| eyre!("Invalid issue directory format: {name}"))?
			};
			parent_selectors.push(IssueSelector::GitId(issue_number));
		}

		Ok(IssueIndex::with_index(owner, repo, parent_selectors))
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
	pub(crate) fn load_project_meta_from_reader<R: LocalReader>(repo_info: RepoInfo, reader: &R) -> ProjectMeta {
		let meta_path = Self::project_meta_path(repo_info.owner(), repo_info.repo());
		let content = reader.read_content(&meta_path);

		match content {
			Some(c) => match serde_json::from_str(&c) {
				Ok(meta) => meta,
				Err(e) => panic!("corrupted project metadata at {}: {e}", meta_path.display()),
			},
			None => ProjectMeta::default(),
		}
	}

	/// Load metadata for a specific issue using the provided reader.
	pub(crate) fn load_issue_meta_from_reader<R: LocalReader>(repo_info: RepoInfo, issue_number: u64, reader: &R) -> Option<IssueMeta> {
		let project_meta = Self::load_project_meta_from_reader(repo_info, reader);
		project_meta.issues.get(&issue_number).cloned()
	}

	/// Save metadata for a specific issue to the project's .meta.json.
	fn save_issue_meta(owner: &str, repo: &str, issue_number: u64, meta: &IssueMeta) -> Result<()> {
		let mut project_meta = Self::load_project_meta(owner, repo);
		project_meta.issues.insert(issue_number, meta.clone());
		Self::save_project_meta(owner, repo, &project_meta)
	}

	/// Remove metadata for a specific issue from the project's .meta.json.
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

//==============================================================================
// LocalPath - On-demand path construction
//==============================================================================

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

	async fn parent_index(source: &Self::Source) -> Result<crate::IssueIndex, Self::Error> {
		// The source's IssueIndex IS the issue's index; parent_index is derived from it
		let index = source.index();
		Ok(index.parent())
	}

	async fn identity(&mut self, mut source: Self::Source) -> Result<crate::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let index = *source.index();

		if self.contents.title.is_empty() {
			let file_path = source.local_path.find_file_path(&source.reader)?;
			let content = source.reader.read_content(&file_path).ok_or_else(|| LocalError::FileNotFound { path: file_path.clone() })?;
			let parsed = Local::parse_single_node(&content, index, &file_path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta_from_reader(index.repo_info(), issue_number, &source.reader)
			&& let Some(linked) = self.identity.remote.as_linked_mut()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	async fn contents(&mut self, mut source: Self::Source) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let index = *source.index();
		let file_path = source.local_path.find_file_path(&source.reader)?;
		let content = source.reader.read_content(&file_path).ok_or_else(|| LocalError::FileNotFound { path: file_path.clone() })?;
		let parsed = Local::parse_single_node(&content, index, &file_path)?;
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		Ok(self.contents.clone())
	}

	async fn children(&mut self, mut source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		// Check if this issue has a directory (children stored in separate files)
		let dir_path = source.local_path.find_dir_path(&source.reader);
		let Some(dir_path) = dir_path else {
			return Ok(Vec::new());
		};

		let entries = source.reader.list_dir(&dir_path);
		let mut children = Vec::new();
		let this_index = *source.index();

		for name in entries {
			if name.starts_with(Local::MAIN_ISSUE_FILENAME) {
				continue;
			}

			// Parse child issue number from filename/dirname
			let child_selector = match Local::parse_issue_selector_from_name(&name) {
				Some(sel) => sel,
				None => continue,
			};

			let child_index = this_index.child(child_selector);
			let child_source = source.child(child_index);

			let child = <Issue as crate::LazyIssue<Local>>::load(child_source).await?;
			children.push(child);
		}

		children.sort_by(|a, b| {
			let a_num = a.number().unwrap_or(0); //IGNORED_ERROR: pending issues (no number) sort first
			let b_num = b.number().unwrap_or(0); //IGNORED_ERROR: pending issues (no number) sort first
			a_num.cmp(&b_num)
		});

		self.children = children.clone();
		Ok(children)
	}

	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		// Check for unresolved conflicts (only for FsReader/Submitted, and only for root loads)
		conflict::check_conflict(source.index().owner())?;

		let parent_index = <Self as crate::LazyIssue<Local>>::parent_index(&source).await?;
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

	async fn parent_index(source: &Self::Source) -> Result<crate::IssueIndex, Self::Error> {
		let index = source.index();
		Ok(index.parent())
	}

	async fn identity(&mut self, mut source: Self::Source) -> Result<crate::IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let index = *source.index();

		if self.contents.title.is_empty() {
			let file_path = source.local_path.find_file_path(&source.reader)?;
			let content = source.reader.read_content(&file_path).ok_or_else(|| LocalError::FileNotFound { path: file_path.clone() })?;
			let parsed = Local::parse_single_node(&content, index, &file_path)?;
			self.identity = parsed.identity;
			self.contents = parsed.contents;
		}

		if let Some(issue_number) = self.identity.number()
			&& let Some(meta) = Local::load_issue_meta_from_reader(index.repo_info(), issue_number, &source.reader)
			&& let Some(linked) = self.identity.remote.as_linked_mut()
		{
			linked.timestamps = meta.timestamps;
		}

		Ok(self.identity.clone())
	}

	async fn contents(&mut self, mut source: Self::Source) -> Result<crate::IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let index = *source.index();
		let file_path = source.local_path.find_file_path(&source.reader)?;
		let content = source.reader.read_content(&file_path).ok_or_else(|| LocalError::FileNotFound { path: file_path.clone() })?;
		let parsed = Local::parse_single_node(&content, index, &file_path)?;
		self.identity = parsed.identity;
		self.contents = parsed.contents;

		Ok(self.contents.clone())
	}

	async fn children(&mut self, mut source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let dir_path = source.local_path.find_dir_path(&source.reader);
		let Some(dir_path) = dir_path else {
			return Ok(Vec::new());
		};

		let entries = source.reader.list_dir(&dir_path);
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

		children.sort_by(|a, b| {
			let a_num = a.number().unwrap_or(0); //IGNORED_ERROR: pending issues (no number) sort first
			let b_num = b.number().unwrap_or(0); //IGNORED_ERROR: pending issues (no number) sort first
			a_num.cmp(&b_num)
		});

		self.children = children.clone();
		Ok(children)
	}

	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		// No conflict check for consensus loading (we're reading committed state)
		let parent_index = <Self as crate::LazyIssue<LocalConsensus>>::parent_index(&source).await?;
		let mut issue = Issue::empty_local(parent_index);
		<Self as crate::LazyIssue<LocalConsensus>>::identity(&mut issue, source.clone()).await?;
		<Self as crate::LazyIssue<LocalConsensus>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Self as crate::LazyIssue<LocalConsensus>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}
