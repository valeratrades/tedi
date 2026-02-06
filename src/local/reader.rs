//! LocalReader trait and implementations for reading from different sources.
//!
//! - `FsReader`: reads from filesystem (submitted/current state)
//! - `GitReader`: reads from git HEAD (consensus state)

use std::path::{Path, PathBuf};

use miette::{NamedSource, SourceSpan};
use tracing::{info, instrument};
use tracing_error::SpanTrace;

use super::Local;

/// Trait for reading content from different sources (filesystem or git).
///
/// Separates the read abstraction from path computation.
pub trait LocalReader: Copy + std::fmt::Debug {
	/// Read file content at the given path.
	fn read_content(&self, path: &Path) -> Result<String, ReaderError>;
	/// List directory entries at the given path.
	fn list_dir(&self, path: &Path) -> Result<Vec<String>, ReaderError>;
	/// Check if the path is a directory.
	fn is_dir(&self, path: &Path) -> Result<bool, ReaderError>;
	/// Check if the path exists.
	fn exists(&self, path: &Path) -> Result<bool, ReaderError>;
	/// Whether this reader supports filesystem mutations (cleanup operations).
	fn is_mutable(&self) -> bool {
		false
	}
}
/// Error type for LocalReader operations.
///
/// Contains the error kind (for matching), pre-rendered miette diagnostic,
/// and a SpanTrace captured at error creation time.
#[derive(Debug, thiserror::Error)]
#[error("{rendered}\n\n{spantrace}")]
pub struct ReaderError {
	pub kind: ReaderErrorKind,
	rendered: String,
	spantrace: SpanTrace,
}
impl ReaderError {
	fn from_diagnostic(kind: ReaderErrorKind, diag: ReaderDiagnostic) -> Self {
		let rendered = format!("{:?}", miette::Report::new(diag));
		Self {
			kind,
			rendered,
			spantrace: SpanTrace::capture(),
		}
	}

	pub fn not_found(path: &Path) -> Self {
		Self::from_diagnostic(ReaderErrorKind::NotFound, ReaderDiagnostic::NotFound { path: path.to_path_buf() })
	}

	pub fn not_a_directory(path: &Path) -> Self {
		let path_str = path.display().to_string();
		let last_component = path.file_name().map(|s| s.to_string_lossy().to_string()).unwrap_or_default();
		let span_start = path_str.len().saturating_sub(last_component.len());
		let span_len = last_component.len();

		Self::from_diagnostic(
			ReaderErrorKind::NotADirectory,
			ReaderDiagnostic::NotADirectory {
				path_source: NamedSource::new("path", path_str),
				span: (span_start, span_len).into(),
			},
		)
	}

	pub fn permission_denied(path: &Path) -> Self {
		Self::from_diagnostic(ReaderErrorKind::PermissionDenied, ReaderDiagnostic::PermissionDenied { path: path.to_path_buf() })
	}

	pub fn invalid_utf8(path: &Path) -> Self {
		Self::from_diagnostic(ReaderErrorKind::InvalidUtf8, ReaderDiagnostic::InvalidUtf8 { path: path.to_path_buf() })
	}

	pub fn outside_issues_dir(path: &Path) -> Self {
		Self::from_diagnostic(ReaderErrorKind::OutsideIssuesDir, ReaderDiagnostic::OutsideIssuesDir { path: path.to_path_buf() })
	}

	pub fn git_not_initialized() -> Self {
		Self::from_diagnostic(ReaderErrorKind::GitNotInitialized, ReaderDiagnostic::GitNotInitialized)
	}

	pub fn other(err: impl std::fmt::Display) -> Self {
		Self::from_diagnostic(ReaderErrorKind::Other, ReaderDiagnostic::Other(err.to_string()))
	}

	pub fn is_not_found(&self) -> bool {
		self.kind == ReaderErrorKind::NotFound
	}
}

/// The kind of reader error (for pattern matching).
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReaderErrorKind {
	NotFound,
	NotADirectory,
	PermissionDenied,
	InvalidUtf8,
	OutsideIssuesDir,
	GitNotInitialized,
	Other,
}

/// Reader that reads from the filesystem (submitted/current state).
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct FsReader;
impl FsReader {
	/// Convert std::io::Error to ReaderError with path context.
	fn io_err(e: std::io::Error, path: &Path) -> ReaderError {
		match e.kind() {
			std::io::ErrorKind::NotFound => ReaderError::not_found(path),
			std::io::ErrorKind::NotADirectory => ReaderError::not_a_directory(path),
			std::io::ErrorKind::PermissionDenied => ReaderError::permission_denied(path),
			_ => ReaderError::other(format!("I/O error on {}: {e}", path.display())),
		}
	}
}

/// Reader that reads from git HEAD (consensus state).
#[derive(Clone, Copy, Debug, Default)]
pub struct GitReader;
impl GitReader {
	/// Get relative path within issues dir, or error if outside.
	fn rel_path(path: &Path) -> Result<(&Path, PathBuf), ReaderError> {
		let data_dir = Local::issues_dir();
		let rel_path = path.strip_prefix(&data_dir).map_err(|_| ReaderError::outside_issues_dir(path))?;
		Ok((rel_path, data_dir))
	}

	/// Check if git is initialized in the issues directory.
	fn check_git_initialized(data_dir: &Path) -> Result<(), ReaderError> {
		use std::process::Command;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::other("issues dir path not valid UTF-8".to_string()))?;
		let git_check = Command::new("git")
			.args(["-C", data_dir_str, "rev-parse", "--git-dir"])
			.output()
			.map_err(|e| ReaderError::other(format!("failed to run git: {e}")))?;
		if !git_check.status.success() {
			return Err(ReaderError::git_not_initialized());
		}
		Ok(())
	}
}

/// Internal miette diagnostic for nice error rendering with source highlighting.
#[derive(Debug, miette::Diagnostic, thiserror::Error)]
enum ReaderDiagnostic {
	#[error("path not found: {}", path.display())]
	#[diagnostic(code(tedi::reader::not_found))]
	NotFound { path: PathBuf },

	#[error("expected directory, found file")]
	#[diagnostic(code(tedi::reader::not_a_directory))]
	NotADirectory {
		#[source_code]
		path_source: NamedSource<String>,
		#[label("this is a file, not a directory")]
		span: SourceSpan,
	},

	#[error("permission denied: {}", path.display())]
	#[diagnostic(code(tedi::reader::permission_denied))]
	PermissionDenied { path: PathBuf },

	#[error("invalid UTF-8 in file: {}", path.display())]
	#[diagnostic(code(tedi::reader::invalid_utf8))]
	InvalidUtf8 { path: PathBuf },

	#[error("path outside issues directory: {}", path.display())]
	#[diagnostic(code(tedi::reader::outside_issues_dir))]
	OutsideIssuesDir { path: PathBuf },

	#[error("git not initialized in issues directory")]
	#[diagnostic(code(tedi::reader::git_not_initialized), help("Run 'git init' in the issues directory"))]
	GitNotInitialized,

	#[error("{0}")]
	#[diagnostic(code(tedi::reader::other))]
	Other(String),
}

impl LocalReader for FsReader {
	fn read_content(&self, path: &Path) -> Result<String, ReaderError> {
		match std::fs::read(path) {
			Ok(bytes) => String::from_utf8(bytes).map_err(|_| ReaderError::invalid_utf8(path)),
			Err(e) => Err(Self::io_err(e, path)),
		}
	}

	fn list_dir(&self, path: &Path) -> Result<Vec<String>, ReaderError> {
		let entries = std::fs::read_dir(path).map_err(|e| Self::io_err(e, path))?;
		let mut result = Vec::new();
		for entry in entries {
			let entry = entry.map_err(|e| Self::io_err(e, path))?;
			if let Some(name) = entry.file_name().to_str() {
				result.push(name.to_string());
			}
		}
		Ok(result)
	}

	fn is_dir(&self, path: &Path) -> Result<bool, ReaderError> {
		match std::fs::metadata(path) {
			Ok(meta) => Ok(meta.is_dir()),
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
			Err(e) => Err(Self::io_err(e, path)),
		}
	}

	fn exists(&self, path: &Path) -> Result<bool, ReaderError> {
		match path.try_exists() {
			Ok(exists) => Ok(exists),
			Err(e) => Err(Self::io_err(e, path)),
		}
	}

	fn is_mutable(&self) -> bool {
		true
	}
}

impl LocalReader for GitReader {
	#[instrument]
	fn read_content(&self, path: &Path) -> Result<String, ReaderError> {
		use std::process::Command;

		let (rel_path, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::other("issues dir path not valid UTF-8".to_string()))?;
		let rel_path_str = rel_path.to_str().ok_or_else(|| ReaderError::other(format!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		// Check if file is tracked
		let ls_output = Command::new("git")
			.args(["-C", data_dir_str, "ls-files", rel_path_str])
			.output()
			.map_err(|e| ReaderError::other(format!("git ls-files failed: {e}")))?;
		if !ls_output.status.success() || ls_output.stdout.is_empty() {
			return Err(ReaderError::not_found(path));
		}

		// Read from HEAD
		let output = Command::new("git")
			.args(["-C", data_dir_str, "show", &format!("HEAD:./{rel_path_str}")])
			.output()
			.map_err(|e| ReaderError::other(format!("git show failed: {e}")))?;

		if !output.status.success() {
			return Err(ReaderError::not_found(path));
		}

		String::from_utf8(output.stdout).map_err(|_| ReaderError::invalid_utf8(path))
	}

	#[instrument]
	fn list_dir(&self, path: &Path) -> Result<Vec<String>, ReaderError> {
		use std::process::Command;

		let (rel_dir, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::other("issues dir path not valid UTF-8".to_string()))?;
		let rel_dir_str = rel_dir.to_str().ok_or_else(|| ReaderError::other(format!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		let output = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "--name-only", "HEAD", &format!("{rel_dir_str}/")])
			.output()
			.map_err(|e| ReaderError::other(format!("git ls-tree failed: {e}")))?;

		if !output.status.success() {
			// Directory doesn't exist in git
			let e = ReaderError::not_found(path);
			info!("directory doesn't exist in git: {e}");
			return Err(e);
		}

		let prefix = format!("{rel_dir_str}/");
		let entries = std::str::from_utf8(&output.stdout)
			.map_err(|_| ReaderError::other("git output not valid UTF-8".to_string()))?
			.lines()
			.filter_map(|line| line.strip_prefix(&prefix))
			.map(|s| s.to_string())
			.collect();
		Ok(entries)
	}

	fn is_dir(&self, path: &Path) -> Result<bool, ReaderError> {
		use std::process::Command;

		let (rel_dir, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::other("issues dir path not valid UTF-8".to_string()))?;
		let rel_dir_str = rel_dir.to_str().ok_or_else(|| ReaderError::other(format!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		let check = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "HEAD", &format!("{rel_dir_str}/")])
			.output()
			.map_err(|e| ReaderError::other(format!("git ls-tree failed: {e}")))?;

		Ok(check.status.success() && !check.stdout.is_empty())
	}

	fn exists(&self, path: &Path) -> Result<bool, ReaderError> {
		// For git, existence = either file content readable or is a directory
		match self.read_content(path) {
			Ok(_) => Ok(true),
			Err(e) if e.is_not_found() => self.is_dir(path),
			Err(e) => Err(e),
		}
	}
}
