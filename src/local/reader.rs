//! LocalReader trait and implementations for reading from different sources.
//!
//! - `FsReader`: reads from filesystem (submitted/current state)
//! - `GitReader`: reads from git HEAD (consensus state)

use std::path::{Path, PathBuf};

use super::Local;

/// Error type for LocalReader operations.
///
/// Only covers failures possible from both FsReader and GitReader.
/// Random unrecoverable errors go to Other.
#[derive(Debug, thiserror::Error)]
pub enum ReaderError {
	/// Path does not exist
	#[error("path not found: {}", path.display())]
	NotFound { path: PathBuf },

	/// Permission denied reading path
	#[error("permission denied: {}", path.display())]
	PermissionDenied { path: PathBuf },

	/// File content is not valid UTF-8
	#[error("invalid UTF-8 in file: {}", path.display())]
	InvalidUtf8 { path: PathBuf },

	/// Path is outside the issues directory (security boundary)
	#[error("path outside issues directory: {}", path.display())]
	OutsideIssuesDir { path: PathBuf },

	/// Git repository not initialized
	#[error("git not initialized in issues directory")]
	GitNotInitialized,

	/// Other errors
	#[error("{0}")]
	Other(#[from] color_eyre::Report),
}

/// Trait for reading content from different sources (filesystem or git).
///
/// Separates the read abstraction from path computation.
pub trait LocalReader: Clone {
	/// Read file content at the given path.
	fn read_content(&self, path: &Path) -> Result<String, ReaderError>;
	/// List directory entries at the given path.
	fn list_dir(&self, path: &Path) -> Result<Vec<String>, ReaderError>;
	/// Check if the path is a directory.
	fn is_dir(&self, path: &Path) -> Result<bool, ReaderError>;
	/// Check if the path exists.
	fn exists(&self, path: &Path) -> Result<bool, ReaderError>;
}

/// Reader that reads from the filesystem (submitted/current state).
#[derive(Clone, Copy, Debug, Default)]
pub struct FsReader;

impl FsReader {
	/// Convert std::io::Error to ReaderError with path context.
	fn io_err(e: std::io::Error, path: &Path) -> ReaderError {
		match e.kind() {
			std::io::ErrorKind::NotFound => ReaderError::NotFound { path: path.to_path_buf() },
			std::io::ErrorKind::PermissionDenied => ReaderError::PermissionDenied { path: path.to_path_buf() },
			_ => ReaderError::Other(color_eyre::eyre::eyre!("I/O error on {}: {e}", path.display())),
		}
	}
}

impl LocalReader for FsReader {
	fn read_content(&self, path: &Path) -> Result<String, ReaderError> {
		match std::fs::read(path) {
			Ok(bytes) => String::from_utf8(bytes).map_err(|_| ReaderError::InvalidUtf8 { path: path.to_path_buf() }),
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
}

/// Reader that reads from git HEAD (consensus state).
#[derive(Clone, Copy, Debug, Default)]
pub struct GitReader;

impl GitReader {
	/// Get relative path within issues dir, or error if outside.
	fn rel_path(path: &Path) -> Result<(&Path, PathBuf), ReaderError> {
		let data_dir = Local::issues_dir();
		let rel_path = path.strip_prefix(&data_dir).map_err(|_| ReaderError::OutsideIssuesDir { path: path.to_path_buf() })?;
		Ok((rel_path, data_dir))
	}

	/// Check if git is initialized in the issues directory.
	fn check_git_initialized(data_dir: &Path) -> Result<(), ReaderError> {
		use std::process::Command;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("issues dir path not valid UTF-8")))?;
		let git_check = Command::new("git")
			.args(["-C", data_dir_str, "rev-parse", "--git-dir"])
			.output()
			.map_err(|e| ReaderError::Other(color_eyre::eyre::eyre!("failed to run git: {e}")))?;
		if !git_check.status.success() {
			return Err(ReaderError::GitNotInitialized);
		}
		Ok(())
	}
}

impl LocalReader for GitReader {
	fn read_content(&self, path: &Path) -> Result<String, ReaderError> {
		use std::process::Command;

		let (rel_path, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("issues dir path not valid UTF-8")))?;
		let rel_path_str = rel_path
			.to_str()
			.ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		// Check if file is tracked
		let ls_output = Command::new("git")
			.args(["-C", data_dir_str, "ls-files", rel_path_str])
			.output()
			.map_err(|e| ReaderError::Other(color_eyre::eyre::eyre!("git ls-files failed: {e}")))?;
		if !ls_output.status.success() || ls_output.stdout.is_empty() {
			return Err(ReaderError::NotFound { path: path.to_path_buf() });
		}

		// Read from HEAD
		let output = Command::new("git")
			.args(["-C", data_dir_str, "show", &format!("HEAD:./{rel_path_str}")])
			.output()
			.map_err(|e| ReaderError::Other(color_eyre::eyre::eyre!("git show failed: {e}")))?;

		if !output.status.success() {
			return Err(ReaderError::NotFound { path: path.to_path_buf() });
		}

		String::from_utf8(output.stdout).map_err(|_| ReaderError::InvalidUtf8 { path: path.to_path_buf() })
	}

	fn list_dir(&self, path: &Path) -> Result<Vec<String>, ReaderError> {
		use std::process::Command;

		let (rel_dir, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("issues dir path not valid UTF-8")))?;
		let rel_dir_str = rel_dir
			.to_str()
			.ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		let output = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "--name-only", "HEAD", &format!("{rel_dir_str}/")])
			.output()
			.map_err(|e| ReaderError::Other(color_eyre::eyre::eyre!("git ls-tree failed: {e}")))?;

		if !output.status.success() {
			// Directory doesn't exist in git
			return Err(ReaderError::NotFound { path: path.to_path_buf() });
		}

		let prefix = format!("{rel_dir_str}/");
		let entries = std::str::from_utf8(&output.stdout)
			.map_err(|_| ReaderError::Other(color_eyre::eyre::eyre!("git output not valid UTF-8")))?
			.lines()
			.filter_map(|line| line.strip_prefix(&prefix))
			.map(|s| s.to_string())
			.collect();
		Ok(entries)
	}

	fn is_dir(&self, path: &Path) -> Result<bool, ReaderError> {
		use std::process::Command;

		let (rel_dir, data_dir) = Self::rel_path(path)?;
		let data_dir_str = data_dir.to_str().ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("issues dir path not valid UTF-8")))?;
		let rel_dir_str = rel_dir
			.to_str()
			.ok_or_else(|| ReaderError::Other(color_eyre::eyre::eyre!("path not valid UTF-8: {}", path.display())))?;

		Self::check_git_initialized(&data_dir)?;

		let check = Command::new("git")
			.args(["-C", data_dir_str, "ls-tree", "HEAD", &format!("{rel_dir_str}/")])
			.output()
			.map_err(|e| ReaderError::Other(color_eyre::eyre::eyre!("git ls-tree failed: {e}")))?;

		Ok(check.status.success() && !check.stdout.is_empty())
	}

	fn exists(&self, path: &Path) -> Result<bool, ReaderError> {
		// For git, existence = either file content readable or is a directory
		match self.read_content(path) {
			Ok(_) => Ok(true),
			Err(ReaderError::NotFound { .. }) => self.is_dir(path),
			Err(e) => Err(e),
		}
	}
}
