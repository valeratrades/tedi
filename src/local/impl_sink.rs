//! Sink implementations for local filesystem mutations.
//!
//! r[local.sink-only-mutation]

use std::{
	collections::{HashMap, HashSet},
	path::Path,
};

use v_utils::prelude::*;

use super::{ConsensusSinkError, FsReader, IssueMeta, Local, LocalPath, LocalReader, ReaderError};
use crate::{Issue, IssueIndex, local::LocalPathError, sink::Sink};

/// Marker type for sinking to filesystem (submitted state).
pub struct Submitted;
/// Marker type for sinking to git (consensus state).
pub struct Consensus;
/// Remove a file, ignoring NotFound errors (file may not exist).
/// Propagates other errors (permission denied, etc.).
fn try_remove_file(path: &Path) -> Result<()> {
	match std::fs::remove_file(path) {
		Ok(()) => Ok(()),
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
		Err(e) => Err(e.into()),
	}
}

//TODO: @claude: create proper error type for Submitted sink (see ConsensusSinkError for reference)
/// r[local.sink-only-mutation]
impl Sink<Submitted> for Issue {
	type Error = color_eyre::Report;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		sink_issue_node(self, old, &FsReader)
	}
}

/// r[local.sink-only-mutation]
impl Sink<Consensus> for Issue {
	type Error = ConsensusSinkError;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		use std::process::Command;

		let owner = self.identity.owner();
		let repo = self.identity.repo();

		// Write files to filesystem (same as Submitted)
		let any_written = sink_issue_node(self, old, &FsReader).map_err(ConsensusSinkError::Write)?;

		if !any_written {
			return Ok(false);
		}

		// Stage and commit to git
		let data_dir = Local::issues_dir();
		let data_dir_str = data_dir.to_str().ok_or(ConsensusSinkError::InvalidDataDir)?;

		// Stage all changes
		let add_output = Command::new("git").args(["-C", data_dir_str, "add", "-A"]).output()?;
		if !add_output.status.success() {
			return Err(ConsensusSinkError::GitAdd(String::from_utf8_lossy(&add_output.stderr).into_owned()));
		}

		// Check if any files were ignored (would indicate .gitignore rejection)
		let status_output = Command::new("git").args(["-C", data_dir_str, "status", "--porcelain"]).output()?;
		if !status_output.status.success() {
			return Err(ConsensusSinkError::GitStatus(String::from_utf8_lossy(&status_output.stderr).into_owned()));
		}

		// Check for ignored files that we tried to add
		let project_dir = Local::project_dir(owner, repo);
		if let Ok(rel) = project_dir.strip_prefix(&data_dir) {
			let check_ignored = Command::new("git").args(["-C", data_dir_str, "check-ignore", "--no-index", "-v"]).arg(rel.join("**")).output()?;
			// check-ignore returns 0 if files ARE ignored, 1 if none are ignored
			if check_ignored.status.success() && !check_ignored.stdout.is_empty() {
				return Err(ConsensusSinkError::GitIgnoreRejection(String::from_utf8_lossy(&check_ignored.stdout).into_owned()));
			}
		}

		// Check if there are staged changes to commit
		let diff_output = Command::new("git").args(["-C", data_dir_str, "diff", "--cached", "--quiet"]).status()?;
		if diff_output.success() {
			// No staged changes - nothing to commit
			return Ok(false);
		}

		// Commit with a descriptive message
		let issue_number = self.number().map(|n| format!("#{n}")).unwrap_or_else(|| "new".to_string());
		let commit_msg = format!("sync: {owner}/{repo}{issue_number}");
		let commit_output = Command::new("git").args(["-C", data_dir_str, "commit", "-m", &commit_msg]).output()?;
		if !commit_output.status.success() {
			return Err(ConsensusSinkError::GitCommit(String::from_utf8_lossy(&commit_output.stderr).into_owned()));
		}

		Ok(true)
	}
}

/// r[local.sink-only-mutation]
/// Sink an issue node to the local filesystem.
///
/// Uses LocalPath for path construction and handles all filesystem mutations:
/// - Converting flat-file parents to directory format
/// - Creating directories as needed
/// - Removing old file locations when format changes
/// - Writing issue content
fn sink_issue_node<R: LocalReader>(issue: &Issue, old: Option<&Issue>, reader: &R) -> Result<bool> {
	// Duplicate issues self-eliminate: remove local files instead of writing
	if let crate::CloseState::Duplicate(dup_of) = issue.contents.state {
		tracing::info!(issue = ?issue.number(), duplicate_of = dup_of, "Removing duplicate issue from local storage");
		return remove_issue_files(issue, reader);
	}

	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	let has_children = !issue.children.is_empty();
	let old_has_children = old.map(|o| !o.children.is_empty()).unwrap_or(false);
	let format_changed = has_children != old_has_children;

	eprintln!("[sink_issue_node] issue #{:?} '{title}', closed: {closed}, state: {:?}", issue.number(), issue.contents.state);

	// Extract owner/repo directly from issue
	let owner = issue.identity.owner().to_string();
	let repo = issue.identity.repo().to_string();

	let index = IssueIndex::from(issue);

	let issue_file_path = LocalPath::from(index).resolve_parent(*reader)?.deterministic(title, closed, has_children).path();

	// Clean up old file locations if format changed, state changed, or issue got a number
	// (issue getting a number means a pending file may exist that needs removal)
	if format_changed || old.is_some() || issue.number().is_some() {
		cleanup_old_locations(issue, old, has_children, closed, reader)?;
	}

	// Ensure parent directory exists
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Write content if changed
	let content = issue.serialize_filesystem();
	let node_changed = if format_changed {
		true
	} else if let Some(old_issue) = old {
		content != old_issue.serialize_filesystem()
	} else {
		true
	};

	let mut any_written = false;

	if node_changed {
		std::fs::write(&issue_file_path, &content)?;
		any_written = true;
	}

	// Save metadata
	if let Some(issue_num) = issue.number()
		&& let Some(timestamps) = issue.identity.timestamps()
	{
		let meta = IssueMeta { timestamps: timestamps.clone() };
		Local::save_issue_meta(&owner, &repo, issue_num, &meta)?;
	}

	// Recursively sink children
	let old_children_map: HashMap<u64, &Issue> = old.map(|o| o.children.iter().filter_map(|c| c.number().map(|n| (n, c))).collect()).unwrap_or_default();

	let new_child_numbers: HashSet<u64> = issue.children.iter().filter_map(|c| c.number()).collect();

	for child in &issue.children {
		let old_child = child.number().and_then(|n| old_children_map.get(&n).copied());
		any_written |= sink_issue_node(child, old_child, reader)?;
	}

	// Remove deleted children
	for (&old_num, &old_child) in &old_children_map {
		if !new_child_numbers.contains(&old_num) {
			remove_issue_files(old_child, reader)?;
			Local::remove_issue_meta(&owner, &repo, old_num)?;
		}
	}

	Ok(any_written)
}

/// Clean up old file locations when format or state changes.
///
/// Strategy: find all matching paths via `matching_subpaths`, compute the correct target path
/// via `deterministic`, then remove any matching paths that aren't the target.
fn cleanup_old_locations<R: LocalReader>(issue: &Issue, _old: Option<&Issue>, has_children: bool, closed: bool, reader: &R) -> Result<()> {
	let local_path = LocalPath::from(issue);
	let title = &issue.contents.title;

	// Resolve parent - if this fails, there's nothing to clean up (new issue in new location)
	let resolved = match local_path.resolve_parent(*reader) {
		Ok(r) => r,
		Err(LocalPathError::MissingParent { .. } | LocalPathError::NotFound { .. }) => return Ok(()),
		Err(e) => return Err(e.into()),
	};

	// Get all existing paths that match our selector
	// If the directory doesn't exist yet (NotFound from Reader), there's nothing to clean up
	let matching = match resolved.matching_subpaths() {
		Ok(m) => m,
		Err(LocalPathError::Reader(ReaderError::NotFound { .. })) => return Ok(()),
		Err(e) => return Err(e.into()),
	};
	if matching.is_empty() {
		return Ok(()); // Nothing exists yet, nothing to clean up
	}

	// Compute the correct target path
	let target = LocalPath::from(issue).resolve_parent(*reader)?.deterministic(title, closed, has_children).path();

	// Remove any matching paths that aren't the target
	for path in matching {
		if path != target {
			// If it's a __main__.md file, we need to check if the parent dir should be removed
			// (only if we're moving from directory to flat format)
			if path.file_name().map(|n| n.to_string_lossy().starts_with(Local::MAIN_ISSUE_FILENAME)).unwrap_or(false) {
				if let Some(parent_dir) = path.parent() {
					// Remove the whole directory if we're converting to flat format
					if !has_children {
						std::fs::remove_dir_all(parent_dir)?;
						continue;
					}
				}
			}
			try_remove_file(&path)?;
		}
	}

	Ok(())
}

/// Remove all file variants for an issue.
fn remove_issue_files<R: LocalReader>(issue: &Issue, reader: &R) -> Result<bool> {
	let owner = issue.identity.owner().to_string();
	let repo = issue.identity.repo().to_string();

	match LocalPath::from(issue).resolve_parent(*reader)?.search() {
		Ok(resolved_path) =>
			if let Some(dir) = resolved_path.clone().issue_dir() {
				std::fs::remove_dir_all(dir)?;
			} else {
				let p = resolved_path.path();
				try_remove_file(&p)?;
			},
		Err(LocalPathError::NotFound { .. }) => {}
		Err(e) => color_eyre::eyre::bail!(e),
	};

	// Remove metadata
	if let Some(num) = issue.number() {
		Local::remove_issue_meta(&owner, &repo, num)?;
	}

	println!("Removed issue #{:?}", issue.number());
	Ok(true)
}
