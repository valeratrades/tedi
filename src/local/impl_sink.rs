//! Sink implementations for local filesystem mutations.
//!
//! r[local.sink-only-mutation]

use std::{
	collections::{HashMap, HashSet},
	path::Path,
};

use tracing::{debug, info, instrument, trace, warn};
use v_utils::prelude::*;

use super::{ConsensusSinkError, FsReader, IssueMeta, Local, LocalPath, LocalReader};
use crate::{Issue, IssueIndex, local::LocalPathErrorKind, sink::Sink};

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
		let issue_number = self.git_id().map(|n| format!("#{n}")).unwrap_or_else(|| "new".to_string());
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
#[instrument(skip(reader))]
fn sink_issue_node<R: LocalReader>(new: &Issue, maybe_old: Option<&Issue>, reader: &R) -> Result<bool> {
	// Duplicate issues self-eliminate: remove local files instead of writing
	if let crate::CloseState::Duplicate(dup_of) = new.contents.state {
		info!(issue = ?new.git_id(), duplicate_of = dup_of, "Removing duplicate issue from local storage");
		return remove_issue_files(new, reader);
	}

	let title = &new.contents.title;
	let closed = new.contents.state.is_closed();

	//let state_changed = maybe_old.map(|o| o.contents.state != closed).unwrap_or(true);

	let has_children = !new.children.is_empty();
	let old_has_children = maybe_old.map(|o| !o.children.is_empty()).unwrap_or(false); //IGNORED_ERROR: if doesn't exist, then it doesn't have children
	let format_changed = has_children != old_has_children;

	// Extract owner/repo directly from issue
	let owner = new.identity.owner().to_string();
	let repo = new.identity.repo().to_string();

	let index = IssueIndex::from(new);

	let issue_file_path = LocalPath::from(index).resolve_parent(*reader)?.deterministic(title, closed, has_children).path();
	debug!(issue_file_path = %issue_file_path.display(), "computed target path");

	// Clean up old file locations if format changed, state changed, or issue got a number
	// (issue getting a number means a pending file may exist that needs removal)
	let should_cleanup: bool = {
		let title_or_state_or_id_changed: bool = {
			match maybe_old {
				Some(old) => {
					let title_changed = title != &old.contents.title;
					let state_changed = new.contents.state != old.contents.state;
					let id_changed = new.git_id() != old.git_id();
					title_changed || state_changed || id_changed
				}
				None => true,
			}
		};
		format_changed | title_or_state_or_id_changed
	};
	if should_cleanup {
		cleanup_old_locations(new, maybe_old, has_children, closed, reader)?;
	}

	// Write content if changed
	let content = new.serialize_filesystem(); //DEPENDS: relies on `serialize_filesystem` including `title`, `close_state`, `git_id`
	let node_changed = match maybe_old {
		Some(old_issue) => content != old_issue.serialize_filesystem() || format_changed, // we check for `format_changed` instead of checking for exact match of children, because we start individual sinks for each of the mismatched children later
		None => true,
	};
	let mut any_written = false;
	if node_changed {
		std::fs::create_dir_all(issue_file_path.parent().unwrap())?;
		std::fs::write(&issue_file_path, &content)?;
		any_written = true;
	}

	// Save metadata
	if let Some(issue_num) = new.git_id()
		&& let Some(timestamps) = new.identity.timestamps()
	{
		let meta = IssueMeta { timestamps: timestamps.clone() };
		Local::save_issue_meta(&owner, &repo, issue_num, &meta)?;
	}

	// Recursively sink children
	let old_children_map: HashMap<u64, &Issue> = maybe_old.map(|o| o.children.iter().filter_map(|c| c.git_id().map(|n| (n, c))).collect()).unwrap_or_default();

	let new_child_numbers: HashSet<u64> = new.children.iter().filter_map(|c| c.git_id()).collect();

	for child in &new.children {
		let old_child = child.git_id().and_then(|n| old_children_map.get(&n).copied());
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
#[instrument(skip(issue, _old, reader), fields(
	issue_number = ?issue.git_id(),
	title = %issue.contents.title,
	has_children,
	closed,
))]
fn cleanup_old_locations<R: LocalReader>(issue: &Issue, _old: Option<&Issue>, has_children: bool, closed: bool, reader: &R) -> Result<()> {
	let local_path = LocalPath::from(issue);
	let title = &issue.contents.title;

	// Resolve parent - if this fails, there's nothing to clean up (new issue in new location)
	let resolved = match local_path.resolve_parent(*reader) {
		Ok(r) => r,
		Err(e) if matches!(e.kind, LocalPathErrorKind::MissingParent | LocalPathErrorKind::NotFound) => {
			debug!("parent missing or not found, nothing to clean up");
			return Ok(());
		}
		Err(e) => return Err(e.into()),
	};

	// Get all existing paths that match our selector
	// If the directory doesn't exist yet, there's nothing to clean up
	let matching = match resolved.matching_subpaths() {
		Ok(m) => m,
		Err(e) if e.kind == LocalPathErrorKind::Reader => {
			debug!("directory doesn't exist yet, nothing to clean up");
			return Ok(()); // Directory doesn't exist yet
		}
		Err(e) => return Err(e.into()),
	};
	if matching.is_empty() {
		debug!("no matching paths found, nothing to clean up");
		return Ok(()); // Nothing exists yet, nothing to clean up
	}

	debug!(matching_count = matching.len(), ?matching, "found matching paths");

	// Compute the correct target path
	let target = LocalPath::from(issue).resolve_parent(*reader)?.deterministic(title, closed, has_children).path();
	debug!(target = %target.display(), "computed target path for cleanup comparison");

	// Remove any matching paths that aren't the target
	for path in matching {
		match path == target {
			true => {
				trace!(path = %path.display(), "path matches target, keeping");
			}
			false => {
				debug!(path = %path.display(), "path differs from target, will remove");
				// If it's a __main__.md file, we need to check if the parent dir should be removed
				// (only if we're moving from directory to flat format)
				match path.file_name().map(|n| n.to_string_lossy().starts_with(Local::MAIN_ISSUE_FILENAME)).unwrap() {
					true => {
						// Remove the whole directory if we're converting to flat format
						let parent_dir = path.parent().unwrap();
						info!(parent_dir = %parent_dir.display(), "removing directory (converting to flat format)");
						std::fs::remove_dir_all(parent_dir)?;
					}
					false => {
						info!(path = %path.display(), "removing old file location");
						try_remove_file(&path)?;
					}
				}
			}
		}
	}

	Ok(())
}

/// Remove all file variants for an issue.
#[instrument(skip(issue, reader), fields(issue_number = ?issue.git_id(), title = %issue.contents.title))]
fn remove_issue_files<R: LocalReader>(issue: &Issue, reader: &R) -> Result<bool> {
	let owner = issue.identity.owner().to_string();
	let repo = issue.identity.repo().to_string();

	match LocalPath::from(issue).resolve_parent(*reader)?.search() {
		Ok(resolved_path) =>
			if let Some(dir) = resolved_path.clone().issue_dir() {
				info!(dir = %dir.display(), "removing issue directory");
				std::fs::remove_dir_all(dir)?;
			} else {
				let p = resolved_path.path();
				info!(path = %p.display(), "removing issue file");
				try_remove_file(&p)?;
			},
		Err(e) if e.kind == LocalPathErrorKind::NotFound => {
			debug!("issue not found, nothing to remove");
		}
		Err(e) => color_eyre::eyre::bail!(e),
	};

	// Remove metadata
	if let Some(num) = issue.git_id() {
		trace!(num, "removing issue metadata");
		Local::remove_issue_meta(&owner, &repo, num)?;
	}

	info!(issue_number = ?issue.git_id(), "removed issue");
	Ok(true)
}
