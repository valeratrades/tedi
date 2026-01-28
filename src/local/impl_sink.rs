//! Sink implementations for local filesystem mutations.
//!
//! r[local.sink-only-mutation]

use std::{
	collections::{HashMap, HashSet},
	path::Path,
};

use v_utils::prelude::*;

use super::{ConsensusSinkError, FsReader, IssueMeta, Local, LocalPath, LocalReader};
use crate::{Issue, IssueIndex, IssueSelector, sink::Sink};

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

	// Use LocalPath for path computation
	let mut local_path = LocalPath::from(issue);

	// Ensure parent directories exist (converts flat files to directories as needed)
	ensure_parent_dirs(&mut local_path, reader)?;

	// Compute the target file path
	let issue_file_path = local_path.file_path(title, closed, has_children, reader)?;

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

/// Ensure parent directories exist, converting flat files to directory format as needed.
///
/// r[local.sink-only-mutation]
fn ensure_parent_dirs<R: LocalReader>(local_path: &mut LocalPath, reader: &R) -> Result<()> {
	let mut path = Local::project_dir(local_path.index.owner(), local_path.index.repo());

	if !path.exists() {
		std::fs::create_dir_all(&path)?;
	}

	let parent_nums = local_path.index.parent_nums();

	for issue_number in parent_nums {
		// Find existing dir name using reader
		let entries = reader.list_dir(&path).unwrap_or_default();
		let prefix_with_sep = format!("{issue_number}_-_");
		let exact_match = format!("{issue_number}");

		let dir_name = entries
			.iter()
			.find(|name| {
				let entry_path = path.join(name);
				if reader.is_dir(&entry_path).unwrap_or(false) {
					name.starts_with(&prefix_with_sep) || *name == &exact_match
				} else if let Some(base) = name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")) {
					base.starts_with(&prefix_with_sep) || base == exact_match
				} else {
					false
				}
			})
			.cloned()
			.ok_or_else(|| color_eyre::eyre::eyre!("Parent issue #{issue_number} not found locally in {}. Fetch the parent issue first.", path.display()))?;

		// Strip extension if it's a flat file name
		let dir_name = dir_name.strip_suffix(".md.bak").or_else(|| dir_name.strip_suffix(".md")).unwrap_or(&dir_name);
		let parent_dir_path = path.join(dir_name);

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
	}

	Ok(())
}

/// Clean up old file locations when format or state changes.
fn cleanup_old_locations<R: LocalReader>(issue: &Issue, old: Option<&Issue>, has_children: bool, closed: bool, reader: &R) -> Result<()> {
	let mut local_path = LocalPath::from(issue);
	let title = &issue.contents.title;
	let issue_number = issue.number();

	// Try to get file path (may fail for new issues, that's ok)
	if local_path.file_path(title, closed, has_children, reader).is_err() {
		return Ok(());
	}

	let old_has_children = old.map(|o| !o.children.is_empty()).unwrap_or(false);
	let format_changed = has_children != old_has_children;

	if has_children && format_changed {
		// Switching to directory format: remove old flat files
		if let Ok(flat_open) = local_path.file_path(title, false, false, reader) {
			try_remove_file(&flat_open)?;
		}
		if let Ok(flat_closed) = local_path.file_path(title, true, false, reader) {
			try_remove_file(&flat_closed)?;
		}
	}

	if has_children {
		// Remove old main file with opposite closed state
		if let Ok(old_main) = local_path.file_path(title, !closed, true, reader) {
			try_remove_file(&old_main)?;
		}
	} else {
		// Flat file: remove file with opposite closed state
		if let Ok(old_flat) = local_path.file_path(title, !closed, false, reader) {
			try_remove_file(&old_flat)?;
		}

		// If issue now has a number, clean up old pending file
		if issue_number.is_some() {
			// Create a pending version of the index to find old pending files
			// This has parents as GitIds + Title (so issue_number() returns None)
			let issue_index = IssueIndex::from(issue);
			let mut pending_selectors: Vec<IssueSelector> = issue_index.parent_nums().into_iter().map(IssueSelector::GitId).collect();
			pending_selectors.push(IssueSelector::title(title));
			let pending_index = IssueIndex::with_index(issue_index.owner(), issue_index.repo(), pending_selectors);
			let mut pending_path = LocalPath::from(pending_index);
			// Try cleanup - ok if paths don't exist
			if let Ok(pending_open) = pending_path.file_path(title, false, false, reader) {
				try_remove_file(&pending_open)?;
			}
			if let Ok(pending_closed) = pending_path.file_path(title, true, false, reader) {
				try_remove_file(&pending_closed)?;
			}
		}
	}

	Ok(())
}

/// Remove all file variants for an issue.
fn remove_issue_files<R: LocalReader>(issue: &Issue, reader: &R) -> Result<bool> {
	let title = &issue.contents.title;
	let owner = issue.identity.owner().to_string();
	let repo = issue.identity.repo().to_string();

	let mut local_path = LocalPath::from(issue);

	// Remove flat file variants (ok if they don't exist)
	if let Ok(flat_open) = local_path.file_path(title, false, false, reader) {
		try_remove_file(&flat_open)?;
	}
	if let Ok(flat_closed) = local_path.file_path(title, true, false, reader) {
		try_remove_file(&flat_closed)?;
	}

	// Remove directory variant - get path via file_path with has_children=true
	if let Ok(main_file) = local_path.file_path(title, false, true, reader) {
		if let Some(issue_dir) = main_file.parent() {
			if issue_dir.is_dir() {
				std::fs::remove_dir_all(issue_dir)?;
			}
		}
	}

	// Remove metadata
	if let Some(num) = issue.number() {
		Local::remove_issue_meta(&owner, &repo, num)?;
	}

	println!("Removed issue #{:?}", issue.number());
	Ok(true)
}
