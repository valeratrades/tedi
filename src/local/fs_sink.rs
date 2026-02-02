//! Sink implementation for local filesystem (submitted state).
//!
//! r[local.sink-only-mutation]

use std::path::{Path, PathBuf};

use tracing::{debug, info, instrument, trace, warn};
use v_utils::prelude::*;

use super::{FsReader, IssueMeta, Local, LocalPath, LocalReader};
use crate::{Issue, RepoInfo, local::LocalPathErrorKind, sink::Sink};

/// Marker type for sinking to filesystem (submitted state).
pub struct LocalFs;

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
impl Sink<LocalFs> for Issue {
	type Error = color_eyre::Report;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		sink_issue_node(self, old, &FsReader)
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
#[instrument]
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

	let issue_file_path = LocalPath::from(new).resolve_parent(*reader)?.deterministic(title, closed, has_children).path();
	debug!(issue_file_path = %issue_file_path.display(), "computed target path");

	// Write content if changed
	let content = new.serialize_filesystem(); //DEPENDS: relies on `serialize_filesystem` including `title`, `close_state`, `git_id`
	let node_changed = match maybe_old {
		Some(old_issue) => content != old_issue.serialize_filesystem() || format_changed, // we check for `format_changed` instead of checking for exact match of children, because we start individual sinks for each of the mismatched children later
		None => true,
	};
	let mut any_written = false;
	if node_changed {
		//DEPENDS: with current logic, `issue_file_path` must be computed before cleanup happens. Otherwise it can't guarantee to be able to resolve parent from fs state.
		std::fs::create_dir_all(issue_file_path.parent().unwrap())?;
		std::fs::write(&issue_file_path, &content)?;
		any_written = true;
	}

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
	if should_cleanup && reader.is_mutable() {
		cleanup_old_locations(new, has_children, closed)?;
	}

	// If old identity had different parent_index, remove old file/directory
	if let Some(old) = maybe_old
		&& old.identity.parent_index != new.identity.parent_index
		&& reader.is_mutable()
		&& let Ok(old_path) = LocalPath::from(old).resolve_parent(*reader)
	{
		let old_location = old_path.deterministic(&old.contents.title, old.contents.state.is_closed(), !old.children.is_empty()).path();
		if old_location.is_dir() {
			let _ = std::fs::remove_dir_all(&old_location);
		} else if old_location.is_file() {
			let _ = std::fs::remove_file(&old_location);
		}
	}

	// Save metadata
	if let Some(issue_num) = new.git_id()
		&& let Some(timestamps) = new.identity.timestamps()
	{
		let meta = IssueMeta { timestamps: timestamps.clone() };
		Local::save_issue_meta(RepoInfo::new(&owner, &repo), issue_num, &meta)?;
	}

	// Recursively sink children - match by title (full_index changes when parent syncs)
	let old_children: Vec<&Issue> = maybe_old.map(|o| o.children.iter().collect()).unwrap_or_default();

	for child in &new.children {
		let old_child = old_children.iter().find(|c| c.contents.title == child.contents.title).copied();
		any_written |= sink_issue_node(child, old_child, reader)?;
	}

	// Remove deleted children (by title, since parent_index may have changed)
	for old_child in &old_children {
		if !new.children.iter().any(|c| c.contents.title == old_child.contents.title) {
			remove_issue_files(old_child, reader)?;
			if let Some(old_num) = old_child.git_id() {
				Local::remove_issue_meta(RepoInfo::new(&owner, &repo), old_num)?;
			}
		}
	}
	Ok(any_written)
}

/// Clean up old file locations when format or state changes.
///
/// Strategy: find all matching paths via `matching_subpaths`, compute the correct target path
/// via `deterministic`, then remove any matching paths that aren't the target.
#[instrument(skip(issue), fields(
	issue_number = ?issue.git_id(),
	title = %issue.contents.title,
	has_children,
	closed,
))]
fn cleanup_old_locations(issue: &Issue, has_children: bool, closed: bool) -> Result<()> {
	let parent_issue_idx = issue.parent_index();
	let parent_path = LocalPath::from(parent_issue_idx);
	let reader = FsReader;

	let resolved_parent_pwd = match parent_path.resolve_parent(reader) {
		Ok(r) => r,
		Err(e) if matches!(e.kind, LocalPathErrorKind::MissingParent | LocalPathErrorKind::NotFound) => {
			debug!("parent missing or not found, - nothing to clean");
			return Ok(());
		}
		Err(e) => return Err(e.into()),
	};

	fn is_main_file(path: &Path) -> bool {
		path.file_name().unwrap().to_string_lossy().starts_with(Local::MAIN_ISSUE_FILENAME) // use `starts_with`, as it could be `.bak`
	}

	match resolved_parent_pwd.matching_subpaths() {
		Ok(matches_for_parent) => {
			let misplaced_standalone_files: Vec<PathBuf> = matches_for_parent.into_iter().filter(|p| !p.is_dir() && !is_main_file(p)).collect();
			match misplaced_standalone_files.len() {
				0 => debug!("no files to cleanup for parent"),
				1 => {
					let standalone = misplaced_standalone_files.into_iter().next().unwrap();
					let name = standalone.file_name().unwrap().to_string_lossy();
					let is_closed = name.ends_with(".bak");
					let dirname = format!("{}/", name.strip_suffix(".md.bak").or_else(|| name.strip_suffix(".md")).unwrap());
					let main_file_path = Local::main_file_path(&standalone.parent().unwrap().join(dirname), is_closed);
					std::fs::create_dir_all(main_file_path.parent().unwrap())?;
					std::fs::rename(standalone, main_file_path)?;
				}
				_ => todo!("should have a good error here"),
			}
		}
		Err(e) if e.kind == LocalPathErrorKind::NotFound => {
			debug!("no trace of parent's dir or old issue files, - safe to assume issue doesn't exist either; nothing to clean");
			return Ok(());
		}
		Err(e) => return Err(e.into()),
	}

	// Get all existing paths that match our selector
	// If the directory doesn't exist yet, there's nothing to clean up
	let resolved_parent_dir = LocalPath::from(issue).resolve_parent(reader)?; //HACK: this is horrible. We recompute already resolved part of the path, just because we can't deterministically `select` as we do in `resolve_parent` //TODO: just update `select` to be aware of whether it's operating on last selector
	let matching = resolved_parent_dir.matching_subpaths()?; // Reader error is unreachable, - we've already checked for it

	// Remove any matching paths that aren't the target
	let title = &issue.contents.title;
	let target = resolved_parent_dir.deterministic(title, closed, has_children).path();
	for path in matching {
		match path == target {
			true => {
				trace!(path = %path.display(), "path matches target, keeping");
			}
			false => {
				debug!(path = %path.display(), "path differs from target, will remove");
				// If it's a __main__.md file, we need to check if the parent dir should be removed
				// (only if we're moving from directory to flat format)
				match is_main_file(&path) {
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

	// If issue has git_id, also cleanup old title-based paths
	if issue.git_id().is_some() {
		use crate::IssueSelector;
		// Build index with Title selector instead of GitId
		let mut title_index = issue.identity.parent_index;
		title_index.push(IssueSelector::title(title));
		let title_path = LocalPath::new(title_index);
		if let Ok(title_resolved) = title_path.resolve_parent(reader)
			&& let Ok(title_matching) = title_resolved.matching_subpaths()
		{
			for path in title_matching {
				if path != target {
					debug!(path = %path.display(), "removing old title-based path");
					if is_main_file(&path) {
						if let Some(parent_dir) = path.parent() {
							let _ = std::fs::remove_dir_all(parent_dir);
						}
					} else {
						let _ = try_remove_file(&path);
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
		Local::remove_issue_meta(RepoInfo::new(&owner, &repo), num)?;
	}

	info!(issue_number = ?issue.git_id(), "removed issue");
	Ok(true)
}
