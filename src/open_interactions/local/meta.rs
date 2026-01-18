//! Project and issue metadata.
//!
//! This module provides:
//! 1. Path identity parsing - extracting issue number/title from file paths
//! 2. .meta.json operations - project config and per-issue metadata
//!
//! The .meta.json file stores:
//! - Virtual project configuration (for offline-only projects)
//! - Per-issue metadata keyed by issue number (timestamps, etc.)
//!
//! Issue structural metadata (title, parent) is derived from file paths.
//! Consensus state for sync comes from git (last committed version).

use std::path::PathBuf;

use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use v_utils::prelude::*;

use super::files::{MAIN_ISSUE_FILENAME, extract_owner_repo_from_path, get_project_dir, issues_dir};

//==============================================================================
// Path Identity Parsing
//==============================================================================

/// Issue identity extracted from a file path.
///
/// This is NOT metadata from .meta.json - it's computed on demand by parsing
/// the file path structure: `{number}_-_{title}.md` or `{number}_-_{title}/__main__.md`
#[derive(Clone, Debug)]
pub struct PathIdentity {
	pub issue_number: u64,
	pub title: String,
	/// Parent issue number if this is a sub-issue
	pub parent_issue: Option<u64>,
}

/// Parse issue identity from a file path.
///
/// Extracts issue number, title, and parent from the path structure.
/// Handles both flat format (`{number}_-_{title}.md`) and directory format
/// (`{number}_-_{title}/__main__.md`).
///
/// Returns `issue_number = 0` for pending issues (no number assigned yet).
pub fn parse_path_identity(issue_file_path: &std::path::Path) -> Result<PathIdentity> {
	let (_owner, _repo) = extract_owner_repo_from_path(issue_file_path)?;

	// Extract issue number and title from filename or parent directory name
	let filename = issue_file_path.file_name().and_then(|n| n.to_str()).ok_or_else(|| eyre!("Invalid issue file path"))?;

	// Handle .bak suffix for closed issues
	let filename_no_bak = filename.strip_suffix(".bak").unwrap_or(filename);

	// Check if this is a __main__ file (directory format)
	let (name_to_parse, parent_dir) = if filename_no_bak.starts_with(MAIN_ISSUE_FILENAME) {
		// Get issue number from parent directory name instead
		// Parent directory format: {number}_-_{title}
		let parent_dir = issue_file_path.parent();
		let name = parent_dir
			.and_then(|p| p.file_name())
			.and_then(|n| n.to_str())
			.ok_or_else(|| eyre!("Could not extract parent directory for __main__ file"))?;
		(name, parent_dir)
	} else {
		// Strip extension for flat format
		let name = filename_no_bak.strip_suffix(".md").unwrap_or(filename_no_bak);
		(name, issue_file_path.parent())
	};

	// Extract issue number and title from name format: {number}_-_{title}, just {number}, or just {title} (pending)
	let (issue_number, title) = if let Some(sep_pos) = name_to_parse.find("_-_") {
		let number: u64 = name_to_parse[..sep_pos].parse()?;
		let title = name_to_parse[sep_pos + 3..].replace('_', " ");
		(number, title)
	} else if let Ok(number) = name_to_parse.parse::<u64>() {
		// Just a number, no title separator
		(number, String::new())
	} else {
		// No number - this is a pending issue (title only)
		// Use 0 as a sentinel value for pending issues
		let title = name_to_parse.replace('_', " ");
		(0, title)
	};

	// Determine parent issue by looking at the directory structure
	// Path structure: issues/{owner}/{repo}/{parent_dir}?/{file}
	// If there's a parent_dir that looks like {number}_-_{title}, it's a sub-issue
	let parent_issue = if let Some(parent) = parent_dir {
		// Check if the parent is the issue directory (for __main__ files) or grandparent (for nested issues)
		let check_dir = if filename_no_bak.starts_with(MAIN_ISSUE_FILENAME) {
			// For __main__.md, the parent dir is THIS issue's dir, check grandparent for parent issue
			parent.parent()
		} else {
			// For flat files, check the parent directory
			Some(parent)
		};

		if let Some(dir) = check_dir {
			// Get relative path from issues base
			let issues_base = issues_dir();
			if let Ok(rel) = dir.strip_prefix(&issues_base) {
				// Skip owner/repo components
				let components: Vec<_> = rel.components().collect();
				if components.len() > 2 {
					// There's a parent issue directory
					if let Some(parent_dir_name) = components.last().and_then(|c| c.as_os_str().to_str()) {
						// Try to extract parent issue number
						if let Some(sep_pos) = parent_dir_name.find("_-_") {
							parent_dir_name[..sep_pos].parse::<u64>().ok()
						} else {
							parent_dir_name.parse::<u64>().ok()
						}
					} else {
						None
					}
				} else {
					None
				}
			} else {
				None
			}
		} else {
			None
		}
	} else {
		None
	};

	Ok(PathIdentity { issue_number, title, parent_issue })
}

//==============================================================================
// .meta.json Project Metadata
//==============================================================================

/// Project-level metadata file.
/// Stored at: issues/{owner}/{repo}/.meta.json
///
/// Contains both project configuration and per-issue metadata.
/// Note: owner/repo are NOT stored here - they're derived from the file path.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ProjectMeta {
	/// Virtual project: has no Github remote, all operations are offline-only.
	/// Issue numbers are locally generated (starting from 1).
	#[serde(default)]
	pub virtual_project: bool,
	/// Next issue number for virtual projects (auto-incremented)
	#[serde(default)]
	pub next_virtual_issue_number: u64,
	/// Per-issue metadata, keyed by issue number.
	#[serde(default, skip_serializing_if = "std::collections::HashMap::is_empty")]
	pub issues: std::collections::HashMap<u64, IssueMeta>,
}

/// Per-issue metadata stored in .meta.json.
/// Keyed by issue number in the issues map.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct IssueMeta {
	/// Timestamp of last content change (body/comments, not children).
	/// Used for sync conflict resolution.
	#[serde(default, skip_serializing_if = "Option::is_none")]
	pub ts: Option<Timestamp>,
}

/// Get the metadata file path for a project
pub fn get_project_meta_path(owner: &str, repo: &str) -> PathBuf {
	get_project_dir(owner, repo).join(".meta.json")
}

/// Load project metadata, creating empty if not exists
pub fn load_project_meta(owner: &str, repo: &str) -> ProjectMeta {
	let meta_path = get_project_meta_path(owner, repo);
	match std::fs::read_to_string(&meta_path) {
		Ok(content) => serde_json::from_str(&content).unwrap_or_default(),
		Err(_) => ProjectMeta::default(),
	}
}

/// Save project metadata
pub fn save_project_meta(owner: &str, repo: &str, meta: &ProjectMeta) -> Result<()> {
	let meta_path = get_project_meta_path(owner, repo);
	if let Some(parent) = meta_path.parent() {
		std::fs::create_dir_all(parent)?;
	}
	let content = serde_json::to_string_pretty(meta)?;
	std::fs::write(&meta_path, content)?;
	Ok(())
}

//==============================================================================
// Virtual Project Support
//==============================================================================

/// Check if a project is virtual (has no Github remote)
pub fn is_virtual_project(owner: &str, repo: &str) -> bool {
	load_project_meta(owner, repo).virtual_project
}

/// Allocate the next issue number for a virtual project.
/// Returns the allocated number and saves the updated meta.
pub fn allocate_virtual_issue_number(owner: &str, repo: &str) -> Result<u64> {
	let mut project_meta = load_project_meta(owner, repo);
	if !project_meta.virtual_project {
		bail!("Cannot allocate virtual issue number for non-virtual project {owner}/{repo}");
	}

	// Ensure we start from 1
	if project_meta.next_virtual_issue_number == 0 {
		project_meta.next_virtual_issue_number = 1;
	}

	let issue_number = project_meta.next_virtual_issue_number;
	project_meta.next_virtual_issue_number += 1;
	save_project_meta(owner, repo, &project_meta)?;

	Ok(issue_number)
}

/// Create or load a virtual project meta. If project doesn't exist, creates it as virtual.
/// If it exists and is not virtual, returns an error.
pub fn ensure_virtual_project(owner: &str, repo: &str) -> Result<ProjectMeta> {
	let meta_path = get_project_meta_path(owner, repo);
	if meta_path.exists() {
		let project_meta = load_project_meta(owner, repo);
		if !project_meta.virtual_project {
			bail!("Project {owner}/{repo} exists but is not a virtual project");
		}
		Ok(project_meta)
	} else {
		// Create new virtual project
		let project_meta = ProjectMeta {
			virtual_project: true,
			next_virtual_issue_number: 1,
			issues: std::collections::HashMap::new(),
		};
		save_project_meta(owner, repo, &project_meta)?;
		Ok(project_meta)
	}
}

//==============================================================================
// Per-Issue Metadata Operations
//==============================================================================

/// Load metadata for a specific issue from the project's .meta.json.
/// Returns None if no metadata exists for this issue.
pub fn load_issue_meta(owner: &str, repo: &str, issue_number: u64) -> Option<IssueMeta> {
	let project_meta = load_project_meta(owner, repo);
	project_meta.issues.get(&issue_number).cloned()
}

/// Save metadata for a specific issue to the project's .meta.json.
/// Creates the file if it doesn't exist.
pub fn save_issue_meta(owner: &str, repo: &str, issue_number: u64, meta: &IssueMeta) -> Result<()> {
	let mut project_meta = load_project_meta(owner, repo);
	project_meta.issues.insert(issue_number, meta.clone());
	save_project_meta(owner, repo, &project_meta)
}

/// Remove metadata for a specific issue from the project's .meta.json.
/// Does nothing if the issue has no stored metadata.
pub fn remove_issue_meta(owner: &str, repo: &str, issue_number: u64) -> Result<()> {
	let mut project_meta = load_project_meta(owner, repo);
	if project_meta.issues.remove(&issue_number).is_some() {
		save_project_meta(owner, repo, &project_meta)?;
	}
	Ok(())
}
