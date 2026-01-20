//! Touch mode: create or open issues from paths.

use std::path::PathBuf;

use v_utils::prelude::*;

use super::local::Local;

/// Parsed touch path components
/// Format: workspace/project/issue[.md] or workspace/project/parent/child[.md] (for sub-issues)
#[derive(Debug)]
pub struct TouchPath {
	pub owner: String,
	pub repo: String,
	/// Chain of issue titles (parent issues first, the target issue last)
	/// For a simple issue: ["issue_title"]
	/// For a sub-issue: ["parent_title", "child_title"]
	/// For nested: ["grandparent", "parent", "child"]
	pub issue_chain: Vec<String>,
}

/// Parse a path for --touch mode
/// Format: workspace/project/issue[.md] or workspace/project/parent_issue/child_issue[.md]
//TODO: at the callsite of this thing, we reject creation of nested sub-issues. WRONG
pub fn parse_touch_path(user_input: &str) -> Result<TouchPath /*TODO: switch to return Ancestry*/> {
	if user_input.starts_with('/') {
		// don't think there is an actually good way to ensure semantic correctness of the user input beyond this
		bail!("Expecting semantic per-component match string for owner/repo/issue/optional-sub-issues, got: {user_input}")
	}
	let mut subpath_regexes: Vec<String> = user_input.split('/').cloned().collect();

	// step-wise construct through string-matching, starting at issues-dir top
	let mut actual_path = Local::issues_dir();
	for rgx in subpath_regexes {
		let children = actual_path.children(); // pseudo-code
		let matches = apply_regex(children);
		if !matches.len() == 1 {
			bail!("a good miette error pointing to regex substring that didn't match (in the context of full submitted input)")
		}
		actual_path.extend(PathBuf::from(matches[0])).unwrap();
	}

	let ancestry = Local::extract_ancestry(&actual_path).unwrap();

	//TODO: delegate impl-sensitive parts (ie issue dirs top) to Local // probably would pay to separate the patten matching logic into its own module too btw

	//TODO: update to be constructing Ancestry
	Ok(ancestry)
}

/// Create a pending issue locally that will be pushed to Github on first sync.
/// The issue file is created with an empty identity marker `<!-- -->` indicating it's pending.
/// When the user saves and syncs, the Sink trait will create the issue on Github.
pub fn create_pending_issue(touch_path: &TouchPath) -> Result<PathBuf> {
	let owner = &touch_path.owner;
	let repo = &touch_path.repo;

	// For now, only support single-level issues (no sub-issues for pending creation)
	if touch_path.issue_chain.len() > 1 {
		bail!(
			"Cannot create nested sub-issue via --touch.\n\
			 Only immediate issues are supported for deferred creation.\n\
			 Path: {owner}/{repo}/{}\n\
			 \n\
			 For sub-issues, create the parent first, then edit it to add children.",
			touch_path.issue_chain.join("/")
		);
	}

	// Get the issue title
	let issue_title = touch_path.issue_chain.last().unwrap();

	// Determine file path - use "pending" as placeholder for the number
	let issue_file_path = Local::issue_file_path(owner, repo, None, issue_title, false, &[]);

	// Create parent directories
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Create the issue file with pending marker (empty identity)
	// Format: `- [ ] Title <!-- -->` means pending issue
	let content = format!("- [ ] {issue_title} <!-- -->\n\t\n");

	std::fs::write(&issue_file_path, &content)?;

	println!("Created pending issue: {issue_title}");
	println!("Stored at: {issue_file_path:?}");
	println!("Issue will be created on Github when you save and sync.");

	Ok(issue_file_path)
}

/// Create a new virtual issue locally (no Github).
/// Virtual issues have locally-generated issue numbers and are stored in the same format.
pub fn create_virtual_issue(touch_path: &TouchPath) -> Result<PathBuf> {
	let owner = &touch_path.owner;
	let repo = &touch_path.repo;

	// Ensure virtual project exists (creates if needed)
	Local::ensure_virtual_project(owner, repo)?;

	// For now, only support single-level issues (no sub-issues for virtual projects)
	if touch_path.issue_chain.len() > 1 {
		// TODO: Support sub-issues for virtual projects
		bail!("Sub-issues are not yet supported for virtual projects. Use a flat issue structure.");
	}

	// Get the issue title (last in chain)
	let issue_title = touch_path.issue_chain.last().unwrap();

	// Allocate a virtual issue number (for metadata tracking, not filename)
	let issue_number = Local::allocate_virtual_issue_number(owner, repo)?;

	// Determine file path (no number prefix for virtual issues)
	let issue_file_path = Local::issue_file_path(owner, repo, None, issue_title, false, &[]);

	// Create parent directories
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Create the issue file with basic structure
	// Virtual issues don't have a Github URL, so we use a special marker
	let content = format!("- [ ] {issue_title} <!--virtual:{owner}/{repo}#{issue_number}-->\n");

	std::fs::write(&issue_file_path, &content)?;

	// No longer saving metadata - it's derived from file paths

	println!("Created virtual issue #{issue_number}: {issue_title}");
	println!("Stored at: {issue_file_path:?}");

	Ok(issue_file_path)
}

/// Try to find an existing local issue file matching the touch path
/// Returns the path if found, None otherwise
pub fn find_local_issue_for_touch(touch_path: &TouchPath) -> Option<PathBuf> {
	let issues_base = Local::issues_dir();

	// Path structure: issues/{owner}/{repo}/{number}_-_{title}.md
	let project_dir = issues_base.join(&touch_path.owner).join(&touch_path.repo);
	if !project_dir.exists() {
		return None;
	}

	// Search for files matching the issue title (last in chain)
	let issue_title = touch_path.issue_chain.last()?;
	// Sanitize and lowercase for comparison
	let sanitized_title_lower = Local::sanitize_title(issue_title).to_lowercase();

	// Search using the sanitized title
	if let Ok(matches) = Local::search_issue_files(&sanitized_title_lower) {
		// Filter matches to only those in the correct project directory
		for path in matches {
			// Check if it's in the right project directory
			if !path.starts_with(&project_dir) {
				continue;
			}

			// Check extension matches
			if path.extension().and_then(|e| e.to_str()) != Some("md") {
				continue;
			}

			// Check the filename contains the sanitized title
			// Filename format: {number}_-_{sanitized_title}.md
			if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
				let stem_lower = stem.to_lowercase();
				// Extract the title part after "_-_" if present
				let title_part = stem_lower.split("_-_").nth(1).unwrap_or(&stem_lower);
				if title_part == sanitized_title_lower {
					return Some(path);
				}
			}
		}
	}

	None
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_touch_path_simple_with_extension() {
		// Simple issue with extension: workspace/project/issue.md
		let result = parse_touch_path("owner/repo/my-issue.md").unwrap();
		assert_eq!(result.owner, "owner");
		assert_eq!(result.repo, "repo");
		assert_eq!(result.issue_chain, vec!["my-issue"]);
	}

	#[test]
	fn test_parse_touch_path_simple_without_extension() {
		// Simple issue without extension: workspace/project/issue
		let result = parse_touch_path("owner/repo/my-issue").unwrap();
		assert_eq!(result.owner, "owner");
		assert_eq!(result.repo, "repo");
		assert_eq!(result.issue_chain, vec!["my-issue"]);
	}

	#[test]
	fn test_parse_touch_path_sub_issue() {
		// Sub-issue: workspace/project/parent/child.md
		let result = parse_touch_path("owner/repo/parent-issue/child-issue.md").unwrap();
		assert_eq!(result.owner, "owner");
		assert_eq!(result.repo, "repo");
		assert_eq!(result.issue_chain, vec!["parent-issue", "child-issue"]);
	}

	#[test]
	fn test_parse_touch_path_nested_sub_issue() {
		// Nested sub-issue: workspace/project/grandparent/parent/child.md
		let result = parse_touch_path("owner/repo/grandparent/parent/child.md").unwrap();
		assert_eq!(result.owner, "owner");
		assert_eq!(result.repo, "repo");
		assert_eq!(result.issue_chain, vec!["grandparent", "parent", "child"]);
	}

	#[test]
	fn test_parse_touch_path_unknown_extension_treated_as_no_extension() {
		// Unknown extension is treated as part of the filename (no extension detected)
		let result = parse_touch_path("owner/repo/issue.txt").unwrap();
		assert_eq!(result.owner, "owner");
		assert_eq!(result.repo, "repo");
		// "issue.txt" is treated as the issue title since .txt is not a valid extension
		assert_eq!(result.issue_chain, vec!["issue.txt"]);
	}

	#[test]
	fn test_parse_touch_path_errors() {
		// Too few components
		assert!(parse_touch_path("owner/issue.md").is_err());
		assert!(parse_touch_path("issue.md").is_err());
	}
}
