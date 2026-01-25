//! Touch mode: create or open issues from paths.

use std::path::PathBuf;

use regex::Regex;
use tedi::{Issue, IssueSelector, LazyIssue, MinIssueDescriptor, local::Local};
use v_utils::prelude::*;

/// Result of parsing a touch path.
pub enum TouchPathResult {
	/// Found an existing issue file on disk.
	Exists(PathBuf),
	/// Issue doesn't exist, descriptor for creation.
	Create(MinIssueDescriptor),
}

/// Resolve a TouchPathResult to an Issue.
pub async fn resolve_touch_path(result: TouchPathResult) -> Result<Issue> {
	match result {
		TouchPathResult::Exists(path) => {
			let issue = <Issue as LazyIssue<Local>>::load(path.into()).await?;
			Ok(issue)
		}
		TouchPathResult::Create(descriptor) => {
			let project_is_virtual = Local::is_virtual_project(descriptor.repo_info());
			let issue = Issue::pending_from_descriptor(&descriptor, project_is_virtual);
			Ok(issue)
		}
	}
}

/// Parse a path for --touch mode using regex matching against filesystem
///
/// Format: `owner_regex/repo_regex/issue_regex[/sub_issue_regex...]`
///
/// Each segment is a regex matched against actual filesystem entries.
/// - First segment matches against owners in the issues directory
/// - Second segment matches against repos under the matched owner
/// - Remaining segments match against issue files/directories
///
/// If all segments match, returns `Exists` with the file path.
/// If owner/repo match but issue doesn't exist, returns `Create` with descriptor for creating a new issue.
/// For nested paths, matches as far as possible, then returns `Create` for the remaining title.
///
/// The final component may have `.md` extension which is stripped before matching.
pub fn parse_touch_path(user_input: &str) -> Result<TouchPathResult> {
	if user_input.starts_with('/') {
		bail!("Expecting semantic per-component match string for owner/repo/issue/optional-sub-issues, got: {user_input}")
	}

	let segments: Vec<&str> = user_input.split('/').collect();
	if segments.len() < 3 {
		bail!("Path must have at least 3 components: owner/repo/issue, got: {user_input}")
	}

	let owner_rgx = segments[0];
	let repo_rgx = segments[1];
	let lineage_rgxs = &segments[2..];
	let title = strip_md_extension(lineage_rgxs.last().unwrap());

	let mut actual_path = Local::issues_dir();

	// Match owner
	let owner_children = list_children(&actual_path)?;
	let owner = match match_single_or_none(&owner_children, owner_rgx) {
		MatchOrNone::Unique(matched) => matched,
		MatchOrNone::NoMatch => bail!("No owner matches pattern '{owner_rgx}'"),
		MatchOrNone::Ambiguous(matches) => {
			bail!("Ambiguous owner: pattern '{owner_rgx}' matches multiple entries.\nMatches: {}", matches.join(", "))
		}
	};
	actual_path = actual_path.join(&owner);

	// Match repo
	let repo_children = list_children(&actual_path)?;
	let repo = match match_single_or_none(&repo_children, repo_rgx) {
		MatchOrNone::Unique(matched) => matched,
		MatchOrNone::NoMatch => bail!("No repo matches pattern '{repo_rgx}' under owner '{owner}'"),
		MatchOrNone::Ambiguous(matches) => {
			bail!("Ambiguous repo: pattern '{repo_rgx}' matches multiple entries.\nMatches: {}", matches.join(", "))
		}
	};
	actual_path = actual_path.join(&repo);

	// Match remaining segments (issue and optional sub-issues)
	let mut matched_lineage: Vec<String> = Vec::new();
	for (i, segment) in lineage_rgxs.iter().enumerate() {
		let is_last = i == lineage_rgxs.len() - 1;
		let pattern = strip_md_extension(segment);

		let children = list_children(&actual_path)?;
		match match_single_or_none(&children, pattern) {
			MatchOrNone::Unique(matched) => {
				let matched_path = actual_path.join(&matched);

				// If matched a flat file but not the last segment, user wants to create a child.
				if !is_last && matched_path.is_file() {
					let parent_num = extract_issue_number(&matched).ok_or_else(|| eyre!("Cannot extract issue number from '{matched}'"))?;
					let mut index: Vec<IssueSelector> = matched_lineage.iter().filter_map(|s| extract_issue_number(s).map(IssueSelector::GitId)).collect();
					index.push(IssueSelector::GitId(parent_num));
					let child_title = strip_md_extension(lineage_rgxs[i + 1]);
					index.push(IssueSelector::Title(child_title.to_string()));
					return Ok(TouchPathResult::Create(MinIssueDescriptor::with_index(&owner, &repo, index)));
				}

				matched_lineage.push(matched);
				actual_path = matched_path;
			}
			MatchOrNone::NoMatch => {
				// No match - this is a create request
				let mut index: Vec<IssueSelector> = matched_lineage.iter().filter_map(|s| extract_issue_number(s).map(IssueSelector::GitId)).collect();
				index.push(IssueSelector::Title(pattern.to_string()));
				return Ok(TouchPathResult::Create(MinIssueDescriptor::with_index(&owner, &repo, index)));
			}
			MatchOrNone::Ambiguous(matches) => {
				let desc = if is_last { "issue" } else { "parent issue" };
				bail!("Ambiguous {desc}: pattern '{pattern}' matches multiple entries.\nMatches: {}", matches.join(", "))
			}
		}
	}

	// All segments matched - resolve to file path
	let file_path = if actual_path.is_file() {
		actual_path
	} else {
		// Directory format - find main file inside
		Local::main_file_in_dir(&actual_path).ok_or_else(|| eyre!("Matched directory but no main file found: {actual_path:?}"))?
	};
	Ok(TouchPathResult::Exists(file_path))
}

/// Strip .md extension if present
fn strip_md_extension(s: &str) -> &str {
	s.strip_suffix(".md").unwrap_or(s)
}

/// Extract issue number from a filename like "123_-_title" or "123"
fn extract_issue_number(name: &str) -> Option<u64> {
	if let Some(sep_pos) = name.find("_-_") {
		name[..sep_pos].parse().ok()
	} else {
		// Try parsing the whole thing as a number (for numberonly dirs)
		name.split('.').next()?.parse().ok()
	}
}

/// List children of a directory (names only, not full paths)
fn list_children(dir: &PathBuf) -> Result<Vec<String>> {
	if !dir.exists() {
		return Ok(Vec::new());
	}
	if !dir.is_dir() {
		return Ok(Vec::new());
	}

	let entries = std::fs::read_dir(dir)?;
	Ok(entries
		.flatten()
		.filter_map(|e| e.file_name().to_str().map(|s| s.to_string()))
		.filter(|name| !name.starts_with('.')) // Skip hidden files
		.collect())
}

/// Result of matching a pattern against children
enum MatchOrNone {
	Unique(String),
	NoMatch,
	Ambiguous(Vec<String>),
}

/// Match a pattern against children, distinguishing no-match from ambiguous
fn match_single_or_none(children: &[String], pattern: &str) -> MatchOrNone {
	let regex = match Regex::new(pattern) {
		Ok(r) => r,
		Err(_) => {
			// Invalid regex - treat pattern as literal
			if children.contains(&pattern.to_string()) {
				return MatchOrNone::Unique(pattern.to_string());
			}
			return MatchOrNone::NoMatch;
		}
	};

	let matches: Vec<&String> = children.iter().filter(|name| regex.is_match(name)).collect();

	match matches.len() {
		0 => MatchOrNone::NoMatch,
		1 => MatchOrNone::Unique(matches[0].clone()),
		_ => MatchOrNone::Ambiguous(matches.iter().map(|s| s.to_string()).collect()),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_touch_path_errors() {
		// Too few components
		assert!(parse_touch_path("owner/issue.md").is_err());
		assert!(parse_touch_path("issue.md").is_err());
	}

	#[test]
	fn test_parse_touch_path_nonexistent_owner_fails() {
		// When owner doesn't exist, parse_touch_path fails
		let result = parse_touch_path("nonexistent-owner/nonexistent-repo/my-issue.md");
		assert!(result.is_err());
	}

	#[test]
	fn test_extract_issue_number() {
		assert_eq!(extract_issue_number("123_-_my_title"), Some(123));
		assert_eq!(extract_issue_number("456"), Some(456));
		assert_eq!(extract_issue_number("123.md"), Some(123));
		assert_eq!(extract_issue_number("no_number"), None);
		assert_eq!(extract_issue_number("_-_title_only"), None);
	}

	#[test]
	fn test_match_single_or_none() {
		let children = vec!["foo".to_string(), "bar".to_string(), "foobar".to_string()];

		// Exact match
		assert!(matches!(match_single_or_none(&children, "foo"), MatchOrNone::Ambiguous(_))); // "foo" matches both "foo" and "foobar"

		// Unique match with anchors
		assert!(matches!(match_single_or_none(&children, "^foo$"), MatchOrNone::Unique(s) if s == "foo"));

		// No match
		assert!(matches!(match_single_or_none(&children, "^baz$"), MatchOrNone::NoMatch));

		// Ambiguous
		assert!(matches!(match_single_or_none(&children, "^f"), MatchOrNone::Ambiguous(_)));
	}
}
