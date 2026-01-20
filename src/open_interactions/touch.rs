//! Touch mode: create or open issues from paths.

use std::path::PathBuf;

use regex::Regex;
use tedi::Ancestry;
use v_utils::prelude::*;

use super::local::Local;

/// Result of parsing a touch path
#[derive(Debug)]
pub enum TouchPathResult {
	/// Found an existing issue - ancestry identifies it completely
	Found(Ancestry),
	/// No existing issue found, but path is valid for creation.
	/// Ancestry contains owner/repo and parent lineage (empty if root issue).
	Create { title: String, ancestry: Ancestry },
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
/// If all segments match, returns `Found` with the ancestry.
/// If owner/repo match but issue doesn't exist, returns `Create` with details for creating a new issue.
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

	let mut actual_path = Local::issues_dir();

	// Match owner
	let owner_children = list_children(&actual_path)?;
	let owner = match match_single_or_none(&owner_children, segments[0]) {
		MatchOrNone::Unique(matched) => matched,
		MatchOrNone::NoMatch => {
			// Owner doesn't exist - create request with literal names
			return Ok(TouchPathResult::Create {
				title: strip_md_extension(segments.last().unwrap()).to_string(),
				ancestry: Ancestry::root(segments[0], segments[1]),
			});
		}
		MatchOrNone::Ambiguous(matches) => {
			bail!("Ambiguous owner: pattern '{}' matches multiple entries.\nMatches: {}", segments[0], matches.join(", "))
		}
	};
	actual_path = actual_path.join(&owner);

	// Match repo
	let repo_children = list_children(&actual_path)?;
	let repo = match match_single_or_none(&repo_children, segments[1]) {
		MatchOrNone::Unique(matched) => matched,
		MatchOrNone::NoMatch => {
			// Repo doesn't exist - create request
			return Ok(TouchPathResult::Create {
				title: strip_md_extension(segments.last().unwrap()).to_string(),
				ancestry: Ancestry::root(&owner, segments[1]),
			});
		}
		MatchOrNone::Ambiguous(matches) => {
			bail!("Ambiguous repo: pattern '{}' matches multiple entries.\nMatches: {}", segments[1], matches.join(", "))
		}
	};
	actual_path = actual_path.join(&repo);

	// Match remaining segments (issue and optional sub-issues)
	let mut lineage: Vec<u64> = Vec::new();
	for (i, segment) in segments[2..].iter().enumerate() {
		let is_last = i == segments.len() - 3; // -3 because we skip first 2 (owner/repo)
		let pattern = strip_md_extension(segment);

		let children = list_children(&actual_path)?;
		match match_single_or_none(&children, pattern) {
			MatchOrNone::Unique(matched) => {
				let matched_path = actual_path.join(&matched);

				// Extract issue number from matched name
				if let Some(issue_num) = extract_issue_number(&matched) {
					lineage.push(issue_num);
				}

				// If matched a flat file but not the last segment, user wants to create a child.
				// We still add the issue to lineage (done above) - the Sink will handle
				// converting the flat file to directory format when creating the sub-issue.
				if !is_last && matched_path.is_file() {
					// Can't descend into a file, but we've recorded the parent in lineage.
					// Remaining segments define what to create under this parent.
					let remaining_title = segments[2 + i + 1..].last().map(|s| strip_md_extension(s)).unwrap_or(pattern);
					return Ok(TouchPathResult::Create {
						title: remaining_title.to_string(),
						ancestry: Ancestry::with_lineage(&owner, &repo, &lineage),
					});
				}

				actual_path = matched_path;
			}
			MatchOrNone::NoMatch => {
				// No match - this is a create request
				return Ok(TouchPathResult::Create {
					title: pattern.to_string(),
					ancestry: Ancestry::with_lineage(&owner, &repo, &lineage),
				});
			}
			MatchOrNone::Ambiguous(matches) => {
				let desc = if is_last { "issue" } else { "parent issue" };
				bail!("Ambiguous {desc}: pattern '{pattern}' matches multiple entries.\nMatches: {}", matches.join(", "))
			}
		}
	}

	// All segments matched - build ancestry from what we found
	Ok(TouchPathResult::Found(Ancestry::with_lineage(&owner, &repo, &lineage)))
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

/// Create a pending issue locally that will be pushed to Github on first sync.
/// The issue file is created with an empty identity marker `<!-- -->` indicating it's pending.
/// When the user saves and syncs, the Sink trait will create the issue on Github.
///
/// Ancestry contains owner/repo and parent lineage (if creating a sub-issue).
pub fn create_pending_issue(title: &str, ancestry: &Ancestry) -> Result<PathBuf> {
	// Build ancestor directory names if creating a sub-issue
	let ancestor_dir_names = if ancestry.lineage().is_empty() { vec![] } else { Local::build_ancestor_dir_names(ancestry)? };

	// Determine file path - None for number since it's pending
	let issue_file_path = Local::issue_file_path_from_dir_names(ancestry.owner(), ancestry.repo(), None, title, false, &ancestor_dir_names);

	// Create parent directories
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Create the issue file with pending marker (empty identity)
	// Format: `- [ ] Title <!-- -->` means pending issue
	let content = format!("- [ ] {title} <!-- -->\n\t\n");

	std::fs::write(&issue_file_path, &content)?;

	if !ancestry.lineage().is_empty() {
		println!("Created pending sub-issue: {title}");
	} else {
		println!("Created pending issue: {title}");
	}
	println!("Stored at: {issue_file_path:?}");
	println!("Issue will be created on Github when you save and sync.");

	Ok(issue_file_path)
}

/// Create a new virtual issue locally (no Github).
/// Virtual issues have locally-generated issue numbers and are stored in the same format.
pub fn create_virtual_issue(title: &str, ancestry: &Ancestry) -> Result<PathBuf> {
	let owner = ancestry.owner();
	let repo = ancestry.repo();

	// Ensure virtual project exists (creates if needed)
	Local::ensure_virtual_project(owner, repo)?;

	// Allocate a virtual issue number (for metadata tracking, not filename)
	let issue_number = Local::allocate_virtual_issue_number(owner, repo)?;

	// Determine file path (no number prefix for virtual issues)
	let issue_file_path = Local::issue_file_path(owner, repo, None, title, false, &[]);

	// Create parent directories
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Create the issue file with basic structure
	// Virtual issues don't have a Github URL, so we use a special marker
	let content = format!("- [ ] {title} <!--virtual:{owner}/{repo}#{issue_number}-->\n");

	std::fs::write(&issue_file_path, &content)?;

	println!("Created virtual issue #{issue_number}: {title}");
	println!("Stored at: {issue_file_path:?}");

	Ok(issue_file_path)
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
	fn test_parse_touch_path_nonexistent_creates() {
		// When nothing exists on filesystem, parse_touch_path returns Create
		let result = parse_touch_path("nonexistent-owner/nonexistent-repo/my-issue.md").unwrap();
		match result {
			TouchPathResult::Create { title, ancestry } => {
				assert_eq!(ancestry.owner(), "nonexistent-owner");
				assert_eq!(ancestry.repo(), "nonexistent-repo");
				assert_eq!(title, "my-issue");
				assert!(ancestry.lineage().is_empty());
			}
			TouchPathResult::Found(_) => panic!("Expected Create, got Found"),
		}
	}

	#[test]
	fn test_strip_md_extension() {
		assert_eq!(strip_md_extension("issue.md"), "issue");
		assert_eq!(strip_md_extension("issue"), "issue");
		assert_eq!(strip_md_extension("issue.txt"), "issue.txt");
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
