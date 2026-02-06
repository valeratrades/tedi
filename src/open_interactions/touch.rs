//! Touch mode: create or open issues from paths.

use std::path::PathBuf;

use regex::Regex;
use tedi::{
	IssueIndex, IssueSelector, RepoInfo, github,
	local::{FsReader, Local, LocalError, LocalIssueSource, LocalPath, LocalPathError, LocalPathErrorKind, ReaderError},
};
use v_utils::utils::exit_on_error;

use super::command::ProjectType;

#[derive(Debug, thiserror::Error)]
pub enum TouchError {
	#[error(transparent)]
	Path(LocalPathError),

	#[error(transparent)]
	Local(LocalError),

	#[error("Ambiguous {kind}: pattern '{pattern}' matches multiple entries\nMatches: {matches}")]
	Ambiguous { kind: &'static str, pattern: String, matches: String },

	#[error("Repository '{owner}/{repo}' doesn't exist locally and is not accessible on GitHub\nCheck that the owner/repo is correct, or use --parent=virtual for local-only tracking.")]
	RepoNotAccessible { owner: String, repo: String },
}

impl From<LocalPathError> for TouchError {
	fn from(e: LocalPathError) -> Self {
		TouchError::Path(e)
	}
}

impl From<LocalError> for TouchError {
	fn from(e: LocalError) -> Self {
		TouchError::Local(e)
	}
}

/// Parse a path for --touch mode using regex matching against filesystem
///
/// Format: `owner_regex/repo_regex/issue_regex[/sub_issue_regex...]`
///
/// Each segment is a regex matched against actual filesystem entries.
/// - First segment matches against owners in the issues directory
/// - Second segment matches against repos under the matched owner
/// - Remaining segments use LocalPath with Regex selectors
///
/// If all segments match, returns source for existing issue.
/// If owner/repo match but issue doesn't exist, returns source for creation.
///
/// If owner/repo don't exist locally but are accessible on GitHub, returns source for creation.
/// If `parent` is Some, missing repos will be created (on GitHub or as virtual, depending on the value).
///
/// The final component may have `.md` extension which is stripped before matching.
pub async fn parse_touch_path(user_input: &str, parent: Option<ProjectType>, offline: bool) -> Result<LocalIssueSource<FsReader>, TouchError> {
	let segments: Vec<&str> = user_input.split('/').collect();
	if segments.len() < 3 {
		return Err(LocalPathError::not_found(IssueSelector::regex(user_input), Local::issues_dir()).into());
	}

	let owner_rgx = segments[0];
	let repo_rgx = segments[1];
	let issue_rgxs = &segments[2..];

	// Try to match locally first
	let local_result: Result<LocalIssueSource<FsReader>, TouchError> = async {
		let issues_dir = Local::issues_dir();

		// Match owner and repo
		let owner = regex_match_unique(&issues_dir, owner_rgx, "owner")?;
		let repo = regex_match_unique(&issues_dir.join(&owner), repo_rgx, "repo")?;

		let repo_info = RepoInfo::new(&owner, &repo);

		// Build LocalPath with Regex selectors
		let selectors: Vec<IssueSelector> = issue_rgxs.iter().map(|s| IssueSelector::regex(strip_md_extension(s))).collect();
		let index = IssueIndex::with_index(repo_info, selectors);
		let local_path = LocalPath::new(index);

		let resolved = local_path.resolve_parent(FsReader)?;

		// HACK: clone before search() since search() consumes self
		match resolved.clone().search() {
			Ok(found) => Ok(LocalIssueSource::<FsReader>::build_from_path(&found.path()).await?),
			Err(e) => match e.kind {
				// These mean we should create the issue
				LocalPathErrorKind::NotFound | LocalPathErrorKind::MissingParent | LocalPathErrorKind::ParentIsFlat => {
					let title = strip_md_extension(issue_rgxs.last().unwrap());
					let create_path = resolved.deterministic(title, false, false).path();
					Ok(LocalIssueSource::<FsReader>::build_from_path(&create_path).await?)
				}
				LocalPathErrorKind::NotUnique => Err(TouchError::Ambiguous {
					kind: "issue",
					pattern: issue_rgxs.last().unwrap().to_string(),
					matches: format!("{e}"),
				}),
				LocalPathErrorKind::Reader => Err(e.into()),
			},
		}
	}
	.await;

	if let Ok(source) = local_result {
		return Ok(source);
	}

	// Local match failed - try GitHub or create with --parent
	let owner = owner_rgx.to_string();
	let repo = repo_rgx.to_string();
	let repo_info = RepoInfo::new(&owner, &repo);

	// Check if we can access this repo on GitHub (unless offline)
	let repo_accessible = if offline { false } else { exit_on_error(github::client::get().repo_exists(repo_info).await) };

	if repo_accessible {
		let selectors: Vec<IssueSelector> = issue_rgxs.iter().map(|s| IssueSelector::title(strip_md_extension(s))).collect();
		let index = IssueIndex::with_index(repo_info, selectors);
		let local_path = LocalPath::new(index);
		let resolved = local_path.resolve_parent(FsReader)?;
		let title = strip_md_extension(issue_rgxs.last().unwrap());
		let create_path = resolved.deterministic(title, false, false).path();
		return Ok(LocalIssueSource::<FsReader>::build_from_path(&create_path).await?);
	}

	// Repo not accessible - check if we should create it with --parent
	match parent {
		Some(ProjectType::Virtual) => {
			Local::ensure_virtual_project(repo_info).expect("failed to create virtual project");
			let selectors: Vec<IssueSelector> = issue_rgxs.iter().map(|s| IssueSelector::title(strip_md_extension(s))).collect();
			let index = IssueIndex::with_index(repo_info, selectors);
			let local_path = LocalPath::new(index);
			let resolved = local_path.resolve_parent(FsReader)?;
			let title = strip_md_extension(issue_rgxs.last().unwrap());
			let create_path = resolved.deterministic(title, false, false).path();
			Ok(LocalIssueSource::<FsReader>::build_from_path(&create_path).await?)
		}
		Some(ProjectType::Default) | None => Err(TouchError::RepoNotAccessible { owner, repo }),
	}
}

fn strip_md_extension(s: &str) -> &str {
	s.strip_suffix(".md").unwrap_or(s)
}

/// Regex match a single entry in a directory. Returns error if no match or ambiguous.
fn regex_match_unique(dir: &PathBuf, pattern: &str, kind: &'static str) -> Result<String, TouchError> {
	if !dir.exists() || !dir.is_dir() {
		return Err(LocalPathError::not_found(IssueSelector::regex(pattern), dir.clone()).into());
	}

	let entries: Vec<String> = std::fs::read_dir(dir)
		.map_err(|e| LocalPathError::reader(IssueSelector::regex(pattern), ReaderError::other(e)))?
		.flatten()
		.filter_map(|e| e.file_name().to_str().map(|s| s.to_string()))
		.filter(|name| !name.starts_with('.'))
		.collect();

	let regex = Regex::new(pattern).expect("invalid regex pattern");
	let matches: Vec<&String> = entries.iter().filter(|name| regex.is_match(name)).collect();

	match matches.len() {
		0 => Err(LocalPathError::not_found(IssueSelector::regex(pattern), dir.clone()).into()),
		1 => Ok(matches[0].clone()),
		_ => Err(TouchError::Ambiguous {
			kind,
			pattern: pattern.to_string(),
			matches: matches.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(", "),
		}),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[tokio::test]
	async fn test_parse_touch_path_errors() {
		// Too few components (offline mode to avoid network)
		assert!(parse_touch_path("owner/issue.md", None, true).await.is_err());
		assert!(parse_touch_path("issue.md", None, true).await.is_err());
	}
}
