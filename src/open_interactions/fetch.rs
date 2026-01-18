//! Fetch issues from Github and store locally.

use std::path::PathBuf;

use todo::{Ancestry, CloseState, FetchedIssue, Issue};
use v_utils::prelude::*;

use super::{github_sync::IssueGithubExt, local::Local};
use crate::github::{BoxedGithubClient, GithubIssue};

/// Fetch the lineage (parent issue numbers) for an issue from GitHub.
///
/// Traverses up the parent chain to find the root issue.
/// Returns issue numbers from root to immediate parent (not including the target issue).
pub async fn fetch_lineage(gh: &BoxedGithubClient, owner: &str, repo: &str, issue_number: u64) -> Result<Vec<u64>> {
	let mut lineage = Vec::new();
	let mut current_issue_number = issue_number;

	while let Some(parent) = gh.fetch_parent_issue(owner, repo, current_issue_number).await? {
		lineage.push(parent.number);
		current_issue_number = parent.number;
	}

	// Reverse so it goes from root to immediate parent
	lineage.reverse();
	Ok(lineage)
}

/// Fetch an issue and all its sub-issues recursively, writing them to XDG_DATA.
///
/// For sub-issues: first fetches the lineage from GitHub, validates that all parent
/// issues exist locally, then fetches only the requested issue and its descendants.
/// Returns an error if any parent issue is not found locally.
///
/// Returns the path to the requested issue file.
pub async fn fetch_and_store_issue(gh: &BoxedGithubClient, owner: &str, repo: &str, issue_number: u64, ancestors: Option<Vec<FetchedIssue>>) -> Result<PathBuf> {
	// If we already have ancestor info, this is a recursive call - use it directly
	let ancestors = match ancestors {
		Some(a) => a,
		None => {
			// First, fetch the lineage from GitHub to know where this issue belongs
			let lineage = fetch_lineage(gh, owner, repo, issue_number).await?;

			if !lineage.is_empty() {
				// This is a sub-issue - validate parents exist locally and build FetchedIssue chain
				println!("Issue #{issue_number} is a sub-issue with lineage: {lineage:?}");

				let ancestry = Ancestry::with_lineage(owner, repo, &lineage);
				Local::build_ancestry_path(&ancestry)?
			} else {
				vec![]
			}
		}
	};

	store_issue_tree(gh, owner, repo, issue_number, ancestors).await
}

/// Store an issue and all its sub-issues recursively.
/// This is the core logic shared by all issue fetching operations.
fn store_issue_tree<'a>(
	gh: &'a BoxedGithubClient,
	owner: &'a str,
	repo: &'a str,
	issue_number: u64,
	ancestors: Vec<FetchedIssue>,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<PathBuf>> + Send + 'a>> {
	Box::pin(async move {
		// Fetch issue data
		let (current_user, issue, comments, sub_issues) = tokio::try_join!(
			gh.fetch_authenticated_user(),
			gh.fetch_issue(owner, repo, issue_number),
			gh.fetch_comments(owner, repo, issue_number),
			gh.fetch_sub_issues(owner, repo, issue_number),
		)?;

		store_issue_node(gh, owner, repo, &issue, &comments, &sub_issues, &current_user, ancestors).await
	})
}

/// Store a single issue node and recurse into its children.
/// Extracted to allow reuse when we already have the GithubIssue.
#[allow(clippy::too_many_arguments)]
async fn store_issue_node(
	gh: &BoxedGithubClient,
	owner: &str,
	repo: &str,
	issue: &GithubIssue,
	comments: &[crate::github::GithubComment],
	sub_issues: &[GithubIssue],
	current_user: &str,
	ancestors: Vec<FetchedIssue>,
) -> Result<PathBuf> {
	// Filter out duplicate sub-issues - they shouldn't appear locally
	let filtered_sub_issues: Vec<_> = sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).cloned().collect();

	let issue_closed = issue.state == "closed";
	let has_sub_issues = !filtered_sub_issues.is_empty();

	// Determine file path - use directory format if there are sub-issues
	let issue_file_path = if has_sub_issues {
		// Use directory format: {dir}/__main__.md
		let issue_dir = Local::issue_dir_path(owner, repo, Some(issue.number), &issue.title, &ancestors);
		std::fs::create_dir_all(&issue_dir)?;

		// Clean up old flat file if it exists (format is changing)
		let old_flat_path = Local::issue_file_path(owner, repo, Some(issue.number), &issue.title, false, &ancestors);
		if old_flat_path.exists() {
			std::fs::remove_file(&old_flat_path)?;
		}
		let old_flat_closed = Local::issue_file_path(owner, repo, Some(issue.number), &issue.title, true, &ancestors);
		if old_flat_closed.exists() {
			std::fs::remove_file(&old_flat_closed)?;
		}

		Local::main_file_path(&issue_dir, issue_closed)
	} else {
		// Check if there's an existing file (might be in either format)
		if let Some(existing) = Local::find_issue_file(owner, repo, Some(issue.number), &issue.title, &ancestors) {
			existing
		} else {
			// No existing file, use flat format
			Local::issue_file_path(owner, repo, Some(issue.number), &issue.title, issue_closed, &ancestors)
		}
	};

	// Create parent directories
	if let Some(parent) = issue_file_path.parent() {
		std::fs::create_dir_all(parent)?;
	}

	// Convert GitHub data to Issue struct, then serialize for filesystem storage
	// Note: from_github takes sub_issues for building children metadata, but serialize_filesystem
	// doesn't embed them (they're stored as separate files via the recursive calls below)
	let issue_struct = Issue::from_github(issue, comments, &filtered_sub_issues, owner, repo, current_user);
	let content = issue_struct.serialize_filesystem();
	std::fs::write(&issue_file_path, &content)?;

	// Build ancestors for children (current issue becomes part of ancestors)
	let mut child_ancestors = ancestors;
	let this_issue = FetchedIssue::from_parts(owner, repo, issue.number, &issue.title).ok_or_else(|| eyre!("Failed to construct FetchedIssue for #{}", issue.number))?;
	child_ancestors.push(this_issue);

	// Recursively fetch and store all sub-issues
	for sub_issue in &filtered_sub_issues {
		if let Err(e) = store_issue_tree(gh, owner, repo, sub_issue.number, child_ancestors.clone()).await {
			eprintln!("Warning: Failed to fetch sub-issue #{}: {e}", sub_issue.number);
		}
	}

	Ok(issue_file_path)
}
