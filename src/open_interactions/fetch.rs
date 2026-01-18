//! Fetch issues from Github and store locally.
//!
//! This module provides the `fetch_and_store_issue` function which:
//! 1. Loads an issue tree from GitHub via `LazyIssue<Remote>`
//! 2. Writes it to the local filesystem via `Sink<Local>`

use std::path::PathBuf;

use todo::{Ancestry, FetchedIssue, Issue};
use v_utils::prelude::*;

use super::{
	local::Local,
	remote::{Remote, RemoteSource},
	sink::IssueSinkExt,
};
use crate::github::BoxedGithubClient;

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
			let lineage = Remote::fetch_lineage(gh, owner, repo, issue_number).await?;

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

	// Load issue tree from GitHub via LazyIssue<Remote>
	let source = RemoteSource::root(gh.clone(), owner, repo, issue_number);
	let ancestry = Ancestry::root(owner, repo);
	let mut issue = Issue::empty_local(ancestry);

	<Issue as todo::LazyIssue<Remote>>::identity(&mut issue, source.clone()).await;
	<Issue as todo::LazyIssue<Remote>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut issue, source)).await;

	// Write to local filesystem via Sink<Local>
	issue.sink_local(None).await?;

	// Determine the file path (matches logic from sink_local)
	let issue_number = issue.number().expect("issue loaded from GitHub has number");
	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	let has_children = !issue.children.is_empty();

	let issue_file_path = if has_children {
		let issue_dir = Local::issue_dir_path(owner, repo, Some(issue_number), title, &ancestors);
		Local::main_file_path(&issue_dir, closed)
	} else if let Some(existing) = Local::find_issue_file(owner, repo, Some(issue_number), title, &ancestors) {
		existing
	} else {
		Local::issue_file_path(owner, repo, Some(issue_number), title, closed, &ancestors)
	};

	Ok(issue_file_path)
}
