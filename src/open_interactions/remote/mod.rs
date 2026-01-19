//! Remote GitHub operations for issues.
//!
//! This module handles all remote GitHub concerns:
//! - Fetching issues via `LazyIssue<Remote>`
//! - Pushing changes via `Sink<Remote>` (defined in sink.rs, uses helpers from here)
//!
//! The key insight is that `LazyIssue<Remote>` mirrors `LazyIssue<Local>`:
//! - Local loads from PathBuf (filesystem)
//! - Remote loads from IssueLink + optional lineage

use jiff::Timestamp;

//==============================================================================
// Error Types
//==============================================================================

/// Error type for remote GitHub operations.
#[derive(Debug, thiserror::Error)]
pub enum RemoteError {
	/// Failed to fetch issue from GitHub.
	#[error("failed to fetch issue #{number} from {owner}/{repo}")]
	FetchIssue {
		owner: String,
		repo: String,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to fetch issue comments from GitHub.
	#[error("failed to fetch comments for issue #{number} from {owner}/{repo}")]
	FetchComments {
		owner: String,
		repo: String,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to fetch sub-issues from GitHub.
	#[error("failed to fetch sub-issues for issue #{number} from {owner}/{repo}")]
	FetchSubIssues {
		owner: String,
		repo: String,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to resolve ancestry (parent issue chain).
	#[error("failed to resolve ancestry for issue #{number} in {owner}/{repo}")]
	ResolveAncestry {
		owner: String,
		repo: String,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Issue not found on GitHub (404).
	#[error("issue #{number} not found in {owner}/{repo}")]
	NotFound { owner: String, repo: String, number: u64 },
}
use todo::{Ancestry, CloseState, Comment, CommentIdentity, Issue, IssueChangeTimestamps, IssueContents, IssueIdentity, IssueLink, MAX_LINEAGE_DEPTH, split_blockers};
use v_utils::prelude::*;

use crate::github::{self, GithubComment, GithubIssue};

/// Marker type for remote GitHub operations.
pub enum Remote {}

impl Remote {
	/// Load a full issue tree from GitHub.
	///
	/// This is the primary entry point for loading issues from the remote.
	/// It recursively loads identity, contents, and children.
	pub async fn load_issue(source: RemoteSource) -> Result<Issue, RemoteError> {
		let ancestry = source.resolve_ancestry().await?;
		let mut issue = Issue::empty_local(ancestry);

		<Issue as todo::LazyIssue<Remote>>::identity(&mut issue, source.clone()).await?;
		<Issue as todo::LazyIssue<Remote>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut issue, source)).await?;

		Ok(issue)
	}
}

/// Source for loading issues from GitHub.
///
/// Contains the issue link and optional lineage (parent issue numbers from root to immediate parent).
/// If lineage is None, it will be fetched from GitHub by traversing parent issues.
#[derive(Clone)]
pub struct RemoteSource {
	pub link: IssueLink,
	lineage: Option<[u64; MAX_LINEAGE_DEPTH]>,
	lineage_len: u8,
}

impl RemoteSource {
	/// Create source from a link. Lineage will be fetched from GitHub if needed.
	pub fn new(link: IssueLink) -> Self {
		Self {
			link,
			lineage: None,
			lineage_len: 0,
		}
	}

	/// Create source with known lineage (for sub-issues during recursive fetch).
	pub fn with_lineage(link: IssueLink, lineage: &[u64]) -> Self {
		assert!(lineage.len() <= MAX_LINEAGE_DEPTH);
		let mut arr = [0u64; MAX_LINEAGE_DEPTH];
		arr[..lineage.len()].copy_from_slice(lineage);
		Self {
			link,
			lineage: Some(arr),
			lineage_len: lineage.len() as u8,
		}
	}

	fn lineage_slice(&self) -> Option<&[u64]> {
		self.lineage.as_ref().map(|arr| &arr[..self.lineage_len as usize])
	}

	/// Resolve ancestry, fetching lineage from GitHub if not provided.
	async fn resolve_ancestry(&self) -> Result<Ancestry, RemoteError> {
		let owner = self.link.owner().to_string();
		let repo = self.link.repo().to_string();
		let number = self.link.number();

		let lineage = match self.lineage_slice() {
			Some(l) => l.to_vec(),
			None => {
				// Fetch lineage from GitHub by traversing parent chain
				let gh = github::client::get();
				let mut current = number;
				let mut parents = Vec::new();

				loop {
					match gh.fetch_parent_issue(&owner, &repo, current).await {
						Ok(Some(parent)) => {
							parents.push(parent.number);
							current = parent.number;
						}
						Ok(None) => break,
						Err(e) => {
							return Err(RemoteError::ResolveAncestry { owner, repo, number, source: e });
						}
					}
				}
				parents.reverse();
				parents
			}
		};
		Ok(Ancestry::with_lineage(&owner, &repo, &lineage))
	}

	/// Create a child source for a sub-issue.
	fn child(&self, child_link: IssueLink, parent_number: u64) -> Self {
		let parent_lineage = self.lineage_slice().unwrap_or(&[]);
		let mut new_lineage = [0u64; MAX_LINEAGE_DEPTH];
		let new_len = parent_lineage.len() + 1;
		assert!(new_len <= MAX_LINEAGE_DEPTH);
		new_lineage[..parent_lineage.len()].copy_from_slice(parent_lineage);
		new_lineage[parent_lineage.len()] = parent_number;
		Self {
			link: child_link,
			lineage: Some(new_lineage),
			lineage_len: new_len as u8,
		}
	}
}

impl todo::LazyIssue<Remote> for Issue {
	type Error = RemoteError;
	type Source = RemoteSource;

	async fn identity(&mut self, source: Self::Source) -> Result<IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let gh = github::client::get();
		let owner = source.link.owner().to_string();
		let repo = source.link.repo().to_string();
		let number = source.link.number();

		let issue = gh.fetch_issue(&owner, &repo, number).await.map_err(|e| RemoteError::FetchIssue {
			owner: owner.clone(),
			repo: repo.clone(),
			number,
			source: e,
		})?;
		let ancestry = source.resolve_ancestry().await?;
		// For now, we only have the updated_at from GitHub which represents the most recent overall change.
		// Set it as comments timestamp since that's the field we can always get.
		let timestamps = issue.updated_at.parse::<Timestamp>().ok().map(IssueChangeTimestamps::from_comments).unwrap_or_default();

		self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), timestamps);
		Ok(self.identity.clone())
	}

	async fn contents(&mut self, source: Self::Source) -> Result<IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let gh = github::client::get();
		let owner = source.link.owner().to_string();
		let repo = source.link.repo().to_string();
		let number = source.link.number();

		let issue_fut = gh.fetch_issue(&owner, &repo, number);
		let comments_fut = gh.fetch_comments(&owner, &repo, number);

		let (issue_result, comments_result) = tokio::join!(issue_fut, comments_fut);

		let issue = issue_result.map_err(|e| RemoteError::FetchIssue {
			owner: owner.clone(),
			repo: repo.clone(),
			number,
			source: e,
		})?;
		let comments = comments_result.map_err(|e| RemoteError::FetchComments {
			owner: owner.clone(),
			repo: repo.clone(),
			number,
			source: e,
		})?;

		self.contents = build_contents_from_github(&issue, &comments);

		// Also ensure identity is populated if not already
		if !self.identity.is_linked() {
			let ancestry = source.resolve_ancestry().await?;
			let timestamps = issue.updated_at.parse::<Timestamp>().ok().map(IssueChangeTimestamps::from_comments).unwrap_or_default();
			self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), timestamps);
		}

		Ok(self.contents.clone())
	}

	async fn children(&mut self, source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let gh = github::client::get();
		let owner = source.link.owner().to_string();
		let repo = source.link.repo().to_string();
		let number = source.link.number();

		let sub_issues = gh.fetch_sub_issues(&owner, &repo, number).await.map_err(|e| RemoteError::FetchSubIssues {
			owner: owner.clone(),
			repo: repo.clone(),
			number,
			source: e,
		})?;

		let filtered: Vec<&GithubIssue> = sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).collect();

		if filtered.is_empty() {
			return Ok(Vec::new());
		}

		let parent_number = source.link.number();
		let child_ancestry = self.identity.child_ancestry().expect("parent must be linked before fetching children");

		let mut children = Vec::new();
		for sub_issue in filtered {
			let child_url = format!("https://github.com/{owner}/{repo}/issues/{}", sub_issue.number);
			let child_link = IssueLink::parse(&child_url).expect("valid URL");
			let child_source = source.child(child_link, parent_number);
			let mut child = Issue::empty_local(child_ancestry);

			<Issue as todo::LazyIssue<Remote>>::identity(&mut child, child_source.clone()).await?;
			<Issue as todo::LazyIssue<Remote>>::contents(&mut child, child_source.clone()).await?;
			Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut child, child_source)).await?;

			children.push(child);
		}

		children.sort_by_key(|c| c.number().unwrap_or(0));
		self.children = children.clone();
		Ok(children)
	}
}

/// Build IssueContents from GitHub API data.
fn build_contents_from_github(issue: &GithubIssue, comments: &[GithubComment]) -> IssueContents {
	let close_state = CloseState::from_github(&issue.state, issue.state_reason.as_deref());
	let labels: Vec<String> = issue.labels.iter().map(|l| l.name.clone()).collect();

	let raw_body = issue.body.as_deref().unwrap_or("");
	let (body, blockers) = split_blockers(raw_body);

	let mut issue_comments = vec![Comment {
		identity: CommentIdentity::Body,
		body: todo::Events::parse(&body),
	}];

	for c in comments {
		issue_comments.push(Comment {
			identity: CommentIdentity::Created {
				user: c.user.login.clone(),
				id: c.id,
			},
			body: todo::Events::parse(c.body.as_deref().unwrap_or("")),
		});
	}

	IssueContents {
		title: issue.title.clone(),
		labels,
		state: close_state,
		comments: issue_comments,
		blockers,
	}
}
