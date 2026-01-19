//! Remote GitHub operations for issues.
//!
//! This module handles all remote GitHub concerns:
//! - Fetching issues via `LazyIssue<Remote>`
//! - Pushing changes via `Sink<Remote>` (defined in sink.rs, uses helpers from here)
//!
//! The key insight is that `LazyIssue<Remote>` mirrors `LazyIssue<Local>`:
//! - Local loads from PathBuf (filesystem)
//! - Remote loads from IssueLink + optional lineage

//==============================================================================
// Error Types
//==============================================================================

/// Error type for remote GitHub operations.
#[derive(Debug, thiserror::Error)]
pub enum RemoteError {
	/// Failed to fetch issue from GitHub.
	#[error("failed to fetch issue #{number} from {}/{}", repo.owner(), repo.repo())]
	FetchIssue {
		repo: RepoInfo,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to fetch issue comments from GitHub.
	#[error("failed to fetch comments for issue #{number} from {}/{}", repo.owner(), repo.repo())]
	FetchComments {
		repo: RepoInfo,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to fetch sub-issues from GitHub.
	#[error("failed to fetch sub-issues for issue #{number} from {}/{}", repo.owner(), repo.repo())]
	FetchSubIssues {
		repo: RepoInfo,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to resolve ancestry (parent issue chain).
	#[error("failed to resolve ancestry for issue #{number} in {}/{}", repo.owner(), repo.repo())]
	ResolveAncestry {
		repo: RepoInfo,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Failed to fetch timestamps from GitHub GraphQL API.
	#[error("failed to fetch timestamps for issue #{number} from {}/{}", repo.owner(), repo.repo())]
	FetchTimestamps {
		repo: RepoInfo,
		number: u64,
		#[source]
		source: color_eyre::Report,
	},

	/// Issue not found on GitHub (404).
	#[error("issue #{number} not found in {}/{}", repo.owner(), repo.repo())]
	NotFound { repo: RepoInfo, number: u64 },
}
use tedi::{Ancestry, CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueLink, IssueTimestamps, MAX_LINEAGE_DEPTH, RepoInfo, split_blockers};
use v_utils::prelude::*;

use crate::github::{self, GithubComment, GithubIssue};

/// Marker type for remote GitHub operations.
pub enum Remote {}

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
	pub async fn resolve_ancestry(&self) -> Result<Ancestry, RemoteError> {
		let repo_info = self.link.repo_info();
		let number = self.link.number();

		let lineage = match self.lineage_slice() {
			Some(l) => l.to_vec(),
			None => {
				// Fetch lineage from GitHub by traversing parent chain
				let gh = github::client::get();
				let mut current = number;
				let mut parents = Vec::new();

				loop {
					match gh.fetch_parent_issue(repo_info, current).await {
						Ok(Some(parent)) => {
							parents.push(parent.number);
							current = parent.number;
						}
						Ok(None) => break,
						Err(e) => {
							return Err(RemoteError::ResolveAncestry { repo: repo_info, number, source: e });
						}
					}
				}
				parents.reverse();
				parents
			}
		};
		Ok(Ancestry::with_lineage(repo_info.owner(), repo_info.repo(), &lineage))
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

impl tedi::LazyIssue<Remote> for Issue {
	type Error = RemoteError;
	type Source = RemoteSource;

	async fn ancestry(source: &Self::Source) -> Result<Ancestry, Self::Error> {
		source.resolve_ancestry().await
	}

	async fn identity(&mut self, source: Self::Source) -> Result<IssueIdentity, Self::Error> {
		if self.identity.is_linked() {
			return Ok(self.identity.clone());
		}

		let gh = github::client::get();
		let repo_info = source.link.repo_info();
		let number = source.link.number();

		// Fetch issue and timeline timestamps in parallel
		// Note: We don't have comment timestamps here since we don't fetch comments in identity()
		let issue_fut = gh.fetch_issue(repo_info, number);
		let timeline_fut = gh.fetch_timeline_timestamps(repo_info, number);

		let (issue_result, timeline_result) = tokio::join!(issue_fut, timeline_fut);

		let issue = issue_result.map_err(|e| RemoteError::FetchIssue { repo: repo_info, number, source: e })?;

		// Build IssueTimestamps from GraphQL timeline (comments will be empty here)
		let timeline = timeline_result.map_err(|e| RemoteError::FetchTimestamps { repo: repo_info, number, source: e })?;
		let timestamps = IssueTimestamps {
			title: timeline.title,
			description: timeline.description,
			labels: timeline.labels,
			state: timeline.state,
			comments: vec![], // Will be populated when contents() fetches comments
		};

		let ancestry = source.resolve_ancestry().await?;
		self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), timestamps);
		Ok(self.identity.clone())
	}

	async fn contents(&mut self, source: Self::Source) -> Result<IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let gh = github::client::get();
		let repo_info = source.link.repo_info();
		let number = source.link.number();

		let issue_fut = gh.fetch_issue(repo_info, number);
		let comments_fut = gh.fetch_comments(repo_info, number);
		let timeline_fut = gh.fetch_timeline_timestamps(repo_info, number);

		let (issue_result, comments_result, timeline_result) = tokio::join!(issue_fut, comments_fut, timeline_fut);

		let issue = issue_result.map_err(|e| RemoteError::FetchIssue { repo: repo_info, number, source: e })?;
		let comments = comments_result.map_err(|e| RemoteError::FetchComments { repo: repo_info, number, source: e })?;

		self.contents = build_contents_from_github(&issue, &comments);

		// Also ensure identity is populated if not already
		if !self.identity.is_linked() {
			let ancestry = source.resolve_ancestry().await?;
			let timeline = timeline_result.map_err(|e| RemoteError::FetchTimestamps { repo: repo_info, number, source: e })?;

			// Build per-comment timestamps from REST API data (updated_at, falling back to created_at)
			let comments_ts: Vec<_> = comments
				.iter()
				.filter_map(|c| jiff::Timestamp::from_str(&c.updated_at).or_else(|_| jiff::Timestamp::from_str(&c.created_at)).ok())
				.collect();

			let timestamps = IssueTimestamps {
				title: timeline.title,
				description: timeline.description,
				labels: timeline.labels,
				state: timeline.state,
				comments: comments_ts,
			};
			self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), timestamps);
		}

		Ok(self.contents.clone())
	}

	async fn children(&mut self, source: Self::Source) -> Result<Vec<Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let gh = github::client::get();
		let repo_info = source.link.repo_info();
		let number = source.link.number();

		let sub_issues = gh
			.fetch_sub_issues(repo_info, number)
			.await
			.map_err(|e| RemoteError::FetchSubIssues { repo: repo_info, number, source: e })?;

		let filtered: Vec<&GithubIssue> = sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).collect();

		if filtered.is_empty() {
			return Ok(Vec::new());
		}

		let parent_number = source.link.number();
		let child_ancestry = self.identity.child_ancestry().expect("parent must be linked before fetching children");

		let mut children = Vec::new();
		for sub_issue in filtered {
			let child_url = format!("https://github.com/{}/{}/issues/{}", repo_info.owner(), repo_info.repo(), sub_issue.number);
			let child_link = IssueLink::parse(&child_url).expect("valid URL");
			let child_source = source.child(child_link, parent_number);
			let mut child = Issue::empty_local(child_ancestry);

			<Issue as tedi::LazyIssue<Remote>>::identity(&mut child, child_source.clone()).await?;
			<Issue as tedi::LazyIssue<Remote>>::contents(&mut child, child_source.clone()).await?;
			Box::pin(<Issue as tedi::LazyIssue<Remote>>::children(&mut child, child_source)).await?;

			children.push(child);
		}

		children.sort_by_key(|c| c.number().unwrap_or(0));
		self.children = children.clone();
		Ok(children)
	}

	async fn load(source: Self::Source) -> Result<Issue, Self::Error> {
		let ancestry = <Self as tedi::LazyIssue<Remote>>::ancestry(&source).await?;
		let mut issue = Issue::empty_local(ancestry);
		<Self as tedi::LazyIssue<Remote>>::identity(&mut issue, source.clone()).await?;
		<Self as tedi::LazyIssue<Remote>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Self as tedi::LazyIssue<Remote>>::children(&mut issue, source)).await?;
		Ok(issue)
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
		body: tedi::Events::parse(&body),
	}];

	for c in comments {
		issue_comments.push(Comment {
			identity: CommentIdentity::Created {
				user: c.user.login.clone(),
				id: c.id,
			},
			body: tedi::Events::parse(c.body.as_deref().unwrap_or("")),
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
