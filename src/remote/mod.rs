//! Remote GitHub operations for issues.
//!
//! This module handles all remote GitHub concerns:
//! - Fetching issues via `LazyIssue<RemoteSource>`
//! - Pushing changes via `Sink<Remote>`
//!
//! The key insight is that loading mirrors local operations:
//! - Local: `Issue::load(LocalIssueSource<FsReader>)` - loads from filesystem
//! - Remote: `Issue::load(RemoteSource)` - loads from GitHub via IssueLink

use HashMap;

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

	/// Required executable not found.
	#[error("`{executable}` not found in PATH (required for {operation})")]
	MissingExecutable { executable: &'static str, operation: &'static str },
}
use copy_arrayvec::CopyArrayVec;
use v_utils::prelude::*;

use crate::{
	CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector, IssueTimestamps, MAX_LINEAGE_DEPTH, RepoInfo,
	github::{self, GithubComment, GithubIssue},
	sink::{Sink, compute_node_diff},
	split_blockers,
};

/// Marker type for remote GitHub sink operations.
pub enum Remote {}

/// Source for loading issues from GitHub.
///
/// Contains the issue link and optional lineage (parent issue numbers from root to immediate parent).
/// If lineage is None, it will be fetched from GitHub by traversing parent issues.
/// Use `build()` to construct with validation of required tools.
#[derive(Clone, Debug)]
pub struct RemoteSource {
	pub link: IssueLink,
	lineage: Option<CopyArrayVec<u64, MAX_LINEAGE_DEPTH>>,
}

impl RemoteSource {
	/// Build source for loading an issue from GitHub.
	///
	/// `lineage`: parent issue numbers from root to immediate parent.
	/// `None` means lineage is unknown and will be fetched from GitHub on demand;
	/// `Some(&[])` means the issue is known to be at root level.
	///
	/// Checks that `gh` executable is available.
	pub fn build(link: IssueLink, lineage: Option<&[u64]>) -> Result<Self, RemoteError> {
		if std::process::Command::new("gh").arg("--version").output().is_err() {
			return Err(RemoteError::MissingExecutable {
				executable: "gh",
				operation: "GitHub operations",
			});
		}
		Ok(Self {
			link,
			lineage: lineage.map(|l| l.iter().copied().collect()),
		})
	}

	fn lineage_slice(&self) -> Option<&[u64]> {
		self.lineage.as_ref().map(|v| v.as_slice())
	}

	/// Resolve parent_index, fetching lineage from GitHub if not provided.
	pub async fn resolve_parent_index(&self) -> Result<Option<IssueIndex>, RemoteError> {
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
		// Build parent_index with all parent numbers as GitId selectors
		// Return None for root-level issues (empty lineage)
		if lineage.is_empty() {
			Ok(Some(IssueIndex::repo_only(repo_info)))
		} else {
			let selectors: Vec<IssueSelector> = lineage.iter().map(|&n| IssueSelector::GitId(n)).collect();
			Ok(Some(IssueIndex::with_index(repo_info, selectors)))
		}
	}

	/// Create a child source for a sub-issue.
	fn child(&self, child_link: IssueLink, parent_number: u64) -> Self {
		let mut new_lineage: CopyArrayVec<u64, MAX_LINEAGE_DEPTH> = self.lineage_slice().unwrap_or(&[]).iter().copied().collect();
		new_lineage.push(parent_number);
		Self {
			link: child_link,
			lineage: Some(new_lineage),
		}
	}
}

impl crate::LazyIssue<RemoteSource> for Issue {
	type Error = RemoteError;

	async fn parent_index(source: &RemoteSource) -> Result<Option<IssueIndex>, Self::Error> {
		source.resolve_parent_index().await
	}

	#[instrument(skip_all)]
	async fn identity(&mut self, source: RemoteSource) -> Result<IssueIdentity, Self::Error> {
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

		let parent_index = source.resolve_parent_index().await?;
		self.identity = IssueIdentity::new_linked(parent_index, issue.user.login.clone(), source.link.clone(), timestamps);
		Ok(self.identity.clone())
	}

	#[instrument(skip_all)]
	async fn contents(&mut self, source: RemoteSource) -> Result<IssueContents, Self::Error> {
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
			let parent_index = source.resolve_parent_index().await?;
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
			self.identity = IssueIdentity::new_linked(parent_index, issue.user.login.clone(), source.link.clone(), timestamps);
		}

		Ok(self.contents.clone())
	}

	#[instrument(skip_all)]
	async fn children(&mut self, source: RemoteSource) -> Result<HashMap<IssueSelector, Issue>, Self::Error> {
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
			return Ok(HashMap::new());
		}

		let parent_number = source.link.number();
		let child_parent_index = self.identity.child_parent_index().expect("parent must be linked before fetching children");

		let mut children = HashMap::new();
		for sub_issue in filtered {
			let child_url = format!("https://github.com/{}/{}/issues/{}", repo_info.owner(), repo_info.repo(), sub_issue.number);
			let child_link = IssueLink::parse(&child_url).expect("valid URL");
			let child_source = source.child(child_link, parent_number);
			let mut child = Issue::empty_local(child_parent_index);

			Self::identity(&mut child, child_source.clone()).await?;
			Self::contents(&mut child, child_source.clone()).await?;
			Box::pin(Self::children(&mut child, child_source)).await?;

			children.insert(child.selector(), child);
		}

		self.children = children.clone();
		Ok(children)
	}

	// Uses default load() impl from LazyIssue trait
}

/// Build IssueContents from GitHub API data.
#[instrument(skip_all, fields(issue_number = issue.number, title = %issue.title))]
fn build_contents_from_github(issue: &GithubIssue, comments: &[GithubComment]) -> IssueContents {
	let close_state = CloseState::from_github(&issue.state, issue.state_reason.as_deref());
	let labels: Vec<String> = issue.labels.iter().map(|l| l.name.clone()).collect();

	let raw_body = issue.body.as_deref().unwrap_or(""); //IGNORED_ERROR: GitHub API null body is valid (empty issue)
	let (body, blockers) = split_blockers(raw_body);

	let mut issue_comments = vec![Comment {
		identity: CommentIdentity::Body,
		body: crate::Events::parse(&body),
	}];

	for c in comments {
		issue_comments.push(Comment {
			identity: CommentIdentity::Created {
				user: c.user.login.clone(),
				id: c.id,
			},
			body: crate::Events::parse(c.body.as_deref().unwrap_or("")), //IGNORED_ERROR: GitHub API null comment body is valid
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

//==============================================================================
// Sink<Remote> Implementation
//==============================================================================

//TODO: @claude: create proper error type for Remote sink (see ConsensusSinkError for reference)
// in the perfect world would have a common SinkError<T> type with encapsulation of shared failure modes. Generic <T> there is for tool-specific errors, - so for local and consensus it should embed `LocalError`, for remote `GithubError`
impl Sink<Remote> for Issue {
	type Error = color_eyre::Report;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		// Virtual issues never sync to remote - they're local-only
		if self.identity.is_virtual {
			return Ok(false);
		}

		let gh = crate::github::client::get();
		let repo_info = self.identity.parent_index.repo_info();

		let mut changed = false;

		// If this is a pending (local) issue, create it first
		if self.is_local() {
			let title = &self.contents.title;
			let body = self.body();
			let closed = self.contents.state.is_closed();
			let parent_index = self.identity.parent_index;

			println!("Creating issue: {title}");
			let created = gh.create_issue(repo_info, title, &body).await?;
			println!("Created issue #{}: {}", created.number, created.html_url);

			// Close if needed
			if closed {
				gh.update_issue_state(repo_info, created.number, "closed").await?;
			}

			// Link to parent if this issue has one
			let lineage = self.identity.git_lineage()?;
			if let Some(&parent_number) = lineage.last() {
				gh.add_sub_issue(repo_info, parent_number, created.id).await?;
			}

			// Update identity - keep same parent_index, just add linking info
			let url = format!("https://github.com/{}/{}/issues/{}", repo_info.owner(), repo_info.repo(), created.number);
			let link = IssueLink::parse(&url).expect("just constructed valid URL");
			let user = gh.fetch_authenticated_user().await?;
			self.identity = IssueIdentity::new_linked(Some(parent_index), user, link, crate::IssueTimestamps::now());
			changed = true;
		}

		let issue_number = self.git_id().expect("issue must have number after creation");

		// Sync content against old (if we have old state)
		let diff = compute_node_diff(self, old);

		// Compare full GitHub body (text + blockers), not just comments[0]
		let body_changed = match old {
			Some(old) => self.body() != old.body(),
			None => false,
		};
		if body_changed {
			let body = self.body();
			println!("Updating issue #{issue_number} body...");
			gh.update_issue_body(repo_info, issue_number, &body).await?;
			changed = true;
		}

		// Compare at GitHub granularity (open vs closed), not local granularity (Closed vs NotPlanned)
		let remote_state_changed = match old {
			Some(old) => self.contents.state.to_github_state() != old.contents.state.to_github_state(),
			None => false,
		};
		if remote_state_changed {
			let state = self.contents.state.to_github_state();
			println!("Updating issue #{issue_number} state to {state}...");
			gh.update_issue_state(repo_info, issue_number, state).await?;
			changed = true;
		}

		// Create pending comments sequentially (order matters)
		for comment in self.contents.comments.iter_mut().skip(1) {
			if comment.identity.is_pending() && !comment.body.is_empty() {
				let body_str = comment.body.render();
				println!("Creating new comment on issue #{issue_number}...");
				gh.create_comment(repo_info, issue_number, &body_str).await?;
				changed = true;
			}
		}

		// Update existing comments
		for (comment_id, comment) in &diff.comments_to_update {
			if let CommentIdentity::Created { user, .. } = &comment.identity
				&& !crate::current_user::is(user)
			{
				continue;
			}
			let body_str = comment.body.render();
			println!("Updating comment {comment_id}...");
			gh.update_comment(repo_info, *comment_id, &body_str).await?;
			changed = true;
		}

		// Delete removed comments
		for comment_id in &diff.comments_to_delete {
			println!("Deleting comment {comment_id} from issue #{issue_number}...");
			gh.delete_comment(repo_info, *comment_id).await?;
			changed = true;
		}

		// Update children's parent_index to use our git number, then sink
		if let Some(child_parent_index) = self.identity.child_parent_index() {
			for child in self.children.values_mut() {
				child.identity.parent_index = child_parent_index;
			}
		}
		for (selector, child) in self.children.iter_mut() {
			let old_child = old.and_then(|o| o.children.get(selector));
			changed |= Box::pin(<Issue as Sink<Remote>>::sink(child, old_child)).await?;
		}

		Ok(changed)
	}
}
