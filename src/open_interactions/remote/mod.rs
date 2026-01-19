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
use todo::{Ancestry, CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueLink, MAX_LINEAGE_DEPTH, split_blockers};
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
	async fn resolve_ancestry(&self) -> Result<Ancestry> {
		let lineage = match self.lineage_slice() {
			Some(l) => l.to_vec(),
			None => {
				// Fetch lineage from GitHub by traversing parent chain
				let gh = github::client::get();
				let owner = self.link.owner();
				let repo = self.link.repo();
				let mut current = self.link.number();
				let mut parents = Vec::new();

				while let Some(parent) = gh.fetch_parent_issue(owner, repo, current).await? {
					parents.push(parent.number);
					current = parent.number;
				}
				parents.reverse();
				parents
			}
		};
		Ok(Ancestry::with_lineage(self.link.owner(), self.link.repo(), &lineage))
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
	type Source = RemoteSource;

	async fn identity(&mut self, source: Self::Source) -> IssueIdentity {
		if self.identity.is_linked() {
			return self.identity.clone();
		}

		let gh = github::client::get();
		let owner = source.link.owner();
		let repo = source.link.repo();
		let number = source.link.number();

		let issue = gh.fetch_issue(owner, repo, number).await.expect("failed to fetch issue");
		let ancestry = source.resolve_ancestry().await.expect("failed to resolve ancestry");
		let ts = issue.updated_at.parse::<Timestamp>().ok();

		self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), ts);
		self.identity.clone()
	}

	async fn contents(&mut self, source: Self::Source) -> IssueContents {
		if !self.contents.title.is_empty() {
			return self.contents.clone();
		}

		let gh = github::client::get();
		let owner = source.link.owner();
		let repo = source.link.repo();
		let number = source.link.number();

		let (issue, comments) = tokio::try_join!(gh.fetch_issue(owner, repo, number), gh.fetch_comments(owner, repo, number),).expect("failed to fetch issue contents");

		self.contents = build_contents_from_github(&issue, &comments);

		// Also ensure identity is populated if not already
		if !self.identity.is_linked() {
			let ancestry = source.resolve_ancestry().await.expect("failed to resolve ancestry");
			let ts = issue.updated_at.parse::<Timestamp>().ok();
			self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), source.link.clone(), ts);
		}

		self.contents.clone()
	}

	async fn children(&mut self, source: Self::Source) -> Vec<Issue> {
		if !self.children.is_empty() {
			return self.children.clone();
		}

		let gh = github::client::get();
		let owner = source.link.owner();
		let repo = source.link.repo();
		let number = source.link.number();

		let sub_issues = gh.fetch_sub_issues(owner, repo, number).await.expect("failed to fetch sub-issues");

		let filtered: Vec<&GithubIssue> = sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).collect();

		if filtered.is_empty() {
			return Vec::new();
		}

		let parent_number = source.link.number();
		let child_ancestry = self.identity.child_ancestry().expect("parent must be linked before fetching children");

		let mut children = Vec::new();
		for sub_issue in filtered {
			let child_url = format!("https://github.com/{}/{}/issues/{}", owner, repo, sub_issue.number);
			let child_link = IssueLink::parse(&child_url).expect("valid URL");
			let child_source = source.child(child_link, parent_number);
			let mut child = Issue::empty_local(child_ancestry);

			<Issue as todo::LazyIssue<Remote>>::identity(&mut child, child_source.clone()).await;
			<Issue as todo::LazyIssue<Remote>>::contents(&mut child, child_source.clone()).await;
			Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut child, child_source)).await;

			children.push(child);
		}

		children.sort_by_key(|c| c.number().unwrap_or(0));
		self.children = children.clone();
		children
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

/// Load a full issue tree from GitHub.
pub async fn load_full_issue_tree(source: RemoteSource) -> Result<Issue> {
	let ancestry = source.resolve_ancestry().await?;
	let mut issue = Issue::empty_local(ancestry);

	<Issue as todo::LazyIssue<Remote>>::identity(&mut issue, source.clone()).await;
	<Issue as todo::LazyIssue<Remote>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut issue, source)).await;

	Ok(issue)
}
