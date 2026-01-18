//! Remote GitHub operations for issues.
//!
//! This module handles all remote GitHub concerns:
//! - Fetching issues via `LazyIssue<Remote>`
//! - Pushing changes via `Sink<Remote>` (defined in sink.rs, uses helpers from here)
//!
//! The key insight is that `LazyIssue<Remote>` mirrors `LazyIssue<Local>`:
//! - Local loads from PathBuf (filesystem)
//! - Remote loads from (GithubClient, owner, repo, issue_number)

use jiff::Timestamp;
use todo::{Ancestry, CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueLink, split_blockers};
use v_utils::prelude::*;

use crate::github::{BoxedGithubClient, GithubComment, GithubIssue};

//==============================================================================
// Remote - The interface for remote GitHub operations
//==============================================================================

/// Marker type for remote GitHub operations.
///
/// All custom logic for remote issue operations is consolidated as methods on this type.
pub enum Remote {}

impl Remote {
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
}

//==============================================================================
// Source type for LazyIssue<Remote>
//==============================================================================

/// Source for loading issues from GitHub.
/// Contains the client and issue coordinates.
#[derive(Clone)]
pub struct RemoteSource {
	pub gh: BoxedGithubClient,
	pub owner: String,
	pub repo: String,
	pub issue_number: u64,
	/// Parent ancestry (for building child ancestry)
	pub parent_ancestry: Option<Ancestry>,
}

impl RemoteSource {
	/// Create a new remote source for a root issue.
	pub fn root(gh: BoxedGithubClient, owner: impl Into<String>, repo: impl Into<String>, issue_number: u64) -> Self {
		Self {
			gh,
			owner: owner.into(),
			repo: repo.into(),
			issue_number,
			parent_ancestry: None,
		}
	}

	/// Create a child source with the given parent ancestry.
	pub fn child(&self, issue_number: u64, parent_ancestry: Ancestry) -> Self {
		Self {
			gh: self.gh.clone(),
			owner: self.owner.clone(),
			repo: self.repo.clone(),
			issue_number,
			parent_ancestry: Some(parent_ancestry),
		}
	}
}

//==============================================================================
// LazyIssue Implementation
//==============================================================================

impl todo::LazyIssue<Remote> for Issue {
	type Source = RemoteSource;

	async fn identity(&mut self, source: Self::Source) -> IssueIdentity {
		// Return cached if already loaded
		if self.identity.is_linked() {
			return self.identity.clone();
		}

		// Fetch the issue from GitHub
		let issue = source.gh.fetch_issue(&source.owner, &source.repo, source.issue_number).await.expect("failed to fetch issue");

		// Build ancestry from parent or create root
		let ancestry = source.parent_ancestry.unwrap_or_else(|| Ancestry::root(&source.owner, &source.repo));

		// Build link and identity
		let url = format!("https://github.com/{}/{}/issues/{}", source.owner, source.repo, source.issue_number);
		let link = IssueLink::parse(&url).expect("just constructed valid URL");
		let ts = issue.updated_at.parse::<Timestamp>().ok();
		let user = issue.user.login.clone();

		self.identity = IssueIdentity::linked(ancestry, user, link, ts);
		self.identity.clone()
	}

	async fn contents(&mut self, source: Self::Source) -> IssueContents {
		// Return cached if already loaded
		if !self.contents.title.is_empty() {
			return self.contents.clone();
		}

		// Fetch issue and comments in parallel
		let (issue, comments) = tokio::try_join!(
			source.gh.fetch_issue(&source.owner, &source.repo, source.issue_number),
			source.gh.fetch_comments(&source.owner, &source.repo, source.issue_number),
		)
		.expect("failed to fetch issue contents");

		// Build contents from GitHub data
		self.contents = build_contents_from_github(&issue, &comments);

		// Also ensure identity is populated if not already
		if !self.identity.is_linked() {
			let ancestry = source.parent_ancestry.unwrap_or_else(|| Ancestry::root(&source.owner, &source.repo));
			let url = format!("https://github.com/{}/{}/issues/{}", source.owner, source.repo, source.issue_number);
			let link = IssueLink::parse(&url).expect("just constructed valid URL");
			let ts = issue.updated_at.parse::<Timestamp>().ok();
			self.identity = IssueIdentity::linked(ancestry, issue.user.login.clone(), link, ts);
		}

		self.contents.clone()
	}

	async fn children(&mut self, source: Self::Source) -> Vec<Issue> {
		// Return cached if already loaded
		if !self.children.is_empty() {
			return self.children.clone();
		}

		// Fetch sub-issues
		let sub_issues = source
			.gh
			.fetch_sub_issues(&source.owner, &source.repo, source.issue_number)
			.await
			.expect("failed to fetch sub-issues");

		// Filter out duplicates
		let filtered: Vec<&GithubIssue> = sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).collect();

		if filtered.is_empty() {
			return Vec::new();
		}

		// Build child ancestry
		let child_ancestry = self.identity.child_ancestry().expect("parent must be linked before fetching children");

		// Fetch each child recursively
		let mut children = Vec::new();
		for sub_issue in filtered {
			let child_source = source.child(sub_issue.number, child_ancestry);
			let mut child = Issue::empty_local(child_ancestry);

			// Load identity and contents
			<Issue as todo::LazyIssue<Remote>>::identity(&mut child, child_source.clone()).await;
			<Issue as todo::LazyIssue<Remote>>::contents(&mut child, child_source.clone()).await;
			// Recursively load children
			Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut child, child_source)).await;

			children.push(child);
		}

		// Sort by issue number for consistent ordering
		children.sort_by(|a, b| {
			let a_num = a.number().unwrap_or(0);
			let b_num = b.number().unwrap_or(0);
			a_num.cmp(&b_num)
		});

		self.children = children.clone();
		children
	}
}

//==============================================================================
// Helper functions
//==============================================================================

/// Build IssueContents from GitHub API data.
fn build_contents_from_github(issue: &GithubIssue, comments: &[GithubComment]) -> IssueContents {
	let close_state = CloseState::from_github(&issue.state, issue.state_reason.as_deref());
	let labels: Vec<String> = issue.labels.iter().map(|l| l.name.clone()).collect();

	// Split out blockers from body
	let raw_body = issue.body.as_deref().unwrap_or("");
	let (body, blockers) = split_blockers(raw_body);

	// Build comments: body is first comment
	let mut issue_comments = Vec::new();
	issue_comments.push(Comment {
		identity: CommentIdentity::Body,
		body: todo::Events::parse(&body),
	});

	// Actual comments
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

/// Convenience function to fully load an issue tree from GitHub.
///
/// This is the replacement for `fetch_full_issue_tree` from tree.rs.
/// Uses `LazyIssue<Remote>` internally.
pub async fn load_full_issue_tree(gh: &BoxedGithubClient, owner: &str, repo: &str, issue_number: u64) -> Result<Issue> {
	let source = RemoteSource::root(gh.clone(), owner, repo, issue_number);
	let ancestry = Ancestry::root(owner, repo);
	let mut issue = Issue::empty_local(ancestry);

	<Issue as todo::LazyIssue<Remote>>::identity(&mut issue, source.clone()).await;
	<Issue as todo::LazyIssue<Remote>>::contents(&mut issue, source.clone()).await;
	Box::pin(<Issue as todo::LazyIssue<Remote>>::children(&mut issue, source)).await;

	Ok(issue)
}
