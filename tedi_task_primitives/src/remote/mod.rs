//! The GitHub boundary: `LazyIssue<RemoteSource>` in, `Sink<Remote>` out.
//!
//! Loading takes the same shape as it does locally — `Issue::load(RemoteSource)` against
//! `Issue::load(LocalIssueSource<FsReader>)`.
//!
//! Both directions, field by field, are drawn in `remote.typ` at the crate root.

//==============================================================================
// Error Types
//==============================================================================
use std::{collections::HashMap, str::FromStr as _};

use color_eyre::eyre::Result;
use copy_arrayvec::CopyArrayVec;
use tracing::{instrument, warn};
use v_utils::macros::wrap_err;

use crate::{
	CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector, IssueTimestamps, MAX_LINEAGE_DEPTH, RepoInfo,
	github::{self, GithubComment, GithubIssue},
	sink::{Sink, compute_node_diff},
	split_blockers,
};

mod milestone;
pub use milestone::load_remote_milestone;

/// Error type for remote GitHub operations.
#[wrap_err]
#[derive(Debug, thiserror::Error)]
pub enum RemoteError {
	/// Failed to fetch issue from GitHub.
	#[error("failed to fetch issue #{number} from {repo}")]
	FetchIssue {
		repo: RepoInfo,
		number: u64,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},

	/// Failed to fetch issue comments from GitHub.
	#[error("failed to fetch comments for issue #{number} from {repo}")]
	FetchComments {
		repo: RepoInfo,
		number: u64,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},

	/// Failed to fetch sub-issues from GitHub.
	#[error("failed to fetch sub-issues for issue #{number} from {repo}")]
	FetchSubIssues {
		repo: RepoInfo,
		number: u64,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},

	/// Failed to resolve ancestry (parent issue chain).
	#[error("failed to resolve ancestry for issue #{number} in {repo}")]
	ResolveAncestry {
		repo: RepoInfo,
		number: u64,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},

	/// Failed to fetch timestamps from GitHub GraphQL API.
	#[error("failed to fetch timestamps for issue #{number} from {repo}")]
	FetchTimestamps {
		repo: RepoInfo,
		number: u64,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},

	/// A 404 on fetch: the repo or the issue is gone (deleted/renamed/transferred/private), or the
	/// token lost access. Pre-rendered because the printer at the top is eyre, not miette.
	#[leaf]
	#[error("{rendered}")]
	Gone { rendered: String },

	/// Required executable not found.
	#[leaf]
	#[error("`{executable}` not found in PATH (required for {operation})")]
	MissingExecutable { executable: &'static str, operation: &'static str },

	/// GitHub client not available.
	#[own]
	NoClient(crate::github::GithubError),
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{repo}#{number} is gone from GitHub")]
#[diagnostic(help(
	"the repo or the issue was deleted, renamed, transferred or made private — or your token lost access to it.\n\
		 Your local copy is untouched. Repoint the link (a local-only task is `<!-- virtual <path> -->`, not a github URL) \
		 or drop it from whatever references it, then re-run."
))]
struct Gone {
	repo: RepoInfo,
	number: u64,
}

impl RemoteError {
	/// A failed issue fetch, with 404 split off — it is not a transport problem to retry but a
	/// dangling link, and only the caller here knows which issue was behind it.
	fn fetch_issue(repo: RepoInfo, number: u64, source: crate::github::GithubError) -> Self {
		match &source {
			crate::github::GithubError::Api { status, .. } if status.as_u16() == 404 => Self::new_gone(format!("{:?}", miette::Report::new(Gone { repo, number }))),
			_ => Self::FetchIssue { repo, number, source },
		}
	}
}

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
	pub fn build(link: IssueLink, lineage: Option<&[u64]>) -> Result<Self, Box<RemoteError>> {
		if std::process::Command::new("gh").arg("--version").output().is_err() {
			return Err(Box::new(RemoteError::new_missing_executable("gh", "GitHub operations")));
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
		let repo_info = self.link.project();
		let number = self.link.number();

		let lineage = match self.lineage_slice() {
			Some(l) => l.to_vec(),
			None => {
				// Fetch lineage from GitHub by traversing parent chain
				let gh = github::client::get().map_err(RemoteError::NoClient)?;
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

		let gh = github::client::get().map_err(RemoteError::NoClient)?;
		let repo_info = source.link.project();
		let number = source.link.number();

		// Fetch issue and timeline timestamps in parallel
		// Note: We don't have comment timestamps here since we don't fetch comments in identity()
		let issue_fut = gh.fetch_issue(repo_info, number);
		let timeline_fut = gh.fetch_timeline_timestamps(repo_info, number);

		let (issue_result, timeline_result) = tokio::join!(issue_fut, timeline_fut);

		let issue = issue_result.map_err(|e| RemoteError::fetch_issue(repo_info, number, e))?;

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
		self.identity = IssueIdentity::new_linked(parent_index, Some(issue.user.login.clone()), source.link.clone(), timestamps);
		Ok(self.identity.clone())
	}

	#[instrument(skip_all)]
	async fn contents(&mut self, source: RemoteSource) -> Result<IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}

		let gh = github::client::get().map_err(RemoteError::NoClient)?;
		let repo_info = source.link.project();
		let number = source.link.number();

		let issue_fut = gh.fetch_issue(repo_info, number);
		let comments_fut = gh.fetch_comments(repo_info, number);
		let timeline_fut = gh.fetch_timeline_timestamps(repo_info, number);

		let (issue_result, comments_result, timeline_result) = tokio::join!(issue_fut, comments_fut, timeline_fut);

		let issue = issue_result.map_err(|e| RemoteError::fetch_issue(repo_info, number, e))?;
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
			self.identity = IssueIdentity::new_linked(parent_index, Some(issue.user.login.clone()), source.link.clone(), timestamps);
		}

		Ok(self.contents.clone())
	}

	#[instrument(skip_all)]
	async fn children(&mut self, source: RemoteSource) -> Result<HashMap<IssueSelector, Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let gh = github::client::get().map_err(RemoteError::NoClient)?;
		let repo_info = source.link.project();
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
			let child_link = IssueLink::in_project(repo_info, sub_issue.number);
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

/// Error type for remote sink operations.
#[wrap_err]
#[derive(Debug, thiserror::Error)]
pub enum RemoteSinkError {
	/// GitHub API operation failed.
	#[own]
	Github(crate::github::GithubError),

	/// Parent issue has unresolved title-based selector (pending issue in lineage).
	#[own]
	TitleInGitPath(crate::TitleInGitPathError),
}
/// Build IssueContents from GitHub API data.
#[instrument(skip_all, fields(issue_number = issue.number, title = %issue.title))]
fn build_contents_from_github(issue: &GithubIssue, comments: &[GithubComment]) -> IssueContents {
	let all_labels: Vec<String> = issue.labels.iter().map(|l| l.name.clone()).collect();
	let close_state = CloseState::from_github(&issue.state, issue.state_reason.as_deref(), &all_labels);
	// the `p:` label *is* the state here; leaving it in would also render it in the title line's `(labels)` slot
	let labels: Vec<String> = all_labels.into_iter().filter(|l| l.parse::<crate::Progress>().is_err()).collect();

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
		comments: issue_comments.into(),
		blockers,
	}
}

/// Labels as Github holds them: the issue's own, plus the managed `p:` label carrying a
/// progress state Github has no slot for. The only outbound label view — `contents.labels`
/// alone would let a `[ ] → [.]` flip push nothing.
pub(crate) fn remote_labels(contents: &IssueContents) -> Vec<String> {
	let mut labels = contents.labels.clone();
	labels.extend(contents.state.progress_label());
	labels
}

//==============================================================================
// Sink<Remote> Implementation
//==============================================================================

impl Sink<Remote> for Issue {
	type Error = RemoteSinkError;

	async fn sink(&mut self, old: Option<&Issue>) -> Result<bool, Self::Error> {
		// Virtual issues never sync to remote - they're local-only
		if self.identity.is_virtual {
			return Ok(false);
		}

		let gh = crate::github::client::get()?;
		let repo_info = self.identity.parent_index.repo_info();

		let mut changed = false;

		// If this is a pending (local) issue, create it first
		if self.is_local() {
			let title = &self.contents.title;
			let body: String = self.body().into();
			let closed = self.contents.state.is_closed();
			let parent_index = self.identity.parent_index;

			println!("Creating issue: {title}");
			let created = gh.create_issue(repo_info, title, &body).await?;
			println!("Created issue #{}: {}", created.number, created.html_url);

			// Set labels if any
			let labels = remote_labels(&self.contents);
			if !labels.is_empty() {
				gh.set_labels(repo_info, created.number, &labels).await?;
			}

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
			let link = IssueLink::in_project(repo_info, created.number);
			let user = gh.fetch_authenticated_user().await?;
			self.identity = IssueIdentity::new_linked(Some(parent_index), Some(user), link, crate::IssueTimestamps::now());
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
			let body: String = self.body().into();
			println!("Updating issue #{issue_number} body...");
			gh.update_issue_body(repo_info, issue_number, &body).await?;
			changed = true;
		}

		if diff.labels_changed {
			println!("Updating issue #{issue_number} labels...");
			gh.set_labels(repo_info, issue_number, &remote_labels(&self.contents)).await?;
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
			if comment.is_pending() && !comment.body.is_empty() {
				let body_str = comment.body.to_string();
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
			let body_str = comment.body.to_string();
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

#[cfg(test)]
mod tests {
	use std::sync::Arc;

	use super::*;
	use crate::{LazyIssue, mock_github::MockGithubClient};

	fn seeded(labels: Vec<&str>) -> (Arc<MockGithubClient>, RepoInfo) {
		let client = Arc::new(MockGithubClient::new("testuser"));
		let repo = RepoInfo::new("o", "r");
		client.add_issue(repo, 1, "Half done", "body", "open", labels, "testuser", Some(jiff::Timestamp::from_second(1704067200).unwrap()));
		github::client::set(client.clone());
		(client, repo)
	}

	fn source(repo: RepoInfo) -> RemoteSource {
		RemoteSource {
			link: IssueLink::in_project(repo, 1),
			lineage: Some(CopyArrayVec::new()),
		}
	}

	/// A `p:` label is the state, not a label: it parses back into `InProgress` and never
	/// reaches the `(labels)` slot of the title line.
	#[tokio::test]
	async fn progress_label_parses_back_into_the_state() {
		let (_client, repo) = seeded(vec!["bug", "p:partial"]);
		let issue = Issue::load(source(repo)).await.unwrap();
		assert_eq!(issue.contents.state, CloseState::InProgress(crate::Progress::Partial));
		assert_eq!(issue.contents.labels, ["bug"]);
	}

	/// A pure `[ ] → [.]` flip moves neither `contents.labels` nor the GitHub open/closed state,
	/// so it must still push — as the managed label.
	#[tokio::test]
	async fn progress_flip_pushes_the_managed_label() {
		let (client, repo) = seeded(vec!["bug"]);
		let old = Issue::load(source(repo)).await.unwrap();
		let mut new = old.clone();
		new.contents.state = CloseState::InProgress(crate::Progress::Partial);

		client.clear_call_log();
		assert!(<Issue as Sink<Remote>>::sink(&mut new, Some(&old)).await.unwrap());
		insta::assert_snapshot!(client.get_call_log().join("\n"), @r#"set_labels(o, r, 1, ["bug", "p:partial"])"#);
	}
}
