//! The GitHub boundary: `LazyIssue<RemoteSource>` in, `Sink<Remote>` out.
//!
//! Loading takes the same shape as it does locally — `Issue::load(RemoteSource)` against
//! `Issue::load(LocalIssueSource<FsReader>)`.
//!
//! Both directions, field by field, are drawn in `remote.typ` at the crate root.

//==============================================================================
// Error Types
//==============================================================================
use std::{
	collections::HashMap,
	str::FromStr as _,
	sync::{Arc, Mutex},
};

use color_eyre::eyre::Result;
use copy_arrayvec::CopyArrayVec;
use tracing::{instrument, warn};
use v_utils::macros::wrap_err;

use crate::{
	CloseState, Comment, CommentIdentity, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector, IssueTimestamps, MAX_LINEAGE_DEPTH, RepoInfo,
	github::{self, FetchedIssue, GithubComment, GithubIssue},
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

	/// A 404 on fetch: the repo or the issue is gone (deleted/renamed/transferred/private), or the
	/// token lost access. Pre-rendered because the printer at the top is eyre, not miette.
	#[leaf]
	#[error("{rendered}")]
	Gone { rendered: String },

	/// The title line can't hold it. Pre-rendered for the same reason as `Gone`.
	#[leaf]
	#[error("{rendered}")]
	MultilineTitle { rendered: String },

	/// Closed as a duplicate, which tedi doesn't track. Pre-rendered for the same reason as `Gone`.
	#[leaf]
	#[error("{rendered}")]
	Duplicate { rendered: String },

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

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{repo}#{number} is closed as a duplicate on GitHub")]
#[diagnostic(help("duplicates aren't tracked. Point whatever links it at the original issue, or drop the link, then re-run."))]
struct Duplicate {
	repo: RepoInfo,
	number: u64,
}

#[derive(Debug, miette::Diagnostic, thiserror::Error)]
#[error("{repo}#{number} has a multi-line title on GitHub: {title:?}")]
#[diagnostic(help(
	"usually left by GitHub's \"convert to issue\" on a task-list item that had nested content. \
		 Edit the title on GitHub down to one line, then re-run."
))]
struct MultilineTitle {
	repo: RepoInfo,
	number: u64,
	title: String,
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
#[derive(Clone, Debug)]
pub struct RemoteSource {
	pub link: IssueLink,
	lineage: Option<CopyArrayVec<u64, MAX_LINEAGE_DEPTH>>,
	/// By number in `link`'s repo: a load reads each issue once, whichever of its parts asks first.
	read: Arc<Mutex<HashMap<u64, Arc<FetchedIssue>>>>,
}

impl RemoteSource {
	/// `lineage`: parent issue numbers from root to immediate parent.
	/// `None` means lineage is unknown and will be fetched from GitHub on demand;
	/// `Some(&[])` means the issue is known to be at root level.
	pub fn build(link: IssueLink, lineage: Option<&[u64]>) -> Self {
		Self::prefetched(link, lineage, &[])
	}

	/// `read`: issues of `link`'s repo read already, as a listed page.
	pub fn prefetched(link: IssueLink, lineage: Option<&[u64]>, read: &[Arc<FetchedIssue>]) -> Self {
		Self {
			link,
			lineage: lineage.map(|l| l.iter().copied().collect()),
			read: Arc::new(Mutex::new(read.iter().map(|f| (f.issue.number, f.clone())).collect())),
		}
	}

	async fn read(&self, number: u64) -> Result<Arc<FetchedIssue>, RemoteError> {
		if let Some(read) = self.read.lock().expect("never held across a panic").get(&number) {
			return Ok(read.clone());
		}
		let gh = github::client::get().map_err(RemoteError::NoClient)?;
		let repo = self.link.project();
		let fetched = Arc::new(gh.fetch_full_issue(repo, number).await.map_err(|e| RemoteError::fetch_issue(repo, number, e))?);
		self.read.lock().expect("never held across a panic").insert(number, fetched.clone());
		Ok(fetched)
	}

	/// Resolve parent_index, fetching lineage from GitHub if not provided.
	pub async fn resolve_parent_index(&self) -> Result<Option<IssueIndex>, RemoteError> {
		let repo_info = self.link.project();
		let lineage = match &self.lineage {
			Some(l) => l.as_slice().to_vec(),
			None => {
				let mut parents = Vec::new();
				let mut current = self.link.number();
				while let Some(parent) = self.read(current).await?.parent {
					parents.push(parent);
					current = parent;
				}
				parents.reverse();
				parents
			}
		};
		match lineage.is_empty() {
			true => Ok(Some(IssueIndex::repo_only(repo_info))),
			false => Ok(Some(IssueIndex::with_index(repo_info, lineage.iter().map(|&n| IssueSelector::GitId(n)).collect()))),
		}
	}

	fn child(&self, child_link: IssueLink, parent_number: u64) -> Self {
		let mut lineage: CopyArrayVec<u64, MAX_LINEAGE_DEPTH> = self.lineage.as_ref().map(|l| l.as_slice()).unwrap_or(&[]).iter().copied().collect();
		lineage.push(parent_number);
		Self {
			link: child_link,
			lineage: Some(lineage),
			read: self.read.clone(),
		}
	}

	async fn linked_identity(&self) -> Result<IssueIdentity, RemoteError> {
		let fetched = self.read(self.link.number()).await?;
		let t = &fetched.timeline;
		let timestamps = IssueTimestamps {
			title: t.title,
			description: t.description,
			labels: t.labels,
			state: t.state,
			assignees: t.assignees,
			comments: comment_timestamps(&fetched.comments),
		};
		let mut identity = IssueIdentity::new_linked(
			self.resolve_parent_index().await?,
			Some(fetched.issue.user.login.clone()),
			self.link.clone(),
			timestamps,
			t.blocked_by.clone(),
		);
		let meta = identity.mut_linked_issue_meta().expect("just linked");
		meta.blocking = Some(t.blocking.clone());
		meta.prs = t.prs.clone();
		Ok(identity)
	}
}

/// Per comment, its last edit.
fn comment_timestamps(comments: &[GithubComment]) -> Vec<jiff::Timestamp> {
	comments
		.iter()
		.filter_map(|c| jiff::Timestamp::from_str(&c.updated_at).or_else(|_| jiff::Timestamp::from_str(&c.created_at)).ok())
		.collect()
}

impl crate::LazyIssue<RemoteSource> for Issue {
	type Error = RemoteError;

	async fn parent_index(source: &RemoteSource) -> Result<Option<IssueIndex>, Self::Error> {
		source.resolve_parent_index().await
	}

	#[instrument(skip_all)]
	async fn identity(&mut self, source: RemoteSource) -> Result<IssueIdentity, Self::Error> {
		if !self.identity.is_linked() {
			self.identity = source.linked_identity().await?;
		}
		Ok(self.identity.clone())
	}

	#[instrument(skip_all)]
	async fn contents(&mut self, source: RemoteSource) -> Result<IssueContents, Self::Error> {
		if !self.contents.title.is_empty() {
			return Ok(self.contents.clone());
		}
		let fetched = source.read(source.link.number()).await?;
		self.contents = build_contents_from_github(source.link.project(), &fetched.issue, &fetched.comments, &fetched.timeline.blocked_by)?;
		// an identity linked before this load may hold another read's comments and links
		match self.identity.mut_linked_issue_meta() {
			Some(meta) => {
				meta.timestamps.comments = comment_timestamps(&fetched.comments);
				meta.blocked_by = fetched.timeline.blocked_by.clone();
				meta.blocking = Some(fetched.timeline.blocking.clone());
				meta.prs = fetched.timeline.prs.clone();
			}
			None => self.identity = source.linked_identity().await?,
		}
		Ok(self.contents.clone())
	}

	#[instrument(skip_all)]
	async fn children(&mut self, source: RemoteSource) -> Result<HashMap<IssueSelector, Issue>, Self::Error> {
		if !self.children.is_empty() {
			return Ok(self.children.clone());
		}

		let repo_info = source.link.project();
		let fetched = source.read(source.link.number()).await?;
		let filtered: Vec<&github::SubIssue> = fetched.sub_issues.iter().filter(|si| !CloseState::is_duplicate_reason(si.state_reason.as_deref())).collect();

		if filtered.is_empty() {
			return Ok(HashMap::new());
		}

		let parent_number = source.link.number();
		let child_parent_index = self.identity.child_parent_index().expect("parent must be linked before fetching children");

		let children: HashMap<IssueSelector, Issue> = futures::future::try_join_all(filtered.into_iter().map(|sub_issue| {
			let child_source = source.child(IssueLink::in_project(repo_info, sub_issue.number), parent_number);
			async move {
				let mut child = Issue::empty_local(child_parent_index);
				Self::identity(&mut child, child_source.clone()).await?;
				Self::contents(&mut child, child_source.clone()).await?;
				Box::pin(Self::children(&mut child, child_source)).await?;
				Ok::<_, RemoteError>((child.selector(), child))
			}
		}))
		.await?
		.into_iter()
		.collect();

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

	/// A blocker names an issue Github won't link.
	#[error("can't link blocker {link} on Github")]
	Blocker {
		link: IssueLink,
		#[source]
		#[backtrace]
		source: crate::github::GithubError,
	},
}
/// Build IssueContents from GitHub API data.
#[instrument(skip_all, fields(issue_number = issue.number, title = %issue.title))]
fn build_contents_from_github(repo: RepoInfo, issue: &GithubIssue, comments: &[GithubComment], blocked_by: &[IssueLink]) -> Result<IssueContents, RemoteError> {
	if issue.title.contains(['\n', '\r']) {
		let report = miette::Report::new(MultilineTitle {
			repo,
			number: issue.number,
			title: issue.title.clone(),
		});
		return Err(RemoteError::new_multiline_title(format!("{report:?}")));
	}
	if CloseState::is_duplicate_reason(issue.state_reason.as_deref()) {
		let report = miette::Report::new(Duplicate { repo, number: issue.number });
		return Err(RemoteError::new_duplicate(format!("{report:?}")));
	}

	let all_labels: Vec<String> = issue.labels.iter().map(|l| l.name.clone()).collect();
	let close_state = CloseState::from_github(&issue.state, issue.state_reason.as_deref(), &all_labels);
	// the `p:` label *is* the state here; leaving it in would also render it in the title line's `(labels)` slot
	let labels: Vec<String> = all_labels.into_iter().filter(|l| l.parse::<crate::Progress>().is_err()).collect();

	let raw_body = issue.body.as_deref().unwrap_or(""); //IGNORED_ERROR: GitHub API null body is valid (empty issue)
	let (body, mut blockers) = split_blockers(raw_body);
	let named = blockers.issue_links(repo);
	// one linked on Github alone goes last in sequence, which is the top of the list
	for link in blocked_by.iter().filter(|l| !named.contains(l)) {
		blockers.items.insert(
			0,
			crate::BlockerItem {
				text: link.shorthand(repo),
				..Default::default()
			},
		);
	}

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

	let mut assignees: Vec<String> = issue.assignees.iter().map(|a| a.login.clone()).collect();
	assignees.sort_unstable();

	Ok(IssueContents {
		title: issue.title.clone(),
		labels,
		assignees,
		state: close_state,
		comments: issue_comments.into(),
		blockers,
	})
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
		let created = self.is_local();

		// If this is a pending (local) issue, create it first
		if created {
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

			if !self.contents.assignees.is_empty() {
				gh.set_assignees(repo_info, created.number, &self.contents.assignees).await?;
			}

			// Close if needed
			if closed {
				gh.update_issue_state(repo_info, created.number, &self.contents.state).await?;
			}

			// Link to parent if this issue has one
			let lineage = self.identity.git_lineage()?;
			if let Some(&parent_number) = lineage.last() {
				gh.add_sub_issue(repo_info, parent_number, created.id).await?;
			}

			// Update identity - keep same parent_index, just add linking info
			let link = IssueLink::in_project(repo_info, created.number);
			let user = gh.fetch_authenticated_user().await?;
			self.identity = IssueIdentity::new_linked(Some(parent_index), Some(user), link, crate::IssueTimestamps::now(), Vec::new());
			changed = true;
		}

		let issue_number = self.git_id().expect("issue must have number after creation");

		// Sync content against old (if we have old state)
		let diff = compute_node_diff(self, old);

		if old.is_some_and(|old| old.contents.title != self.contents.title) {
			println!("Updating issue #{issue_number} title...");
			gh.update_issue_title(repo_info, issue_number, &self.contents.title).await?;
			changed = true;
		}

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

		if old.is_some_and(|old| old.contents.assignees != self.contents.assignees) {
			println!("Updating issue #{issue_number} assignees...");
			gh.set_assignees(repo_info, issue_number, &self.contents.assignees).await?;
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
			println!("Updating issue #{issue_number} state to {}...", self.contents.state.to_github_state());
			gh.update_issue_state(repo_info, issue_number, &self.contents.state).await?;
			changed = true;
		}

		let held = match old {
			Some(old) => Some(old.identity.as_linked().expect("an old state is on Github").blocked_by.clone()),
			None if created => Some(Vec::new()),
			None => None,
		};
		if let Some(held) = held {
			let own = IssueLink::in_project(repo_info, issue_number);
			let mut wanted = self.contents.blockers.issue_links(repo_info);
			wanted.retain(|l| matches!(l, IssueLink::Owned(_)) && *l != own);
			let mut linked: Vec<IssueLink> = held.iter().filter(|l| wanted.contains(l)).cloned().collect();
			let gh = &gh;
			let blocker = move |link: &IssueLink| {
				let link = link.clone();
				async move { gh.fetch_issue(link.project(), link.number()).await.map_err(|source| RemoteSinkError::Blocker { link, source }) }
			};
			for link in wanted.iter().filter(|l| !held.contains(l)) {
				let blocker = blocker(link).await?;
				if blocker.pull_request.is_some() {
					continue; // GitHub links issues only
				}
				if IssueLink::parse(&blocker.html_url).as_ref() != Some(link) {
					eprintln!(
						"warning: blocker {} of #{issue_number} moved to {}; relink it to have it linked",
						link.shorthand(repo_info),
						blocker.html_url
					);
					continue;
				}
				println!("Linking #{issue_number} as blocked by {}...", link.shorthand(repo_info));
				gh.add_blocked_by(repo_info, issue_number, blocker.id)
					.await
					.map_err(|source| RemoteSinkError::Blocker { link: link.clone(), source })?;
				linked.push(link.clone());
				changed = true;
			}
			for link in held.iter().filter(|l| !wanted.contains(l)) {
				let blocker = blocker(link).await?;
				if blocker.pull_request.is_some() || IssueLink::parse(&blocker.html_url).as_ref() != Some(link) {
					continue; // never linked as named
				}
				println!("Unlinking {} from blocking #{issue_number}...", link.shorthand(repo_info));
				gh.remove_blocked_by(repo_info, issue_number, blocker.id)
					.await
					.map_err(|source| RemoteSinkError::Blocker { link: link.clone(), source })?;
				changed = true;
			}
			self.identity.mut_linked_issue_meta().expect("linked once created").blocked_by = linked;
		}

		// Create pending comments sequentially (order matters)
		for comment in self.contents.comments.iter_mut().skip(1) {
			if comment.is_pending() && !comment.body.is_empty() {
				let body_str = comment.body.to_string();
				println!("Creating new comment on issue #{issue_number}...");
				let created = gh.create_comment(repo_info, issue_number, &body_str).await?;
				comment.identity = CommentIdentity::Created {
					user: created.user.login,
					id: created.id,
				};
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
		for detached in old.iter().flat_map(|o| &o.children).filter(|(s, _)| !self.children.contains_key(s)).map(|(_, c)| c) {
			let n = detached.git_id().expect("children of a remote state are numbered");
			let id = gh.fetch_issue(repo_info, n).await?.id;
			println!("Detaching #{n} from #{issue_number}...");
			gh.remove_sub_issue(repo_info, issue_number, id).await?;
			changed = true;
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

	/// The GitHub client is process-wide: each test holds it for its whole run.
	static CLIENT_HELD: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

	async fn seeded(labels: Vec<&str>) -> (Arc<MockGithubClient>, RepoInfo, tokio::sync::MutexGuard<'static, ()>) {
		let held = CLIENT_HELD.lock().await;
		let client = Arc::new(MockGithubClient::new("testuser"));
		let repo = RepoInfo::new("o", "r");
		client.add_issue(repo, 1, "Half done", "body", "open", labels, "testuser", Some(jiff::Timestamp::from_second(1704067200).unwrap()));
		github::client::set(client.clone());
		(client, repo, held)
	}

	fn source(repo: RepoInfo) -> RemoteSource {
		RemoteSource::build(IssueLink::in_project(repo, 1), Some(&[]))
	}

	/// A `p:` label is the state, not a label: it parses back into `InProgress` and never
	/// reaches the `(labels)` slot of the title line.
	#[tokio::test]
	async fn progress_label_parses_back_into_the_state() {
		let (_client, repo, held) = seeded(vec!["bug", "p:partial"]).await;
		let issue = Issue::load(source(repo)).await.unwrap();
		assert_eq!(issue.contents.state, CloseState::InProgress(crate::Progress::Partial));
		assert_eq!(issue.contents.labels, ["bug"]);
		drop(held);
	}

	/// A pure `[ ] → [.]` flip moves neither `contents.labels` nor the GitHub open/closed state,
	/// so it must still push — as the managed label.
	#[tokio::test]
	async fn progress_flip_pushes_the_managed_label() {
		let (client, repo, held) = seeded(vec!["bug"]).await;
		let old = Issue::load(source(repo)).await.unwrap();
		let mut new = old.clone();
		new.contents.state = CloseState::InProgress(crate::Progress::Partial);

		client.clear_call_log();
		assert!(<Issue as Sink<Remote>>::sink(&mut new, Some(&old)).await.unwrap());
		insta::assert_snapshot!(client.get_call_log().join("\n"), @r#"set_labels(o, r, 1, ["bug", "p:partial"])"#);
		drop(held);
	}
}
