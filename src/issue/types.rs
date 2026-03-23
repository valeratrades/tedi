//! Core issue data structures and parsing/serialization.
//!
//! This module contains the pure Issue type with parsing and serialization.

use std::{collections::HashMap, path::PathBuf};

use arrayvec::ArrayString;
use copy_arrayvec::CopyArrayVec;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use url::Url;

use super::events::{indent_into, wrap_inline_in_paragraphs};

/// Maximum title length enforced by Github.
pub const MAX_TITLE_LENGTH: usize = 256;

/// Maximum index depth (lineage + the issue itself).
pub const MAX_INDEX_DEPTH: usize = MAX_LINEAGE_DEPTH + 1;
pub const MAX_LINEAGE_DEPTH: usize = 8;
pub type IssueChildren<T> = HashMap<IssueSelector, T>;
/// Trait for lazily loading an issue from a source.
///
/// `S` is the source type directly (e.g., `LocalIssueSource<FsReader>`, `RemoteSource`).
/// Methods load data on demand; `&mut self` allows caching intermediate results.
#[allow(async_fn_in_trait)]
pub trait LazyIssue<S: Clone + std::fmt::Debug>: Sized {
	/// Error type for operations on this source.
	type Error: std::error::Error;

	/// Resolve parent_index from the source.
	/// Returns None for root-level issues (no parent).
	async fn parent_index(source: &S) -> Result<Option<IssueIndex>, Self::Error>;
	async fn identity(&mut self, source: S) -> Result<IssueIdentity, Self::Error>;
	async fn contents(&mut self, source: S) -> Result<IssueContents, Self::Error>;
	async fn children(&mut self, source: S) -> Result<IssueChildren<Issue>, Self::Error>;

	/// Load a full issue tree from the source.
	/// Default implementation calls parent_index, then populates identity, contents, and children.
	async fn load(source: S) -> Result<Issue, Self::Error>
	where
		Issue: LazyIssue<S, Error = Self::Error>, {
		let parent_index = <Issue as LazyIssue<S>>::parent_index(&source).await?.expect("load requires parent_index to be Some");
		let mut issue = Issue::empty_local(parent_index);
		<Issue as LazyIssue<S>>::identity(&mut issue, source.clone()).await?;
		<Issue as LazyIssue<S>>::contents(&mut issue, source.clone()).await?;
		Box::pin(<Issue as LazyIssue<S>>::children(&mut issue, source)).await?;
		Ok(issue)
	}
}
/// Repository identification: owner and repo name.
/// Uses fixed-size `ArrayString`s to be `Copy`.
/// GitHub limits: owner max 39 chars, repo max 100 chars.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct RepoInfo {
	/// Repository owner (fixed max length; following Github spec)
	owner: ArrayString<39>,
	/// Repository name (fixed max length; following Github spec)
	repo: ArrayString<100>,
}

impl RepoInfo {
	/// Create a new RepoInfo.
	/// Panics if owner exceeds 39 chars or repo exceeds 100 chars.
	pub fn new(owner: &str, repo: &str) -> Self {
		Self {
			owner: ArrayString::from(owner).expect("owner name too long (max 39 chars)"),
			repo: ArrayString::from(repo).expect("repo name too long (max 100 chars)"),
		}
	}

	/// Get the owner.
	pub fn owner(&self) -> &str {
		self.owner.as_str()
	}

	/// Get the repo.
	pub fn repo(&self) -> &str {
		self.repo.as_str()
	}
}

impl From<(&str, &str)> for RepoInfo {
	fn from((owner, repo): (&str, &str)) -> Self {
		Self::new(owner, repo)
	}
}

/// A Github issue identifier. Wraps a URL and derives all properties on demand.
/// Format: `https://github.com/{owner}/{repo}/issues/{number}`
#[derive(Clone, Debug, derive_more::Deref, derive_more::DerefMut, Eq, Hash, PartialEq)]
pub struct IssueLink(Url);

impl IssueLink /*{{{1*/ {
	/// Create from a URL. Returns None if not a valid Github issue URL.
	pub fn new(url: Url) -> Option<Self> {
		// Validate it's a Github issue URL
		if url.host_str() != Some("github.com") {
			return None;
		}
		let segments: Vec<_> = url.path_segments()?.collect();
		// Must be: owner/repo/issues/number
		if segments.len() < 4 || segments[2] != "issues" {
			return None;
		}
		// Number must be valid
		segments[3].parse::<u64>().ok()?;
		Some(Self(url))
	}

	/// Parse from a URL string.
	pub fn parse(url: &str) -> Option<Self> {
		let url = Url::parse(url).ok()?;
		Self::new(url)
	}

	/// Get the underlying URL.
	pub fn url(&self) -> &Url {
		&self.0
	}

	/// Get the repository info (owner and repo).
	pub fn repo_info(&self) -> RepoInfo {
		let mut segments = self.0.path_segments().unwrap();
		let owner = segments.next().unwrap();
		let repo = segments.next().unwrap();
		RepoInfo::new(owner, repo)
	}

	/// Get the owner (first path segment).
	pub fn owner(&self) -> &str {
		self.0.path_segments().unwrap().next().unwrap()
	}

	/// Get the repo (second path segment).
	pub fn repo(&self) -> &str {
		self.0.path_segments().unwrap().nth(1).unwrap()
	}

	/// Get the issue number (fourth path segment).
	pub fn number(&self) -> u64 {
		self.0.path_segments().unwrap().nth(3).unwrap().parse().unwrap()
	}

	/// Build URL string.
	pub fn as_str(&self) -> &str {
		self.0.as_str()
	}
}
//,}}}1

/// Identity of a comment - either linked to Github or pending creation.
/// Note: The first comment (issue body) is always `Body`, not `Linked` or `Pending`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum CommentIdentity {
	/// This is the issue body (first comment), not a separate Github comment
	Body,
	/// Comment exists on Github with this ID, created by given user
	Created { user: String, id: u64 },
	/// Comment is pending creation on Github (will be created in post-sync)
	#[default]
	Pending,
}

//,}}}1

use super::{
	IssueMarker, Marker,
	blocker::BlockerSequence,
	error::{ParseContext, ParseError},
};

/// Close state of an issue.
/// Maps to Github's binary open/closed, but locally supports additional variants.
#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum CloseState {
	/// Issue is open: `- [ ]`
	#[default]
	Open,
	/// Issue is closed normally: `- [x]`
	Closed,
	/// Issue was closed as not planned: `- [-]`
	/// Treated same as Closed for storage (embedded with .bak)
	NotPlanned,
	/// Issue is a duplicate of another issue: `- [123]`
	/// The number references another issue in the same repo.
	/// These should be removed from local storage entirely.
	Duplicate(u64),
}
impl CloseState /*{{{1*/ {
	/// Returns true if the issue is closed (any close variant)
	pub fn is_closed(&self) -> bool {
		!matches!(self, CloseState::Open)
	}

	/// Returns true if this close state means the issue should be removed from local storage
	pub fn should_remove(&self) -> bool {
		matches!(self, CloseState::Duplicate(_))
	}

	/// Convert to Github API state string
	pub fn to_github_state(&self) -> &'static str {
		match self {
			CloseState::Open => "open",
			_ => "closed",
		}
	}

	/// Convert to Github API state_reason string (for closed issues)
	pub fn to_github_state_reason(&self) -> Option<&'static str> {
		match self {
			CloseState::Open => None,
			CloseState::Closed => Some("completed"),
			CloseState::NotPlanned => Some("not_planned"),
			CloseState::Duplicate(_) => Some("duplicate"),
		}
	}

	/// Create from Github API state and state_reason.
	///
	/// # Panics
	/// Panics if state_reason is "duplicate" - duplicates must be filtered before calling this.
	pub fn from_github(state: &str, state_reason: Option<&str>) -> Self {
		assert!(state_reason != Some("duplicate"), "Duplicate issues must be filtered before calling from_github");

		match (state, state_reason) {
			("open", _) => CloseState::Open,
			("closed", Some("not_planned")) => CloseState::NotPlanned,
			("closed", Some("completed") | None) => CloseState::Closed,
			("closed", Some(unknown)) => {
				tracing::warn!("Unknown state_reason '{unknown}', treating as Closed");
				CloseState::Closed
			}
			(unknown, _) => {
				tracing::warn!("Unknown state '{unknown}', treating as Open");
				CloseState::Open
			}
		}
	}

	/// Returns true if this represents a duplicate (should be filtered from fetch results)
	pub fn is_duplicate_reason(state_reason: Option<&str>) -> bool {
		state_reason == Some("duplicate")
	}

	/// Parse from checkbox content (the character(s) inside `[ ]`)
	pub fn from_checkbox(content: &str) -> Result<Self, String> {
		let content = content.trim();
		match content {
			"" | " " => Ok(CloseState::Open),
			"x" | "X" => Ok(CloseState::Closed),
			"-" => Ok(CloseState::NotPlanned),
			s => s.parse::<u64>().map(CloseState::Duplicate).map_err(|_| s.to_string()),
		}
	}

	/// Convert to checkbox character(s) for serialization
	pub fn to_checkbox_contents(&self) -> String {
		match self {
			CloseState::Open => " ".to_string(),
			CloseState::Closed => "x".to_string(),
			CloseState::NotPlanned => "-".to_string(),
			CloseState::Duplicate(n) => n.to_string(),
		}
	}
}

/// Timestamps tracking when individual fields of an issue were last changed.
/// Each node keeps change info about itself only (not sub-issues).
/// Used for sync conflict resolution.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct IssueTimestamps {
	/// Last change to the issue title. Optional because we can't always get this from GitHub.
	pub title: Option<Timestamp>,
	/// Last change to the issue description/body (includes blockers). Optional because we can't always get this from GitHub.
	pub description: Option<Timestamp>,
	/// Last change to labels. Optional because we can't always get this from GitHub.
	pub labels: Option<Timestamp>,
	/// Last state change (open/closed). Optional because we can't always get this from GitHub.
	pub state: Option<Timestamp>,
	/// Per-comment timestamps (indexed by position, excluding body at index 0).
	/// Empty vec means no timestamp info available.
	pub comments: Vec<Timestamp>,
}
impl IssueTimestamps {
	pub fn now() -> Self {
		let now = Timestamp::now();
		Self {
			title: Some(now),
			description: Some(now),
			labels: Some(now),
			state: Some(now),
			comments: Vec::new(),
		}
	}

	/// Update timestamps based on what changed between old and new issue contents.
	/// Sets the current time for any field that changed.
	pub fn update_from_diff(&mut self, old: &super::IssueContents, new: &super::IssueContents) {
		let now = Timestamp::now();

		if old.title != new.title {
			self.title = Some(now);
		}

		// Compare body (first comment) and blockers for description changes
		let old_body = old.comments.description();
		let new_body = new.comments.description();
		if old_body != new_body || old.blockers != new.blockers {
			self.description = Some(now);
		}

		if old.labels != new.labels {
			self.labels = Some(now);
		}

		if old.state != new.state {
			self.state = Some(now);
		}

		// Per-comment timestamps (skip body at index 0)
		let old_comments: Vec<_> = old.comments.iter().skip(1).collect();
		let new_comments: Vec<_> = new.comments.iter().skip(1).collect();
		let new_comment_count = new_comments.len();

		// Resize timestamps vec to match new comment count
		self.comments.resize(new_comment_count, now);

		// Update timestamps for changed comments
		for (i, (old_c, new_c)) in old_comments.iter().zip(&new_comments).enumerate() {
			if old_c.body.to_string() != new_c.body.to_string() {
				self.comments[i] = now;
			}
		}
		// New comments (beyond old length) already have `now` from resize
	}
}

/// Metadata for an issue linked to Github.
/// Parent chain is stored in `IssueIdentity.parent_index`.
#[derive(Clone, Debug, PartialEq)]
pub struct LinkedIssueMeta {
	/// User who created the issue. None = unknown (treated as ourselves).
	pub user: Option<String>,
	/// Link to the issue on Github
	//TODO: I'm pretty sure we can remove this field and just construct it from context when needed
	link: IssueLink,
	/// Timestamps of last changes to individual fields.
	/// Used for sync conflict resolution.
	pub timestamps: IssueTimestamps,
}
impl LinkedIssueMeta {
	pub fn new(user: Option<String>, link: IssueLink, timestamps: IssueTimestamps) -> Self {
		if user.is_none() {
			tracing::warn!("LinkedIssueMeta created without user");
		}
		Self { user, link, timestamps }
	}

	/// Get the link.
	pub fn link(&self) -> &IssueLink {
		&self.link
	}

	/// Get the repository owner from the link.
	pub fn owner(&self) -> &str {
		self.link.owner()
	}

	/// Get the repository name from the link.
	pub fn repo(&self) -> &str {
		self.link.repo()
	}

	/// Get the issue number from the link.
	pub fn number(&self) -> u64 {
		self.link.number()
	}
}

/// Identity of an issue - has optional parent_index (for location) with remote connection info.
#[derive(Clone, Debug)]
pub struct IssueIdentity {
	/// Parent's IssueIndex. For root it's the default variant with null ancestry (but owner+repo specified)
	/// NB: it's index of the PARENT, not ourselves. Done this way to allow for eg title changes.
	pub parent_index: IssueIndex,
	/// Whether this issue belongs to a virtual project (local-only, never synced to Github).
	pub is_virtual: bool,
	/// Linked Github metadata, if this issue has been synced.
	/// `None` = pending creation (or virtual). `Some(_)` = linked to Github.
	pub remote: Option<Box<LinkedIssueMeta>>,
}
impl IssueIdentity {
	/// Create a new linked Github issue identity.
	/// If `parent_index` is None, derives repo_only from the link.
	pub fn new_linked(parent_index: Option<IssueIndex>, user: Option<String>, link: IssueLink, timestamps: IssueTimestamps) -> Self {
		let parent_index = parent_index.unwrap_or_else(|| {
			let repo_info = link.repo_info();
			IssueIndex::repo_only(repo_info)
		});
		Self {
			parent_index,
			is_virtual: false,
			remote: Some(Box::new(LinkedIssueMeta::new(user, link, timestamps))),
		}
	}

	/// Create a new pending Github issue identity (will be created on first sync).
	pub fn pending(parent_index: IssueIndex) -> Self {
		Self {
			parent_index,
			is_virtual: false,
			remote: None,
		}
	}

	/// Create a new virtual (local-only) issue identity.
	pub fn virtual_issue(parent_index: IssueIndex) -> Self {
		Self {
			parent_index,
			is_virtual: true,
			remote: None,
		}
	}

	/// Check if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		self.remote.is_some()
	}

	/// Check if this issue is local only (pending creation on Github).
	/// Note: Virtual issues are also local-only but should be checked via is_virtual().
	pub fn is_local(&self) -> bool {
		self.remote.is_none() && !self.is_virtual
	}

	/// Get the linked metadata if linked.
	pub fn as_linked(&self) -> Option<&LinkedIssueMeta> {
		self.remote.as_deref()
	}

	/// Get mutable access to the linked metadata.
	pub fn mut_linked_issue_meta(&mut self) -> Option<&mut LinkedIssueMeta> {
		self.remote.as_deref_mut()
	}

	/// Get the issue link if linked.
	pub fn link(&self) -> Option<&IssueLink> {
		self.as_linked().map(|m| &m.link)
	}

	/// Get the issue number if linked.
	pub fn number(&self) -> Option<u64> {
		self.link().map(|l| l.number())
	}

	/// Get the URL string if linked.
	pub fn url_str(&self) -> Option<&str> {
		self.link().map(|l| l.as_str())
	}

	/// Get the user who created this issue if linked and known.
	pub fn user(&self) -> Option<&str> {
		self.as_linked().and_then(|m| m.user.as_deref())
	}

	/// Check if this issue is owned by the current user.
	/// True if: virtual, or linked with unknown user (None), or linked with matching user.
	pub fn is_owned(&self) -> bool {
		self.is_virtual
			|| match self.as_linked() {
				None => false,
				Some(meta) => meta.user.as_ref().is_none_or(|u| crate::current_user::is(u)),
			}
	}

	/// Get the timestamps if linked.
	pub fn timestamps(&self) -> Option<&IssueTimestamps> {
		self.as_linked().map(|m| &m.timestamps)
	}

	/// Get the repository info.
	pub fn repo_info(&self) -> RepoInfo {
		self.parent_index.repo_info()
	}

	/// Get owner.
	pub fn owner(&self) -> &str {
		self.parent_index.owner()
	}

	/// Get repo.
	pub fn repo(&self) -> &str {
		self.parent_index.repo()
	}

	/// Get lineage (parent issue numbers).
	/// For root issues: empty slice.
	/// For child issues: all GitId numbers in parent_index.
	///
	/// # Errors
	/// Returns `TitleInGitPathError` if any parent selector is a Title (pending issue).
	pub fn git_lineage(&self) -> Result<Vec<u64>, super::error::TitleInGitPathError> {
		self.parent_index.git_num_path()
	}

	/// Create a child's parent_index by appending this issue's number.
	/// Returns None if this issue is not linked (has no number to append).
	pub fn child_parent_index(&self) -> Option<IssueIndex> {
		self.number().map(|n| self.parent_index.child(IssueSelector::GitId(n)))
	}
}

/// Selector for identifying an issue within a repo.
/// GitId is preferred when available, as title can change.
/// Uses `ArrayString` for `Copy` semantics.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[allow(clippy::large_enum_variant)] // Intentional: Title variant is large for Copy semantics
pub enum IssueSelector {
	/// Github issue number (stable identifier)
	GitId(u64),
	/// Issue title (for pending issues not yet synced to Github)
	Title(ArrayString<MAX_TITLE_LENGTH>),
	/// Regex pattern for fuzzy matching (touch mode only, lower priority than Title)
	Regex(ArrayString<MAX_TITLE_LENGTH>),
}

impl IssueSelector {
	/// Create a Title selector from a string.
	/// Panics if title exceeds MAX_TITLE_LENGTH (256 chars).
	pub fn title(title: &str) -> Self {
		Self::Title(ArrayString::from(title).unwrap_or_else(|_| panic!("title too long (max {MAX_TITLE_LENGTH} chars): {}", title.len())))
	}

	/// Try to create a Title selector from a string.
	/// Returns None if title exceeds MAX_TITLE_LENGTH.
	pub fn try_title(title: &str) -> Option<Self> {
		ArrayString::from(title).ok().map(Self::Title)
	}

	/// Create a Regex selector from a pattern string.
	pub fn regex(pattern: &str) -> Self {
		Self::Regex(ArrayString::from(pattern).unwrap_or_else(|_| panic!("pattern too long (max {MAX_TITLE_LENGTH} chars): {}", pattern.len())))
	}
}

/// Minimal descriptor for locating an issue.
/// Contains repo info and a path of selectors from root to the target issue (inclusive).
/// Uses fixed-size storage to be `Copy`.
#[derive(Clone, Copy, Debug, derive_more::Deref, derive_more::DerefMut, Eq, PartialEq)]
pub struct IssueIndex {
	repo_info: RepoInfo,
	/// Path from root to target issue (inclusive).
	#[deref]
	#[deref_mut]
	index: CopyArrayVec<IssueSelector, MAX_INDEX_DEPTH>,
}

impl IssueIndex {
	/// Create descriptor for a root-level issue.
	pub fn root(repo_info: RepoInfo, selector: IssueSelector) -> Self {
		let mut index = CopyArrayVec::new();
		index.push(selector);
		Self { repo_info, index }
	}

	/// Create descriptor with full index path.
	/// Panics if index exceeds MAX_INDEX_DEPTH.
	pub fn with_index(repo_info: RepoInfo, index: Vec<IssueSelector>) -> Self {
		Self {
			repo_info,
			index: index.into_iter().collect(),
		}
	}

	/// Create descriptor for repo only (no specific issue).
	pub fn repo_only(repo_info: RepoInfo) -> Self {
		Self {
			repo_info,
			index: CopyArrayVec::new(),
		}
	}

	/// Add a child selector, returning new descriptor.
	/// Panics if result would exceed MAX_INDEX_DEPTH.
	pub fn child(&self, selector: IssueSelector) -> Self {
		let mut index = self.index;
		index.push(selector);
		Self { repo_info: self.repo_info, index }
	}

	/// Get the index path.
	pub fn index(&self) -> &[IssueSelector] {
		&self.index
	}

	/// Get the repository info.
	pub fn repo_info(&self) -> RepoInfo {
		self.repo_info
	}

	/// Get the owner.
	pub fn owner(&self) -> &str {
		self.repo_info.owner()
	}

	/// Get the repo.
	pub fn repo(&self) -> &str {
		self.repo_info.repo()
	}

	/// Extract numeric issue numbers from the index (GitId selectors only).
	///
	/// # Errors
	/// Returns `TitleInGitPathError` if any selector is a Title (pending issue).
	pub fn git_num_path(&self) -> Result<Vec<u64>, super::error::TitleInGitPathError> {
		use miette::{NamedSource, SourceSpan};

		let mut result = Vec::with_capacity(self.index().len());
		let mut offset = format!("{}/{}", self.repo_info.owner(), self.repo_info.repo()).len();

		for selector in self.index() {
			match selector {
				IssueSelector::GitId(n) => {
					let s = format!("/{n}");
					offset += s.len();
					result.push(*n);
				}
				IssueSelector::Title(title) | IssueSelector::Regex(title) => {
					let span: SourceSpan = (offset + 1, title.len()).into(); // +1 to skip the '/'
					return Err(super::error::TitleInGitPathError {
						index_display: NamedSource::new("IssueIndex", self.to_string()),
						span,
					});
				}
			}
		}
		Ok(result)
	}

	/// Get the issue's own number if the last selector is a GitId.
	pub fn issue_number(&self) -> Option<u64> {
		match self.index().last() {
			Some(IssueSelector::GitId(n)) => Some(*n),
			_ => None,
		}
	}

	/// Get the parent's IssueIndex (all selectors except the last one).
	/// For repo-only or single-selector indices, returns repo_only.
	pub fn parent(&self) -> Option<Self> {
		if self.index.is_empty() {
			None
		} else {
			let mut index = self.index;
			index.pop();
			Some(Self { repo_info: self.repo_info, index })
		}
	}
}

impl std::fmt::Display for IssueIndex {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}/{}", self.repo_info.owner(), self.repo_info.repo())?;
		for selector in self.index() {
			match selector {
				IssueSelector::GitId(n) => write!(f, "/{n}")?,
				IssueSelector::Title(t) | IssueSelector::Regex(t) => write!(f, "/{t}")?,
			}
		}
		Ok(())
	}
}

impl std::str::FromStr for IssueIndex {
	type Err = color_eyre::eyre::Report;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let parts: Vec<&str> = s.split('/').collect();
		if parts.len() < 2 {
			color_eyre::eyre::bail!("IssueIndex requires at least owner/repo, got: {s}");
		}
		let repo_info = RepoInfo::new(parts[0], parts[1]);
		let selectors: Vec<IssueSelector> = parts[2..]
			.iter()
			.map(|p| match p.parse::<u64>() {
				Ok(n) => IssueSelector::GitId(n),
				Err(_) => IssueSelector::title(p),
			})
			.collect();
		Ok(Self::with_index(repo_info, selectors))
	}
}

impl From<&Issue> for IssueIndex {
	fn from(issue: &Issue) -> Self {
		// Build this issue's IssueIndex by extending parent_index with this issue's selector
		let parent_index = issue.identity.parent_index;

		if let Some(n) = issue.git_id() {
			// Issue has a number - add it as the last selector
			parent_index.child(IssueSelector::GitId(n))
		} else {
			// Pending issue - add title as last selector to distinguish from parent-only paths
			parent_index.child(IssueSelector::title(&issue.contents.title))
		}
	}
}

/// A comment in the issue conversation (first one is always the issue body)
#[derive(Clone, Debug, Default, derive_more::Deref, PartialEq)]
pub struct Comment {
	/// Comment identity - body, linked to Github, or pending creation
	pub identity: CommentIdentity,
	/// The markdown body stored as parsed events for lossless roundtripping
	#[deref]
	pub body: super::Events,
}
impl Comment {
	/// Get the comment ID if linked.
	pub fn id(&self) -> Option<u64> {
		match &self.identity {
			CommentIdentity::Created { id, .. } => Some(*id),
			CommentIdentity::Body | CommentIdentity::Pending => None,
		}
	}

	/// Get the user who created this comment if linked.
	pub fn user(&self) -> Option<&str> {
		match &self.identity {
			CommentIdentity::Created { user, .. } => Some(user),
			CommentIdentity::Body | CommentIdentity::Pending => None,
		}
	}

	/// Check if this is a Github comment (not the issue body).
	pub fn is_comment(&self) -> bool {
		!matches!(self.identity, CommentIdentity::Body)
	}

	/// Check if this comment is pending creation.
	pub fn is_pending(&self) -> bool {
		matches!(self.identity, CommentIdentity::Pending)
	}
}
#[derive(Clone, Debug, derive_more::Deref, derive_more::DerefMut, PartialEq)]
pub struct Comments(pub Vec<Comment>);
impl Comments {
	pub fn description(&self) -> String {
		self.first().map(|c| c.body.to_string()).expect("`new` should've asserted that description comment was provided")
	}

	pub fn new(v: Vec<Comment>) -> Self {
		assert!(
			v.first().is_some_and(|c| matches!(c.identity, CommentIdentity::Body)),
			"Trying to instantiate issue `Comments` without description"
		);
		{
			// creation of pending comments intertwined with submitted ones is invalid, - they're linked //Q: should I use a linked list here?
			let first_pending_id = v.iter().position(|c| c.is_pending());
			if let Some(id) = first_pending_id {
				assert!(v.iter().rev().take(v.len() - id).all(|c| c.is_pending()))
			}
		}
		Self(v)
	}
}
impl Default for Comments {
	fn default() -> Self {
		Self(vec![Comment {
			identity: CommentIdentity::Body,
			..Default::default()
		}])
	}
}
impl From<Vec<Comment>> for Comments {
	fn from(value: Vec<Comment>) -> Self {
		match value.is_empty() {
			true => Self::default(),
			false => Self::new(value),
		}
	}
}

/// The full editable content of an issue.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IssueContents {
	pub title: String,
	pub labels: Vec<String>,
	pub state: CloseState,
	pub comments: Comments,
	pub blockers: BlockerSequence,
}
/// Complete representation of an issue file
#[derive(Clone, Debug, PartialEq)]
pub struct Issue {
	/// Identity - linked to Github or local only
	pub identity: IssueIdentity,
	pub contents: IssueContents,
	/// Sub-issues keyed by selector
	pub children: HashMap<IssueSelector, Issue>,
}
impl Issue /*{{{1*/ {
	/// Iterate over children by mutable reference.
	pub fn iter_mut(&mut self) -> std::collections::hash_map::IterMut<'_, IssueSelector, Issue> {
		self.children.iter_mut()
	}

	/// Check if this issue is local only (not yet on Github).
	pub fn is_local(&self) -> bool {
		self.identity.is_local()
	}

	/// Create an empty local issue with the given parent_index.
	/// Used as initial state when lazily loading an issue.
	pub fn empty_local(parent_index: IssueIndex) -> Self {
		Self {
			identity: IssueIdentity::pending(parent_index),
			contents: IssueContents::default(),
			children: HashMap::new(),
		}
	}

	/// Create a new pending issue with the given title and parent_index.
	///
	/// If `virtual_project` is true, creates a virtual issue (local-only, no Github sync).
	/// Otherwise creates a pending Github issue that will be created on first sync.
	pub fn pending_with_parent(title: impl Into<String>, parent_index: IssueIndex, virtual_project: bool) -> Self {
		let identity = if virtual_project {
			IssueIdentity::virtual_issue(parent_index)
		} else {
			IssueIdentity::pending(parent_index)
		};
		let contents = IssueContents {
			title: title.into(),
			..Default::default()
		};
		Self {
			identity,
			contents,
			children: HashMap::new(),
		}
	}

	/// Create a new pending issue from an IssueIndex descriptor.
	/// The last element of descriptor.index() must be a Title selector.
	///
	/// If `virtual_project` is true, creates a virtual issue (local-only, no Github sync).
	/// Otherwise creates a pending Github issue that will be created on first sync.
	pub fn pending_from_descriptor(descriptor: &IssueIndex, virtual_project: bool) -> Self {
		let index = descriptor.index();
		let (title, parent_selectors): (String, Vec<IssueSelector>) = match index.last() {
			Some(IssueSelector::Title(t)) => {
				let selectors = index[..index.len() - 1].to_vec();
				(t.to_string(), selectors)
			}
			Some(IssueSelector::GitId(_)) => panic!("pending_from_descriptor requires last selector to be Title"),
			Some(IssueSelector::Regex(_)) => panic!("pending_from_descriptor requires last selector to be Title, not Regex"),
			None => panic!("pending_from_descriptor requires non-empty index"),
		};
		let parent_index = IssueIndex::with_index(descriptor.repo_info(), parent_selectors);
		Self::pending_with_parent(title, parent_index, virtual_project)
	}

	/// Check if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		self.identity.is_linked()
	}

	/// Get the issue number if linked to Github.
	pub fn git_id(&self) -> Option<u64> {
		self.identity.number()
	}

	/// Get the URL string if linked to Github.
	pub fn url_str(&self) -> Option<&str> {
		self.identity.url_str()
	}

	/// Get the user who created this issue if linked to Github.
	pub fn user(&self) -> Option<&str> {
		self.identity.user()
	}

	/// Update timestamps based on what changed compared to old issue.
	/// This should be called after local modifications to track when fields changed.
	/// Recursively updates children's timestamps too.
	fn update_timestamps_from_diff(&mut self, old: &Issue) {
		if let Some(linked) = self.identity.mut_linked_issue_meta() {
			linked.timestamps.update_from_diff(&old.contents, &self.contents);
		}

		// Recursively update children's timestamps
		// Match children by selector
		for (selector, new_child) in &mut self.children {
			if let Some(old_child) = old.children.get(selector) {
				new_child.update_timestamps_from_diff(old_child);
			}
			// New children (not in old) don't need timestamp updates - they'll use defaults
		}
	}

	/// If this issue is closed, propagate its exact close state to all children recursively.
	fn propagate_closed(&mut self) {
		if !self.contents.state.is_closed() {
			return;
		}
		let state = self.contents.state.clone();
		for child in self.children.values_mut() {
			child.contents.state = state.clone();
			child.propagate_closed();
		}
	}

	/// Post-update hook: call after any modification to an issue.
	/// Updates timestamps from diff and propagates closed state to children.
	pub fn post_update(&mut self, old: &Issue) {
		self.update_timestamps_from_diff(old);
		self.propagate_closed();
	}

	/// Get parent_index.
	pub fn parent_index(&self) -> IssueIndex {
		self.identity.parent_index
	}

	pub fn selector(&self) -> IssueSelector {
		match self.git_id() {
			Some(n) => IssueSelector::GitId(n),
			None => IssueSelector::title(&self.contents.title),
		}
	}

	/// Get full index pointing to this issue.
	/// Combines parent_index with this issue's selector.
	pub fn full_index(&self) -> IssueIndex {
		self.identity.parent_index.child(self.selector())
	}

	/// Get lineage (parent issue numbers).
	///
	/// # Errors
	/// Returns `TitleInGitPathError` if any parent selector is a Title (pending issue).
	pub fn lineage(&self) -> Result<Vec<u64>, super::error::TitleInGitPathError> {
		self.identity.git_lineage()
	}

	/// Get repository info.
	pub fn repo_info(&self) -> RepoInfo {
		self.identity.repo_info()
	}

	/// Get the full issue body including blockers section as events.
	/// This is what should be synced to Github as the issue body.
	pub fn body(&self) -> super::Events {
		//NB: DO NOT CHANGE Output Type
		let mut events: Vec<super::OwnedEvent> = self.contents.comments.first().map(|c| c.body.to_vec()).unwrap_or_default();
		if !self.contents.blockers.is_empty() {
			events.push(super::OwnedEvent::Start(super::OwnedTag::Heading {
				level: pulldown_cmark::HeadingLevel::H1,
				id: None,
				classes: Vec::new(),
				attrs: Vec::new(),
			}));
			events.push(super::OwnedEvent::Text("Blockers".to_string()));
			events.push(super::OwnedEvent::End(super::OwnedTagEnd::Heading(pulldown_cmark::HeadingLevel::H1)));
			events.extend(self.contents.blockers.to_events());
		}
		events.into()
	}

	/// Combine a HollowIssue (identity/metadata) with a VirtualIssue (parsed content) into a full Issue.
	///
	/// The hollow provides identity (remote info, timestamps) while virtual provides contents.
	/// `is_virtual` comes from project metadata - the hollow itself doesn't know.
	/// For children, recursively combines hollow.children with virtual.children by selector.
	///
	/// # Errors
	/// Returns `IssueError::ErroneousComposition` if a git-linked child (GitId selector) exists
	/// in virtual but not in hollow — either an internal bug or user manually embedded a linked marker.
	pub fn from_combined(hollow: HollowIssue, virtual_issue: VirtualIssue, parent_idx: IssueIndex, is_virtual: bool) -> Result<Self, super::error::IssueError> {
		let identity = IssueIdentity {
			parent_index: parent_idx,
			is_virtual,
			remote: hollow.remote.clone(),
		};

		// Recursively combine children
		let mut children = HashMap::new();
		for (selector, virtual_child) in virtual_issue.children {
			let child_hollow = match hollow.children.get(&selector) {
				Some(ch) => ch.clone(),
				None => {
					if let IssueSelector::GitId(n) = selector {
						return Err(super::error::IssueError::ErroneousComposition {
							issue_number: n,
							detail: "either internal bug (HollowIssue was constructed incorrectly) or user manually embedded a `<!-- @user url -->` marker, which is not permitted"
								.to_string(),
						});
					}
					HollowIssue::default()
				}
			};

			// Build child's parent_index
			let child_parent_idx = if let Some(meta) = &identity.remote {
				parent_idx.child(IssueSelector::GitId(meta.number()))
			} else {
				parent_idx
			};

			let child = Self::from_combined(child_hollow, virtual_child, child_parent_idx, is_virtual)?;
			children.insert(selector, child);
		}

		Ok(Issue {
			identity,
			contents: virtual_issue.contents,
			children,
		})
	}

	/// Parse `@user url#issuecomment-id` format into CommentIdentity.
	/// Returns Pending if parsing fails.
	fn parse_comment_identity(s: &str) -> CommentIdentity {
		let s = s.trim();

		// Format: `@username url#issuecomment-123`
		if let Some(rest) = s.strip_prefix('@')
			&& let Some(space_idx) = rest.find(' ')
		{
			let user = rest[..space_idx].to_string();
			let url = rest[space_idx + 1..].trim();
			if let Some(id) = url.split("#issuecomment-").nth(1).and_then(|s| s.parse().ok()) {
				return CommentIdentity::Created { user, id };
			}
		}

		CommentIdentity::Pending
	}

	//==========================================================================
	// Serialization Methods
	//==========================================================================

	/// Serialize for virtual file representation (human-readable, full tree).
	/// Creates a complete markdown file with all children recursively embedded.
	/// Used for temp files in /tmp where user views/edits the full issue tree.
	pub fn serialize_virtual(&self) -> String {
		self.serialize_virtual_at_depth(0, true)
	}

	/// Internal: serialize virtual representation using milestone-style rendering.
	///
	/// Renders title line via cmark as a standalone list item, then renders body/comments/blockers/children
	/// separately and indents them under the title. This matches how `milestone_embed::serialize_item` works,
	/// avoiding cmark's paragraph spacing that would insert blank lines between title and body.
	fn serialize_virtual_at_depth(&self, depth: usize, include_children: bool) -> String {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};

		if self.identity.is_virtual {
			assert!(self.user().is_none(), "virtual issue must not have a user tag, got: {:?}", self.user());
		}

		let content_indent = "  ".repeat(depth + 1);
		let mut out = String::new();

		// === Title line: render via cmark as standalone list item ===
		let checkbox_contents = self.contents.state.to_checkbox_contents();
		let issue_marker = IssueMarker::from(&self.identity);
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("({}) ", self.contents.labels.join(", "))
		};

		let title_str: String = super::Events::from(vec![
			OwnedEvent::Start(OwnedTag::List(None)),
			OwnedEvent::Start(OwnedTag::Item),
			OwnedEvent::CheckBox(checkbox_contents),
			OwnedEvent::Text(format!("{labels_part}{} ", self.contents.title)),
			OwnedEvent::InlineHtml(format!("<!-- {} -->", issue_marker.encode())),
			OwnedEvent::End(OwnedTagEnd::Item),
			OwnedEvent::End(OwnedTagEnd::List(false)),
		])
		.into();

		// Indent the title line to the correct depth
		let depth_indent = "  ".repeat(depth);
		for line in title_str.lines() {
			out.push_str(&depth_indent);
			out.push_str(line);
			out.push('\n');
		}

		let is_owned = self.identity.is_owned();

		// Helper: render content below title, indented under the item
		let mut content = String::new();

		// === Body (first comment) ===
		if let Some(body_comment) = self.contents.comments.first()
			&& !body_comment.body.is_empty()
		{
			let body_str: String = super::Events::from(body_comment.body.to_vec()).into();
			if !is_owned {
				// Extra indent for unowned issues
				indent_into(&mut content, &body_str, "  ");
			} else {
				content.push_str(&body_str);
			}
		}

		// === Additional comments ===
		for comment in self.contents.comments.iter().skip(1) {
			if self.identity.is_virtual {
				assert!(
					!comment.is_comment() || comment.user().is_none(),
					"virtual issue must not have linked comments, got: {:?}",
					comment.identity
				);
			}
			let comment_is_owned = comment.user().is_none() || comment.user().is_some_and(crate::current_user::is);

			// Blank line before comment separator
			if content.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				content.push('\n');
			}

			// Comment separator marker
			let marker_html = match &comment.identity {
				CommentIdentity::Body | CommentIdentity::Pending => Marker::NewComment.encode(),
				CommentIdentity::Created { user, id } => {
					let url = self.url_str().expect("remote must be initialized");
					format!("<!-- @{user} {url}#issuecomment-{id} -->")
				}
			};
			content.push_str(&marker_html);
			content.push('\n');

			// Comment body
			if !comment.body.is_empty() {
				let body_str: String = super::Events::from(comment.body.to_vec()).into();
				if !comment_is_owned {
					indent_into(&mut content, &body_str, "  ");
				} else {
					content.push_str(&body_str);
				}
			}
		}

		// === Blockers section ===
		if !self.contents.blockers.is_empty() {
			if content.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				content.push('\n');
			}
			let header = crate::Header::new(1, "Blockers");
			content.push_str(&header.encode());
			content.push('\n');
			let blockers_str: String = String::from(&self.contents.blockers);
			content.push_str(&blockers_str);
		}

		if !content.is_empty() {
			indent_into(&mut out, &content, &content_indent);
		}

		// === Children — sort: open first by selector, then closed by selector ===
		if include_children {
			let (mut open, mut closed): (Vec<_>, Vec<_>) = self.children.iter().partition(|(_, child)| !child.contents.state.is_closed());
			open.sort_by_key(|(sel, _)| *sel);
			closed.sort_by_key(|(sel, _)| *sel);
			let sorted_children = open.into_iter().chain(closed);

			for (_, child) in sorted_children {
				if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
					out.push_str(&content_indent);
					out.push('\n');
				}

				if child.contents.state.is_closed() {
					let child_str = child.serialize_virtual_at_depth(depth + 1, true);
					let omitted_start = Marker::OmittedStart.encode();
					let omitted_end = Marker::OmittedEnd.encode();
					let child_content_indent = "  ".repeat(depth + 2);

					let mut lines = child_str.lines();
					// First line is the title — inject omitted marker
					if let Some(title_line) = lines.next() {
						out.push_str(&format!("{title_line} {omitted_start}\n"));
					}
					// Remaining lines are body content
					for line in lines {
						out.push_str(line);
						out.push('\n');
					}
					// Vim fold end
					out.push_str(&child_content_indent);
					out.push_str(&omitted_end);
					out.push('\n');
				} else {
					let child_str = child.serialize_virtual_at_depth(depth + 1, true);
					out.push_str(&child_str);
				}
			}
		}

		out
	}

	/// Serialize for filesystem storage (single node, no children).
	/// Children are stored in separate files within the parent's directory.
	pub fn serialize_filesystem(&self) -> String {
		self.serialize_virtual_at_depth(0, false)
	}

	/// Serialize for GitHub API (markdown body only, no local markers).
	/// This is what gets sent to GitHub as the issue body.
	/// Always outputs markdown format regardless of local file extension.
	pub fn render_github(&self) -> super::Events {
		//NB: DO NOT CHANGE Output Type
		// GitHub body is: body text + blockers section (if any)
		// No title line, no URL markers, no comments - just the body content
		self.body()
	}

	/// parse_virtual but ignore
	///
	/// Find the position (line, col) of the last blocker item in the serialized content.
	/// Returns None if there are no blockers.
	/// Line numbers are 1-indexed to match editor conventions.
	/// Column points to the first character of the item text (after `- `).
	pub fn find_last_blocker_position(&self) -> Option<(u32, u32)> {
		if self.contents.blockers.is_empty() {
			return None;
		}

		// Serialize and find the last blocker item line
		let serialized = self.serialize_virtual();
		let lines: Vec<&str> = serialized.lines().collect();

		// Find where blockers section starts
		let blockers_header = crate::Header::new(1, "Blockers").encode();
		let blockers_start_idx = lines.iter().position(|line| line.trim() == blockers_header)?;

		// Track the last line that's a blocker item (starts with `- ` but not `- [` which is sub-issue)
		let mut last_item_line_num: Option<u32> = None;
		let mut last_item_col: Option<u32> = None;

		for (offset, line) in lines[blockers_start_idx + 1..].iter().enumerate() {
			let trimmed = line.trim();

			// Check if we've reached sub-issues (they start with `- [`)
			if trimmed.starts_with("- [") {
				break;
			}

			// A blocker item starts with `- ` (but not `- [`)
			if trimmed.starts_with("- ") {
				// Line number is 1-indexed
				let line_num = (blockers_start_idx + 1 + offset + 1) as u32;
				// Column: find where `- ` starts, then add 2 to skip past it
				let dash_pos = line.find("- ").unwrap_or(0);
				let col = (dash_pos + 3) as u32; // +2 for "- ", +1 for 1-indexing
				last_item_line_num = Some(line_num);
				last_item_col = Some(col);
			}
		}

		last_item_line_num.zip(last_item_col)
	}

	/// Get a reference to a descendant by lineage (chain of issue numbers).
	/// Returns None if the path doesn't exist.
	pub fn get(&self, lineage: &[u64]) -> Option<&Issue> {
		let mut current = self;
		for &num in lineage {
			current = current.children.get(&IssueSelector::GitId(num))?;
		}
		Some(current)
	}

	/// Get a mutable reference to a descendant by lineage.
	/// Returns None if the path doesn't exist.
	pub fn get_mut(&mut self, lineage: &[u64]) -> Option<&mut Issue> {
		let mut current = self;
		for &num in lineage {
			current = current.children.get_mut(&IssueSelector::GitId(num))?;
		}
		Some(current)
	}
}

#[derive(Clone, Debug, Default, PartialEq, derive_new::new)]
/// Hollow Issue container, - used for parsing virtual repr
///
/// Stripped of all info parsable from virtual
pub struct HollowIssue {
	pub remote: Option<Box<LinkedIssueMeta>>,
	pub children: IssueChildren<HollowIssue>,
}
impl From<Issue> for HollowIssue {
	fn from(value: Issue) -> Self {
		let mut children = HashMap::with_capacity(value.children.capacity());
		for (selector, child) in value.children.into_iter() {
			children.insert(selector, child.into());
		}
		HollowIssue {
			remote: value.identity.remote,
			children,
		}
	}
}

#[derive(Clone, Debug, PartialEq, derive_new::new)]
pub struct VirtualIssue {
	pub selector: IssueSelector,
	pub contents: IssueContents,
	pub children: IssueChildren<Self>,
}
impl VirtualIssue {
	/// Parse virtual representation (markdown with full tree) into a VirtualIssue.
	///
	/// Unlike `Issue::parse_virtual`, this doesn't need a `HollowIssue` - it purely parses content.
	/// Use `Issue::from_combined` to merge with identity info from a `HollowIssue`.
	pub fn parse(content: &str, path: PathBuf) -> Result<Self, ParseError> {
		let ctx = ParseContext::new(content.to_owned(), path);
		let events = super::Events::parse(content);
		Self::parse_from_events(&events, &ctx)
	}

	/// Parse a VirtualIssue from a cmark event stream.
	///
	/// Expects: `Start(List) > Start(Item) > ... > End(Item) > End(List)`
	/// The item contains: CheckBox, title text, InlineHtml marker, body events,
	/// comment markers (Html), blocker heading+list, child issue lists.
	fn parse_from_events(events: &[super::OwnedEvent], ctx: &ParseContext) -> Result<Self, ParseError> {
		Self::parse_from_events_inner(events, ctx, false)
	}

	fn parse_from_events_inner(events: &[super::OwnedEvent], ctx: &ParseContext, default_pending: bool) -> Result<Self, ParseError> {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};

		let mut pos = 0;

		// Skip to first Start(Item) inside the top-level List
		while pos < events.len() && !matches!(&events[pos], OwnedEvent::Start(OwnedTag::Item)) {
			pos += 1;
		}
		if pos >= events.len() {
			return Err(ParseError::empty_file());
		}
		pos += 1; // past Start(Item)

		// --- Parse title inline content: CheckBox, Text(labels+title), InlineHtml(marker) ---
		let close_state = match &events[pos] {
			OwnedEvent::CheckBox(inner) => {
				pos += 1;
				let needle = format!("[{inner}]");
				CloseState::from_checkbox(inner).map_err(|content| ParseError::invalid_checkbox(ctx.named_source(), ctx.find_line_span(&needle, 1), content))?
			}
			_ => return Err(ParseError::invalid_title(ctx.named_source(), ctx.line_span(1), "missing checkbox".into())),
		};

		// Collect text before the issue marker InlineHtml
		let mut title_text = String::new();
		while pos < events.len() {
			match &events[pos] {
				OwnedEvent::InlineHtml(_) => break,
				OwnedEvent::Text(t) => {
					title_text.push_str(t);
					pos += 1;
				}
				OwnedEvent::Code(c) => {
					title_text.push('`');
					title_text.push_str(c);
					title_text.push('`');
					pos += 1;
				}
				_ => break,
			}
		}

		// Parse the InlineHtml marker, or fall back to `!n` shorthand embedded in title text.
		// When `default_pending` is true (post-blocker checkbox items), missing markers
		// default to Pending instead of erroring.
		let identity_info = match &events[pos] {
			OwnedEvent::InlineHtml(html) => {
				let (marker, _) = IssueMarker::parse_from_end(&format!("x {html}")).ok_or_else(|| ParseError::missing_url_marker(ctx.named_source(), ctx.line_span(1)))?;
				pos += 1;
				marker
			}
			_ => {
				// `!n` shorthand: pulldown_cmark embeds it in the Text event (no separate InlineHtml)
				match IssueMarker::parse_from_end(&title_text) {
					Some((marker, rest)) => {
						title_text = rest.to_string();
						marker
					}
					None if default_pending => IssueMarker::Pending,
					None => return Err(ParseError::missing_url_marker(ctx.named_source(), ctx.line_span(1))),
				}
			}
		};

		// Skip inline omitted markers (and intervening whitespace) that follow the issue marker on the title line
		loop {
			match events.get(pos) {
				Some(OwnedEvent::InlineHtml(html)) => {
					let trimmed = html.trim();
					if (trimmed.starts_with("<!--omitted") && trimmed.contains("{{{")) || trimmed.starts_with("<!--,}}}") {
						pos += 1;
					} else {
						break;
					}
				}
				Some(OwnedEvent::Text(t)) if t.trim().is_empty() => {
					pos += 1; // whitespace between marker and omitted marker
				}
				_ => break,
			}
		}

		// Skip SoftBreak after title (title flows directly into body in tight items)
		if matches!(events.get(pos), Some(OwnedEvent::SoftBreak)) {
			pos += 1;
		}

		// Parse labels from title text
		let title_text = title_text.trim_end();
		let (labels, title) = if title_text.starts_with('(') {
			if let Some(paren_end) = title_text.find(") ") {
				let labels_str = &title_text[1..paren_end];
				let labels: Vec<String> = labels_str.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect();
				(labels, title_text[paren_end + 2..].to_string())
			} else {
				(vec![], title_text.to_string())
			}
		} else {
			(vec![], title_text.to_string())
		};

		let selector = identity_info.selector(&title);

		// --- Walk remaining events inside this Item to collect body, comments, blockers, children ---
		let mut comments: Vec<Comment> = Vec::new();
		let mut children = HashMap::new();
		let mut body_events: Vec<OwnedEvent> = Vec::new();
		let mut current_comment_events: Vec<OwnedEvent> = Vec::new();
		let mut current_comment_meta: Option<CommentIdentity> = None;
		let mut blocker_events: Vec<OwnedEvent> = Vec::new();
		let mut in_body = true;
		let mut in_blockers = false;
		let mut blocker_list_consumed = false;
		let mut select_blockers = false;

		// Helper: flush current body/comment events into the comments list.
		// Events are wrapped in paragraphs so they match the structure produced
		// by standalone parsing (e.g., remote body from GitHub).
		let flush = |in_body: &mut bool,
		             current_comment_meta: &mut Option<CommentIdentity>,
		             body_events: &mut Vec<OwnedEvent>,
		             current_comment_events: &mut Vec<OwnedEvent>,
		             comments: &mut Vec<Comment>| {
			if *in_body {
				*in_body = false;
				comments.push(Comment {
					identity: CommentIdentity::Body,
					body: wrap_inline_in_paragraphs(std::mem::take(body_events)).into(),
				});
			} else if let Some(identity) = current_comment_meta.take() {
				comments.push(Comment {
					identity,
					body: wrap_inline_in_paragraphs(std::mem::take(current_comment_events)).into(),
				});
			}
		};

		while pos < events.len() {
			match &events[pos] {
				// End of this item — we're done
				OwnedEvent::End(OwnedTagEnd::Item) => break,

				// Inline vim fold markers — skip (these appear on the title line)
				OwnedEvent::InlineHtml(html) if html.trim().starts_with("<!--omitted") && html.contains("{{{") => {
					pos += 1;
					continue;
				}
				OwnedEvent::InlineHtml(html) if html.trim().starts_with("<!--,}}}") => {
					pos += 1;
					continue;
				}

				// HtmlBlock wrappers — skip (the inner Html event is handled below)
				OwnedEvent::Start(OwnedTag::HtmlBlock) | OwnedEvent::End(OwnedTagEnd::HtmlBlock) => {
					pos += 1;
					continue;
				}

				// Html block: comment markers, omitted markers, or `!s`/`!c`
				OwnedEvent::Html(html) => {
					let trimmed = html.trim();

					// vim fold markers — skip
					if trimmed.starts_with("<!--omitted") && trimmed.contains("{{{") {
						pos += 1;
						continue;
					}
					if trimmed.starts_with("<!--,}}}") {
						pos += 1;
						continue;
					}

					// Comment separator: `<!-- @user url#issuecomment-id -->` or `<!-- new comment -->`
					if trimmed.starts_with("<!--") && trimmed.ends_with("-->") {
						let inner = trimmed.strip_prefix("<!--").unwrap().strip_suffix("-->").unwrap().trim();

						if inner == "new comment" || inner.eq_ignore_ascii_case("!c") {
							flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);
							current_comment_meta = Some(CommentIdentity::Pending);
							pos += 1;
							continue;
						}
						if inner.contains("#issuecomment-") {
							flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);
							current_comment_meta = Some(Issue::parse_comment_identity(inner));
							pos += 1;
							continue;
						}
					}

					// Fallthrough: treat as body/comment content
					if in_blockers {
						blocker_events.push(events[pos].clone());
					} else if in_body {
						body_events.push(events[pos].clone());
					} else if current_comment_meta.is_some() {
						current_comment_events.push(events[pos].clone());
					}
					pos += 1;
				}

				// "!s" or "!c" as standalone paragraph (loose items preserve paragraphs)
				OwnedEvent::Start(OwnedTag::Paragraph)
					if matches!(events.get(pos + 1), Some(OwnedEvent::Text(t)) if t.trim().eq_ignore_ascii_case("!s") || t.trim().eq_ignore_ascii_case("!c"))
						&& matches!(events.get(pos + 2), Some(OwnedEvent::End(OwnedTagEnd::Paragraph))) =>
				{
					let text = match &events[pos + 1] {
						OwnedEvent::Text(t) => t.trim().to_ascii_lowercase(),
						_ => unreachable!(),
					};
					if text == "!s" {
						select_blockers = true;
					} else {
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);
						current_comment_meta = Some(CommentIdentity::Pending);
					}
					pos += 3; // skip Start(Paragraph), Text, End(Paragraph)
				}

				// "!s" or "!c" as bare text (tight items have no paragraph wrappers)
				OwnedEvent::Text(t) if t.trim().eq_ignore_ascii_case("!s") || t.trim().eq_ignore_ascii_case("!c") => {
					// Remove trailing SoftBreak from accumulated events (normalization artifact)
					if in_body {
						if matches!(body_events.last(), Some(OwnedEvent::SoftBreak)) {
							body_events.pop();
						}
					} else if current_comment_meta.is_some() && matches!(current_comment_events.last(), Some(OwnedEvent::SoftBreak)) {
						current_comment_events.pop();
					}
					if t.trim().eq_ignore_ascii_case("!s") {
						select_blockers = true;
					} else {
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);
						current_comment_meta = Some(CommentIdentity::Pending);
					}
					pos += 1;
				}

				// Heading: check for "# Blockers" (with optional !s suffix)
				OwnedEvent::Start(OwnedTag::Heading {
					level: pulldown_cmark::HeadingLevel::H1,
					..
				}) => {
					// Collect heading text
					let heading_start = pos;
					pos += 1; // past Start(Heading)
					let mut heading_text = String::new();
					while pos < events.len() && !matches!(&events[pos], OwnedEvent::End(OwnedTagEnd::Heading(pulldown_cmark::HeadingLevel::H1))) {
						if let OwnedEvent::Text(t) = &events[pos] {
							heading_text.push_str(t);
						}
						pos += 1;
					}
					pos += 1; // past End(Heading)

					let heading_trimmed = heading_text.trim();
					let (effective, has_select_suffix) = match heading_trimmed.strip_suffix("!s").or_else(|| heading_trimmed.strip_suffix("!S")) {
						Some(before) => (before.trim(), true),
						None => (heading_trimmed, false),
					};

					if effective.eq_ignore_ascii_case("blockers") {
						if has_select_suffix {
							select_blockers = true;
						}
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);
						in_blockers = true;
					} else {
						// Not a blockers heading, treat as body/comment content
						let heading_events = &events[heading_start..pos];
						if in_body {
							body_events.extend(heading_events.iter().cloned());
						} else if current_comment_meta.is_some() {
							current_comment_events.extend(heading_events.iter().cloned());
						}
					}
				}

				// List: could be blockers list or child issues list
				OwnedEvent::Start(OwnedTag::List(_)) => {
					// Peek inside to determine what kind of list this is.
					// After blockers, any checkbox list terminates the blocker section
					// (split_blockers_from_checkboxes already separated checkbox items out).
					let has_checkbox = Self::list_has_checkbox(&events[pos..]);
					let is_child_list = has_checkbox;

					if is_child_list {
						// After blockers, checkbox items without markers auto-become Pending
						let children_default_pending = blocker_list_consumed;
						in_blockers = false;
						// Child issues list — parse each item as a child VirtualIssue
						// Collect the full list as a sub-slice and parse each Item
						let list_end = Self::find_matching_end_list(events, pos);
						let list_events = &events[pos..list_end];

						// Flush body/comment before children
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);

						// Walk items within this list
						let mut inner_pos = 1; // skip Start(List)
						while inner_pos < list_events.len() {
							if matches!(&list_events[inner_pos], OwnedEvent::Start(OwnedTag::Item)) {
								// Find the matching End(Item)
								let item_start = inner_pos;
								let item_end = Self::find_matching_end_item(list_events, inner_pos);

								// Wrap this item in a List for recursive parsing
								let mut child_events = vec![OwnedEvent::Start(OwnedTag::List(None))];
								child_events.extend(list_events[item_start..item_end].iter().cloned());
								child_events.push(OwnedEvent::End(OwnedTagEnd::List(false)));

								let child = Self::parse_from_events_inner(&child_events, ctx, children_default_pending)?;
								children.insert(child.selector, child);

								inner_pos = item_end;
							} else {
								inner_pos += 1;
							}
						}

						pos = list_end;
					} else if in_blockers {
						if blocker_list_consumed {
							// A non-checkbox list appeared after the blocker list was already consumed.
							// This means mixed content types after blockers — invalid.
							return Err(ParseError::invalid_composition(
								ctx.named_source(),
								ctx.line_span(1),
								"non-checkbox list after blockers section".into(),
							));
						}
						// Blocker list — collect events for BlockerSequence parsing
						let list_end = Self::find_matching_end_list(events, pos);
						blocker_events.extend(events[pos..list_end].iter().cloned());
						blocker_list_consumed = true;
						pos = list_end;
					} else {
						// Regular list in body/comment content
						let list_end = Self::find_matching_end_list(events, pos);
						let list_slice = &events[pos..list_end];
						if in_body {
							body_events.extend(list_slice.iter().cloned());
						} else if current_comment_meta.is_some() {
							current_comment_events.extend(list_slice.iter().cloned());
						}
						pos = list_end;
					}
				}

				// Any other event: body or comment content
				_ => {
					if in_blockers && blocker_list_consumed {
						// After the blocker list was consumed, non-list content means
						// invalid composition (only checkbox lists are allowed after blockers).
						// SoftBreak / Html("\n") between blocks are harmless — skip them.
						match &events[pos] {
							OwnedEvent::SoftBreak | OwnedEvent::HardBreak => {
								pos += 1;
								continue;
							}
							OwnedEvent::Html(h) if h.trim().is_empty() => {
								pos += 1;
								continue;
							}
							OwnedEvent::Text(t) if t.trim().is_empty() => {
								pos += 1;
								continue;
							}
							_ => {
								return Err(ParseError::invalid_composition(
									ctx.named_source(),
									ctx.line_span(1),
									format!("unexpected content after blockers section: {:?}", &events[pos]),
								));
							}
						}
					} else if in_blockers {
						blocker_events.push(events[pos].clone());
					} else if in_body {
						body_events.push(events[pos].clone());
					} else if current_comment_meta.is_some() {
						current_comment_events.push(events[pos].clone());
					}
					pos += 1;
				}
			}
		}

		// Flush final body/comment
		flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comments);

		// Parse blockers from collected events
		let blockers = if blocker_events.is_empty() {
			BlockerSequence::default()
		} else {
			// Render blocker events to string and parse with BlockerSequence::parse
			let blocker_text: String = super::Events::from(blocker_events).into();
			BlockerSequence::parse(&blocker_text)
		};

		Ok(VirtualIssue {
			selector,
			contents: IssueContents {
				title,
				labels,
				state: close_state,
				comments: comments.into(),
				blockers: {
					let mut seq = blockers;
					if select_blockers {
						seq.set_state = Some(crate::issue::BlockerSetState::Pending);
					}
					seq
				},
			},
			children,
		})
	}

	/// Check if a list (starting at `Start(List(...))`) contains any CheckBox items.
	fn list_has_checkbox(events: &[super::OwnedEvent]) -> bool {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};
		let mut depth = 0;
		for ev in events {
			match ev {
				OwnedEvent::Start(OwnedTag::List(_)) => depth += 1,
				OwnedEvent::End(OwnedTagEnd::List(_)) => {
					depth -= 1;
					if depth == 0 {
						break;
					}
				}
				OwnedEvent::CheckBox(_) if depth == 1 => return true,
				_ => {}
			}
		}
		false
	}

	/// Find the position after the matching `End(List)` for a `Start(List)` at `start`.
	fn find_matching_end_list(events: &[super::OwnedEvent], start: usize) -> usize {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};
		let mut depth = 0;
		for (i, event) in events.iter().enumerate().skip(start) {
			match event {
				OwnedEvent::Start(OwnedTag::List(_)) => depth += 1,
				OwnedEvent::End(OwnedTagEnd::List(_)) => {
					depth -= 1;
					if depth == 0 {
						return i + 1;
					}
				}
				_ => {}
			}
		}
		events.len()
	}

	/// Find the position after the matching `End(Item)` for a `Start(Item)` at `start`.
	fn find_matching_end_item(events: &[super::OwnedEvent], start: usize) -> usize {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};
		let mut depth = 0;
		for (i, event) in events.iter().enumerate().skip(start) {
			match event {
				OwnedEvent::Start(OwnedTag::Item) => depth += 1,
				OwnedEvent::End(OwnedTagEnd::Item) => {
					depth -= 1;
					if depth == 0 {
						return i + 1;
					}
				}
				_ => {}
			}
		}
		events.len()
	}
}

impl From<Issue> for VirtualIssue {
	fn from(issue: Issue) -> Self {
		let selector = issue.selector();
		let children = issue.children.into_iter().map(|(sel, child)| (sel, child.into())).collect();
		Self {
			selector,
			contents: issue.contents,
			children,
		}
	}
}

//,}}}1

impl PartialEq for IssueIdentity {
	fn eq(&self, other: &IssueIdentity) -> bool {
		self.parent_index == other.parent_index
	}
}

//,}}}1

//==============================================================================
// Index by issue number
//==============================================================================

// PERF: Linear search through children for each index operation.
// We sacrifice some performance for determinism - the tree structure
// is navigated by issue numbers rather than positional indices.

/// Implement Index<u64> and IndexMut<u64> for types with `children: IssueChildren<Self>`.
macro_rules! impl_issue_tree_index {
	($ty:ty) => {
		impl std::ops::Index<u64> for $ty {
			type Output = $ty;

			/// Index into children by issue number.
			/// Panics if no child with that number exists.
			fn index(&self, issue_number: u64) -> &Self::Output {
				self.children
					.get(&IssueSelector::GitId(issue_number))
					.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
			}
		}

		impl std::ops::IndexMut<u64> for $ty {
			/// Index into children by issue number (mutable).
			/// Panics if no child with that number exists.
			fn index_mut(&mut self, issue_number: u64) -> &mut Self::Output {
				self.children
					.get_mut(&IssueSelector::GitId(issue_number))
					.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
			}
		}

		impl std::ops::Index<IssueSelector> for $ty {
			type Output = $ty;

			/// Index into children by selector.
			/// Panics if no child with that selector exists.
			fn index(&self, selector: IssueSelector) -> &Self::Output {
				self.children.get(&selector).unwrap_or_else(|| panic!("no child with selector {selector:?}"))
			}
		}

		impl std::ops::IndexMut<IssueSelector> for $ty {
			/// Index into children by selector (mutable).
			/// Panics if no child with that selector exists.
			fn index_mut(&mut self, selector: IssueSelector) -> &mut Self::Output {
				self.children.get_mut(&selector).unwrap_or_else(|| panic!("no child with selector {selector:?}"))
			}
		}
	};
}

impl_issue_tree_index!(Issue);
impl_issue_tree_index!(HollowIssue);
impl_issue_tree_index!(VirtualIssue);

#[cfg(test)]
mod tests {
	use super::*;

	fn unsafe_mock_parse_virtual(content: &str) -> Issue {
		let virtual_issue = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		let hollow = hollow_from_virtual(&virtual_issue);
		let parent_idx = IssueIndex::repo_only(("owner", "repo").into());
		Issue::from_combined(hollow, virtual_issue, parent_idx, false).unwrap()
	}

	/// needed to not fall for checks for existence of GitId-linked issue on the base structure (useful in the real world; but our tests here really only care about parsing itself.
	fn hollow_from_virtual(v: &VirtualIssue) -> HollowIssue {
		let remote = match &v.selector {
			IssueSelector::GitId(n) => {
				let link = IssueLink::parse(&format!("https://github.com/owner/repo/issues/{n}")).unwrap();
				Some(Box::new(LinkedIssueMeta::new(None, link, IssueTimestamps::default())))
			}
			IssueSelector::Title(_) | IssueSelector::Regex(_) => None,
		};
		let children = v.children.iter().map(|(sel, child)| (*sel, hollow_from_virtual(child))).collect();
		HollowIssue::new(remote, children)
	}

	#[test]
	fn test_close_state_from_checkbox() {
		let cases = [" ", "", "x", "X", "-", "123", "42", "invalid"];
		let results: Vec<_> = cases.iter().map(|c| format!("{c:?} => {:?}", CloseState::from_checkbox(c))).collect();
		insta::assert_snapshot!(results.join("\n"), @r#"
		" " => Ok(Open)
		"" => Ok(Open)
		"x" => Ok(Closed)
		"X" => Ok(Closed)
		"-" => Ok(NotPlanned)
		"123" => Ok(Duplicate(123))
		"42" => Ok(Duplicate(42))
		"invalid" => Err("invalid")
		"#);
	}

	#[test]
	fn test_close_state_to_checkbox() {
		let cases = [CloseState::Open, CloseState::Closed, CloseState::NotPlanned, CloseState::Duplicate(123)];
		let results: Vec<_> = cases.iter().map(|s| format!("{s:?} => {:?}", s.to_checkbox_contents())).collect();
		insta::assert_snapshot!(results.join("\n"), @r#"
		Open => " "
		Closed => "x"
		NotPlanned => "-"
		Duplicate(123) => "123"
		"#);
	}

	#[test]
	fn test_close_state_is_closed() {
		let cases = [CloseState::Open, CloseState::Closed, CloseState::NotPlanned, CloseState::Duplicate(123)];
		let results: Vec<_> = cases.iter().map(|s| format!("{s:?} => {}", s.is_closed())).collect();
		insta::assert_snapshot!(results.join("\n"), @"
		Open => false
		Closed => true
		NotPlanned => true
		Duplicate(123) => true
		");
	}

	#[test]
	fn test_close_state_should_remove() {
		let cases = [CloseState::Open, CloseState::Closed, CloseState::NotPlanned, CloseState::Duplicate(123)];
		let results: Vec<_> = cases.iter().map(|s| format!("{s:?} => {}", s.should_remove())).collect();
		insta::assert_snapshot!(results.join("\n"), @"
		Open => false
		Closed => false
		NotPlanned => false
		Duplicate(123) => true
		");
	}

	#[test]
	fn test_close_state_to_github_state() {
		let cases = [CloseState::Open, CloseState::Closed, CloseState::NotPlanned, CloseState::Duplicate(123)];
		let results: Vec<_> = cases.iter().map(|s| format!("{s:?} => {}", s.to_github_state())).collect();
		insta::assert_snapshot!(results.join("\n"), @"
		Open => open
		Closed => closed
		NotPlanned => closed
		Duplicate(123) => closed
		");
	}

	#[test]
	fn test_parse_invalid_checkbox_returns_error() {
		let root_err = VirtualIssue::parse("- [abc] Invalid issue <!-- https://github.com/owner/repo/issues/123 -->\n\n  Body\n", PathBuf::from("test.md")).unwrap_err();
		let sub_err = VirtualIssue::parse(
			"- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [xyz] Bad sub <!--sub https://github.com/owner/repo/issues/2 -->\n",
			PathBuf::from("test.md"),
		)
		.unwrap_err();
		insta::assert_snapshot!(format!("root: {root_err}\nsub: {sub_err}"), @r#"
		root: tedi::parse::invalid_checkbox

		  × invalid checkbox content: 'abc'
		   ╭─[test.md:1:1]
		 1 │ - [abc] Invalid issue <!-- https://github.com/owner/repo/issues/123 -->
		   · ───────────────────────────────────┬───────────────────────────────────
		   ·                                    ╰── unrecognized checkbox value
		 2 │ 
		   ╰────
		  help: valid checkbox values are: ' ' (open), 'x' (closed), '-' (not
		        planned), or a number like '123' (duplicate of issue #123)



		sub: tedi::parse::invalid_checkbox

		  × invalid checkbox content: 'xyz'
		   ╭─[test.md:5:1]
		 4 │ 
		 5 │   - [xyz] Bad sub <!--sub https://github.com/owner/repo/issues/2 -->
		   · ──────────────────────────────────┬─────────────────────────────────
		   ·                                   ╰── unrecognized checkbox value
		   ╰────
		  help: valid checkbox values are: ' ' (open), 'x' (closed), '-' (not
		        planned), or a number like '123' (duplicate of issue #123)
		"#);
	}

	#[test]
	fn test_parse_and_serialize_not_planned() {
		let content = "- [-] Not planned issue <!-- https://github.com/owner/repo/issues/123 -->\n\n  Body text\n";
		let vi = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		insta::assert_snapshot!(format!("state: {:?}\ntitle: {}", vi.contents.state, vi.contents.title), @"
		state: NotPlanned
		title: Not planned issue
		");
	}

	#[test]
	fn test_parse_and_serialize_duplicate() {
		let content = "- [456] Duplicate issue <!-- https://github.com/owner/repo/issues/123 -->\n\n  Body text\n";
		let vi = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		insta::assert_snapshot!(format!("state: {:?}\ntitle: {}", vi.contents.state, vi.contents.title), @"
		state: Duplicate(456)
		title: Duplicate issue
		");
	}

	#[test]
	fn test_parse_sub_issue_close_types() {
		let content = "- [ ] Parent issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [x] Closed sub <!-- https://github.com/owner/repo/issues/2 --> <!--omitted {{{always-->\n\n    closed body\n    <!--,}}}-->\n\n  - [-] Not planned sub <!-- https://github.com/owner/repo/issues/3 --> <!--omitted {{{always-->\n\n    not planned body\n    <!--,}}}-->\n\n  - [42] Duplicate sub <!-- https://github.com/owner/repo/issues/4 --> <!--omitted {{{always-->\n\n    duplicate body\n    <!--,}}}-->\n";
		let issue = unsafe_mock_parse_virtual(content);
		// snapshot has trailing spaces on lines between closed children
		insta::assert_snapshot!(issue.serialize_virtual(), @r"
		- [ ] Parent issue <!-- https://github.com/owner/repo/issues/1 -->
		  Body
		  
		  - [x] Closed sub <!-- https://github.com/owner/repo/issues/2 --> <!--omitted {{{always-->
		    closed body
		    <!--,}}}-->
		  
		  - \[-] Not planned sub <!-- https://github.com/owner/repo/issues/3 --> <!--omitted {{{always-->
		    not planned body
		    <!--,}}}-->
		  
		  - \[42] Duplicate sub <!-- https://github.com/owner/repo/issues/4 --> <!--omitted {{{always-->
		    duplicate body
		    <!--,}}}-->
		");
	}

	#[test]
	fn test_find_last_blocker_position_empty() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n";
		let issue = unsafe_mock_parse_virtual(content);
		assert!(issue.find_last_blocker_position().is_none());
	}

	#[test]
	fn test_find_last_blocker_position_single_item() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - task 1\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.find_last_blocker_position()), @"Some((4, 5))");
	}

	#[test]
	fn test_find_last_blocker_position_multiple_items() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - task 1\n  - task 2\n  - task 3\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.find_last_blocker_position()), @"Some((6, 5))");
	}

	#[test]
	fn test_find_last_blocker_position_with_nesting() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - Phase 1\n    - task a\n  - Phase 2\n    - task b\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.find_last_blocker_position()), @"Some((7, 7))");
	}

	#[test]
	fn test_find_last_blocker_position_before_sub_issues() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - blocker task\n\n  - [ ] Sub issue <!--sub https://github.com/owner/repo/issues/2 -->\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.find_last_blocker_position()), @"Some((4, 5))");
	}

	#[test]
	fn test_serialize_filesystem_no_children() {
		let content = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child 1 <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child 1 body\n\n  - [ ] Child 2 <!--sub https://github.com/owner/repo/issues/3 -->\n\n    Child 2 body\n";
		let issue = unsafe_mock_parse_virtual(content);
		assert_eq!(issue.children.len(), 2);
		// filesystem serialization should NOT include children
		insta::assert_snapshot!(issue.serialize_filesystem(), @"
		- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->
		  Parent body
		");
	}

	#[test]
	fn test_serialize_filesystem_roundtrip_blocker_escaping() {
		let cases = vec![
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - `insert` semantics on `RoutingHub`\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - `insert`semantics on`RoutingHub`\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - move clap interface into \\_strategy\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - some certainty\\*val range\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - text with ` lone backtick\n",
		];

		for initial_content in cases {
			let issue = unsafe_mock_parse_virtual(initial_content);
			let s1 = issue.serialize_filesystem();
			for cycle in 1..=5 {
				let re = unsafe_mock_parse_virtual(&s1);
				let sn = re.serialize_filesystem();
				assert_eq!(s1, sn, "serialize_filesystem not idempotent at cycle {cycle} for input: {initial_content:?}");
			}
		}
	}

	#[test]
	fn test_serialize_virtual_includes_children() {
		let content =
			"- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child 1 <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child 1 body\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(issue.serialize_virtual(), @"
		- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->
		  Parent body
		  
		  - [ ] Child 1 <!-- https://github.com/owner/repo/issues/2 -->
		    Child 1 body
		");
	}

	#[test]
	fn test_parse_virtual_includes_inline_children() {
		let content = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let vi = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		insta::assert_snapshot!(
			format!("children: {}\nparent: {}\nchild: {}", vi.children.len(), vi.contents.title, vi[2].contents.title),
			@"
		children: 1
		parent: Parent
		child: Child
		"
		);
	}

	#[test]
	fn test_virtual_roundtrip() {
		let content = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let issue = unsafe_mock_parse_virtual(content);
		let serialized = issue.serialize_virtual();
		let reparsed = unsafe_mock_parse_virtual(&serialized);
		insta::assert_snapshot!(
			format!("title: {}\nchildren: {}", reparsed.contents.title, reparsed.children.len()),
			@"
		title: Parent
		children: 1
		"
		);
	}

	#[test]
	fn test_select_blockers_standalone_line() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  !s\n  # Blockers\n  - task 1\n  - task 2\n";
		let vi = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		assert!(matches!(vi.contents.blockers.set_state, Some(crate::issue::BlockerSetState::Pending)));
		// !s must not appear in re-serialized output, blockers preserved
		insta::assert_snapshot!(unsafe_mock_parse_virtual(content).serialize_virtual(), @"
		- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->
		  Body
		  # Blockers
		  - task 1
		  - task 2
		");
	}

	#[test]
	fn test_select_blockers_suffix_on_header() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers !s\n  - one\n";
		let vi = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();
		assert!(matches!(vi.contents.blockers.set_state, Some(crate::issue::BlockerSetState::Pending)));
		// blocker text parsed correctly despite !s suffix on header
		insta::assert_snapshot!(vi.contents.blockers.items[0].text, @"one");
	}

	#[test]
	fn test_select_blockers_not_set_by_default() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - task\n";
		let virtual_issue = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();

		assert!(virtual_issue.contents.blockers.set_state.is_none());
	}

	#[test]
	fn test_select_blockers_case_insensitive() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  !S\n  # Blockers\n  - task\n";
		let virtual_issue = VirtualIssue::parse(content, PathBuf::from("test.md")).unwrap();

		assert!(matches!(virtual_issue.contents.blockers.set_state, Some(crate::issue::BlockerSetState::Pending)));
	}

	#[test]
	fn test_serialize_github_body_only() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  This is the body text.\n\n  # Blockers\n  - task 1\n  - task 2\n";
		let issue = unsafe_mock_parse_virtual(content);
		let github_body: String = issue.render_github().into();
		insta::assert_snapshot!(github_body, @"
		This is the body text.
		# Blockers
		- task 1
		- task 2
		");
	}

	#[test]
	fn test_parse_virtual_child_open_to_closed() {
		let initial = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let updated = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [x] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let initial_vi = VirtualIssue::parse(initial, PathBuf::from("test.md")).unwrap();
		assert_eq!(initial_vi[2].contents.state, CloseState::Open);
		let updated_vi = VirtualIssue::parse(updated, PathBuf::from("test.md")).unwrap();
		// child should transition to Closed
		insta::assert_snapshot!(format!("{:?}", updated_vi[2].contents.state), @"Closed");
	}

	#[test]
	fn test_parse_virtual_child_open_to_not_planned() {
		let initial = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let updated = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [-] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let initial_vi = VirtualIssue::parse(initial, PathBuf::from("test.md")).unwrap();
		assert_eq!(initial_vi[2].contents.state, CloseState::Open);
		let updated_vi = VirtualIssue::parse(updated, PathBuf::from("test.md")).unwrap();
		// child should transition to NotPlanned
		insta::assert_snapshot!(format!("{:?}", updated_vi[2].contents.state), @"NotPlanned");
	}

	#[test]
	fn test_parse_virtual_child_open_to_duplicate() {
		let initial = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let updated = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [99] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let initial_vi = VirtualIssue::parse(initial, PathBuf::from("test.md")).unwrap();
		assert_eq!(initial_vi[2].contents.state, CloseState::Open);
		let updated_vi = VirtualIssue::parse(updated, PathBuf::from("test.md")).unwrap();
		// child should transition to Duplicate(99)
		insta::assert_snapshot!(format!("{:?}", updated_vi[2].contents.state), @"Duplicate(99)");
	}

	#[test]
	fn test_parse_virtual_child_closed_to_open() {
		let initial = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [x] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let updated = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [ ] Child <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child body\n";
		let initial_vi = VirtualIssue::parse(initial, PathBuf::from("test.md")).unwrap();
		assert_eq!(initial_vi[2].contents.state, CloseState::Closed);
		let updated_vi = VirtualIssue::parse(updated, PathBuf::from("test.md")).unwrap();
		// child should transition to Open
		insta::assert_snapshot!(format!("{:?}", updated_vi[2].contents.state), @"Open");
	}

	#[test]
	fn test_serialize_roundtrip_idempotent() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Test Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  original body\n";
		let issue = unsafe_mock_parse_virtual(input);
		let s1 = issue.serialize_virtual();
		let issue2 = unsafe_mock_parse_virtual(&s1);
		let s2 = issue2.serialize_virtual();
		assert_eq!(s1, s2, "serialize_virtual must be idempotent");
	}

	#[test]
	fn test_serialize_roundtrip_custom_checkboxes_idempotent() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  Body\n\n  - [-] Not planned <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n    np body\n\n  - [42] Duplicate <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n    dup body\n";
		let issue = unsafe_mock_parse_virtual(input);
		let s1 = issue.serialize_virtual();
		for cycle in 1..=5 {
			let re = unsafe_mock_parse_virtual(&s1);
			let sn = re.serialize_virtual();
			assert_eq!(s1, sn, "serialize_virtual must be idempotent at cycle {cycle}");
		}
	}

	#[test]
	fn test_parse_nested_subissues() {
		let input = "- [ ] Grandparent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  grandparent body\n\n  - [ ] Parent <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n    original parent body\n\n    - [ ] Child <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n      child body\n";
		let vi = VirtualIssue::parse(input, PathBuf::from("test.md")).unwrap();
		eprintln!("parsed: title={}, children={}", vi.contents.title, vi.children.len());
	}

	#[test]
	fn test_parse_blockers_and_child_at_same_indent() {
		let input = "- [ ] Parent <!-- https://github.com/owner/repo/issues/2 -->\n  \n  body\n  \n  # Blockers\n  - local blocker\n  \n  - [ ] Child <!--sub https://github.com/owner/repo/issues/3 -->\n    \n    child body\n";
		let vi = VirtualIssue::parse(input, PathBuf::from("test.md")).unwrap();
		assert_eq!(vi.contents.blockers.items.len(), 1, "should have 1 blocker");
		assert_eq!(vi.children.len(), 1, "should have 1 child");
	}

	#[test]
	fn test_blocker_section_does_not_absorb_checkbox_items() {
		// Checkbox list items after blockers must NOT be treated as blockers.
		// They are separate child issues (with implicit Pending marker).
		let input = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  body\n\n  # Blockers\n  - vector series\n    - all the others\n    - ex 10\n\n  - [ ] series\n    up to and including ex 8\n";
		let vi = VirtualIssue::parse(input, PathBuf::from("test.md")).unwrap();
		assert_eq!(vi.contents.blockers.items.len(), 1, "should have 1 blocker (vector series)");
		assert_eq!(vi.contents.blockers.items[0].text, "vector series");
		assert_eq!(vi.contents.blockers.items[0].children.len(), 2, "vector series has 2 children");
		assert_eq!(vi.children.len(), 1, "should have 1 child issue (series)");
		let child = vi.children.values().next().unwrap();
		assert_eq!(child.contents.title, "series");
	}

	#[test]
	fn test_blocker_section_terminates_on_empty_line_before_checkbox() {
		// Even with an empty line between blockers and checkbox items,
		// the checkbox items must NOT be absorbed into blockers.
		let input = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  # Blockers\n  - task A\n  - task B\n\n  - [ ] new child\n    child body\n";
		let vi = VirtualIssue::parse(input, PathBuf::from("test.md")).unwrap();
		assert_eq!(vi.contents.blockers.items.len(), 2, "should have 2 blockers");
		assert_eq!(vi.children.len(), 1, "checkbox item after empty line becomes child");
		let child = vi.children.values().next().unwrap();
		assert_eq!(child.contents.title, "new child");
	}
}
