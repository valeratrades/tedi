//! Core issue data structures and parsing/serialization.
//!
//! This module contains the pure Issue type with parsing and serialization.

use std::{collections::HashMap, path::PathBuf, str::FromStr as _};

use arrayvec::ArrayString;
use copy_arrayvec::CopyArrayVec;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use url::Url;

/// Maximum title length enforced by Github.
pub const MAX_TITLE_LENGTH: usize = 256;

/// Maximum index depth (lineage + the issue itself).
pub const MAX_INDEX_DEPTH: usize = MAX_LINEAGE_DEPTH + 1;
pub const MAX_LINEAGE_DEPTH: usize = 8;
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
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CommentIdentity {
	/// This is the issue body (first comment), not a separate Github comment
	Body,
	/// Comment exists on Github with this ID, created by given user
	Created { user: String, id: u64 },
	/// Comment is pending creation on Github (will be created in post-sync)
	Pending,
}

impl CommentIdentity /*{{{1*/ {
	/// Get the comment ID if linked.
	pub fn id(&self) -> Option<u64> {
		match self {
			Self::Created { id, .. } => Some(*id),
			Self::Body | Self::Pending => None,
		}
	}

	/// Get the user who created this comment if linked.
	pub fn user(&self) -> Option<&str> {
		match self {
			Self::Created { user, .. } => Some(user),
			Self::Body | Self::Pending => None,
		}
	}

	/// Check if this is a Github comment (not the issue body).
	pub fn is_comment(&self) -> bool {
		!matches!(self, Self::Body)
	}

	/// Check if this comment is pending creation.
	pub fn is_pending(&self) -> bool {
		matches!(self, Self::Pending)
	}
}
//,}}}1

use super::{
	IssueMarker, Marker,
	blocker::{BlockerSequence, classify_line, join_with_blockers},
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
	pub fn from_checkbox(content: &str) -> Option<Self> {
		let content = content.trim();
		match content {
			"" | " " => Some(CloseState::Open),
			"x" | "X" => Some(CloseState::Closed),
			"-" => Some(CloseState::NotPlanned),
			s => s.parse::<u64>().ok().map(CloseState::Duplicate),
		}
	}

	/// Convert to checkbox character(s) for serialization
	pub fn to_checkbox(&self) -> String {
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
	/// Update timestamps based on what changed between old and new issue contents.
	/// Sets the current time for any field that changed.
	pub fn update_from_diff(&mut self, old: &super::IssueContents, new: &super::IssueContents) {
		let now = Timestamp::now();

		if old.title != new.title {
			self.title = Some(now);
		}

		// Compare body (first comment) and blockers for description changes
		let old_body = old.comments.first().map(|c| c.body.render()).unwrap_or_default();
		let new_body = new.comments.first().map(|c| c.body.render()).unwrap_or_default();
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
			if old_c.body.render() != new_c.body.render() {
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
	/// User who created the issue
	pub user: String,
	/// Link to the issue on Github
	pub link: IssueLink,
	/// Timestamps of last changes to individual fields.
	/// Used for sync conflict resolution.
	pub timestamps: IssueTimestamps,
}
impl LinkedIssueMeta {
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

/// Remote connection status for an issue.
///
/// Distinguishes between:
/// - Github issues (linked or pending creation)
/// - Virtual issues (local-only, never synced to Github)
#[derive(Clone, Debug, PartialEq)]
pub enum IssueRemote {
	/// Issue is (or will be) on Github.
	/// `None` = pending creation (will be created on first sync)
	/// `Some(_)` = already linked to Github
	Github(Box<Option<LinkedIssueMeta>>),
	/// Virtual issue - local only, never synced to Github.
	/// Used for projects without a Github remote.
	Virtual,
}
impl IssueRemote {
	/// Returns true if this is a virtual (local-only) issue.
	pub fn is_virtual(&self) -> bool {
		matches!(self, IssueRemote::Virtual)
	}

	/// Returns true if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		matches!(self, IssueRemote::Github(inner) if inner.is_some())
	}

	/// Returns true if this is a pending Github issue (not yet created).
	pub fn is_pending(&self) -> bool {
		matches!(self, IssueRemote::Github(inner) if inner.is_none())
	}

	/// Get the linked metadata if this is a linked Github issue.
	pub fn as_linked(&self) -> Option<&LinkedIssueMeta> {
		match self {
			IssueRemote::Github(inner) => (**inner).as_ref(),
			_ => None,
		}
	}

	/// Get mutable access to the linked metadata.
	pub fn as_linked_mut(&mut self) -> Option<&mut LinkedIssueMeta> {
		match self {
			IssueRemote::Github(inner) => (**inner).as_mut(),
			_ => None,
		}
	}
}
impl Default for IssueRemote {
	fn default() -> Self {
		Self::Github(Box::new(None))
	}
}

/// Identity of an issue - has optional parent_index (for location) with remote connection info.
#[derive(Clone, Debug)]
pub struct IssueIdentity {
	/// Parent's IssueIndex. For root it's the default variant with null ancestry (but owner+repo specified)
	/// NB: it's index of the PARENT, not ourselves. Done this way to allow for eg title changes.
	pub parent_index: IssueIndex,
	/// Remote connection status
	remote: IssueRemote,
}
impl IssueIdentity {
	/// Create a new linked Github issue identity.
	/// If `parent_index` is None, derives repo_only from the link.
	pub fn new_linked(parent_index: Option<IssueIndex>, user: String, link: IssueLink, timestamps: IssueTimestamps) -> Self {
		let parent_index = parent_index.unwrap_or_else(|| {
			let repo_info = link.repo_info();
			IssueIndex::repo_only(repo_info)
		});
		Self {
			parent_index,
			remote: IssueRemote::Github(Box::new(Some(LinkedIssueMeta { user, link, timestamps }))),
		}
	}

	/// Create a new pending Github issue identity (will be created on first sync).
	pub fn pending(parent_index: IssueIndex) -> Self {
		Self {
			parent_index,
			remote: IssueRemote::Github(Box::new(None)),
		}
	}

	/// Create a new virtual (local-only) issue identity.
	pub fn virtual_issue(parent_index: IssueIndex) -> Self {
		Self {
			parent_index,
			remote: IssueRemote::Virtual,
		}
	}

	/// Check if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		self.remote.is_linked()
	}

	/// Check if this issue is local only (pending creation on Github).
	/// Note: Virtual issues are also local-only but should be checked via is_virtual().
	pub fn is_local(&self) -> bool {
		self.remote.is_pending()
	}

	/// Check if this is a virtual (local-only, never-sync) issue.
	pub fn is_virtual(&self) -> bool {
		self.remote.is_virtual()
	}

	/// Get the linked metadata if linked.
	pub fn as_linked(&self) -> Option<&LinkedIssueMeta> {
		self.remote.as_linked()
	}

	/// Get mutable access to the linked metadata.
	pub fn mut_linked_issue_meta(&mut self) -> Option<&mut LinkedIssueMeta> {
		self.remote.as_linked_mut()
	}

	/// Get the issue link if linked.
	pub fn link(&self) -> Option<&IssueLink> {
		self.remote.as_linked().map(|m| &m.link)
	}

	/// Get the issue number if linked.
	pub fn number(&self) -> Option<u64> {
		self.link().map(|l| l.number())
	}

	/// Get the URL string if linked.
	pub fn url_str(&self) -> Option<&str> {
		self.link().map(|l| l.as_str())
	}

	/// Get the user who created this issue if linked.
	pub fn user(&self) -> Option<&str> {
		self.remote.as_linked().map(|m| m.user.as_str())
	}

	/// Get the timestamps if linked.
	pub fn timestamps(&self) -> Option<&IssueTimestamps> {
		self.remote.as_linked().map(|m| &m.timestamps)
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
#[derive(Clone, Debug, PartialEq)]
pub struct Comment {
	/// Comment identity - body, linked to Github, or pending creation
	pub identity: CommentIdentity,
	/// The markdown body stored as parsed events for lossless roundtripping
	pub body: super::Events,
}
/// The full editable content of an issue.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct IssueContents {
	pub title: String,
	pub labels: Vec<String>,
	pub state: CloseState,
	pub comments: Vec<Comment>,
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
	/// Used for comparison when an issue doesn't exist yet.
	///
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
	pub fn update_timestamps_from_diff(&mut self, old: &Issue) {
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

	/// Get the full issue body including blockers section.
	/// This is what should be synced to Github as the issue body.
	pub fn body(&self) -> String {
		let base_body = self.contents.comments.first().map(|c| c.body.render()).unwrap_or_default();
		join_with_blockers(&base_body, &self.contents.blockers)
	}

	/// Parse virtual representation (markdown with full tree) into an Issue.
	///
	/// Takes the full `IssueIndex` pointing to this issue (not the parent). Need it to derive the parent_index.
	//TODO: switch to `content: String`, once we don't need to clone it for passing to `parse_virtual_at_depth` twice
	pub fn parse_virtual(content: &str, hollow: HollowIssue, parent_idx: IssueIndex, path: PathBuf) -> Result<Self, ParseError> {
		let ctx = ParseContext::new(content.to_owned(), path); //HACK
		let mut lines = content.lines().peekable();
		Self::parse_virtual_at_depth(&mut lines, 0, 1, &ctx, parent_idx, hollow)
	}

	/// Parse virtual representation at given nesting depth.
	/// `parent_index` is the parent's IssueIndex (for non-root), or None for root issues parsed from file.
	//TODO!!: switch to use lines from inside ParseContext, and take mutable ref to it instead.
	fn parse_virtual_at_depth(
		lines: &mut std::iter::Peekable<std::str::Lines>,
		depth: usize,
		line_num: usize,
		ctx: &ParseContext,
		parent_idx: IssueIndex,
		hollow: HollowIssue,
	) -> Result<Self, ParseError> {
		let indent = "\t".repeat(depth);
		let child_indent = "\t".repeat(depth + 1);

		// Parse title line: `- [ ] [label1, label2] Title <!--url-->` or `- [ ] Title <!--immutable url-->`
		let first_line = lines.next().ok_or_else(ParseError::empty_file)?;
		let title_content = first_line
			.strip_prefix(&indent)
			.ok_or_else(|| ParseError::bad_indentation(ctx.named_source(), ctx.line_span(line_num), depth))?;
		let parsed = Self::parse_title_line(title_content, line_num, ctx)?;

		let mut comments = Vec::new();
		let mut children = HashMap::new();
		let mut blocker_lines = Vec::new();
		let mut current_comment_lines: Vec<String> = Vec::new();
		let mut current_comment_meta: Option<CommentIdentity> = None;
		let mut in_body = true;
		let mut in_blockers = false;
		let mut current_line = line_num;

		// Body is first comment (no marker)
		let mut body_lines: Vec<String> = Vec::new();

		while let Some(&line) = lines.peek() {
			// Check if this line belongs to us (has our indent level or deeper)
			if !line.is_empty() && !line.starts_with(&indent) {
				break; // Less indented = parent's content
			}

			let line = lines.next().unwrap();
			current_line += 1;

			// Empty line handling
			if line.is_empty() {
				if in_blockers {
					// Empty lines in blockers are ignored by classify_line
				} else if current_comment_meta.is_some() {
					current_comment_lines.push(String::new());
				} else if in_body {
					body_lines.push(String::new());
				}
				continue;
			}

			// Strip our indent level to get content
			let content = line.strip_prefix(&child_indent).unwrap_or(line);

			// Check for blockers marker
			if matches!(Marker::decode(content), Some(Marker::BlockersSection(_))) {
				// Flush current comment/body
				if in_body {
					in_body = false;
					if !body_lines.is_empty() {
						let body_text = body_lines.join("\n").trim().to_string();
						comments.push(Comment {
							identity: CommentIdentity::Body,
							body: super::Events::parse(&body_text),
						});
					}
				} else if let Some(identity) = current_comment_meta.take() {
					let body_text = current_comment_lines.join("\n").trim().to_string();
					comments.push(Comment {
						identity,
						body: super::Events::parse(&body_text),
					});
					current_comment_lines.clear();
				}
				in_blockers = true;
				tracing::debug!("[parse] entering blockers section");
				continue;
			}

			// If in blockers section, parse as blocker lines
			// But stop at sub-issue lines (they end the blockers section)
			if in_blockers {
				// Check if this is a sub-issue line - if so, exit blockers mode and process it below
				if content.starts_with("- [") {
					match Self::parse_child_title_line_detailed(content) {
						ChildTitleParseResult::Ok => {
							in_blockers = false;
							tracing::debug!("[parse] exiting blockers section due to sub-issue: {content:?}");
							// Fall through to sub-issue processing below
						}
						ChildTitleParseResult::InvalidCheckbox(invalid_content) => {
							return Err(ParseError::invalid_checkbox(ctx.named_source(), ctx.line_span(current_line), invalid_content));
						}
						ChildTitleParseResult::NotChildTitle => {
							// Not a sub-issue, continue parsing as blocker
							if let Some(line) = classify_line(content) {
								tracing::debug!("[parse] blocker line: {content:?} -> {line:?}");
								blocker_lines.push(line);
							} else {
								tracing::debug!("[parse] blocker line SKIPPED (classify_line returned None): {content:?}");
							}
							continue;
						}
					}
				} else {
					if let Some(line) = classify_line(content) {
						tracing::debug!("[parse] blocker line: {content:?} -> {line:?}");
						blocker_lines.push(line);
					} else {
						tracing::debug!("[parse] blocker line SKIPPED (classify_line returned None): {content:?}");
					}
					continue;
				}
			}

			// Check for comment marker (including !c shorthand)
			let is_new_comment_shorthand = content.trim().eq_ignore_ascii_case("!c");
			if is_new_comment_shorthand || (content.starts_with("<!--") && content.contains("-->")) {
				let inner = content.strip_prefix("<!--").and_then(|s| s.split("-->").next()).unwrap_or("").trim();

				// vim fold markers are just visual wrappers, not comment separators - skip without flushing
				if inner.starts_with("omitted") && inner.contains("{{{") {
					continue;
				}
				if inner.starts_with(",}}}") {
					continue;
				}

				// Flush previous (only for actual comment markers, not fold markers)
				if in_body {
					in_body = false;
					let body_text = body_lines.join("\n").trim().to_string();
					comments.push(Comment {
						identity: CommentIdentity::Body,
						body: super::Events::parse(&body_text),
					});
				} else if let Some(identity) = current_comment_meta.take() {
					let body_text = current_comment_lines.join("\n").trim().to_string();
					comments.push(Comment {
						identity,
						body: super::Events::parse(&body_text),
					});
					current_comment_lines.clear();
				}

				// Handle !c shorthand
				if is_new_comment_shorthand {
					current_comment_meta = Some(CommentIdentity::Pending);
					continue;
				}

				if inner == "new comment" {
					current_comment_meta = Some(CommentIdentity::Pending);
				} else if inner.contains("#issuecomment-") {
					let identity = Self::parse_comment_identity(inner);
					current_comment_meta = Some(identity);
				}
				continue;
			}

			// Check for sub-issue line: `- [x] Title <!--sub url-->` or `- [ ] Title` (new)
			if content.starts_with("- [") {
				let is_child_title = match Self::parse_child_title_line_detailed(content) {
					ChildTitleParseResult::Ok => true,
					ChildTitleParseResult::InvalidCheckbox(invalid_content) => {
						return Err(ParseError::invalid_checkbox(ctx.named_source(), ctx.line_span(current_line), invalid_content));
					}
					ChildTitleParseResult::NotChildTitle => false,
				};

				if !is_child_title {
					// Not a sub-issue line, treat as regular content
					let content_line = content.strip_prefix('\t').unwrap_or(content);
					if in_body {
						body_lines.push(content_line.to_string());
					} else if current_comment_meta.is_some() {
						current_comment_lines.push(content_line.to_string());
					}
					continue;
				}

				// Flush current
				if in_body {
					in_body = false;
					let body_text = body_lines.join("\n").trim().to_string();
					comments.push(Comment {
						identity: CommentIdentity::Body,
						body: super::Events::parse(&body_text),
					});
				} else if let Some(identity) = current_comment_meta.take() {
					let body_text = current_comment_lines.join("\n").trim().to_string();
					comments.push(Comment {
						identity,
						body: super::Events::parse(&body_text),
					});
					current_comment_lines.clear();
				}

				// Collect all lines belonging to this child (at depth+1 and deeper)
				let child_content_indent = "\t".repeat(depth + 2);
				let mut child_lines: Vec<String> = vec![content.to_string()]; // Start with the title line (without parent indent)

				while let Some(&next_line) = lines.peek() {
					if next_line.is_empty() {
						// Preserve empty lines
						let _ = lines.next();
						child_lines.push(String::new());
					} else if next_line.starts_with(&child_content_indent) {
						let _ = lines.next();
						// Strip one level of indent (the child's content indent) to normalize for recursive parsing
						let stripped = next_line.strip_prefix(&child_indent).unwrap_or(next_line);
						child_lines.push(stripped.to_string());
					} else {
						// Not a child content line - break
						break;
					}
				}

				// Trim trailing empty lines
				while child_lines.last().is_some_and(|l| l.is_empty()) {
					child_lines.pop();
				}

				// Recursively parse the child
				// Build child's parent_index from this issue's identity info
				let child_parent_idx = match &parsed.identity_info {
					IssueMarker::Linked { link, .. } => {
						// Parent is linked - child's parent_index is this issue's full index
						parent_idx.child(IssueSelector::GitId(link.number()))
					}
					IssueMarker::Pending | IssueMarker::Virtual => {
						// Local/virtual parent - pass through parent_index
						parent_idx
					}
				};
				let child_content = child_lines.join("\n");
				let mut child_lines_iter = child_content.lines().peekable();
				let child = Self::parse_virtual_at_depth(&mut child_lines_iter, 0, current_line, ctx, child_parent_idx)?;
				children.insert(child.selector(), child);
				continue;
			}

			// Regular content line (doesn't start with "- [")
			let content_line = content.strip_prefix('\t').unwrap_or(content); // Extra indent for immutable
			if in_body {
				body_lines.push(content_line.to_string());
			} else if current_comment_meta.is_some() {
				current_comment_lines.push(content_line.to_string());
			}
		}

		// Flush final
		if in_body {
			let body_text = body_lines.join("\n").trim().to_string();
			comments.push(Comment {
				identity: CommentIdentity::Body,
				body: super::Events::parse(&body_text),
			});
		} else if let Some(identity) = current_comment_meta.take() {
			let body_text = current_comment_lines.join("\n").trim().to_string();
			comments.push(Comment {
				identity,
				body: super::Events::parse(&body_text),
			});
		}

		// Build identity from identity_info
		let identity = match parsed.identity_info {
			IssueMarker::Linked { user, link } => {
				// Linked issues: use parent_index if provided, otherwise derive from link (via None)
				// Timestamps will be loaded from .meta.json separately
				IssueIdentity::new_linked(parent_idx, user, link, IssueTimestamps::default()) //XXX: not something we should be parsing. Should know this already. User shouldn't be able to specify this manually; we should store it ourselves.
			}
			IssueMarker::Pending => {
				// Pending issues require parent_index from caller
				let pi = parent_idx.expect("BUG: pending issue without parent_index - use parse_virtual with the issue's full IssueIndex");
				IssueIdentity::pending(pi)
			}
			IssueMarker::Virtual => {
				// Virtual issues require parent_index from caller
				let pi = parent_idx.expect("BUG: virtual issue without parent_index - use parse_virtual with the issue's full IssueIndex");
				IssueIdentity::virtual_issue(pi)
			}
		};

		Ok(Issue {
			identity,
			contents: IssueContents {
				title: parsed.title,
				labels: parsed.labels,
				state: parsed.close_state,
				comments,
				blockers: BlockerSequence::from_lines(blocker_lines),
			},
			children,
		})
	}

	/// Parse title line: `- [ ] [label1, label2] Title <!--url-->` or `- [ ] Title !n`
	/// Also supports `- [-]` for not-planned and `- [123]` for duplicates.
	fn parse_title_line(line: &str, line_num: usize, ctx: &ParseContext) -> Result<ParsedTitleLine, ParseError> {
		// Parse checkbox: `- [CONTENT] `
		let (close_state, rest) = match Self::parse_checkbox_prefix_detailed(line) {
			CheckboxParseResult::Ok(state, rest) => (state, rest),
			CheckboxParseResult::NotCheckbox => {
				return Err(ParseError::invalid_title(ctx.named_source(), ctx.line_span(line_num), format!("got: {line:?}")));
			}
			CheckboxParseResult::InvalidContent(content) => {
				return Err(ParseError::invalid_checkbox(ctx.named_source(), ctx.line_span(line_num), content));
			}
		};

		// Check for labels: [label1, label2] at the start
		let (labels, rest) = if rest.starts_with('[') {
			if let Some(bracket_end) = rest.find("] ") {
				let labels_str = &rest[1..bracket_end];
				let labels: Vec<String> = labels_str.split(',').map(|s| s.trim().to_string()).filter(|s| !s.is_empty()).collect();
				(labels, &rest[bracket_end + 2..])
			} else {
				(vec![], rest)
			}
		} else {
			(vec![], rest)
		};

		// Parse issue marker from end of line
		let (identity_info, title) = IssueMarker::parse_from_end(rest).ok_or_else(|| ParseError::missing_url_marker(ctx.named_source(), ctx.line_span(line_num)))?;

		Ok(ParsedTitleLine {
			title: title.to_string(),
			identity_info,
			close_state,
			labels,
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

	/// Parse checkbox prefix: `- [CONTENT] ` and return result.
	fn parse_checkbox_prefix_detailed(line: &str) -> CheckboxParseResult<'_> {
		// Match `- [` prefix
		let Some(rest) = line.strip_prefix("- [") else {
			return CheckboxParseResult::NotCheckbox;
		};

		// Find closing `] `
		let Some(bracket_end) = rest.find("] ") else {
			return CheckboxParseResult::NotCheckbox;
		};

		let checkbox_content = &rest[..bracket_end];
		let rest = &rest[bracket_end + 2..];

		match CloseState::from_checkbox(checkbox_content) {
			Some(close_state) => CheckboxParseResult::Ok(close_state, rest),
			None => CheckboxParseResult::InvalidContent(checkbox_content.to_string()),
		}
	}

	/// Parse child/sub-issue title line with detailed result.
	fn parse_child_title_line_detailed(line: &str) -> ChildTitleParseResult {
		let (_, rest) = match Self::parse_checkbox_prefix_detailed(line) {
			CheckboxParseResult::Ok(state, rest) => (state, rest),
			CheckboxParseResult::NotCheckbox => return ChildTitleParseResult::NotChildTitle,
			CheckboxParseResult::InvalidContent(content) => return ChildTitleParseResult::InvalidCheckbox(content),
		};

		// Check if there's a valid issue marker at the end
		if let Some((_, title)) = IssueMarker::parse_from_end(rest) {
			if title.is_empty() { ChildTitleParseResult::NotChildTitle } else { ChildTitleParseResult::Ok }
		} else {
			// No marker - could be a new pending child being added (bare title)
			let title = rest.trim();
			if !title.is_empty() {
				ChildTitleParseResult::Ok
			} else {
				ChildTitleParseResult::NotChildTitle
			}
		}
	}

	//==========================================================================
	// Serialization Methods
	//==========================================================================

	/// Serialize for virtual file representation (human-readable, full tree).
	/// Creates a complete markdown file with all children recursively embedded.
	/// Used for temp files in /tmp where user views/edits the full issue tree.
	pub fn serialize_virtual(&self) -> String {
		self.serialize_virtual_at_depth(0)
	}

	/// Internal: serialize virtual representation at given depth
	fn serialize_virtual_at_depth(&self, depth: usize) -> String {
		let indent = "\t".repeat(depth);
		let content_indent = "\t".repeat(depth + 1);
		let mut out = String::new();

		// Title line with issue marker
		let checked = self.contents.state.to_checkbox();
		let issue_marker = IssueMarker::from(&self.identity);
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("[{}] ", self.contents.labels.join(", "))
		};
		let is_owned = self.user().is_some_and(crate::current_user::is);
		out.push_str(&format!("{indent}- [{checked}] {labels_part}{} {issue_marker}\n", self.contents.title));

		// Body (first comment) - add extra indent if not owned
		if let Some(body_comment) = self.contents.comments.first() {
			let comment_indent = if is_owned { &content_indent } else { &format!("{content_indent}\t") };
			if !body_comment.body.is_empty() {
				let body_rendered = body_comment.body.render();
				for line in body_rendered.lines() {
					out.push_str(&format!("{comment_indent}{line}\n"));
				}
			}
		}

		// Additional comments
		for comment in self.contents.comments.iter().skip(1) {
			let comment_is_owned = comment.identity.user().is_some_and(crate::current_user::is);
			let comment_indent = if comment_is_owned { &content_indent } else { &format!("{content_indent}\t") };

			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}

			match &comment.identity {
				CommentIdentity::Body => {
					out.push_str(&format!("{content_indent}<!-- new comment -->\n"));
				}
				CommentIdentity::Created { user, id } => {
					let url = self.url_str().unwrap_or("");
					out.push_str(&format!("{content_indent}<!-- @{user} {url}#issuecomment-{id} -->\n"));
				}
				CommentIdentity::Pending => {
					out.push_str(&format!("{content_indent}<!-- new comment -->\n"));
				}
			}
			if !comment.body.is_empty() {
				let comment_rendered = comment.body.render();
				for line in comment_rendered.lines() {
					out.push_str(&format!("{comment_indent}{line}\n"));
				}
			}
		}

		// Blockers section
		if !self.contents.blockers.is_empty() {
			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}
			let header = crate::Header::new(1, "Blockers");
			out.push_str(&format!("{content_indent}{}\n", header.encode()));
			for line in self.contents.blockers.lines() {
				out.push_str(&format!("{content_indent}{}\n", line.to_raw()));
			}
		}

		// Children - recursively serialize full tree
		// Closed children wrap their content in vim fold markers
		// Sort: open issues first by selector, then closed issues by selector
		let (mut open, mut closed): (Vec<_>, Vec<_>) = self.children.iter().partition(|(_, child)| !child.contents.state.is_closed());
		open.sort_by_key(|(sel, _)| *sel);
		closed.sort_by_key(|(sel, _)| *sel);
		let sorted_children = open.into_iter().chain(closed);
		for (_, child) in sorted_children {
			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}

			// For closed children, we need to wrap the body in vim folds
			// But still recurse for the full structure
			if child.contents.state.is_closed() {
				// Output child title line
				let child_checked = child.contents.state.to_checkbox();
				let child_issue_marker = IssueMarker::from(&child.identity);
				let child_labels_part = if child.contents.labels.is_empty() {
					String::new()
				} else {
					format!("[{}] ", child.contents.labels.join(", "))
				};
				out.push_str(&format!("{content_indent}- [{child_checked}] {child_labels_part}{} {child_issue_marker}\n", child.contents.title));

				// Vim fold start
				let child_content_indent = "\t".repeat(depth + 2);
				out.push_str(&format!("{child_content_indent}<!--omitted {{{{{{always-->\n"));

				// Child body and nested content (without title line - we already output it)
				let child_serialized = child.serialize_virtual_at_depth(depth + 1);
				// Skip the first line (title) since we already output it with vim fold handling
				for line in child_serialized.lines().skip(1) {
					out.push_str(&format!("{line}\n"));
				}

				// Vim fold end
				out.push_str(&format!("{child_content_indent}<!--,}}}}}}-->\n"));
			} else {
				out.push_str(&child.serialize_virtual_at_depth(depth + 1));
			}
		}

		out
	}

	/// Serialize for filesystem storage (single node, no children).
	/// Children are stored in separate files within the parent's directory.
	/// Parsing is done via `Local::parse_single_node` in the local module.
	pub fn serialize_filesystem(&self) -> String {
		let content_indent = "\t";
		let mut out = String::new();

		// Title line (always at root level for filesystem representation)
		let checked = self.contents.state.to_checkbox();
		let issue_marker = IssueMarker::from(&self.identity);
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("[{}] ", self.contents.labels.join(", "))
		};
		let is_owned = self.user().is_some_and(crate::current_user::is);
		out.push_str(&format!("- [{checked}] {labels_part}{} {issue_marker}\n", self.contents.title));

		// Body (first comment) - add extra indent if not owned
		if let Some(body_comment) = self.contents.comments.first() {
			let comment_indent = if is_owned { content_indent } else { "\t\t" };
			if !body_comment.body.is_empty() {
				let body_rendered = body_comment.body.render();
				for line in body_rendered.lines() {
					out.push_str(&format!("{comment_indent}{line}\n"));
				}
			}
		}

		// Additional comments
		for comment in self.contents.comments.iter().skip(1) {
			let comment_is_owned = comment.identity.user().is_some_and(crate::current_user::is);
			let comment_indent_str = if comment_is_owned { content_indent } else { "\t\t" };

			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}

			match &comment.identity {
				CommentIdentity::Body => {
					out.push_str(&format!("{content_indent}<!-- new comment -->\n"));
				}
				CommentIdentity::Created { user, id } => {
					let url = self.url_str().unwrap_or("");
					out.push_str(&format!("{content_indent}<!-- @{user} {url}#issuecomment-{id} -->\n"));
				}
				CommentIdentity::Pending => {
					out.push_str(&format!("{content_indent}<!-- new comment -->\n"));
				}
			}
			if !comment.body.is_empty() {
				let comment_rendered = comment.body.render();
				for line in comment_rendered.lines() {
					out.push_str(&format!("{comment_indent_str}{line}\n"));
				}
			}
		}

		// Blockers section
		if !self.contents.blockers.is_empty() {
			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}
			let header = crate::Header::new(1, "Blockers");
			out.push_str(&format!("{content_indent}{}\n", header.encode()));
			for line in self.contents.blockers.lines() {
				out.push_str(&format!("{content_indent}{}\n", line.to_raw()));
			}
		}

		// NO children - they are stored as separate files

		out
	}

	/// Serialize for GitHub API (markdown body only, no local markers).
	/// This is what gets sent to GitHub as the issue body.
	/// Always outputs markdown format regardless of local file extension.
	pub fn serialize_github(&self) -> String {
		// GitHub body is: body text + blockers section (if any)
		// No title line, no URL markers, no comments - just the body content
		self.body()
	}

	//==========================================================================
	// Deserialization Methods
	//==========================================================================

	/// Parse from virtual file content (full tree embedded).
	/// This is the inverse of `serialize_virtual`.
	///
	/// Derives the issue index from the URL embedded in the content.
	/// Only works for linked issues - pending/virtual issues will panic.
	#[deprecated(note = "outdated, and is a hack in the first place. We should have content-only parsing of the issue contents as a separate method")]
	pub fn deserialize_virtual(content: &str) -> Result<Self, ParseError> {
		let ctx = ParseContext::new(
			content.to_string(),
			PathBuf::from_str("a naughty naughty test decided to use a bad to not pass the filepath").unwrap(),
		);
		let mut lines = content.lines().peekable();
		Self::parse_virtual_at_depth(&mut lines, 0, 1, &ctx, IssueIndex::repo_only(("owner", "repo").into()), HollowIssue::default()) // a horrible horrible hack, - everything should really be using standalone IssueContents deser or pass an actual path. There should never be a need to use this stupid function
	}

	/// Update this issue from virtual format content.
	///
	/// This preserves identity information (timestamps, link, user, parent_index) while
	/// updating the contents from the parsed virtual format. For children, existing
	/// issues are matched by issue number and their identities are preserved.
	///
	/// Use this instead of `parse_virtual` when re-loading an issue after editor edits.
	//#[deprecated(note = "instead just require &mut self on parse_virtual itself")]
	//pub fn update_from_virtual(&mut self, content: &str) -> Result<(), ParseError> {
	//	// Parse the new content using this issue's full index
	//	let mut parsed = Self::parse_virtual(content, self.full_index())?;
	//
	//	let old = self.clone();
	//
	//	Self::update_timestamps_from_diff(&mut parsed, &old);
	//
	//	*self = parsed;
	//
	//	Ok(())
	//}

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

#[derive(Clone, Debug, Default, PartialEq)]
/// Hollow Issue container, - used for [parsing virtual repr](Issue::parse_virtual)
///
/// Stripped of all info parsable from virtual
pub struct HollowIssue {
	pub remote: IssueRemote,
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
type IssueChildren<T> = HashMap<IssueSelector, T>;

/// Result of parsing a checkbox prefix.
enum CheckboxParseResult<'a> {
	/// Successfully parsed checkbox
	Ok(CloseState, &'a str),
	/// Not a checkbox line (doesn't start with `- [`)
	NotCheckbox,
	/// Has checkbox syntax but invalid content (like `[abc]`)
	InvalidContent(String),
}

/// Result of parsing a child title line.
enum ChildTitleParseResult {
	/// Successfully parsed child/sub-issue
	Ok,
	/// Not a child title line
	NotChildTitle,
	/// Has checkbox syntax but invalid content (like `[abc]`)
	InvalidCheckbox(String),
}
//,}}}1

impl PartialEq for IssueIdentity {
	fn eq(&self, other: &IssueIdentity) -> bool {
		self.parent_index == other.parent_index
	}
}

/// Parsed title line components (internal helper)
struct ParsedTitleLine {
	title: String,
	/// Identity info parsed from the title line
	identity_info: IssueMarker,
	close_state: CloseState,
	labels: Vec<String>,
}

//,}}}1

//==============================================================================
// Index by issue number
//==============================================================================

// PERF: Linear search through children for each index operation.
// We sacrifice some performance for determinism - the tree structure
// is navigated by issue numbers rather than positional indices.

impl std::ops::Index<u64> for Issue {
	type Output = Issue;

	/// Index into children by issue number.
	/// Panics if no child with that number exists.
	fn index(&self, issue_number: u64) -> &Self::Output {
		self.children
			.get(&IssueSelector::GitId(issue_number))
			.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
	}
}

impl std::ops::IndexMut<u64> for Issue {
	/// Index into children by issue number (mutable).
	/// Panics if no child with that number exists.
	fn index_mut(&mut self, issue_number: u64) -> &mut Self::Output {
		self.children
			.get_mut(&IssueSelector::GitId(issue_number))
			.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_close_state_from_checkbox() {
		assert_eq!(CloseState::from_checkbox(" "), Some(CloseState::Open));
		assert_eq!(CloseState::from_checkbox(""), Some(CloseState::Open));
		assert_eq!(CloseState::from_checkbox("x"), Some(CloseState::Closed));
		assert_eq!(CloseState::from_checkbox("X"), Some(CloseState::Closed));
		assert_eq!(CloseState::from_checkbox("-"), Some(CloseState::NotPlanned));
		assert_eq!(CloseState::from_checkbox("123"), Some(CloseState::Duplicate(123)));
		assert_eq!(CloseState::from_checkbox("42"), Some(CloseState::Duplicate(42)));
		assert_eq!(CloseState::from_checkbox("invalid"), None);
	}

	#[test]
	fn test_close_state_to_checkbox() {
		assert_eq!(CloseState::Open.to_checkbox(), " ");
		assert_eq!(CloseState::Closed.to_checkbox(), "x");
		assert_eq!(CloseState::NotPlanned.to_checkbox(), "-");
		assert_eq!(CloseState::Duplicate(123).to_checkbox(), "123");
	}

	#[test]
	fn test_close_state_is_closed() {
		assert!(!CloseState::Open.is_closed());
		assert!(CloseState::Closed.is_closed());
		assert!(CloseState::NotPlanned.is_closed());
		assert!(CloseState::Duplicate(123).is_closed());
	}

	#[test]
	fn test_close_state_should_remove() {
		assert!(!CloseState::Open.should_remove());
		assert!(!CloseState::Closed.should_remove());
		assert!(!CloseState::NotPlanned.should_remove());
		assert!(CloseState::Duplicate(123).should_remove());
	}

	#[test]
	fn test_close_state_to_github_state() {
		assert_eq!(CloseState::Open.to_github_state(), "open");
		assert_eq!(CloseState::Closed.to_github_state(), "closed");
		assert_eq!(CloseState::NotPlanned.to_github_state(), "closed");
		assert_eq!(CloseState::Duplicate(123).to_github_state(), "closed");
	}

	#[test]
	fn test_parse_checkbox_prefix() {
		// Helper to extract (CloseState, rest) from Ok result
		fn extract_ok(result: CheckboxParseResult) -> Option<(CloseState, String)> {
			match result {
				CheckboxParseResult::Ok(state, rest) => Some((state, rest.to_string())),
				_ => None,
			}
		}

		// Standard cases
		assert_eq!(extract_ok(Issue::parse_checkbox_prefix_detailed("- [ ] rest")), Some((CloseState::Open, "rest".to_string())));
		assert_eq!(extract_ok(Issue::parse_checkbox_prefix_detailed("- [x] rest")), Some((CloseState::Closed, "rest".to_string())));
		assert_eq!(extract_ok(Issue::parse_checkbox_prefix_detailed("- [X] rest")), Some((CloseState::Closed, "rest".to_string())));

		// New close types
		assert_eq!(
			extract_ok(Issue::parse_checkbox_prefix_detailed("- [-] rest")),
			Some((CloseState::NotPlanned, "rest".to_string()))
		);
		assert_eq!(
			extract_ok(Issue::parse_checkbox_prefix_detailed("- [123] rest")),
			Some((CloseState::Duplicate(123), "rest".to_string()))
		);
		assert_eq!(
			extract_ok(Issue::parse_checkbox_prefix_detailed("- [42] Title here")),
			Some((CloseState::Duplicate(42), "Title here".to_string()))
		);

		// Not a checkbox line
		assert!(matches!(Issue::parse_checkbox_prefix_detailed("no checkbox"), CheckboxParseResult::NotCheckbox));

		// Invalid checkbox content
		assert!(matches!(
			Issue::parse_checkbox_prefix_detailed("- [invalid] rest"),
			CheckboxParseResult::InvalidContent(s) if s == "invalid"
		));
		assert!(matches!(
			Issue::parse_checkbox_prefix_detailed("- [abc] rest"),
			CheckboxParseResult::InvalidContent(s) if s == "abc"
		));
	}

	#[test]
	fn test_parse_invalid_checkbox_returns_error() {
		// Invalid checkbox on root issue
		let content = "- [abc] Invalid issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody\n";
		let result = Issue::deserialize_virtual(content);
		assert!(result.is_err());
		assert!(result.unwrap_err().to_string().contains("invalid checkbox"));

		// Invalid checkbox on sub-issue
		let content = "- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t- [xyz] Bad sub <!--sub @owner https://github.com/owner/repo/issues/2 -->\n";
		let result = Issue::deserialize_virtual(content);
		assert!(result.is_err());
		assert!(result.unwrap_err().to_string().contains("invalid checkbox"));
	}

	#[test]
	fn test_parse_and_serialize_not_planned() {
		let content = "- [-] Not planned issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody text\n";
		let issue = Issue::deserialize_virtual(content).unwrap();

		assert_eq!(issue.contents.state, CloseState::NotPlanned);
		assert_eq!(issue.contents.title, "Not planned issue");

		// Verify serialization preserves the state
		let serialized = issue.serialize_virtual();
		assert!(serialized.starts_with("- [-] Not planned issue"));
	}

	#[test]
	fn test_parse_and_serialize_duplicate() {
		let content = "- [456] Duplicate issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody text\n";
		let issue = Issue::deserialize_virtual(content).unwrap();

		assert_eq!(issue.contents.state, CloseState::Duplicate(456));
		assert_eq!(issue.contents.title, "Duplicate issue");

		// Verify serialization preserves the state
		let serialized = issue.serialize_virtual();
		assert!(serialized.starts_with("- [456] Duplicate issue"));
	}

	#[test]
	fn test_parse_sub_issue_close_types() {
		// Set current user so content is serialized without extra indent
		crate::current_user::set("owner".to_string());

		let content = r#"- [ ] Parent issue <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [x] Closed sub <!--sub @owner https://github.com/owner/repo/issues/2 -->
		<!--omitted {{{always-->
		closed body
		<!--,}}}-->

	- [-] Not planned sub <!--sub @owner https://github.com/owner/repo/issues/3 -->
		<!--omitted {{{always-->
		not planned body
		<!--,}}}-->

	- [42] Duplicate sub <!--sub @owner https://github.com/owner/repo/issues/4 -->
		<!--omitted {{{always-->
		duplicate body
		<!--,}}}-->
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();
		insta::assert_snapshot!(issue.serialize_virtual(), @"
		- [ ] Parent issue <!-- @owner https://github.com/owner/repo/issues/1 -->
			Body
			
			- [x] Closed sub <!-- @owner https://github.com/owner/repo/issues/2 -->
				<!--omitted {{{always-->
				closed body
				<!--,}}}-->
			
			- [-] Not planned sub <!-- @owner https://github.com/owner/repo/issues/3 -->
				<!--omitted {{{always-->
				not planned body
				<!--,}}}-->
			
			- [42] Duplicate sub <!-- @owner https://github.com/owner/repo/issues/4 -->
				<!--omitted {{{always-->
				duplicate body
				<!--,}}}-->
		");
	}

	#[test]
	fn test_find_last_blocker_position_empty() {
		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n";
		let issue = Issue::deserialize_virtual(content).unwrap();
		assert!(issue.find_last_blocker_position().is_none());
	}

	#[test]
	fn test_find_last_blocker_position_single_item() {
		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- task 1\n";
		let issue = Issue::deserialize_virtual(content).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 5); // Line 5: `\t- task 1`
		// Column 4: 1-indexed position of first char after `\t- ` (tab=1, dash=2, space=3, 't'=4)
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_multiple_items() {
		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- task 1\n\t- task 2\n\t- task 3\n";
		let issue = Issue::deserialize_virtual(content).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 7); // Line 7: `\t- task 3`
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_with_headers() {
		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t# Phase 1\n\t- task a\n\t# Phase 2\n\t- task b\n";
		let issue = Issue::deserialize_virtual(content).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 8); // Line 8: `\t- task b`
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_before_sub_issues() {
		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- blocker task\n\n\t- [ ] Sub issue <!--sub @owner https://github.com/owner/repo/issues/2 -->\n";
		let issue = Issue::deserialize_virtual(content).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 5); // Line 5: `\t- blocker task`, not the sub-issue line
		assert_eq!(col, 4);
	}

	#[test]
	fn test_serialize_filesystem_no_children() {
		// Issue with children
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child 1 <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child 1 body

	- [ ] Child 2 <!--sub @owner https://github.com/owner/repo/issues/3 -->
		Child 2 body
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();
		assert_eq!(issue.children.len(), 2);

		// Filesystem serialization should NOT include children
		let fs_serialized = issue.serialize_filesystem();
		assert!(!fs_serialized.contains("Child 1"));
		assert!(!fs_serialized.contains("Child 2"));
		assert!(fs_serialized.contains("Parent body"));
	}

	#[test]
	fn test_serialize_virtual_includes_children() {
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child 1 <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child 1 body
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		// Virtual serialization should include children
		let virtual_serialized = issue.serialize_virtual();
		assert!(virtual_serialized.contains("Parent body"));
		assert!(virtual_serialized.contains("Child 1"));
		assert!(virtual_serialized.contains("Child 1 body"));
	}

	#[test]
	fn test_parse_virtual_includes_inline_children() {
		// parse_virtual includes inline children (used for virtual file format)
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();
		// parse_virtual preserves inline children
		assert_eq!(issue.children.len(), 1);
		assert_eq!(issue.contents.title, "Parent");
		assert_eq!(issue[2].contents.title, "Child");
	}

	#[test]
	fn test_virtual_roundtrip() {
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		// Serialize to virtual, then deserialize back
		let serialized = issue.serialize_virtual();
		let reparsed = Issue::deserialize_virtual(&serialized).unwrap();

		assert_eq!(issue.contents.title, reparsed.contents.title);
		assert_eq!(issue.children.len(), reparsed.children.len());
	}

	#[test]
	fn test_serialize_github_body_only() {
		let content = r#"- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->
	This is the body text.

	# Blockers
	- task 1
	- task 2
"#;
		let issue = Issue::deserialize_virtual(content).unwrap();

		// GitHub serialization is just the body (with blockers)
		let github = issue.serialize_github();

		// Should NOT contain the title line or URL markers
		assert!(!github.contains("- [ ]"));
		assert!(!github.contains("<!--"));

		// Should contain body and blockers
		assert!(github.contains("This is the body text."));
		assert!(github.contains("# Blockers"));
		assert!(github.contains("- task 1"));
	}

	#[test]
	fn test_update_from_virtual_child_open_to_closed() {
		// Start with parent + open child
		let initial = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let mut issue = Issue::deserialize_virtual(initial).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Open);

		// Update with closed child
		let updated = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [x] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		issue.update_from_virtual(updated).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Closed, "Child should be Closed after update");
	}

	#[test]
	fn test_update_from_virtual_child_open_to_not_planned() {
		let initial = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let mut issue = Issue::deserialize_virtual(initial).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Open);

		let updated = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [-] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		issue.update_from_virtual(updated).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::NotPlanned, "Child should be NotPlanned after update");
	}

	#[test]
	fn test_update_from_virtual_child_open_to_duplicate() {
		let initial = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let mut issue = Issue::deserialize_virtual(initial).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Open);

		let updated = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [99] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		issue.update_from_virtual(updated).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Duplicate(99), "Child should be Duplicate(99) after update");
	}

	#[test]
	fn test_update_from_virtual_child_closed_to_open() {
		let initial = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [x] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let mut issue = Issue::deserialize_virtual(initial).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Closed);

		let updated = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		issue.update_from_virtual(updated).unwrap();
		assert_eq!(issue[2].contents.state, CloseState::Open, "Child should be Open after update");
	}
}
