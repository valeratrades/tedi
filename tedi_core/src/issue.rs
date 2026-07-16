//! The Issue primitive: identity, contents, comments, blockers, children,
//! plus parse/serialize over `tedi_md::Events`.

use std::{collections::HashMap, path::PathBuf};

use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use tedi_md::{indent_into, preserve_paragraph_spacing, wrap_inline_in_paragraphs};

use crate::{Blockers, IssueChildren, IssueIndex, IssueLink, IssueMarker, IssueSelector, Marker, ParseContext, ParseError, RepoInfo, TitleInGitPathError};

/// Filename stem for the main issue file when the issue lives in its own directory (has children).
pub const MAIN_ISSUE_FILENAME: &str = "__main__";
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
		Self { user, link, timestamps }
	}

	/// Get the link.
	pub fn link(&self) -> &IssueLink {
		&self.link
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
		let parent_index = parent_index.unwrap_or_else(|| IssueIndex::repo_only(link.project()));
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

	/// Create a numbered virtual issue identity: local-only, but addressable by number
	/// through its owner-less `IssueLink::Virtual` (never synced to Github).
	pub fn virtual_linked(parent_index: IssueIndex, link: IssueLink) -> Self {
		assert!(matches!(link, IssueLink::Virtual(_)), "virtual_linked requires a virtual link");
		Self {
			parent_index,
			is_virtual: true,
			remote: Some(Box::new(LinkedIssueMeta::new(None, link, IssueTimestamps::now()))),
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

	/// Get the remote URL string if linked to Github. Panics on a virtual link.
	pub fn url_str(&self) -> Option<&str> {
		self.link().map(|l| l.url().as_str())
	}

	/// Get the user who created this issue if linked and known.
	pub fn user(&self) -> Option<&str> {
		self.as_linked().and_then(|m| m.user.as_deref())
	}

	/// Check if this issue is owned by the current user.
	/// True if: virtual, or linked with unknown user (None), or linked with matching user.
	pub fn is_mine(&self) -> bool {
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

	/// Get owner, or `None` for a virtual issue.
	pub fn owner(&self) -> Option<&str> {
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
	pub fn git_lineage(&self) -> Result<Vec<u64>, TitleInGitPathError> {
		self.parent_index.git_num_path()
	}

	/// Create a child's parent_index by appending this issue's number.
	/// Returns None if this issue is not linked (has no number to append).
	pub fn child_parent_index(&self) -> Option<IssueIndex> {
		self.number().map(|n| self.parent_index.child(IssueSelector::GitId(n)))
	}
}

/// Marker view of an issue's identity — how it serializes to a title-line marker.
impl From<&IssueIdentity> for IssueMarker {
	fn from(identity: &IssueIdentity) -> Self {
		// virtual takes precedence over linked: a numbered virtual issue holds a virtual
		// link that must stay visibly distinct from real Github links
		if identity.is_virtual {
			IssueMarker::Virtual { link: identity.link().cloned() }
		} else if let Some(meta) = identity.as_linked() {
			IssueMarker::Linked {
				user: meta.user.clone(),
				link: meta.link().clone(),
			}
		} else {
			IssueMarker::Pending
		}
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
	pub body: crate::Events,
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
	pub blockers: Blockers,
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
			// Exact is accepted as a title source: filesystem-derived indexes round-trip through build_from_path
			Some(IssueSelector::Title(t) | IssueSelector::Exact(t)) => {
				let selectors = index[..index.len() - 1].to_vec();
				(t.to_string(), selectors)
			}
			Some(IssueSelector::GitId(_)) => panic!("pending_from_descriptor requires last selector to be Title"),
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
	pub fn lineage(&self) -> Result<Vec<u64>, TitleInGitPathError> {
		self.identity.git_lineage()
	}

	/// Get repository info.
	pub fn repo_info(&self) -> RepoInfo {
		self.identity.repo_info()
	}

	/// Get the full issue body including blockers section as events.
	/// This is what should be synced to Github as the issue body.
	pub fn body(&self) -> crate::Events {
		//NB: DO NOT CHANGE Output Type
		let mut events: Vec<crate::OwnedEvent> = self.contents.comments.first().map(|c| c.body.to_vec()).unwrap_or_default();
		if !self.contents.blockers.is_empty() {
			events.push(crate::OwnedEvent::Start(crate::OwnedTag::Heading {
				level: pulldown_cmark::HeadingLevel::H1,
				id: None,
				classes: Vec::new(),
				attrs: Vec::new(),
			}));
			events.push(crate::OwnedEvent::Text("Blockers".to_string()));
			events.push(crate::OwnedEvent::End(crate::OwnedTagEnd::Heading(pulldown_cmark::HeadingLevel::H1)));
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
	pub fn from_combined(hollow: HollowIssue, virtual_issue: VirtualIssue, parent_idx: IssueIndex, is_virtual: bool) -> Result<Self, crate::IssueError> {
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
						return Err(crate::IssueError::new_erroneous_composition(
							n,
							"either internal bug (HollowIssue was constructed incorrectly) or user manually embedded a `<!-- @user url -->` marker, which is not permitted".to_string(),
						));
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

	//==========================================================================
	// Serialization
	//==========================================================================

	/// Render this issue's own file: title line · body · folded comments · blockers
	/// · child issues as one-line links. One level — children are links, not embedded.
	/// This is the exact byte content written to disk, opened in the editor, and embedded
	/// in a sprint. See `impl Display for Issue`.
	fn render(&self) -> String {
		if self.identity.is_virtual {
			assert!(self.user().is_none(), "virtual issue must not have a user tag, got: {:?}", self.user());
		}

		let content_indent = "  ";
		// Normalize the title line to exactly one trailing newline before appending content.
		let mut out = String::new();
		for line in TitleLine::of(self).encode().lines() {
			out.push_str(line);
			out.push('\n');
		}

		let is_owned = self.identity.is_mine();
		let mut content = String::new();

		// === Body (first comment) ===
		if let Some(body_comment) = self.contents.comments.first()
			&& !body_comment.body.is_empty()
		{
			let body_str: String = crate::Events::from(body_comment.body.to_vec()).into();
			if is_owned {
				content.push_str(&body_str);
			} else {
				indent_into(&mut content, &body_str, "  ");
			}
		}

		// === Additional comments — wrapped in a default-fold vim marker ===
		if self.contents.comments.len() > 1 {
			ensure_blank_line(&mut content);
			content.push_str(&Marker::OmittedStart.encode());
			content.push('\n');
			for comment in self.contents.comments.iter().skip(1) {
				if self.identity.is_virtual {
					assert!(
						!comment.is_comment() || comment.user().is_none(),
						"virtual issue must not have linked comments, got: {:?}",
						comment.identity
					);
				}
				let comment_is_owned = comment.user().is_none() || comment.user().is_some_and(crate::current_user::is);

				let marker_html = match &comment.identity {
					CommentIdentity::Body | CommentIdentity::Pending => Marker::NewComment.encode(),
					CommentIdentity::Created { user, id } => {
						let url = self.url_str().expect("remote must be initialized");
						format!("<!-- @{user} {url}#issuecomment-{id} -->")
					}
				};
				content.push_str(&marker_html);
				content.push('\n');

				if !comment.body.is_empty() {
					let body_str: String = crate::Events::from(comment.body.to_vec()).into();
					if comment_is_owned {
						content.push_str(&body_str);
					} else {
						indent_into(&mut content, &body_str, "  ");
					}
				}
				ensure_blank_line(&mut content);
			}
			content.push_str(&Marker::OmittedEnd.encode());
			content.push('\n');
		}

		// === Blockers section ===
		if !self.contents.blockers.is_empty() {
			ensure_blank_line(&mut content);
			content.push_str(&crate::Header::new(1, "Blockers").encode());
			content.push('\n');
			content.push_str(&String::from(&self.contents.blockers));
		}

		if !content.is_empty() {
			indent_into(&mut out, &content, content_indent);
		}

		// === Children as links — sort: open first by selector, then closed by selector ===
		if !self.children.is_empty() {
			let (mut open, mut closed): (Vec<_>, Vec<_>) = self.children.iter().partition(|(_, child)| !child.contents.state.is_closed());
			open.sort_by_key(|(sel, _)| *sel);
			closed.sort_by_key(|(sel, _)| *sel);

			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push('\n');
			}
			for (_, child) in open.into_iter().chain(closed) {
				indent_into(&mut out, &child.child_link_line(), content_indent);
			}
		}

		out
	}

	/// One-line `- [state] (labels) [Title](./rel) <!-- marker -->` link to this issue's file,
	/// used when this issue is embedded as a child of its parent. The path is navigational
	/// (`gf`-jumpable) and re-derived on write; identity comes from the marker.
	fn child_link_line(&self) -> String {
		use crate::{OwnedEvent, OwnedTag, OwnedTagEnd};
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("({}) ", self.contents.labels.join(", "))
		};
		let mut events = vec![
			OwnedEvent::Start(OwnedTag::List(None)),
			OwnedEvent::Start(OwnedTag::Item),
			OwnedEvent::CheckBox(self.contents.state.to_checkbox_contents()),
		];
		if !labels_part.is_empty() {
			events.push(OwnedEvent::Text(labels_part));
		}
		events.extend([
			OwnedEvent::Start(OwnedTag::Link {
				link_type: pulldown_cmark::LinkType::Inline,
				dest_url: self.storage_rel_link(),
				title: String::new(),
				id: String::new(),
			}),
			OwnedEvent::Text(self.contents.title.clone()),
			OwnedEvent::End(OwnedTagEnd::Link),
			OwnedEvent::Text(" ".to_string()),
			OwnedEvent::InlineHtml(format!("<!-- {} -->", IssueMarker::from(&self.identity).encode())),
			OwnedEvent::End(OwnedTagEnd::Item),
			OwnedEvent::End(OwnedTagEnd::List(false)),
		]);
		crate::Events::from(events).into()
	}

	/// Relative path (from the parent issue's directory) to this issue's markdown file.
	fn storage_rel_link(&self) -> String {
		let number = self.git_id();
		let title = &self.contents.title;
		let closed = self.contents.state.is_closed();
		if self.children.is_empty() {
			format!("./{}", issue_file_name(number, title, closed))
		} else {
			let dir = issue_dir_name(number, title, closed);
			let main = if closed {
				format!("{MAIN_ISSUE_FILENAME}.md.bak")
			} else {
				format!("{MAIN_ISSUE_FILENAME}.md")
			};
			format!("./{dir}/{main}")
		}
	}

	/// Serialize for GitHub API (markdown body only, no local markers).
	/// This is what gets sent to GitHub as the issue body.
	/// Always outputs markdown format regardless of local file extension.
	pub fn render_github(&self) -> crate::Events {
		//NB: DO NOT CHANGE Output Type
		// GitHub body is: body text + blockers section (if any)
		// No title line, no URL markers, no comments - just the body content
		self.body()
	}

	/// Editor cursor position (1-indexed line, col) in the serialized content: the last
	/// blocker item, or — with no blockers — where the section would be inserted
	/// (below comments, above children).
	pub fn editor_position(&self) -> (u32, u32) {
		let serialized = self.render();
		let lines: Vec<&str> = serialized.lines().collect();

		if !self.contents.blockers.is_empty() {
			let blockers_header = crate::Header::new(1, "Blockers").encode();
			let blockers_start_idx = lines
				.iter()
				.position(|line| line.trim() == blockers_header)
				.expect("render() emits the header for non-empty blockers");

			let mut last_item: Option<(u32, u32)> = None;
			for (offset, line) in lines[blockers_start_idx + 1..].iter().enumerate() {
				let trimmed = line.trim();
				// Child links (`- [`) terminate the blockers section
				if trimmed.starts_with("- [") {
					break;
				}
				if trimmed.starts_with("- ") {
					let dash_pos = line.find("- ").expect("trimmed starts with it") as u32;
					// col points to the first character of the item text (after `- `)
					last_item = Some(((blockers_start_idx + 1 + offset + 1) as u32, dash_pos + 3));
				}
			}
			return last_item.expect("non-empty blockers render at least one `- ` item");
		}

		// Each child renders as exactly one line at the end, preceded by a blank line —
		// the cursor goes on that blank line; with no children, one past the last line.
		let line = match self.children.is_empty() {
			true => lines.len() + 1,
			false => lines.len() - self.children.len(),
		};
		(line as u32, 1)
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

/// The single stable rendering of an issue: the exact bytes written to disk, opened in the
/// editor, and embedded in a sprint. Embedding an issue *is* rendering it in its own file.
impl std::fmt::Display for Issue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(&self.render())
	}
}

/// Sanitize a title for use in filenames: spaces→`_`, drop non-alphanumeric (keep `-`/`_`), trim `_`.
pub fn sanitize_title(title: &str) -> String {
	title
		.chars()
		.map(|c| {
			if c.is_alphanumeric() || c == '-' || c == '_' {
				c
			} else if c == ' ' {
				'_'
			} else {
				'\0'
			}
		})
		.filter(|&c| c != '\0')
		.collect::<String>()
		.trim_matches('_')
		.to_string()
}
/// Flat issue filename: `{base}.md[.bak]`.
pub fn issue_file_name(number: Option<u64>, title: &str, closed: bool) -> String {
	let base = format!("{}.md", issue_base_name(number, title));
	if closed { format!("{base}.bak") } else { base }
}
/// Directory name for an issue with children: `{base}[.bak]`.
pub fn issue_dir_name(number: Option<u64>, title: &str, closed: bool) -> String {
	let base = issue_base_name(number, title);
	if closed { format!("{base}.bak") } else { base }
}
#[derive(Clone, Debug, Default, PartialEq, derive_new::new)]
/// Hollow Issue container, - used for parsing virtual repr
///
/// Stripped of all info parsable from virtual
pub struct HollowIssue {
	pub remote: Option<Box<LinkedIssueMeta>>,
	pub children: IssueChildren<HollowIssue>,
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
		let events = crate::Events::parse(content);
		Self::parse_from_events(&events, &ctx)
	}

	/// Parse a VirtualIssue from a cmark event stream.
	///
	/// Expects: `Start(List) > Start(Item) > ... > End(Item) > End(List)`
	/// The item contains: CheckBox, title text, InlineHtml marker, body events,
	/// comment markers (Html), blocker heading+list, child issue lists.
	fn parse_from_events(events: &[crate::OwnedEvent], ctx: &ParseContext) -> Result<Self, ParseError> {
		Self::parse_from_events_inner(events, ctx, false)
	}

	fn parse_from_events_inner(events: &[crate::OwnedEvent], ctx: &ParseContext, default_pending: bool) -> Result<Self, ParseError> {
		let (title, pos) = TitleLine::decode(events, ctx, default_pending)?;
		let selector = title.marker.selector(&title.title);

		let seg = Self::segment_item(events, pos, ctx)?;

		let comments: Vec<Comment> = seg
			.comments
			.into_iter()
			.map(|(identity, evs)| Comment {
				identity,
				body: preserve_paragraph_spacing(wrap_inline_in_paragraphs(evs)).into(),
			})
			.collect();

		// A child normally renders as a one-line link (`- [x] [Title](./rel) <!-- marker -->`), which
		// parses to a shallow child (empty body/children) since a link carries no content. We still
		// descend recursively so any embedded content a child item does hold is captured — production
		// buffers hold only links, but test fixtures and hand-authored files may embed child bodies.
		let mut children = HashMap::new();
		for (child_events, children_default_pending) in seg.child_slices {
			let child = Self::parse_from_events_inner(&child_events, ctx, children_default_pending)?;
			children.insert(child.selector, child);
		}

		let blockers = if seg.blocker_events.is_empty() {
			Blockers::default()
		} else {
			let blocker_text: String = crate::Events::from(seg.blocker_events).into();
			Blockers::parse(&blocker_text)
		};

		Ok(VirtualIssue {
			selector,
			contents: IssueContents {
				title: title.title,
				labels: title.labels,
				state: title.state,
				comments: comments.into(),
				blockers: {
					let mut seq = blockers;
					if seg.select_blockers {
						seq.set_state = Some(crate::BlockerSetState::Pending);
					}
					seq
				},
			},
			children,
		})
	}

	/// Segmenter: walk one item's events (from `pos`, just past the title) into typed spans —
	/// body/comment spans, blocker events, and child item slices. All `!c`/`!s` tight-vs-loose
	/// quirks and omitted-marker skipping live only here. The blocker span closes on `Rule` or
	/// the first checkbox list.
	fn segment_item(events: &[crate::OwnedEvent], mut pos: usize, ctx: &ParseContext) -> Result<ItemSegments, ParseError> {
		use super::{OwnedEvent, OwnedTag, OwnedTagEnd};

		let mut comment_spans: Vec<(CommentIdentity, Vec<OwnedEvent>)> = Vec::new();
		let mut child_slices: Vec<(Vec<OwnedEvent>, bool)> = Vec::new();
		let mut body_events: Vec<OwnedEvent> = Vec::new();
		let mut current_comment_events: Vec<OwnedEvent> = Vec::new();
		let mut current_comment_meta: Option<CommentIdentity> = None;
		let mut blocker_events: Vec<OwnedEvent> = Vec::new();
		let mut in_body = true;
		let mut in_blockers = false;
		let mut blocker_list_consumed = false;
		let mut select_blockers = false;

		// Flush the current body/comment accumulation into a raw span (identity + events).
		// Per-primitive assembly (paragraph wrapping) happens in the caller.
		let flush = |in_body: &mut bool,
		             current_comment_meta: &mut Option<CommentIdentity>,
		             body_events: &mut Vec<OwnedEvent>,
		             current_comment_events: &mut Vec<OwnedEvent>,
		             comment_spans: &mut Vec<(CommentIdentity, Vec<OwnedEvent>)>| {
			if *in_body {
				*in_body = false;
				comment_spans.push((CommentIdentity::Body, std::mem::take(body_events)));
			} else if let Some(identity) = current_comment_meta.take() {
				comment_spans.push((identity, std::mem::take(current_comment_events)));
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
							flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);
							current_comment_meta = Some(CommentIdentity::Pending);
							pos += 1;
							continue;
						}
						if inner.contains("#issuecomment-") {
							flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);
							current_comment_meta = Some(match Marker::decode(trimmed) {
								Some(Marker::Comment { user, id, .. }) => CommentIdentity::Created { user, id },
								_ => CommentIdentity::Pending,
							});
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
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);
						current_comment_meta = Some(CommentIdentity::Pending);
					}
					pos += 3; // skip Start(Paragraph), Text, End(Paragraph)
				}

				// "!s" or "!c" as bare text (tight items have no paragraph wrappers)
				OwnedEvent::Text(t) if t.trim().eq_ignore_ascii_case("!s") || t.trim().eq_ignore_ascii_case("!c") => {
					// Remove trailing artifacts from accumulated events.
					// When `!c` appears as first text inside a paragraph (e.g. `Start(P), Text("!c"), SoftBreak, ...`),
					// the Start(P) was already pushed. Strip it along with any preceding SoftBreak.
					let strip_trailing = |evs: &mut Vec<OwnedEvent>| {
						// Pop trailing Start(Paragraph) — stray from paragraph containing `!c`
						if matches!(evs.last(), Some(OwnedEvent::Start(OwnedTag::Paragraph))) {
							evs.pop();
						}
						// Pop trailing SoftBreak — normalization artifact
						if matches!(evs.last(), Some(OwnedEvent::SoftBreak)) {
							evs.pop();
						}
					};
					if in_body {
						strip_trailing(&mut body_events);
					} else if current_comment_meta.is_some() {
						strip_trailing(&mut current_comment_events);
					}
					if t.trim().eq_ignore_ascii_case("!s") {
						select_blockers = true;
					} else {
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);
						current_comment_meta = Some(CommentIdentity::Pending);
					}
					pos += 1;
					// Skip SoftBreak following `!c`/`!s` text (continuation within same paragraph)
					if matches!(events.get(pos), Some(OwnedEvent::SoftBreak)) {
						pos += 1;
					}
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
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);
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
						flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);

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

								child_slices.push((child_events, children_default_pending));

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
						// Blocker list — collect events for Blockers parsing
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
							// `---` is the blockers-section terminator emitted by tedi_md's parse
							OwnedEvent::Rule => {
								pos += 1;
								continue;
							}
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

		flush(&mut in_body, &mut current_comment_meta, &mut body_events, &mut current_comment_events, &mut comment_spans);

		Ok(ItemSegments {
			comments: comment_spans,
			blocker_events,
			child_slices,
			select_blockers,
		})
	}

	/// Check if a list (starting at `Start(List(...))`) contains any CheckBox items.
	fn list_has_checkbox(events: &[crate::OwnedEvent]) -> bool {
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
	fn find_matching_end_list(events: &[crate::OwnedEvent], start: usize) -> usize {
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
	fn find_matching_end_item(events: &[crate::OwnedEvent], start: usize) -> usize {
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

/// Ensure `content` ends with a blank-line separator when it has trailing content.
/// cmark rendering sometimes omits trailing newlines, so we normalize before inserting separators.
fn ensure_blank_line(content: &mut String) {
	if content.is_empty() {
		return;
	}
	if !content.ends_with('\n') {
		content.push('\n');
	}
	if content.lines().last().is_some_and(|l| !l.trim().is_empty()) {
		content.push('\n');
	}
}

/// Base name `{number}_-_{sanitized}` (or just `{sanitized}`/`{number}` when one side is empty).
fn issue_base_name(number: Option<u64>, title: &str) -> String {
	let sanitized = sanitize_title(title);
	match number {
		Some(n) if sanitized.is_empty() => n.to_string(),
		Some(n) => format!("{n}_-_{sanitized}"),
		None if sanitized.is_empty() => "untitled".to_string(),
		None => sanitized,
	}
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

/// The `- [state] (labels) Title <!-- marker -->` line — the one-line header of every
/// issue item, in both single-file and embedded forms.
pub(crate) struct TitleLine {
	state: CloseState,
	labels: Vec<String>,
	title: String,
	marker: IssueMarker,
}
impl TitleLine {
	pub(crate) fn of(issue: &Issue) -> Self {
		Self {
			state: issue.contents.state.clone(),
			labels: issue.contents.labels.clone(),
			title: issue.contents.title.clone(),
			marker: IssueMarker::from(&issue.identity),
		}
	}

	/// Render via cmark as a standalone list item (depth 0; keeps its trailing newline).
	pub(crate) fn encode(&self) -> String {
		use crate::{OwnedEvent, OwnedTag, OwnedTagEnd};
		let labels_part = if self.labels.is_empty() { String::new() } else { format!("({}) ", self.labels.join(", ")) };
		crate::Events::from(vec![
			OwnedEvent::Start(OwnedTag::List(None)),
			OwnedEvent::Start(OwnedTag::Item),
			OwnedEvent::CheckBox(self.state.to_checkbox_contents()),
			OwnedEvent::Text(format!("{labels_part}{} ", self.title)),
			OwnedEvent::InlineHtml(format!("<!-- {} -->", self.marker.encode())),
			OwnedEvent::End(OwnedTagEnd::Item),
			OwnedEvent::End(OwnedTagEnd::List(false)),
		])
		.into()
	}

	/// Parse the title line from an item event stream. Returns the parsed line and the
	/// position of the first body event inside the item.
	fn decode(events: &[crate::OwnedEvent], ctx: &ParseContext, default_pending: bool) -> Result<(Self, usize), ParseError> {
		use crate::{OwnedEvent, OwnedTag, OwnedTagEnd};

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

		// Collect text before the issue marker InlineHtml.
		// A child item renders its title as a markdown link (`[Title](./rel)`) so `gf` works;
		// we unwrap the Link and keep its inner text as the title (the path is navigational).
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
				OwnedEvent::Start(OwnedTag::Link { .. }) | OwnedEvent::End(OwnedTagEnd::Link) => {
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

		Ok((
			Self {
				state: close_state,
				labels,
				title,
				marker: identity_info,
			},
			pos,
		))
	}
}

/// Typed spans produced by the segmenter for one issue item.
struct ItemSegments {
	/// Body span first (identity `Body`), then any additional comment spans.
	comments: Vec<(CommentIdentity, Vec<crate::OwnedEvent>)>,
	blocker_events: Vec<crate::OwnedEvent>,
	/// Child item slices (each already wrapped in a List), with their default-pending flag.
	child_slices: Vec<(Vec<crate::OwnedEvent>, bool)>,
	select_blockers: bool,
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
	use std::path::PathBuf;

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
			IssueSelector::Title(_) | IssueSelector::Exact(_) => None,
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
		insta::assert_snapshot!(issue.to_string(), @r"
		- [ ] Parent issue <!-- https://github.com/owner/repo/issues/1 -->
		  Body

		  - [x] [Closed sub](./2_-_Closed_sub.md.bak) <!-- https://github.com/owner/repo/issues/2 -->
		  - \[-] [Not planned sub](./3_-_Not_planned_sub.md.bak) <!-- https://github.com/owner/repo/issues/3 -->
		  - \[42] [Duplicate sub](./4_-_Duplicate_sub.md.bak) <!-- https://github.com/owner/repo/issues/4 -->
		");
	}

	#[test]
	fn test_editor_position_no_blockers() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(3, 1)");
	}

	#[test]
	fn test_editor_position_no_blockers_with_sub_issues() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  - [ ] Sub issue <!--sub https://github.com/owner/repo/issues/2 -->\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(3, 1)");
	}

	#[test]
	fn test_editor_position_single_item() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - task 1\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(5, 5)");
	}

	#[test]
	fn test_editor_position_multiple_items() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - task 1\n  - task 2\n  - task 3\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(7, 5)");
	}

	#[test]
	fn test_editor_position_with_nesting() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - Phase 1\n    - task a\n  - Phase 2\n    - task b\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(8, 7)");
	}

	#[test]
	fn test_editor_position_before_sub_issues() {
		let content = "- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->\n\n  Body\n\n  # Blockers\n  - blocker task\n\n  - [ ] Sub issue <!--sub https://github.com/owner/repo/issues/2 -->\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(format!("{:?}", issue.editor_position()), @"(5, 5)");
	}

	#[test]
	fn test_display_renders_children_as_links() {
		let content = "- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child 1 <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child 1 body\n\n  - [ ] Child 2 <!--sub https://github.com/owner/repo/issues/3 -->\n\n    Child 2 body\n";
		let issue = unsafe_mock_parse_virtual(content);
		assert_eq!(issue.children.len(), 2);
		// Display embeds children as one-line links, not their content
		insta::assert_snapshot!(issue.to_string(), @"
		- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->
		  Parent body

		  - [ ] [Child 1](./2_-_Child_1.md) <!-- https://github.com/owner/repo/issues/2 -->
		  - [ ] [Child 2](./3_-_Child_2.md) <!-- https://github.com/owner/repo/issues/3 -->
		");
	}

	#[test]
	fn test_display_roundtrip_blocker_escaping() {
		let cases = vec![
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - `insert` semantics on `RoutingHub`\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - `insert`semantics on`RoutingHub`\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - move clap interface into \\_strategy\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - some certainty\\*val range\n",
			"- [ ] Title <!-- https://github.com/owner/repo/issues/1 -->\n  # Blockers\n  - text with ` lone backtick\n",
		];

		for initial_content in cases {
			let issue = unsafe_mock_parse_virtual(initial_content);
			let s1 = issue.to_string();
			for cycle in 1..=5 {
				let re = unsafe_mock_parse_virtual(&s1);
				let sn = re.to_string();
				assert_eq!(s1, sn, "Display not idempotent at cycle {cycle} for input: {initial_content:?}");
			}
		}
	}

	#[test]
	fn test_display_includes_children() {
		let content =
			"- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->\n\n  Parent body\n\n  - [ ] Child 1 <!--sub https://github.com/owner/repo/issues/2 -->\n\n    Child 1 body\n";
		let issue = unsafe_mock_parse_virtual(content);
		insta::assert_snapshot!(issue.to_string(), @"
		- [ ] Parent <!-- https://github.com/owner/repo/issues/1 -->
		  Parent body

		  - [ ] [Child 1](./2_-_Child_1.md) <!-- https://github.com/owner/repo/issues/2 -->
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
		let serialized = issue.to_string();
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
		assert!(matches!(vi.contents.blockers.set_state, Some(crate::BlockerSetState::Pending)));
		// !s must not appear in re-serialized output, blockers preserved
		insta::assert_snapshot!(unsafe_mock_parse_virtual(content).to_string(), @"
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
		assert!(matches!(vi.contents.blockers.set_state, Some(crate::BlockerSetState::Pending)));
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

		assert!(matches!(virtual_issue.contents.blockers.set_state, Some(crate::BlockerSetState::Pending)));
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
		let s1 = issue.to_string();
		let issue2 = unsafe_mock_parse_virtual(&s1);
		let s2 = issue2.to_string();
		assert_eq!(s1, s2, "Display must be idempotent");
	}

	#[test]
	fn test_serialize_roundtrip_custom_checkboxes_idempotent() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Parent <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  Body\n\n  - [-] Not planned <!--sub @mock_user https://github.com/o/r/issues/2 -->\n\n    np body\n\n  - [42] Duplicate <!--sub @mock_user https://github.com/o/r/issues/3 -->\n\n    dup body\n";
		let issue = unsafe_mock_parse_virtual(input);
		let s1 = issue.to_string();
		for cycle in 1..=5 {
			let re = unsafe_mock_parse_virtual(&s1);
			let sn = re.to_string();
			assert_eq!(s1, sn, "Display must be idempotent at cycle {cycle}");
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

	#[test]
	fn test_body_blank_lines_preserved_roundtrip() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  some text\n\n  ```rust\n  fn main() {\n    println!(\"hello\");\n  }\n  ```\n\n  more text\n";
		let issue = unsafe_mock_parse_virtual(input);
		let serialized = issue.to_string();
		insta::assert_snapshot!(serialized, @r#"
		- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->
		  some text

		  ````rust
		  fn main() {
		    println!("hello");
		  }
		  ````

		  more text
		"#);
	}

	#[test]
	fn test_body_multiple_paragraphs_preserved_roundtrip() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  first paragraph\n\n  second paragraph\n\n  third paragraph\n";
		let issue = unsafe_mock_parse_virtual(input);
		let serialized = issue.to_string();
		insta::assert_snapshot!(serialized, @"
		- [ ] Issue <!-- https://github.com/owner/repo/issues/1 -->
		  first paragraph

		  second paragraph

		  third paragraph
		");
	}

	#[test]
	fn test_body_blank_lines_idempotent() {
		crate::current_user::set("mock_user".to_string());
		let input = "- [ ] Issue <!-- @mock_user https://github.com/o/r/issues/1 -->\n\n  some text\n\n  ```rust\n  fn main() {\n    println!(\"hello\");\n  }\n  ```\n\n  more text\n";
		let issue = unsafe_mock_parse_virtual(input);
		let s1 = issue.to_string();
		let issue2 = unsafe_mock_parse_virtual(&s1);
		let s2 = issue2.to_string();
		assert_eq!(s1, s2, "body blank lines must be preserved idempotently");
	}
}
