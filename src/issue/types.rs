//! Core issue data structures and parsing/serialization.
//!
//! This module contains the pure Issue type with parsing and serialization.

use arrayvec::ArrayString;
use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use url::Url;

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

/// An issue with its title - used when we need both identity and display name.
/// This is what we have after fetching an issue from Github.
//DEPRECATE: completely pointless
#[derive(Clone, Debug)]
pub struct FetchedIssue {
	pub link: IssueLink,
	pub title: String,
}

impl FetchedIssue /*{{{1*/ {
	pub fn new(link: IssueLink, title: impl Into<String>) -> Self {
		Self { link, title: title.into() }
	}

	/// Create from owner, repo, number, and title (constructs the URL internally).
	pub fn from_parts(owner: &str, repo: &str, number: u64, title: impl Into<String>) -> Option<Self> {
		let url_str = format!("https://github.com/{owner}/{repo}/issues/{number}");
		let link = IssueLink::parse(&url_str)?;
		Some(Self { link, title: title.into() })
	}

	/// Convenience: get the issue number
	pub fn number(&self) -> u64 {
		self.link.number()
	}

	/// Convenience: get owner
	pub fn owner(&self) -> &str {
		self.link.owner()
	}

	/// Convenience: get repo
	pub fn repo(&self) -> &str {
		self.link.repo()
	}
}
//,}}}1

use super::{
	blocker::{BlockerSequence, classify_line, join_with_blockers},
	error::{ParseContext, ParseError},
	util::{is_blockers_marker, normalize_issue_indentation},
};

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
//,}}}1

/// Metadata for an issue linked to Github.
/// Lineage is stored separately in `Ancestry`.
#[derive(Clone, Debug, PartialEq)]
pub struct LinkedIssueMeta {
	/// User who created the issue
	pub user: String,
	/// Link to the issue on Github
	pub link: IssueLink,
	/// Timestamp of last content change (body/comments, not children).
	/// Used for sync conflict resolution. None if unknown.
	pub ts: Option<Timestamp>,
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
	Github(Option<LinkedIssueMeta>),
	/// Virtual issue - local only, never synced to Github.
	/// Used for projects without a Github remote.
	Virtual,
}

impl IssueRemote {
	/// Returns true if this is a Github issue (linked or pending).
	pub fn is_github(&self) -> bool {
		matches!(self, IssueRemote::Github(_))
	}

	/// Returns true if this is a virtual (local-only) issue.
	pub fn is_virtual(&self) -> bool {
		matches!(self, IssueRemote::Virtual)
	}

	/// Returns true if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		matches!(self, IssueRemote::Github(Some(_)))
	}

	/// Returns true if this is a pending Github issue (not yet created).
	pub fn is_pending(&self) -> bool {
		matches!(self, IssueRemote::Github(None))
	}

	/// Get the linked metadata if this is a linked Github issue.
	pub fn as_linked(&self) -> Option<&LinkedIssueMeta> {
		match self {
			IssueRemote::Github(Some(meta)) => Some(meta),
			_ => None,
		}
	}

	/// Get mutable access to the linked metadata.
	pub fn as_linked_mut(&mut self) -> Option<&mut LinkedIssueMeta> {
		match self {
			IssueRemote::Github(Some(meta)) => Some(meta),
			_ => None,
		}
	}
}

/// Identity of an issue - always has ancestry, with remote connection info.
#[derive(Clone, Debug)]
pub struct IssueIdentity {
	/// Where in the tree this issue lives (owner/repo/lineage)
	pub ancestry: Ancestry,
	/// Remote connection status (Github linked/pending, or Virtual)
	pub remote: IssueRemote,
}

impl IssueIdentity {
	/// Create a new linked Github issue identity.
	pub fn linked(ancestry: Ancestry, user: String, link: IssueLink, ts: Option<Timestamp>) -> Self {
		Self {
			ancestry,
			remote: IssueRemote::Github(Some(LinkedIssueMeta { user, link, ts })),
		}
	}

	/// Create a new pending Github issue identity (will be created on first sync).
	pub fn pending(ancestry: Ancestry) -> Self {
		Self {
			ancestry,
			remote: IssueRemote::Github(None),
		}
	}

	/// Create a new virtual (local-only) issue identity.
	pub fn virtual_issue(ancestry: Ancestry) -> Self {
		Self {
			ancestry,
			remote: IssueRemote::Virtual,
		}
	}

	/// Create a new local-only issue identity.
	/// DEPRECATED: use `pending()` for Github issues or `virtual_issue()` for local-only.
	/// This currently maps to `pending()` for backwards compatibility.
	pub fn local(ancestry: Ancestry) -> Self {
		Self::pending(ancestry)
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

	/// Get the timestamp if available.
	pub fn ts(&self) -> Option<Timestamp> {
		self.remote.as_linked().and_then(|m| m.ts)
	}

	/// Get ancestry (always available).
	pub fn ancestry(&self) -> &Ancestry {
		&self.ancestry
	}

	/// Get owner (always available via ancestry).
	pub fn owner(&self) -> &str {
		self.ancestry.owner()
	}

	/// Get repo (always available via ancestry).
	pub fn repo(&self) -> &str {
		self.ancestry.repo()
	}

	/// Create a child's ancestry by appending this issue's number.
	/// Returns None if this issue is not linked (has no number to append).
	pub fn child_ancestry(&self) -> Option<Ancestry> {
		self.number().map(|n| self.ancestry.child(n))
	}

	/// Encode for serialization.
	/// - Linked: `@user url`
	/// - Pending: `local:` (will be created on Github)
	/// - Virtual: `virtual:owner/repo#N` where N is a local tracking number
	pub fn encode(&self) -> String {
		match &self.remote {
			IssueRemote::Github(Some(meta)) => format!("@{} {}", meta.user, meta.link.as_str()),
			IssueRemote::Github(None) => "local:".to_string(),
			IssueRemote::Virtual => "virtual:".to_string(),
		}
	}
}

impl PartialEq for IssueIdentity {
	fn eq(&self, other: &IssueIdentity) -> bool {
		self.ancestry == other.ancestry
	}
}

/// Maximum nesting depth for issues (8 levels should be plenty).
const MAX_LINEAGE_DEPTH: usize = 8;

/// Ancestry information for an issue - where it lives in the filesystem.
/// This is always defined, even for pending issues.
/// Uses fixed-size storage to be `Copy`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Ancestry {
	/// Repository owner (fixed max length; following Github spec)
	owner: ArrayString<39>,
	/// Repository name (fixed max length; following Github spec)
	repo: ArrayString<100>,
	/// Chain of parent issue numbers from root to immediate parent.
	/// Empty for root issues. Uses fixed array with length tracking.
	lineage_arr: [u64; MAX_LINEAGE_DEPTH],
	lineage_len: u8,
}

impl Ancestry {
	/// Create ancestry for a root issue.
	/// Panics if owner or repo exceed MAX_NAME_LEN.
	pub fn root(owner: &str, repo: &str) -> Self {
		Self {
			owner: ArrayString::from(owner).expect("owner name too long"),
			repo: ArrayString::from(repo).expect("repo name too long"),
			lineage_arr: [0; MAX_LINEAGE_DEPTH],
			lineage_len: 0,
		}
	}

	/// Create ancestry with a pre-existing lineage.
	/// Panics if [owner](Ancestry::owner)/[repo](Ancestry::repo) str are too large or lineage exceeds [MAX_LINEAGE_DEPTH].
	pub fn with_lineage(owner: &str, repo: &str, lineage: &[u64]) -> Self {
		assert!(lineage.len() <= MAX_LINEAGE_DEPTH, "Issue nesting too deep (max {MAX_LINEAGE_DEPTH} levels)");
		let mut lineage_arr = [0; MAX_LINEAGE_DEPTH];
		lineage_arr[..lineage.len()].copy_from_slice(lineage);
		Self {
			owner: ArrayString::from(owner).expect("owner name too long"),
			repo: ArrayString::from(repo).expect("repo name too long"),
			lineage_arr,
			lineage_len: lineage.len() as u8,
		}
	}

	/// Create ancestry for a child issue.
	/// Panics if lineage would exceed MAX_LINEAGE_DEPTH.
	pub fn child(&self, parent_number: u64) -> Self {
		let new_len = self.lineage_len as usize + 1;
		assert!(new_len <= MAX_LINEAGE_DEPTH, "Issue nesting too deep (max {MAX_LINEAGE_DEPTH} levels)");
		let mut new_arr = self.lineage_arr;
		new_arr[self.lineage_len as usize] = parent_number;
		Self {
			owner: self.owner,
			repo: self.repo,
			lineage_arr: new_arr,
			lineage_len: new_len as u8,
		}
	}

	/// Get the lineage as a slice.
	pub fn lineage(&self) -> &[u64] {
		&self.lineage_arr[..self.lineage_len as usize]
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

/// Parsed identity info from title line (internal helper).
/// Represents what we can extract from the HTML comment marker.
enum ParsedIdentityInfo {
	/// Linked to Github: `@user url`
	Linked { user: String, link: IssueLink },
	/// Pending creation on Github: `local:` or empty
	Pending,
	/// Virtual (local-only, never sync): `virtual:`
	Virtual,
}

/// Parsed title line components (internal helper)
struct ParsedTitleLine {
	title: String,
	/// Identity info parsed from the title line
	identity_info: ParsedIdentityInfo,
	close_state: CloseState,
	labels: Vec<String>,
}

/// Complete representation of an issue file
#[derive(Clone, Debug, PartialEq)]
pub struct Issue {
	/// Identity - linked to Github or local only
	pub identity: IssueIdentity,
	pub contents: IssueContents,
	/// Sub-issues in order
	pub children: Vec<Issue>,
}

impl Issue /*{{{1*/ {
	/// Check if this issue is local only (not yet on Github).
	pub fn is_local(&self) -> bool {
		self.identity.is_local()
	}

	/// Create an empty local issue with the given ancestry.
	/// Used for comparison when an issue doesn't exist yet.
	pub fn empty_local(ancestry: Ancestry) -> Self {
		Self {
			identity: IssueIdentity::local(ancestry),
			contents: IssueContents::default(),
			children: vec![],
		}
	}

	/// Check if this issue is linked to Github.
	pub fn is_linked(&self) -> bool {
		self.identity.is_linked()
	}

	/// Get the issue number if linked to Github.
	pub fn number(&self) -> Option<u64> {
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

	/// Get the timestamp if available.
	pub fn ts(&self) -> Option<Timestamp> {
		self.identity.ts()
	}

	/// Get ancestry (always available).
	pub fn ancestry(&self) -> &Ancestry {
		self.identity.ancestry()
	}

	/// Get the full issue body including blockers section.
	/// This is what should be synced to Github as the issue body.
	pub fn body(&self) -> String {
		let base_body = self.contents.comments.first().map(|c| c.body.render()).unwrap_or_default();
		join_with_blockers(&base_body, &self.contents.blockers)
	}

	/// Parse virtual representation (markdown with full tree) into an Issue.
	///
	/// For linked issues, ancestry is derived from the URL in the content.
	/// For pending/virtual issues, `ancestry` must be provided or parsing will fail.
	pub fn parse_virtual_with_ancestry(content: &str, path: &std::path::Path, ancestry: Ancestry) -> Result<Self, ParseError> {
		let ctx = ParseContext::new(content.to_string(), path.display().to_string());

		let normalized = normalize_issue_indentation(content);
		let mut lines = normalized.lines().peekable();

		Self::parse_virtual_at_depth(&mut lines, 0, 1, &ctx, Some(&ancestry))
	}

	/// Parse virtual representation (markdown with full tree) into an Issue.
	///
	/// For linked issues, ancestry is derived from the URL in the content.
	/// For pending/virtual root issues, this will panic - use `parse_virtual_with_ancestry` instead.
	pub fn parse_virtual(content: &str, path: &std::path::Path) -> Result<Self, ParseError> {
		let ctx = ParseContext::new(content.to_string(), path.display().to_string());

		let normalized = normalize_issue_indentation(content);
		let mut lines = normalized.lines().peekable();

		Self::parse_virtual_at_depth(&mut lines, 0, 1, &ctx, None)
	}

	/// Parse virtual representation at given nesting depth.
	/// `parent_ancestry` is the ancestry of the parent issue (if any) - used to derive child ancestry.
	fn parse_virtual_at_depth(
		lines: &mut std::iter::Peekable<std::str::Lines>,
		depth: usize,
		line_num: usize,
		ctx: &ParseContext,
		parent_ancestry: Option<&Ancestry>,
	) -> Result<Self, ParseError> {
		let indent = "\t".repeat(depth);
		let child_indent = "\t".repeat(depth + 1);

		// Parse title line: `- [ ] [label1, label2] Title <!--url-->` or `- [ ] Title <!--immutable url-->`
		let first_line = lines.next().ok_or(ParseError::EmptyFile)?;
		let title_content = first_line.strip_prefix(&indent).ok_or_else(|| ParseError::BadIndentation {
			src: ctx.named_source(),
			span: ctx.line_span(line_num),
			expected_tabs: depth,
		})?;
		let parsed = Self::parse_title_line(title_content, line_num, ctx)?;

		let mut comments = Vec::new();
		let mut children = Vec::new();
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
			if is_blockers_marker(content) {
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
							return Err(ParseError::InvalidCheckbox {
								src: ctx.named_source(),
								span: ctx.line_span(current_line),
								content: invalid_content,
							});
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
						return Err(ParseError::InvalidCheckbox {
							src: ctx.named_source(),
							span: ctx.line_span(current_line),
							content: invalid_content,
						});
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
				// Build child's parent ancestry from this issue's identity info
				let child_parent_ancestry = match &parsed.identity_info {
					ParsedIdentityInfo::Linked { link, .. } => {
						// Parent is linked - child ancestry extends from it
						let base_ancestry = parent_ancestry
							.map(|a| a.child(link.number()))
							.unwrap_or_else(|| Ancestry::root(link.owner(), link.repo()).child(link.number()));
						Some(base_ancestry)
					}
					ParsedIdentityInfo::Pending | ParsedIdentityInfo::Virtual => {
						// Local/virtual parent - pass through parent ancestry (Copy)
						parent_ancestry.copied()
					}
				};
				let child_content = child_lines.join("\n");
				let mut child_lines_iter = child_content.lines().peekable();
				let child = Self::parse_virtual_at_depth(&mut child_lines_iter, 0, current_line, ctx, child_parent_ancestry.as_ref())?;
				children.push(child);
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
			ParsedIdentityInfo::Linked { user, link } => {
				// Linked issues get ancestry from the URL, optionally extended by parent
				let ancestry = parent_ancestry.copied().unwrap_or_else(|| Ancestry::root(link.owner(), link.repo()));
				IssueIdentity::linked(ancestry, user, link, None)
			}
			ParsedIdentityInfo::Pending => {
				// Pending issues require ancestry from parent/caller
				let ancestry = parent_ancestry
					.copied()
					.expect("BUG: pending issue without ancestry. Use parse_virtual_with_ancestry for pending root issues.");
				IssueIdentity::pending(ancestry)
			}
			ParsedIdentityInfo::Virtual => {
				// Virtual issues require ancestry from parent/caller
				let ancestry = parent_ancestry
					.copied()
					.expect("BUG: virtual issue without ancestry. Use parse_virtual_with_ancestry for virtual root issues.");
				IssueIdentity::virtual_issue(ancestry)
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

	/// Parse title line: `- [ ] [label1, label2] Title <!--url-->` or `- [ ] Title <!--immutable url-->`
	/// Also supports `- [-]` for not-planned and `- [123]` for duplicates.
	fn parse_title_line(line: &str, line_num: usize, ctx: &ParseContext) -> Result<ParsedTitleLine, ParseError> {
		// Parse checkbox: `- [CONTENT] `
		let (close_state, rest) = match Self::parse_checkbox_prefix_detailed(line) {
			CheckboxParseResult::Ok(state, rest) => (state, rest),
			CheckboxParseResult::NotCheckbox => {
				return Err(ParseError::InvalidTitle {
					src: ctx.named_source(),
					span: ctx.line_span(line_num),
					detail: format!("got: {line:?}"),
				});
			}
			CheckboxParseResult::InvalidContent(content) => {
				return Err(ParseError::InvalidCheckbox {
					src: ctx.named_source(),
					span: ctx.line_span(line_num),
					content,
				});
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

		let marker_start = rest.find("<!--").ok_or_else(|| ParseError::MissingUrlMarker {
			src: ctx.named_source(),
			span: ctx.line_span(line_num),
		})?;
		let marker_end = rest.find("-->").ok_or_else(|| ParseError::MalformedUrlMarker {
			src: ctx.named_source(),
			span: ctx.line_span(line_num),
		})?;
		if marker_end <= marker_start {
			return Err(ParseError::MalformedUrlMarker {
				src: ctx.named_source(),
				span: ctx.line_span(line_num),
			});
		}

		let title = rest[..marker_start].trim().to_string();
		let inner = rest[marker_start + 4..marker_end].trim();

		// Handle both root format `<!-- @user url -->` and sub format `<!--sub @user url -->`
		let inner = inner.strip_prefix("sub ").unwrap_or(inner);

		// Parse identity info from marker content
		let identity_info = Self::parse_identity_info(inner);

		Ok(ParsedTitleLine {
			title,
			identity_info,
			close_state,
			labels,
		})
	}

	/// Parse identity info from the HTML comment content.
	/// Formats:
	/// - `@user url` for linked Github issues (full format with author)
	/// - `url` for linked Github issues (bare URL, author unknown)
	/// - `local:` or empty for pending Github issues
	/// - `virtual:` for virtual (local-only) issues
	fn parse_identity_info(s: &str) -> ParsedIdentityInfo {
		let s = s.trim();

		// Empty marker means pending
		if s.is_empty() {
			return ParsedIdentityInfo::Pending;
		}

		// Virtual format: `virtual:` - local only, never sync
		if s.starts_with("virtual:") {
			return ParsedIdentityInfo::Virtual;
		}

		// Local format: `local:` - pending creation on Github
		if s.starts_with("local:") {
			return ParsedIdentityInfo::Pending;
		}

		// Linked format with user: `@username https://github.com/...`
		if let Some(rest) = s.strip_prefix('@')
			&& let Some(space_idx) = rest.find(' ')
		{
			let user = rest[..space_idx].to_string();
			let url = rest[space_idx + 1..].trim();
			if let Some(link) = IssueLink::parse(url) {
				return ParsedIdentityInfo::Linked { user, link };
			}
		}

		// Default to pending for unrecognized formats
		ParsedIdentityInfo::Pending
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

		// Check for sub marker
		if let Some(marker_start) = rest.find("<!--sub ") {
			if rest.find("-->").is_none() {
				return ChildTitleParseResult::NotChildTitle;
			};
			let title = rest[..marker_start].trim();
			if title.is_empty() { ChildTitleParseResult::NotChildTitle } else { ChildTitleParseResult::Ok }
		} else if !rest.contains("<!--") {
			let title = rest.trim();
			if !title.is_empty() {
				ChildTitleParseResult::Ok
			} else {
				ChildTitleParseResult::NotChildTitle
			}
		} else {
			ChildTitleParseResult::NotChildTitle
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

		// Title line - root uses `<!-- @user url -->`, children use `<!--sub @user url -->`
		let checked = self.contents.state.to_checkbox();
		let identity_part = self.identity.encode();
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("[{}] ", self.contents.labels.join(", "))
		};
		let marker = if depth == 0 { " " } else { "sub " };
		let is_owned = self.user().is_some_and(crate::current_user::is);
		out.push_str(&format!("{indent}- [{checked}] {labels_part}{} <!--{marker}{identity_part} -->\n", self.contents.title));

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
		for child in &self.children {
			if out.lines().last().is_some_and(|l| !l.trim().is_empty()) {
				out.push_str(&format!("{content_indent}\n"));
			}

			// For closed children, we need to wrap the body in vim folds
			// But still recurse for the full structure
			if child.contents.state.is_closed() {
				// Output child title line
				let child_checked = child.contents.state.to_checkbox();
				let child_identity_part = child.identity.encode();
				let child_labels_part = if child.contents.labels.is_empty() {
					String::new()
				} else {
					format!("[{}] ", child.contents.labels.join(", "))
				};
				out.push_str(&format!(
					"{content_indent}- [{child_checked}] {child_labels_part}{} <!--sub {child_identity_part} -->\n",
					child.contents.title
				));

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
		let identity_part = self.identity.encode();
		let labels_part = if self.contents.labels.is_empty() {
			String::new()
		} else {
			format!("[{}] ", self.contents.labels.join(", "))
		};
		let is_owned = self.user().is_some_and(crate::current_user::is);
		out.push_str(&format!("- [{checked}] {labels_part}{} <!-- {identity_part} -->\n", self.contents.title));

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
	pub fn deserialize_virtual(content: &str) -> Result<Self, ParseError> {
		Self::parse_virtual(content, std::path::Path::new("virtual.md"))
	}

	/// Get a mutable reference to a child issue by path
	pub fn get_child_mut(&mut self, path: &[usize]) -> Option<&mut Issue> {
		if path.is_empty() {
			return Some(self);
		}
		let mut current = self;
		for &idx in path.iter().take(path.len() - 1) {
			current = current.children.get_mut(idx)?;
		}
		current.children.get_mut(*path.last()?)
	}

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
			current = current.children.iter().find(|c| c.number() == Some(num))?;
		}
		Some(current)
	}

	/// Get a mutable reference to a descendant by lineage.
	/// Returns None if the path doesn't exist.
	pub fn get_mut(&mut self, lineage: &[u64]) -> Option<&mut Issue> {
		let mut current = self;
		for &num in lineage {
			current = current.children.iter_mut().find(|c| c.number() == Some(num))?;
		}
		Some(current)
	}
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
			.iter()
			.find(|child| child.number() == Some(issue_number))
			.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
	}
}

impl std::ops::IndexMut<u64> for Issue {
	/// Index into children by issue number (mutable).
	/// Panics if no child with that number exists.
	fn index_mut(&mut self, issue_number: u64) -> &mut Self::Output {
		self.children
			.iter_mut()
			.find(|child| child.number() == Some(issue_number))
			.unwrap_or_else(|| panic!("no child with issue number {issue_number}"))
	}
}

/// Trait for lazy loading of issues from a source.
///
/// `S` is a marker type (e.g., `Local` for filesystem, `Remote` for GitHub).
/// The associated `Source` type is what's actually passed to methods.
/// Methods load data on demand; `&mut self` allows caching intermediate results.
#[allow(async_fn_in_trait)]
pub trait LazyIssue<S> {
	/// The actual source reference type (e.g., `PathBuf` for Local, `IssueLink` for Remote).
	type Source;

	async fn identity(&mut self, source: Self::Source) -> IssueIdentity;
	async fn contents(&mut self, source: Self::Source) -> IssueContents;
	async fn children(&mut self, source: Self::Source) -> Vec<Issue>;
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
		use std::path::Path;

		// Invalid checkbox on root issue
		let content = "- [abc] Invalid issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody\n";
		let result = Issue::parse_virtual(content, Path::new("test.md"));
		assert!(matches!(result, Err(ParseError::InvalidCheckbox { content, .. }) if content == "abc"));

		// Invalid checkbox on sub-issue
		let content = "- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t- [xyz] Bad sub <!--sub @owner https://github.com/owner/repo/issues/2 -->\n";
		let result = Issue::parse_virtual(content, Path::new("test.md"));
		assert!(matches!(result, Err(ParseError::InvalidCheckbox { content, .. }) if content == "xyz"));
	}

	#[test]
	fn test_parse_and_serialize_not_planned() {
		use std::path::Path;

		let content = "- [-] Not planned issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody text\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		assert_eq!(issue.contents.state, CloseState::NotPlanned);
		assert_eq!(issue.contents.title, "Not planned issue");

		// Verify serialization preserves the state
		let serialized = issue.serialize_virtual();
		assert!(serialized.starts_with("- [-] Not planned issue"));
	}

	#[test]
	fn test_parse_and_serialize_duplicate() {
		use std::path::Path;

		let content = "- [456] Duplicate issue <!-- @owner https://github.com/owner/repo/issues/123 -->\n\tBody text\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		assert_eq!(issue.contents.state, CloseState::Duplicate(456));
		assert_eq!(issue.contents.title, "Duplicate issue");

		// Verify serialization preserves the state
		let serialized = issue.serialize_virtual();
		assert!(serialized.starts_with("- [456] Duplicate issue"));
	}

	#[test]
	fn test_parse_sub_issue_close_types() {
		use std::path::Path;

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
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		insta::assert_snapshot!(issue.serialize_virtual(), @"
		- [ ] Parent issue <!-- @owner https://github.com/owner/repo/issues/1 -->
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
		");
	}

	#[test]
	fn test_find_last_blocker_position_empty() {
		use std::path::Path;

		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		assert!(issue.find_last_blocker_position().is_none());
	}

	#[test]
	fn test_find_last_blocker_position_single_item() {
		use std::path::Path;

		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- task 1\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 5); // Line 5: `\t- task 1`
		// Column 4: 1-indexed position of first char after `\t- ` (tab=1, dash=2, space=3, 't'=4)
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_multiple_items() {
		use std::path::Path;

		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- task 1\n\t- task 2\n\t- task 3\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 7); // Line 7: `\t- task 3`
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_with_headers() {
		use std::path::Path;

		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t# Phase 1\n\t- task a\n\t# Phase 2\n\t- task b\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 8); // Line 8: `\t- task b`
		assert_eq!(col, 4);
	}

	#[test]
	fn test_find_last_blocker_position_before_sub_issues() {
		use std::path::Path;

		let content = "- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->\n\tBody\n\n\t# Blockers\n\t- blocker task\n\n\t- [ ] Sub issue <!--sub @owner https://github.com/owner/repo/issues/2 -->\n";
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		let pos = issue.find_last_blocker_position();
		assert!(pos.is_some());
		let (line, col) = pos.unwrap();
		assert_eq!(line, 5); // Line 5: `\t- blocker task`, not the sub-issue line
		assert_eq!(col, 4);
	}

	#[test]
	fn test_serialize_filesystem_no_children() {
		use std::path::Path;

		// Issue with children
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child 1 <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child 1 body

	- [ ] Child 2 <!--sub @owner https://github.com/owner/repo/issues/3 -->
		Child 2 body
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		assert_eq!(issue.children.len(), 2);

		// Filesystem serialization should NOT include children
		let fs_serialized = issue.serialize_filesystem();
		assert!(!fs_serialized.contains("Child 1"));
		assert!(!fs_serialized.contains("Child 2"));
		assert!(fs_serialized.contains("Parent body"));
	}

	#[test]
	fn test_serialize_virtual_includes_children() {
		use std::path::Path;

		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child 1 <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child 1 body
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		// Virtual serialization should include children
		let virtual_serialized = issue.serialize_virtual();
		assert!(virtual_serialized.contains("Parent body"));
		assert!(virtual_serialized.contains("Child 1"));
		assert!(virtual_serialized.contains("Child 1 body"));
	}

	#[test]
	fn test_parse_virtual_includes_inline_children() {
		use std::path::Path;

		// parse_virtual includes inline children (used for virtual file format)
		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();
		// parse_virtual preserves inline children
		assert_eq!(issue.children.len(), 1);
		assert_eq!(issue.contents.title, "Parent");
		assert_eq!(issue.children[0].contents.title, "Child");
	}

	#[test]
	fn test_virtual_roundtrip() {
		use std::path::Path;

		let content = r#"- [ ] Parent <!-- @owner https://github.com/owner/repo/issues/1 -->
	Parent body

	- [ ] Child <!--sub @owner https://github.com/owner/repo/issues/2 -->
		Child body
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

		// Serialize to virtual, then deserialize back
		let serialized = issue.serialize_virtual();
		let reparsed = Issue::deserialize_virtual(&serialized).unwrap();

		assert_eq!(issue.contents.title, reparsed.contents.title);
		assert_eq!(issue.children.len(), reparsed.children.len());
	}

	#[test]
	fn test_serialize_github_body_only() {
		use std::path::Path;

		let content = r#"- [ ] Issue <!-- @owner https://github.com/owner/repo/issues/1 -->
	This is the body text.

	# Blockers
	- task 1
	- task 2
"#;
		let issue = Issue::parse_virtual(content, Path::new("test.md")).unwrap();

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
}
