//! Issue addressing primitives: repo identity, links, and refs.
//!
//! How an issue is *named* and *located* — shared by the issue model, blockers,
//! and milestones. The index/selector machinery that walks these lives in `locate`.

use std::fmt;

use arrayvec::ArrayString;
use url::Url;

/// Which local project stores an issue: a Github repo, or the single owner-less `virtual` store.
/// Uses fixed-size `ArrayString`s to be `Copy`. GitHub limits: owner max 39 chars, repo max 100 chars.
/// `virtual` is reserved as an owner name so it can't collide with the virtual store's directory.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum RepoInfo {
	Github {
		/// Repository owner (fixed max length; following Github spec)
		owner: ArrayString<39>,
		/// Repository name (fixed max length; following Github spec)
		repo: ArrayString<100>,
	},
	/// The owner-less local `virtual` project (local-only, never synced to Github).
	Virtual,
}

impl RepoInfo {
	/// Create a Github RepoInfo.
	/// Owner and repo are lowercased: GitHub treats them case-insensitively,
	/// so this is a primitive-level invariant (every `RepoInfo` is normalized).
	/// Panics if owner exceeds 39 chars or repo exceeds 100 chars.
	pub fn new(owner: &str, repo: &str) -> Self {
		Self::Github {
			owner: ArrayString::from(&owner.to_lowercase()).expect("owner name too long (max 39 chars)"),
			repo: ArrayString::from(&repo.to_lowercase()).expect("repo name too long (max 100 chars)"),
		}
	}

	/// Get the owner, or `None` for the owner-less virtual store.
	pub fn owner(&self) -> Option<&str> {
		match self {
			Self::Github { owner, .. } => Some(owner.as_str()),
			Self::Virtual => None,
		}
	}

	/// Get the repo (the virtual store's single project is named `virtual`).
	pub fn repo(&self) -> &str {
		match self {
			Self::Github { repo, .. } => repo.as_str(),
			Self::Virtual => "virtual",
		}
	}

	pub fn is_virtual(&self) -> bool {
		matches!(self, Self::Virtual)
	}
}

impl From<(&str, &str)> for RepoInfo {
	fn from((owner, repo): (&str, &str)) -> Self {
		Self::new(owner, repo)
	}
}

impl fmt::Display for RepoInfo {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Github { owner, repo } => write!(f, "{owner}/{repo}"),
			Self::Virtual => write!(f, "virtual"),
		}
	}
}

/// A URI addressing an issue: an owned Github issue (a real upstream issue you own, identified by
/// its URL), or an owner-less local virtual issue (identified by its file path). Each carries the
/// value that both *names* the issue and *locates* it — a Github URL points at the upstream, a
/// path points at the file.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IssueLink {
	/// `https://github.com/{owner}/{repo}/issues/{number}`
	Owned(Url),
	/// Absolute path to a local virtual issue's file under the `virtual` store. The path is the
	/// id: it doubles as the issue's location (loadable, `gf`-jumpable) and encodes its number.
	Virtual(std::path::PathBuf),
}

impl IssueLink {
	/// Create from a Github issue URL. Returns None if not a valid Github issue URL.
	pub fn new(mut url: Url) -> Option<Self> {
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
		// Normalize owner/repo to lowercase: Github treats them case-insensitively,
		// so every reader sees the same canonical form.
		let normalized: Vec<String> = segments.iter().enumerate().map(|(i, s)| if i < 2 { s.to_lowercase() } else { s.to_string() }).collect();
		url.path_segments_mut().ok()?.clear().extend(normalized.iter().map(String::as_str));
		Some(Self::Owned(url))
	}

	/// Parse a link: a Github issue URL → `Owned`; a path with a `virtual` component whose
	/// filename encodes an issue number → `Virtual`.
	pub fn parse(s: &str) -> Option<Self> {
		if s.contains("://") {
			return Self::new(Url::parse(s).ok()?);
		}
		let path = std::path::Path::new(s);
		if path.components().any(|c| c.as_os_str() == "virtual") && number_from_path(path).is_some() {
			return Some(Self::Virtual(path.to_path_buf()));
		}
		None
	}

	/// The local project that stores this issue.
	pub fn project(&self) -> RepoInfo {
		match self {
			Self::Owned(url) => {
				let mut segments = url.path_segments().unwrap();
				RepoInfo::new(segments.next().unwrap(), segments.next().unwrap())
			}
			Self::Virtual(_) => RepoInfo::Virtual,
		}
	}

	/// The issue number.
	pub fn number(&self) -> u64 {
		match self {
			Self::Owned(url) => url.path_segments().unwrap().nth(3).unwrap().parse().unwrap(),
			Self::Virtual(path) => number_from_path(path).expect("virtual link path encodes an issue number"),
		}
	}

	/// The remote URL. Panics on a virtual link (it has no upstream).
	pub fn url(&self) -> &Url {
		match self {
			Self::Owned(url) => url,
			Self::Virtual(_) => panic!("virtual issue has no remote URL"),
		}
	}

	/// The Github link for issue `n` in `project`. Github-only — virtual issues are addressed
	/// by their file path, so build those with `IssueLink::Virtual` from a resolved path.
	pub fn in_project(project: RepoInfo, n: u64) -> Self {
		match project {
			RepoInfo::Github { owner, repo } => Self::parse(&format!("https://github.com/{owner}/{repo}/issues/{n}")).expect("constructed URL must be valid"),
			RepoInfo::Virtual => panic!("in_project is github-only; virtual issues are addressed by file path"),
		}
	}
}

/// Extract an issue number from a stored issue path: `{n}_-_{title}.md[.bak]`, bare `{n}`, or a
/// directory-format `{n}_-_{title}/__main__.md` (number taken from the parent dir).
fn number_from_path(path: &std::path::Path) -> Option<u64> {
	let name = path.file_name()?.to_str()?;
	let source = if name.starts_with(crate::MAIN_ISSUE_FILENAME) {
		path.parent()?.file_name()?.to_str()?
	} else {
		name
	};
	let base = source.strip_suffix(".md.bak").or_else(|| source.strip_suffix(".md")).unwrap_or(source);
	base.split("_-_").next().unwrap_or(base).parse().ok()
}

impl fmt::Display for IssueLink {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Owned(url) => write!(f, "{}", url.as_str()),
			Self::Virtual(path) => write!(f, "{}", path.display()),
		}
	}
}

/// A Github milestone identifier. Wraps a URL and derives properties on demand.
/// Format: `https://github.com/{owner}/{repo}/milestone/{number}`
#[derive(Clone, Debug, derive_more::Deref, Eq, Hash, PartialEq)]
pub struct MilestoneLink(Url);

impl MilestoneLink {
	/// Create from a URL. Returns None if not a valid Github milestone URL.
	pub fn new(mut url: Url) -> Option<Self> {
		if url.host_str() != Some("github.com") {
			return None;
		}
		let segments: Vec<_> = url.path_segments()?.collect();
		if segments.len() < 4 || segments[2] != "milestone" {
			return None;
		}
		segments[3].parse::<u64>().ok()?;
		let normalized: Vec<String> = segments.iter().enumerate().map(|(i, s)| if i < 2 { s.to_lowercase() } else { s.to_string() }).collect();
		url.path_segments_mut().ok()?.clear().extend(normalized.iter().map(String::as_str));
		Some(Self(url))
	}

	pub fn parse(url: &str) -> Option<Self> {
		Self::new(Url::parse(url).ok()?)
	}

	pub fn repo_info(&self) -> RepoInfo {
		let mut segments = self.0.path_segments().unwrap();
		RepoInfo::new(segments.next().unwrap(), segments.next().unwrap())
	}

	pub fn number(&self) -> u64 {
		self.0.path_segments().unwrap().nth(3).unwrap().parse().unwrap()
	}

	pub fn as_str(&self) -> &str {
		self.0.as_str()
	}
}

/// A link to either node kind of task space: an issue or a milestone.
/// The unit of selection paths — (de)serializes as the bare URL, disambiguated
/// by the `issues`/`milestone` path segment.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum NodeLink {
	Issue(IssueLink),
	Milestone(MilestoneLink),
}

impl NodeLink {
	pub fn parse(url: &str) -> Option<Self> {
		if let Some(link) = IssueLink::parse(url) {
			return Some(Self::Issue(link));
		}
		MilestoneLink::parse(url).map(Self::Milestone)
	}
}

impl fmt::Display for NodeLink {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Issue(link) => write!(f, "{link}"),
			Self::Milestone(link) => write!(f, "{}", link.as_str()),
		}
	}
}

/// A parsed milestone reference. URL-only for now (no established shorthand syntax).
#[derive(Clone, Debug, PartialEq)]
pub struct MilestoneRef(pub MilestoneLink);

impl MilestoneRef {
	/// Try to parse a single word as a milestone reference (a bare milestone URL).
	pub fn parse_word(word: &str) -> Option<Self> {
		MilestoneLink::parse(word).map(Self)
	}

	pub fn to_milestone_link(&self) -> MilestoneLink {
		self.0.clone()
	}
}

impl fmt::Display for MilestoneRef {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.0.as_str())
	}
}

/// A parsed issue reference from text.
///
/// Recognizes three formats in plain text:
/// - Bare URL: `https://github.com/owner/repo/issues/N`
/// - Full shorthand: `owner/repo#N`
/// - Partial shorthand: `repo#N` or `#N` (needs context to resolve)
#[derive(Clone, Debug, PartialEq)]
pub enum IssueRef {
	/// A full GitHub issue URL.
	Url(IssueLink),
	/// Shorthand notation with optional owner/repo.
	Shorthand { owner: Option<String>, repo: Option<String>, number: u64 },
}

impl IssueRef {
	/// Try to parse a single word as an issue reference.
	///
	/// Recognizes bare URLs and shorthands (`owner/repo#N`, `repo#N`, `#N`).
	/// Returns `None` if the word doesn't match any known pattern.
	pub fn parse_word(word: &str) -> Option<Self> {
		// Bare URL, or a collapsed virtual ref (`virtual/{n}`)
		if let Some(link) = IssueLink::parse(word) {
			return Some(Self::Url(link));
		}

		// Try shorthand: owner/repo#N, repo#N, or #N
		let hash_pos = word.find('#')?;
		let before = &word[..hash_pos];
		let after = &word[hash_pos + 1..];
		let number: u64 = after.parse().ok()?;

		match before.find('/') {
			Some(slash_pos) => {
				// `owner/repo#number`
				if before[slash_pos + 1..].contains('/') {
					return None;
				}
				let owner = &before[..slash_pos];
				let repo = &before[slash_pos + 1..];
				if owner.is_empty() || repo.is_empty() {
					return None;
				}
				Some(Self::Shorthand {
					owner: Some(owner.to_string()),
					repo: Some(repo.to_string()),
					number,
				})
			}
			None => {
				if before.is_empty() {
					// Bare `#number` — needs parent context to resolve
					Some(Self::Shorthand { owner: None, repo: None, number })
				} else {
					// `repo#number`
					Some(Self::Shorthand {
						owner: None,
						repo: Some(before.to_string()),
						number,
					})
				}
			}
		}
	}

	/// Resolve this ref to an `IssueLink`, if enough information is present.
	pub fn to_issue_link(&self) -> Option<IssueLink> {
		match self {
			Self::Url(link) => Some(link.clone()),
			Self::Shorthand { owner, repo, number } => {
				let o = owner.as_ref()?;
				let r = repo.as_ref()?;
				let url = format!("https://github.com/{o}/{r}/issues/{number}");
				IssueLink::parse(&url)
			}
		}
	}

	/// Resolve missing owner/repo from a parent context string.
	///
	/// Context can be `"owner/repo"` or just `"repo"` (owner filled from `current_user`).
	pub fn resolve_with_context(&mut self, context: &str) {
		if let Self::Shorthand { owner, repo, .. } = self {
			if repo.is_none() {
				let (resolved_owner, resolved_repo) = parse_repo_context(context);
				*owner = Some(resolved_owner);
				*repo = Some(resolved_repo);
			}
			if owner.is_none()
				&& repo.is_some()
				&& let Some(user) = crate::current_user::get()
			{
				*owner = Some(user);
			}
		}
	}
}

impl fmt::Display for IssueRef {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Self::Url(link) => write!(f, "{link}"),
			Self::Shorthand { owner, repo, number } => match (owner, repo) {
				(Some(o), Some(r)) => write!(f, "{o}/{r}#{number}"),
				(None, Some(r)) => write!(f, "{r}#{number}"),
				(None, None) => write!(f, "#{number}"),
				(Some(_), None) => unreachable!("owner set without repo"),
			},
		}
	}
}

/// Parse a context string into (owner, repo).
///
/// `"owner/repo"` → `(owner, repo)`.
/// `"repo"` → `(current_user, repo)`.
pub fn parse_repo_context(text: &str) -> (String, String) {
	if let Some(slash) = text.find('/') {
		(text[..slash].to_string(), text[slash + 1..].to_string())
	} else {
		let owner = crate::current_user::get().unwrap_or_else(|| panic!("current_user must be set to resolve bare repo context '{text}'"));
		(owner, text.to_string())
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn repo_info_lowercases_owner_and_repo() {
		let info = RepoInfo::new("MyOrg", "MyRepo");
		assert_eq!(info.owner(), Some("myorg"));
		assert_eq!(info.repo(), "myrepo");

		let from_tuple: RepoInfo = ("FooBar", "Baz-Qux").into();
		assert_eq!((from_tuple.owner(), from_tuple.repo()), (Some("foobar"), "baz-qux"));
	}

	#[test]
	fn issue_link_normalizes_owner_and_repo() {
		let link = IssueLink::parse("https://github.com/MyOrg/MyRepo/issues/42").unwrap();
		assert_eq!(link.to_string(), "https://github.com/myorg/myrepo/issues/42");
		assert_eq!((link.project().owner(), link.project().repo()), (Some("myorg"), "myrepo"));
		assert_eq!(link.number(), 42);
	}

	#[test]
	fn virtual_link_roundtrip() {
		let path = "/data/issues/virtual/13_-_some_task.md";
		let link = IssueLink::parse(path).unwrap();
		assert_eq!(link, IssueLink::Virtual(path.into()));
		assert_eq!(link.number(), 13);
		assert_eq!(link.to_string(), path);
		assert!(link.project().is_virtual());
	}

	#[test]
	fn parse_bare_url() {
		let r = IssueRef::parse_word("https://github.com/owner/repo/issues/42").unwrap();
		assert!(matches!(r, IssueRef::Url(_)));
		let link = r.to_issue_link().unwrap();
		assert_eq!(link.project().owner(), Some("owner"));
		assert_eq!(link.project().repo(), "repo");
		assert_eq!(link.number(), 42);
	}

	#[test]
	fn parse_full_shorthand() {
		let r = IssueRef::parse_word("alice/myrepo#99").unwrap();
		assert_eq!(
			r,
			IssueRef::Shorthand {
				owner: Some("alice".into()),
				repo: Some("myrepo".into()),
				number: 99,
			}
		);
		let link = r.to_issue_link().unwrap();
		assert_eq!(link.number(), 99);
	}

	#[test]
	fn parse_repo_only() {
		let r = IssueRef::parse_word("myrepo#7").unwrap();
		assert_eq!(
			r,
			IssueRef::Shorthand {
				owner: None,
				repo: Some("myrepo".into()),
				number: 7,
			}
		);
		assert!(r.to_issue_link().is_none());
	}

	#[test]
	fn parse_bare_number() {
		let r = IssueRef::parse_word("#123").unwrap();
		assert_eq!(
			r,
			IssueRef::Shorthand {
				owner: None,
				repo: None,
				number: 123
			}
		);
	}

	#[test]
	fn reject_non_ref() {
		assert!(IssueRef::parse_word("hello world").is_none());
		assert!(IssueRef::parse_word("no-hash-here").is_none());
		assert!(IssueRef::parse_word("#notanumber").is_none());
		assert!(IssueRef::parse_word("a/b/c#1").is_none()); // too many slashes
		assert!(IssueRef::parse_word("/repo#1").is_none()); // empty owner
		assert!(IssueRef::parse_word("owner/#1").is_none()); // empty repo
	}
}
