//! Shared issue reference parsing.
//!
//! Recognizes three formats in plain text:
//! - Bare URL: `https://github.com/owner/repo/issues/N`
//! - Full shorthand: `owner/repo#N`
//! - Partial shorthand: `repo#N` or `#N` (needs context to resolve)

use std::fmt;

use super::IssueLink;

/// A parsed issue reference from text.
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
		// Try bare URL first
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
			Self::Url(link) => write!(f, "{}", link.as_str()),
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
	fn parse_bare_url() {
		let r = IssueRef::parse_word("https://github.com/owner/repo/issues/42").unwrap();
		assert!(matches!(r, IssueRef::Url(_)));
		let link = r.to_issue_link().unwrap();
		assert_eq!(link.owner(), "owner");
		assert_eq!(link.repo(), "repo");
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
		// Can't resolve to link without owner
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
