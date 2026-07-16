//! Issue selectors and index paths: the machinery that walks the addressing
//! primitives in `uri` to locate an issue within a repo's tree.

use std::fmt;

use arrayvec::ArrayString;
use copy_arrayvec::CopyArrayVec;
use v_utils::macros::wrap_err;

use crate::{error::TitleInGitPathError, uri::RepoInfo};

/// Maximum title length enforced by Github.
pub const MAX_TITLE_LENGTH: usize = 256;

/// Maximum index depth (lineage + the issue itself).
pub const MAX_INDEX_DEPTH: usize = MAX_LINEAGE_DEPTH + 1;
pub const MAX_LINEAGE_DEPTH: usize = 8;

pub type IssueChildren<T> = std::collections::HashMap<IssueSelector, T>;

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
	/// Exact entry name (filesystem-derived, after stripping `.md`/`.md.bak`)
	Exact(ArrayString<MAX_TITLE_LENGTH>),
}

impl IssueSelector {
	/// Create a Title selector from a string.
	/// Panics if title exceeds MAX_TITLE_LENGTH (256 chars).
	pub fn title(title: &str) -> Self {
		Self::Title(ArrayString::from(title).unwrap_or_else(|_| panic!("title too long (max {MAX_TITLE_LENGTH} chars): {}", title.len())))
	}

	/// Try to create an Exact selector from an entry name.
	/// Returns None if the name exceeds MAX_TITLE_LENGTH.
	pub fn try_exact(name: &str) -> Option<Self> {
		ArrayString::from(name).ok().map(Self::Exact)
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

	/// Get the owner, or `None` for the virtual store.
	pub fn owner(&self) -> Option<&str> {
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
	pub fn git_num_path(&self) -> Result<Vec<u64>, TitleInGitPathError> {
		use miette::{NamedSource, SourceSpan};

		let mut result = Vec::with_capacity(self.index().len());
		let mut offset = self.repo_info.to_string().len();

		for selector in self.index() {
			match selector {
				IssueSelector::GitId(n) => {
					let s = format!("/{n}");
					offset += s.len();
					result.push(*n);
				}
				IssueSelector::Title(title) | IssueSelector::Exact(title) => {
					let span: SourceSpan = (offset + 1, title.len()).into(); // +1 to skip the '/'
					return Err(TitleInGitPathError::new(NamedSource::new("IssueIndex", self.to_string()), span));
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

impl fmt::Display for IssueIndex {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.repo_info)?;
		for selector in self.index() {
			match selector {
				IssueSelector::GitId(n) => write!(f, "/{n}")?,
				IssueSelector::Title(t) | IssueSelector::Exact(t) => write!(f, "/{t}")?,
			}
		}
		Ok(())
	}
}

/// Error returned when parsing an `IssueIndex` from a string fails.
#[wrap_err]
#[derive(Debug, thiserror::Error)]
#[error("{msg}")]
pub struct IssueIndexParseError {
	msg: String,
}

impl std::str::FromStr for IssueIndex {
	type Err = IssueIndexParseError;

	fn from_str(s: &str) -> Result<Self, Self::Err> {
		let parts: Vec<&str> = s.split('/').collect();
		let (repo_info, selector_parts) = if parts[0] == "virtual" {
			(RepoInfo::Virtual, &parts[1..])
		} else {
			if parts.len() < 2 {
				return Err(IssueIndexParseError::new(format!("IssueIndex requires at least owner/repo, got: {s}")));
			}
			(RepoInfo::new(parts[0], parts[1]), &parts[2..])
		};
		let selectors: Vec<IssueSelector> = selector_parts
			.iter()
			.map(|p| match p.parse::<u64>() {
				Ok(n) => IssueSelector::GitId(n),
				Err(_) => IssueSelector::title(p),
			})
			.collect();
		Ok(Self::with_index(repo_info, selectors))
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn issue_index_lowercases_owner_and_repo() {
		let idx: IssueIndex = "OWNER/REPO/123".parse().unwrap();
		assert_eq!((idx.repo_info().owner(), idx.repo_info().repo()), (Some("owner"), "repo"));
	}
}
