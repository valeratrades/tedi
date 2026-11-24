//! What a caller may see and act on: every operation over the board takes what it acts on as let in by the caller's reach.

use std::collections::BTreeSet;

use tedi_core::{IssueLink, RepoInfo};

/// The repos a caller reaches.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Reach {
	/// Whatever the store holds: whoever runs this instance.
	All,
	/// Those of owners (`owner`) and repos (`owner/repo`), lowercased as GitHub matches them.
	Of(BTreeSet<String>),
}
impl Reach {
	pub fn covers(&self, repo: RepoInfo) -> bool {
		match self {
			Reach::All => true,
			Reach::Of(named) => repo.owner().is_some_and(|owner| named.contains(owner) || named.contains(&repo.to_string())),
		}
	}

	pub fn issue(&self, link: IssueLink) -> Result<Reached<IssueLink>, OutOfReach> {
		match self.covers(link.project()) {
			true => Ok(Reached(link)),
			false => Err(OutOfReach(link.project())),
		}
	}

	pub fn repo(&self, repo: RepoInfo) -> Result<Reached<RepoInfo>, OutOfReach> {
		match self.covers(repo) {
			true => Ok(Reached(repo)),
			false => Err(OutOfReach(repo)),
		}
	}
}

/// Let in by a [`Reach`], which alone makes one.
#[derive(Clone, Debug, derive_more::Deref)]
pub struct Reached<T>(T);

/// Worded the same whether it is stored here or exists at all.
#[derive(Debug, thiserror::Error)]
#[error("{0} is out of your reach: no scope of yours or the team's holds it")]
pub struct OutOfReach(RepoInfo);
