//! What makes a milestone a sprint: its title, and for a Bottlenecks that it is an org's.

use v_utils::{Timeframe, TimeframeDesignator};

use crate::RepoInfo;

/// The repo an org's own things sit in.
const ORG_REPO: &str = ".github";

/// A collection of musts, by what they are a must by. In the order they are shown in: the shortest period first.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum Sprint {
	/// Before the period rolls over.
	Planning(Timeframe),
	/// First. An org's.
	Bottlenecks,
}
impl Sprint {
	/// What it holds is picked up by being in it.
	pub const DAY: Self = Self::Planning(Timeframe::from_naive(1, TimeframeDesignator::Days));

	/// What `repo`'s milestone titled `title` is a sprint of, if it is one.
	pub fn of(repo: RepoInfo, title: &str) -> Option<Self> {
		let named = title.parse::<Self>().ok().filter(|s| s.to_string() == title); //IGNORED_ERROR: any other milestone's title
		named.filter(|_| Self::check(repo, title).is_ok()) //IGNORED_ERROR: misplaced, it is no sprint
	}

	/// `repo` may have a milestone titled `title`: a Bottlenecks is an org's alone.
	pub fn check(repo: RepoInfo, title: &str) -> Result<(), SprintError> {
		match title == Self::Bottlenecks.to_string() && repo.repo() != ORG_REPO {
			true => Err(SprintError::Misplaced(repo)),
			false => Ok(()),
		}
	}

	/// Where this sprint of whoever owns `repo` sits: a planning one there, Bottlenecks with the org.
	pub fn home(self, repo: RepoInfo) -> RepoInfo {
		match self {
			Self::Planning(_) => repo,
			Self::Bottlenecks => RepoInfo::new(repo.owner().expect("sprints are github-only"), ORG_REPO),
		}
	}
}
impl std::fmt::Display for Sprint {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Self::Planning(tf) => tf.fmt(f),
			Self::Bottlenecks => f.write_str("Bottlenecks"),
		}
	}
}
/// As a person names one: `1q` is `1Q`, `bottlenecks` is `Bottlenecks`.
impl std::str::FromStr for Sprint {
	type Err = SprintError;

	fn from_str(s: &str) -> Result<Self, SprintError> {
		if s.eq_ignore_ascii_case("bottlenecks") {
			return Ok(Self::Bottlenecks);
		}
		match s.parse::<Timeframe>() {
			Ok(Timeframe(0)) | Err(_) => Err(SprintError::Unknown(s.into())),
			Ok(tf) => match tf.designator() {
				TimeframeDesignator::Milliseconds | TimeframeDesignator::Seconds | TimeframeDesignator::Minutes => Err(SprintError::Minutes(s.into())),
				TimeframeDesignator::Hours
				| TimeframeDesignator::Days
				| TimeframeDesignator::Weeks
				| TimeframeDesignator::IfcMonths
				| TimeframeDesignator::IfcQuarters
				| TimeframeDesignator::Years => Ok(Self::Planning(tf)),
			},
		}
	}
}

#[derive(Debug, thiserror::Error)]
pub enum SprintError {
	#[error("{0:?} is no sprint: a period (`1d`, `2w`, `1Q`, …) or `Bottlenecks`")]
	Unknown(String),
	#[error("{0:?} is a minute-designated period (`m`); monthly is `M`")]
	Minutes(String),
	#[error("only an org's `.github` may have a milestone titled `Bottlenecks`, and {0} is none")]
	Misplaced(RepoInfo),
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn titles() {
		let of = |t: &str| Sprint::of(RepoInfo::new("o", ".github"), t).map(|s| s.to_string());
		assert_eq!(Sprint::of(RepoInfo::new("o", "r"), "Bottlenecks"), None);
		assert_eq!(Sprint::of(RepoInfo::new("o", "r"), "1d").map(|s| s.to_string()), of("1d"));
		let mut shown: Vec<Sprint> = ["Bottlenecks", "1Q", "2w", "7y", "1d"].iter().filter_map(|t| Sprint::of(RepoInfo::new("o", ".github"), t)).collect();
		shown.sort();
		insta::assert_snapshot!(format!("{:?}\n{:?}", shown.iter().map(ToString::to_string).collect::<Vec<_>>(), ["bottlenecks", "1q", "2", "5m", "0d", "v1.0", "2026/01/02_1d", ""].map(of)), @r#"
		["1d", "2w", "1Q", "7y", "Bottlenecks"]
		[None, None, None, None, None, None, None, None]
		"#);
	}
}
