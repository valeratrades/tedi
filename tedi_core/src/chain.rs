//! The ownership chain, from an org down to an issue: who holds a thing (its place), and what it needs
//! (ARCHITECTURE.md, `### Composition Rules`). Containment, scopes and Bottlenecks are read off it.

use std::{
	collections::{BTreeMap, HashMap, HashSet},
	fmt,
	str::FromStr,
};

use arrayvec::ArrayString;
use serde::{Deserialize, Serialize};
use v_utils::Timeframe;

pub use crate::error::ChainError;
use crate::{IssueLink, Milestone, MilestoneLink, RepoInfo, error::ChainDiagnostic};

/// What GitHub links start with, and so what tells a [`Point`] from a name.
pub const GITHUB: &str = "https://github.com/";

#[derive(Clone, Debug, thiserror::Error)]
#[error("{0:?} names nothing on GitHub")]
pub struct BadName(pub(crate) String);

/// A GitHub owner, lowercased: GitHub treats it case-insensitively.
#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize, derive_more::Display)]
#[serde(try_from = "String", into = "String")]
pub struct Org(pub(crate) ArrayString<39>);
impl FromStr for Org {
	type Err = BadName;

	fn from_str(s: &str) -> Result<Self, BadName> {
		let named = |c: char| c.is_ascii_alphanumeric() || "-_".contains(c); // `_`: enterprise-managed users
		match !s.is_empty() && s.len() <= 39 && s.chars().all(named) {
			true => Ok(Org(ArrayString::from(&s.to_lowercase()).expect("ASCII of a length checked"))),
			false => Err(BadName(s.into())),
		}
	}
}
impl TryFrom<String> for Org {
	type Error = BadName;

	fn try_from(s: String) -> Result<Self, BadName> {
		s.parse()
	}
}
impl From<Org> for String {
	fn from(org: Org) -> String {
		org.to_string()
	}
}

/// Main org → the orgs it owns: GitHub's can't nest.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(try_from = "BTreeMap<String, Vec<Org>>", into = "BTreeMap<String, Vec<Org>>")]
pub struct Orgs(BTreeMap<Org, Vec<Org>>);
impl Orgs {
	pub fn owned(&self, main: Org) -> &[Org] {
		self.0.get(&main).map_or(&[], Vec::as_slice)
	}

	fn main(&self, owned: Org) -> Option<Org> {
		self.0.iter().find(|(_, orgs)| orgs.contains(&owned)).map(|(main, _)| *main)
	}
}
impl TryFrom<BTreeMap<String, Vec<Org>>> for Orgs {
	type Error = OrgsError;

	fn try_from(held: BTreeMap<String, Vec<Org>>) -> Result<Self, OrgsError> {
		let mut orgs = BTreeMap::new();
		for (main, owned) in held {
			let main: Org = main.parse()?;
			if orgs.insert(main, owned).is_some() {
				return Err(OrgsError::Twice(main));
			}
		}
		let mut seen = HashSet::new();
		for &org in orgs.values().flatten() {
			if orgs.contains_key(&org) {
				return Err(OrgsError::Main(org));
			}
			if !seen.insert(org) {
				return Err(OrgsError::Twice(org));
			}
		}
		Ok(Orgs(orgs))
	}
}
impl From<Orgs> for BTreeMap<String, Vec<Org>> {
	fn from(orgs: Orgs) -> Self {
		orgs.0.into_iter().map(|(main, owned)| (main.to_string(), owned)).collect()
	}
}
#[derive(Debug, thiserror::Error)]
pub enum OrgsError {
	#[error(transparent)]
	Name(#[from] BadName),
	#[error("the org {0} is named twice: one main org owns it")]
	Twice(Org),
	#[error("the org {0} is owned, and owns orgs itself: orgs nest one level")]
	Main(Org),
}

/// A place on the chain. Serialized as its GitHub URL.
#[derive(Clone, Debug, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub enum Point {
	Org(Org),
	Repo(RepoInfo),
	Milestone(MilestoneLink),
	Issue(IssueLink),
}
impl Point {
	/// What holds it.
	pub fn above(&self, orgs: &Orgs) -> Option<Point> {
		match self {
			Point::Org(org) => orgs.main(*org).map(Point::Org),
			Point::Repo(RepoInfo::Github { owner, .. }) => Some(Point::Org(*owner)),
			Point::Repo(RepoInfo::Virtual) => None,
			Point::Milestone(link) => Some(Point::Repo(link.repo_info())),
			Point::Issue(link) => Some(Point::Repo(link.project())),
		}
	}
}
impl fmt::Display for Point {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Point::Org(org) => write!(f, "{GITHUB}{org}"),
			Point::Repo(repo) => write!(f, "{GITHUB}{repo}"),
			Point::Milestone(link) => f.write_str(link.as_str()),
			Point::Issue(link) => write!(f, "{link}"),
		}
	}
}
impl FromStr for Point {
	type Err = BadName;

	fn from_str(s: &str) -> Result<Self, BadName> {
		let bad = || BadName(s.into());
		let at: Vec<&str> = s.strip_prefix(GITHUB).ok_or_else(bad)?.split('/').collect();
		let repo = |owner: &str, repo: &str| format!("{owner}/{repo}").parse::<RepoInfo>();
		let number = |n: &str| n.parse::<u64>().map_err(|_| bad());
		Ok(match at.as_slice() {
			[org] => Point::Org(org.parse()?),
			[owner, name] => Point::Repo(repo(owner, name)?),
			[owner, name, "milestone", n] => Point::Milestone(MilestoneLink::in_project(repo(owner, name)?, number(n)?)),
			[owner, name, "issues", n] => Point::Issue(IssueLink::in_project(repo(owner, name)?, number(n)?)),
			_ => return Err(bad()),
		})
	}
}
impl TryFrom<String> for Point {
	type Error = BadName;

	fn try_from(s: String) -> Result<Self, BadName> {
		s.parse()
	}
}
impl From<Point> for String {
	fn from(point: Point) -> String {
		point.to_string()
	}
}

/// What a milestone's link and title make it; each owns those below it, and any [`Kind::Milestone`].
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, strum::Display)]
pub enum Kind {
	Milestone,
	/// An org's, its sprint.
	Bottlenecks,
	/// A person's, titled by its timeframe.
	Sprint,
}
impl From<(&MilestoneLink, &str)> for Kind {
	fn from((link, title): (&MilestoneLink, &str)) -> Self {
		match () {
			() if title.parse::<Timeframe>().is_ok() => Kind::Sprint,
			() if title == "Bottlenecks" && link.repo_info().repo() == ".github" => Kind::Bottlenecks,
			() => Kind::Milestone,
		}
	}
}

/// How the edges a [`check`] is asked of stand.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Edge {
	/// An edit would introduce them.
	Added,
	/// GitHub holds them.
	Held,
}

/// Whether `owner` may own each of `children`, among the milestones `known` (`owner` as held there is ignored).
pub fn check(known: &[Milestone], owner: &Milestone, children: &[MilestoneLink], edge: Edge) -> Result<(), ChainError> {
	let mut by: HashMap<&MilestoneLink, &Milestone> = known.iter().map(|m| (m.link(), m)).collect();
	by.insert(owner.link(), owner);
	// not known: below a milestone only a milestone belongs
	let kind = |link: &MilestoneLink| by.get(link).map_or(Kind::Milestone, |m| Kind::from((link, m.identity.title.as_str())));
	let mut owners: HashMap<MilestoneLink, Vec<MilestoneLink>> = HashMap::new();
	for m in known {
		for child in m.body.0.milestone_links() {
			owners.entry(child).or_default().push(m.link().clone());
		}
	}
	let plain = |links: Vec<MilestoneLink>| links.into_iter().filter(|l| kind(l) == Kind::Milestone).collect::<Vec<_>>();
	let below = |link: &MilestoneLink| plain(by.get(link).map(|m| m.body.0.milestone_links()).unwrap_or_default()); // not known: nor is what it owns
	let above = |link: &MilestoneLink| plain(owners.get(link).cloned().unwrap_or_default()); // none owns it
	let draw = |links: &[MilestoneLink]| -> Vec<Node> {
		links
			.iter()
			.map(|l| Node {
				link: l.clone(),
				title: by.get(l).map_or_else(|| format!("#{}", l.number()), |m| m.identity.title.clone()),
			})
			.collect()
	};

	let of = owner.link();
	let up = match kind(of) {
		Kind::Milestone => walk(of, above),
		Kind::Bottlenecks | Kind::Sprint => Vec::new(), // a root: its own repo doesn't count
	};
	for child in children {
		if !by.contains_key(child) {
			if edge == Edge::Added {
				return Err(ChainDiagnostic::unknown(child).into());
			}
			continue;
		}
		if kind(child) != Kind::Milestone && kind(child) >= kind(of) {
			return Err(ChainDiagnostic::root(&draw(&[of.clone(), child.clone()]), kind(child), edge).into());
		}
		if kind(of) != Kind::Milestone {
			continue;
		}
		let down = walk(child, below);
		if let Some(at) = down.iter().position(|s| s.link == *of) {
			let mut path = route(&down, at);
			path.pop(); // the owner, the loop's first already
			path.insert(0, of.clone());
			if edge == Edge::Held {
				let first = (0..path.len()).min_by_key(|&i| path[i].as_str().to_string()).expect("a loop holds its owner");
				path.rotate_left(first); // one drawing, whichever of its edges it is found by
			}
			return Err(ChainDiagnostic::looped(&draw(&path), edge).into());
		}
		let mut nearest: HashMap<RepoInfo, usize> = HashMap::new();
		for (i, step) in up.iter().enumerate() {
			nearest.entry(step.link.repo_info()).or_insert(i); // the walk is shortest first
		}
		let twice = down
			.iter()
			.enumerate()
			.filter_map(|(d, step)| Some((*nearest.get(&step.link.repo_info())?, d)))
			.min_by_key(|&(u, d)| up[u].depth + down[d].depth);
		if let Some((u, d)) = twice {
			let mut path = route(&up, u);
			path.reverse();
			let added = path.len();
			path.extend(route(&down, d));
			return Err(ChainDiagnostic::repo_twice(&draw(&path), added, edge).into());
		}
	}
	Ok(())
}

/// A milestone as a chain draws it.
pub(crate) struct Node {
	pub(crate) link: MilestoneLink,
	pub(crate) title: String,
}
impl fmt::Display for Node {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{} · {}", self.link.repo_info(), self.title)
	}
}

struct Step {
	link: MilestoneLink,
	from: Option<usize>,
	depth: usize,
}

/// All that `next` reaches from `start`, itself included, the nearest first.
fn walk(start: &MilestoneLink, next: impl Fn(&MilestoneLink) -> Vec<MilestoneLink>) -> Vec<Step> {
	let mut seen = HashSet::from([start.clone()]);
	let mut steps = vec![Step {
		link: start.clone(),
		from: None,
		depth: 0,
	}];
	let mut at = 0;
	while let Some(Step { link, depth, .. }) = steps.get(at) {
		let depth = depth + 1;
		for link in next(link) {
			if seen.insert(link.clone()) {
				steps.push(Step { link, from: Some(at), depth });
			}
		}
		at += 1;
	}
	steps
}

/// The links from a walk's start to its step `at`.
fn route(steps: &[Step], at: usize) -> Vec<MilestoneLink> {
	let mut out: Vec<MilestoneLink> = std::iter::successors(Some(at), |&i| steps[i].from).map(|i| steps[i].link.clone()).collect();
	out.reverse();
	out
}
