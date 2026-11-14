//! The board: every stored issue as a card, a query over them, and views — named queries with a
//! layout, a grouping and an order. Read-only over the local store; edits go through the sync.

use std::{collections::BTreeMap, str::FromStr};

use color_eyre::eyre::{Result, bail, eyre};
use jiff::{SignedDuration, Timestamp};
use serde::{Deserialize, Serialize};

use crate::{
	Issue, IssueLink, LazyIssue, LinkedPr,
	labels::{Scheme, Type},
	local::{FsReader, GitReader, Local, LocalError, LocalIssueSource, LocalPath},
	rank::{Rank, Task, rank},
	stage::{Stage, stage},
};

/// What an issue without `i:` counts as (`labels.nix`: "`i:3` is the default").
const DEFAULT_IMPORTANCE: u8 = 3;

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Card {
	pub link: IssueLink,
	pub title: String,
	pub labels: Vec<String>,
	pub assignees: Vec<String>,
	pub open: bool,
	pub stage: Stage,
	/// `None` once closed: rank orders open work only.
	pub rank: Option<Rank>,
	pub importance: Option<u8>,
	pub hours: Option<u16>,
	pub kind: Option<Type>,
	pub prs: Vec<LinkedPr>,
	pub blocked_by: Vec<IssueLink>,
	pub parent: Option<IssueLink>,
	pub body: String,
	/// Shown as of consensus: a clash on it waits for resolution.
	pub conflicted: bool,
}

pub struct Board {
	pub cards: Vec<Card>,
	/// Stored issues that don't parse, with why: shown beside the board, not hidden by it.
	pub unreadable: Vec<(std::path::PathBuf, String)>,
}

/// Every issue in the store, one card per node, ranked over their `blocked by` links.
pub async fn load(archive_after: SignedDuration, now: Timestamp) -> Result<Board> {
	let mut cards = Vec::new();
	let mut unreadable = Vec::new();
	for rel in Local::issue_file_relpaths()? {
		let path = Local::issues_dir().join(&rel);
		let index = Local::extract_index_from_path(&path)?;
		if index.index().len() != 1 || index.repo_info().is_virtual() {
			continue; // a child is carded with its root's tree; virtual issues have no board
		}
		let loaded = match LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await {
			Ok(source) => Issue::load(source).await.map(|i| (i, false)),
			Err(LocalError::ConflictBlocked(_)) => Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::new(index))?).await.map(|i| (i, true)),
			Err(e) => Err(e),
		};
		match loaded {
			Ok((issue, conflicted)) => card_tree(&issue, None, conflicted, archive_after, now, &mut cards),
			Err(e) => unreadable.push((path, e.to_string())),
		}
	}

	let tasks: BTreeMap<String, Task> = cards
		.iter()
		.map(|c| {
			let task = Task {
				importance: c.importance.unwrap_or(DEFAULT_IMPORTANCE),
				open: c.open,
			};
			(c.link.to_string(), task)
		})
		.collect();
	let blocks: Vec<(String, String)> = cards.iter().flat_map(|c| c.blocked_by.iter().map(|b| (b.to_string(), c.link.to_string()))).collect();
	let ranking = rank(&tasks, &blocks);
	for card in &mut cards {
		card.rank = ranking.ranks.get(&card.link.to_string()).copied();
	}
	Ok(Board { cards, unreadable })
}

fn card_tree(issue: &Issue, parent: Option<&IssueLink>, conflicted: bool, archive_after: SignedDuration, now: Timestamp, out: &mut Vec<Card>) {
	let meta = issue.identity.as_linked().expect("stored github issues are linked");
	let scheme = Scheme::from(issue.contents.labels.as_slice());
	let link = meta.link().clone();
	out.push(Card {
		link: link.clone(),
		title: issue.contents.title.clone(),
		labels: issue.contents.labels.clone(),
		assignees: issue.contents.assignees.clone(),
		open: !issue.contents.state.is_closed(),
		stage: stage(scheme.pin, &issue.contents.state, &meta.prs, meta.timestamps.state, archive_after, now),
		rank: None,
		importance: scheme.importance,
		hours: scheme.hours,
		kind: scheme.kind,
		prs: meta.prs.clone(),
		blocked_by: meta.blocked_by.clone(),
		parent: parent.cloned(),
		body: issue.body().to_string(),
		conflicted,
	});
	for child in issue.children.values() {
		card_tree(child, Some(&link), conflicted, archive_after, now, out);
	}
}

/// `repo:EV-Service-Arb/* assignee:@me stage:triage,selected t:bug i:>=5 h:<=8 is:blocked label:x free text`.
/// Filters AND; what isn't one is free text, ranked by `tedi_utils::search`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Query {
	filters: Vec<Filter>,
	text: String,
}
#[derive(Clone, Debug, PartialEq)]
enum Filter {
	/// `owner/repo`, `owner/*`, or a bare repo name.
	Repo(String),
	Assignee(Assignee),
	Stage(Vec<Stage>),
	Type(Vec<Type>),
	Importance(Cmp, u8),
	Hours(Cmp, u16),
	Label(String),
	Is(Is),
}
#[derive(Clone, Debug, PartialEq)]
enum Assignee {
	Me,
	Nobody,
	Login(String),
}
#[derive(Clone, Copy, Debug, PartialEq, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
enum Is {
	Open,
	Closed,
	/// By an open issue on the board.
	Blocked,
	Conflicted,
}
#[derive(Clone, Copy, Debug, PartialEq)]
enum Cmp {
	Lt,
	Le,
	Eq,
	Ge,
	Gt,
}
impl Cmp {
	fn split(v: &str) -> (Self, &str) {
		[(">=", Cmp::Ge), ("<=", Cmp::Le), (">", Cmp::Gt), ("<", Cmp::Lt), ("=", Cmp::Eq)]
			.into_iter()
			.find_map(|(op, cmp)| v.strip_prefix(op).map(|rest| (cmp, rest)))
			.unwrap_or((Cmp::Eq, v))
	}

	fn holds<N: Ord>(self, have: N, want: N) -> bool {
		match self {
			Cmp::Lt => have < want,
			Cmp::Le => have <= want,
			Cmp::Eq => have == want,
			Cmp::Ge => have >= want,
			Cmp::Gt => have > want,
		}
	}
}

impl FromStr for Query {
	type Err = color_eyre::Report;

	fn from_str(s: &str) -> Result<Self> {
		let mut query = Query::default();
		let mut text = Vec::new();
		for token in s.split_whitespace() {
			let Some((key, value)) = token.split_once(':') else {
				text.push(token);
				continue;
			};
			let list = || value.split(',').filter(|v| !v.is_empty());
			let filter = match key {
				"repo" => Filter::Repo(value.to_string()),
				"assignee" => Filter::Assignee(match value {
					"@me" => Assignee::Me,
					"none" => Assignee::Nobody,
					login => Assignee::Login(login.trim_start_matches('@').to_string()),
				}),
				"stage" => Filter::Stage(list().map(|v| Stage::from_str(v).map_err(|_| eyre!("no stage {v:?}"))).collect::<Result<_>>()?),
				"t" => Filter::Type(list().map(|v| Type::from_str(&format!("t:{v}")).map_err(|_| eyre!("no type t:{v}"))).collect::<Result<_>>()?),
				"i" => {
					let (cmp, n) = Cmp::split(value);
					Filter::Importance(cmp, n.parse().map_err(|_| eyre!("i: takes 0–9, got {value:?}"))?)
				}
				"h" => {
					let (cmp, n) = Cmp::split(value);
					Filter::Hours(cmp, n.parse().map_err(|_| eyre!("h: takes hours, got {value:?}"))?)
				}
				"label" => Filter::Label(value.to_string()),
				"is" => Filter::Is(Is::from_str(value).map_err(|_| eyre!("is: takes open, closed, blocked or conflicted, got {value:?}"))?),
				// `c:ci`, `ext:hack`, `s:wip` and any other label read as typed
				_ => Filter::Label(token.to_string()),
			};
			query.filters.push(filter);
		}
		query.text = text.join(" ");
		Ok(query)
	}
}

impl Query {
	/// Indices into `cards` that match, best first: by free text when there is any, else by rank.
	pub fn run(&self, cards: &[Card], me: Option<&str>) -> Result<Vec<usize>> {
		let open: std::collections::HashSet<&IssueLink> = cards.iter().filter(|c| c.open).map(|c| &c.link).collect();
		let passing: Vec<usize> = (0..cards.len()).filter(|&i| self.filters.iter().all(|f| f.holds(&cards[i], me, &open))).collect();
		if self.text.is_empty() {
			let mut sorted = passing;
			sorted.sort_by_key(|&i| std::cmp::Reverse(cards[i].rank));
			return Ok(sorted);
		}
		let candidates: Vec<tedi_utils::search::Candidate> = passing
			.iter()
			.map(|&i| tedi_utils::search::Candidate {
				title: cards[i].title.clone(),
				context: vec![cards[i].link.project().to_string()],
				body: cards[i].body.clone(),
			})
			.collect();
		Ok(tedi_utils::search::rank(&candidates, tedi_utils::search::SearchKind::Fuzzy, &self.text)?
			.into_iter()
			.map(|i| passing[i])
			.collect())
	}
}

impl Filter {
	fn holds(&self, card: &Card, me: Option<&str>, open: &std::collections::HashSet<&IssueLink>) -> bool {
		match self {
			Filter::Repo(pattern) => {
				let project = card.link.project();
				match pattern.split_once('/') {
					Some((owner, "*")) => project.owner() == Some(owner),
					Some(_) => project.to_string() == *pattern,
					None => project.repo() == pattern,
				}
			}
			Filter::Assignee(Assignee::Me) => me.is_some_and(|me| card.assignees.iter().any(|a| a == me)),
			Filter::Assignee(Assignee::Nobody) => card.assignees.is_empty(),
			Filter::Assignee(Assignee::Login(login)) => card.assignees.iter().any(|a| a == login),
			Filter::Stage(stages) => stages.contains(&card.stage),
			Filter::Type(kinds) => card.kind.is_some_and(|k| kinds.contains(&k)),
			Filter::Importance(cmp, n) => card.importance.is_some_and(|i| cmp.holds(i, *n)),
			Filter::Hours(cmp, n) => card.hours.is_some_and(|h| cmp.holds(h, *n)),
			Filter::Label(label) => card.labels.iter().any(|l| l == label),
			Filter::Is(Is::Open) => card.open,
			Filter::Is(Is::Closed) => !card.open,
			Filter::Is(Is::Blocked) => card.blocked_by.iter().any(|b| open.contains(b)),
			Filter::Is(Is::Conflicted) => card.conflicted,
		}
	}
}

/// A named query, and how its result is shown.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct View {
	pub name: String,
	pub query: String,
	pub layout: Layout,
	pub group_by: GroupBy,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Layout {
	Board,
	Table,
	List,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum GroupBy {
	Stage,
	Repo,
	Assignee,
	Type,
	Importance,
	Nothing,
}

/// `hits` (as `Query::run` ordered them) split into groups, each keeping that order. Stages come in
/// board order, every stage present even when empty; other keys sort by name, the ungrouped last.
pub fn group(cards: &[Card], hits: &[usize], by: GroupBy) -> Vec<(String, Vec<usize>)> {
	use strum::IntoEnumIterator as _;
	let key = |c: &Card| -> Vec<String> {
		match by {
			GroupBy::Stage => vec![c.stage.to_string()],
			GroupBy::Repo => vec![c.link.project().to_string()],
			GroupBy::Assignee if c.assignees.is_empty() => vec!["unassigned".into()],
			GroupBy::Assignee => c.assignees.iter().map(|a| format!("@{a}")).collect(),
			GroupBy::Type => vec![c.kind.map_or_else(|| "untyped".into(), |k| k.to_string())],
			GroupBy::Importance => vec![c.importance.map_or_else(|| "i:?".into(), |i| format!("i:{i}"))],
			GroupBy::Nothing => vec![String::new()],
		}
	};
	let mut groups: Vec<(String, Vec<usize>)> = match by {
		GroupBy::Stage => Stage::iter().map(|s| (s.to_string(), Vec::new())).collect(),
		_ => Vec::new(),
	};
	for &i in hits {
		for k in key(&cards[i]) {
			match groups.iter_mut().find(|(g, _)| *g == k) {
				Some((_, members)) => members.push(i),
				None => groups.push((k, vec![i])),
			}
		}
	}
	if by != GroupBy::Stage {
		groups.sort_by(|a, b| a.0.cmp(&b.0));
	}
	groups
}

/// Where a card's stage moves when it is dropped on `to`: the labels and close state that read as it.
/// Review and the closed-by-age stage are GitHub's to tell, so they can't be dropped on.
pub fn restage(labels: &[String], to: Stage) -> Result<(Vec<String>, Option<crate::CloseState>)> {
	use crate::{CloseState, labels::Pin};
	let mut labels: Vec<String> = labels.iter().filter(|l| Pin::from_str(l).is_err()).cloned().collect();
	let pin = |labels: &mut Vec<String>, p: Pin| labels.push(p.to_string());
	let state = match to {
		Stage::Icebox => {
			pin(&mut labels, Pin::Icebox);
			Some(CloseState::Open)
		}
		Stage::Triage => Some(CloseState::Open),
		Stage::Selected => {
			pin(&mut labels, Pin::Selected);
			Some(CloseState::Open)
		}
		Stage::InProgress => {
			pin(&mut labels, Pin::Wip);
			Some(CloseState::Open)
		}
		Stage::Done => Some(CloseState::Closed),
		Stage::WontDo => Some(CloseState::NotPlanned),
		Stage::InReview | Stage::Backlog => bail!("{to} is read off GitHub: a ready pull request, or a close long enough ago"),
	};
	Ok((labels, state))
}
