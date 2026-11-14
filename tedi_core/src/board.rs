//! The board: every stored issue as a card, a query over them, and views — named queries with a
//! layout, a grouping and an order. Pure over cards; reading them from the store and editing them
//! through the sync are the operations' business.

use std::{collections::BTreeMap, str::FromStr};

use serde::{Deserialize, Serialize};

use crate::{
	CloseState, IssueLink, LinkedPr,
	labels::{Pin, Type},
	rank::Rank,
	stage::Stage,
};

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Card {
	pub link: IssueLink,
	pub title: String,
	pub labels: Vec<String>,
	pub assignees: Vec<String>,
	pub state: CloseState,
	pub stage: Stage,
	/// `None` once closed: rank orders open work only.
	pub rank: Option<Rank>,
	pub importance: Option<u8>,
	pub hours: Option<u16>,
	pub kind: Option<Type>,
	pub prs: Vec<LinkedPr>,
	pub blocked_by: Vec<IssueLink>,
	pub parent: Option<IssueLink>,
	/// As GitHub holds it: `# Blockers` included.
	pub body: String,
	/// Shown as of consensus: a clash on it waits for resolution.
	pub conflicted: bool,
}
impl Card {
	pub fn open(&self) -> bool {
		!self.state.is_closed()
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Board {
	pub cards: Vec<Card>,
	/// Stored issues that don't parse, with why: shown beside the board, not hidden by it.
	pub unreadable: Vec<(std::path::PathBuf, String)>,
}

#[derive(Debug, thiserror::Error)]
pub enum QueryError {
	#[error("no stage {0:?}")]
	Stage(String),
	#[error("no type t:{0}")]
	Type(String),
	#[error("i: takes 0–9, got {0:?}")]
	Importance(String),
	#[error("h: takes hours, got {0:?}")]
	Hours(String),
	#[error("is: takes open, closed, blocked or conflicted, got {0:?}")]
	Is(String),
	#[error(transparent)]
	Search(#[from] tedi_utils::search::SearchError),
}

/// `repo:EV-Service-Arb/*,EV-Main/tilers assignee:@me stage:triage,selected t:bug i:>=5 h:<=8 is:blocked label:x free text`.
/// Filters AND, the values of one filter OR; what isn't a filter is free text, ranked by `tedi_utils::search`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Query {
	filters: Vec<Filter>,
	text: String,
}
#[derive(Clone, Debug, PartialEq)]
enum Filter {
	/// Each `owner/repo`, `owner/*`, or a bare repo name.
	Repo(Vec<String>),
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
	type Err = QueryError;

	fn from_str(s: &str) -> Result<Self, QueryError> {
		let mut query = Query::default();
		let mut text = Vec::new();
		for token in s.split_whitespace() {
			let Some((key, value)) = token.split_once(':') else {
				text.push(token);
				continue;
			};
			let list = || value.split(',').filter(|v| !v.is_empty());
			let filter = match key {
				"repo" => Filter::Repo(list().map(str::to_string).collect()),
				"assignee" => Filter::Assignee(match value {
					"@me" => Assignee::Me,
					"none" => Assignee::Nobody,
					login => Assignee::Login(login.trim_start_matches('@').to_string()),
				}),
				"stage" => Filter::Stage(list().map(|v| Stage::from_str(v).map_err(|_| QueryError::Stage(v.into()))).collect::<Result<_, _>>()?),
				"t" => Filter::Type(
					list()
						.map(|v| Type::from_str(&format!("t:{v}")).map_err(|_| QueryError::Type(v.into())))
						.collect::<Result<_, _>>()?,
				),
				"i" => {
					let (cmp, n) = Cmp::split(value);
					Filter::Importance(cmp, n.parse().map_err(|_| QueryError::Importance(value.into()))?)
				}
				"h" => {
					let (cmp, n) = Cmp::split(value);
					Filter::Hours(cmp, n.parse().map_err(|_| QueryError::Hours(value.into()))?)
				}
				"label" => Filter::Label(value.to_string()),
				"is" => Filter::Is(Is::from_str(value).map_err(|_| QueryError::Is(value.into()))?),
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
	pub fn run(&self, cards: &[Card], me: Option<&str>) -> Result<Vec<usize>, QueryError> {
		let open: std::collections::HashSet<&IssueLink> = cards.iter().filter(|c| c.open()).map(|c| &c.link).collect();
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
			Filter::Repo(patterns) => {
				let project = card.link.project();
				patterns.iter().any(|pattern| match pattern.split_once('/') {
					// GitHub names are case-insensitive, and links store them lowercased
					Some((owner, "*")) => project.owner().is_some_and(|o| o.eq_ignore_ascii_case(owner)),
					Some(_) => project.to_string().eq_ignore_ascii_case(pattern),
					None => project.repo().eq_ignore_ascii_case(pattern),
				})
			}
			Filter::Assignee(Assignee::Me) => me.is_some_and(|me| card.assignees.iter().any(|a| a == me)),
			Filter::Assignee(Assignee::Nobody) => card.assignees.is_empty(),
			Filter::Assignee(Assignee::Login(login)) => card.assignees.iter().any(|a| a == login),
			Filter::Stage(stages) => stages.contains(&card.stage),
			Filter::Type(kinds) => card.kind.is_some_and(|k| kinds.contains(&k)),
			Filter::Importance(cmp, n) => card.importance.is_some_and(|i| cmp.holds(i, *n)),
			Filter::Hours(cmp, n) => card.hours.is_some_and(|h| cmp.holds(h, *n)),
			Filter::Label(label) => card.labels.iter().any(|l| l == label),
			Filter::Is(Is::Open) => card.open(),
			Filter::Is(Is::Closed) => !card.open(),
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
	pub sort: Sort,
	/// What a card shows beside its title, in this order.
	pub shown: Vec<Shown>,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Layout {
	Board,
	Table,
	List,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Sort {
	/// As the query ordered them: by free-text relevance when there is any, else by rank.
	Rank,
	Importance,
	Hours,
	Title,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Shown {
	Repo,
	Assignees,
	Stage,
	Importance,
	Hours,
	Type,
	Labels,
	Blockers,
	Prs,
}

impl Sort {
	/// Reorders `hits` (as `Query::run` ordered them); ties keep that order, and so does `Rank`.
	pub fn apply(self, cards: &[Card], hits: &mut [usize]) {
		match self {
			Sort::Rank => {}
			Sort::Importance => hits.sort_by_key(|&i| std::cmp::Reverse(cards[i].importance)),
			Sort::Hours => hits.sort_by_key(|&i| cards[i].hours.map_or(u16::MAX, |h| h)), // unestimated last
			Sort::Title => hits.sort_by_cached_key(|&i| cards[i].title.to_lowercase()),
		}
	}
}

/// A named slice of the team's repos, nesting narrower ones: it covers its own `repos` (`owner/repo`,
/// `owner/*`) and those of every scope inside it.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Scope {
	#[serde(default)] // a scope may only group nested ones
	pub repos: Vec<String>,
	#[serde(flatten)]
	pub children: BTreeMap<String, Scope>,
}
impl Scope {
	/// The `repo:` filter matching every repo this scope covers.
	pub fn filter(&self) -> String {
		fn collect<'a>(scope: &'a Scope, out: &mut Vec<&'a str>) {
			out.extend(scope.repos.iter().map(String::as_str));
			for child in scope.children.values() {
				collect(child, out);
			}
		}
		let mut repos = Vec::new();
		collect(self, &mut repos);
		assert!(!repos.is_empty(), "a configured scope names at least one repo, itself or nested");
		format!("repo:{}", repos.join(","))
	}
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

#[derive(Debug, thiserror::Error)]
#[error("{0} is read off GitHub: a ready pull request, or a close long enough ago")]
pub struct NotDroppable(pub Stage);

/// Where a card's stage moves when it is dropped on `to`: the labels and close state that read as it.
/// Review and the closed-by-age stage are GitHub's to tell, so they can't be dropped on.
pub fn restage(labels: &[String], to: Stage) -> Result<(Vec<String>, CloseState), NotDroppable> {
	let mut labels: Vec<String> = labels.iter().filter(|l| Pin::from_str(l).is_err()).cloned().collect();
	let mut pin = |p: Pin| labels.push(p.to_string());
	let state = match to {
		Stage::Icebox => {
			pin(Pin::Icebox);
			CloseState::Open
		}
		Stage::Triage => CloseState::Open,
		Stage::Selected => {
			pin(Pin::Selected);
			CloseState::Open
		}
		Stage::InProgress => {
			pin(Pin::Wip);
			CloseState::Open
		}
		Stage::Done => CloseState::Closed,
		Stage::WontDo => CloseState::NotPlanned,
		Stage::InReview | Stage::Backlog => return Err(NotDroppable(to)),
	};
	Ok((labels, state))
}
