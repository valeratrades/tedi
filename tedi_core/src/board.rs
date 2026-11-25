//! The board: every stored issue as a card, a query over them, their order and grouping. Pure over cards;
//! reading them from the store and editing them through the sync are the operations' business.

use std::{
	collections::{BTreeMap, HashMap, HashSet},
	path::PathBuf,
	str::FromStr,
};

use serde::{Deserialize, Serialize};

use crate::{
	CloseState, Events, IssueLink, LinkedPr, MilestoneLink, NodeLink, PrState, RepoInfo, Review, Sprint, blockers,
	conflict::{Field, FieldError, unlist},
	labels::{Scheme, Type},
	rank::Rank,
	workflow::{State, Trees, WorkflowError},
};

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Card {
	pub link: IssueLink,
	pub title: String,
	pub labels: Vec<String>,
	pub assignees: Vec<String>,
	pub state: CloseState,
	/// When it closed; `None` while open, or when GitHub kept no such event.
	pub closed_at: Option<jiff::Timestamp>,
	/// Its last change GitHub dated; `None` when it dated none.
	pub updated: Option<jiff::Timestamp>,
	/// `None`: not read from GitHub since it was first stored.
	pub created: Option<jiff::Timestamp>,
	/// Where it sits in each workflow the board was loaded with.
	pub states: BTreeMap<String, State>,
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
	/// `Some` on a pull request: read-only but for its review requests, with no blockers or children of its own.
	pub pr: Option<Pr>,
}
impl Card {
	pub fn open(&self) -> bool {
		!self.state.is_closed()
	}

	/// What [`Field::write`] leaves on the issue, on its card. What the whole board decides (rank, who it blocks) stays.
	pub fn write(&mut self, field: Field, value: &str) -> Result<(), FieldError> {
		match field {
			Field::Title => self.title = value.to_string(),
			Field::Labels => {
				self.labels = unlist(value);
				let scheme = Scheme::from(self.labels.as_slice());
				(self.importance, self.hours, self.kind) = (scheme.importance, scheme.hours, scheme.kind);
			}
			Field::Assignees => self.assignees = unlist(value),
			Field::State => {
				self.state = CloseState::from_checkbox(value).map_err(FieldError::State)?;
				match self.open() {
					true => self.closed_at = None,
					false => self.rank = None,
				}
			}
			Field::Body => {
				let (body, blockers) = blockers::split_blockers(value);
				self.body = blockers::with_blockers(&Events::parse(&body), &blockers).to_string();
			}
			Field::Comments | Field::SubIssues => return Err(FieldError::NotWritable(field)),
		}
		Ok(())
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Pr {
	pub state: PrState,
	pub author: String,
	pub closes: Vec<IssueLink>,
	pub requested: Vec<String>,
	pub reviews: BTreeMap<String, Review>,
}

/// What is said on an issue.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Thread {
	/// `None`: unknown.
	pub author: Option<String>,
	pub comments: Vec<Said>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Said {
	/// `None`: not on GitHub yet.
	pub id: Option<u64>,
	pub by: Option<String>,
	pub at: Option<jiff::Timestamp>,
	pub body: String,
}

/// An open milestone.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Target {
	pub link: MilestoneLink,
	pub title: String,
	pub due_on: Option<jiff::Timestamp>,
	pub description: String,
	/// In order. An issue is of any repo: its own carry it as their milestone on GitHub, the rest are named in its body.
	pub nodes: Vec<NodeLink>,
}
impl Target {
	pub fn sprint(&self) -> Option<Sprint> {
		Sprint::titled(&self.title)
	}

	pub fn hosts(&self, link: &IssueLink) -> bool {
		self.nodes.iter().any(|n| matches!(n, NodeLink::Issue(l) if l == link))
	}
}

/// What a query holds for whoever ran it.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Answer {
	/// Who ran it: `@me`. `None`: unknown.
	pub login: Option<String>,
	pub trees: Trees,
	/// In the query's order: by rank, unless it is free text.
	pub hits: Vec<Hit>,
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Hit {
	pub link: IssueLink,
	/// Where it sits in each of the trees.
	pub states: BTreeMap<String, State>,
}

/// A query as a task view lists it: no pull requests.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Listing {
	pub query: String,
	/// Those holding any, by key; ungrouped, one under none.
	pub groups: Vec<(Option<String>, Vec<IssueLink>)>,
	pub pull_requests: usize,
}

#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Board {
	pub cards: Vec<Card>,
	pub targets: Vec<Target>,
	/// Stored issues that don't parse, with why: shown beside the board, not hidden by it.
	pub unreadable: Vec<(PathBuf, String)>,
	/// Per `owner/repo`, its labels' colours as last read from GitHub; a label missing here is uncoloured.
	pub colors: BTreeMap<String, BTreeMap<String, crate::labels::Color>>,
	/// Per `owner/repo`, the logins its issues can be assigned to, as last read from GitHub.
	pub assignable: BTreeMap<String, Vec<String>>,
}
impl Extend<Part> for Board {
	fn extend<T: IntoIterator<Item = Part>>(&mut self, parts: T) {
		for part in parts {
			match part {
				Part::Head { targets, colors, assignable, .. } => (self.targets, self.colors, self.assignable) = (targets, colors, assignable),
				Part::Read { cards, unreadable, .. } => {
					self.cards.extend(cards);
					self.unreadable.extend(unreadable);
				}
				Part::Ranks(mut ranks) =>
					for card in &mut self.cards {
						card.rank = ranks.remove(&card.link.to_string()).or(card.rank);
					},
			}
		}
	}
}

/// A board as it is read: the head, every source of open issues, their ranks, and the other sources.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum Part {
	/// What no card holds, and the bytes each source is read from.
	Head {
		sizes: Vec<(Source, u64)>,
		targets: Vec<Target>,
		colors: BTreeMap<String, BTreeMap<String, crate::labels::Color>>,
		assignable: BTreeMap<String, Vec<String>>,
	},
	/// What `bytes` of a source hold.
	Read {
		source: Source,
		bytes: u64,
		cards: Vec<Card>,
		unreadable: Vec<(PathBuf, String)>,
	},
	/// Of the open issues, which decide them together.
	Ranks(BTreeMap<String, Rank>),
}

/// What a board is read from, apart so that a view waits only for those it shows.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub struct Source {
	/// `owner/repo`
	pub repo: String,
	pub holds: Holds,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum Holds {
	/// Every tree with an open issue in it.
	OpenIssues,
	ClosedIssues,
	PullRequests,
}

#[derive(Debug, thiserror::Error)]
pub enum QueryError {
	#[error("state: takes <workflow>/<bucket>[/<bucket>…], got {0:?}")]
	State(String),
	#[error("no type t:{0}")]
	Type(String),
	#[error("i: takes 0–9 or none, got {0:?}")]
	Importance(String),
	#[error("h: takes hours or none, got {0:?}")]
	Hours(String),
	#[error("is: takes open, closed, blocked, conflicted, issue or pr, got {0:?}")]
	Is(String),
	#[error("{0}: takes a value")]
	Empty(Facet),
	#[error("only clauses (repo:, is:, t:, …) narrow here, and {0:?} is free text")]
	Text(String),
	#[error(transparent)]
	Search(#[from] tedi_utils::search::SearchError),
}

/// `repo:EV-Service-Arb/*,EV-Main/tilers assignee:@me state:kanban/triage,kanban/selected -t:chore i:>=5,none h:<=8 is:blocked label:x free text`.
/// Clauses AND, the values of one OR, a `-` clause holds none of them; `none` is a value unset.
/// What isn't a clause is free text, ranked by `tedi_utils::search`.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct Query {
	clauses: Vec<Clause>,
	text: String,
}
impl Query {
	/// Indices into `cards` that match, best first: by free text when there is any, else by rank.
	pub fn run(&self, cards: &[Card], me: Option<&str>) -> Result<Vec<usize>, QueryError> {
		let open = open(cards);
		let passing: Vec<usize> = (0..cards.len()).filter(|&i| self.clauses.iter().all(|c| c.holds(&cards[i], me, &open))).collect();
		if self.text.is_empty() {
			let mut sorted = passing;
			sorted.sort_by_key(|&i| {
				let Card { rank, importance, link, .. } = &cards[i];
				(std::cmp::Reverse(*rank), importance.is_none(), link.project(), link.number()) // rank counts an unset `i:` as its default
			});
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

	/// Picks `value` of `facet` (`exclude`: rules it out), dropping it from the clause of the other
	/// sense; picked already, it is dropped instead.
	pub fn toggle(&mut self, facet: Facet, value: &str, exclude: bool) -> Result<(), QueryError> {
		let value = Filter::parse(facet, value)?;
		let was = self.clauses.iter().any(|c| c.facet() == facet && c.negated == exclude && c.any.contains(&value));
		for clause in self.clauses.iter_mut().filter(|c| c.facet() == facet) {
			clause.any.retain(|v| *v != value);
		}
		self.clauses.retain(|c| !c.any.is_empty());
		if !was {
			match self.clauses.iter_mut().find(|c| c.facet() == facet && c.negated == exclude) {
				Some(clause) => clause.any.push(value),
				None => self.clauses.push(Clause { negated: exclude, any: vec![value] }),
			}
		}
		Ok(())
	}

	/// Whether what it shows is read from `source`: what may match, and every open issue when one does, as they rank and block each other.
	pub fn needs(&self, source: &Source) -> bool {
		let holds = |holds: Holds| {
			self.clauses.iter().filter(|c| c.facet() == Facet::Is).all(|c| match c.negated {
				false => c.any.iter().any(|v| match (v, holds) {
					(Filter::Is(Is::Pr), _) => holds == Holds::PullRequests,
					(Filter::Is(Is::Issue), _) => holds != Holds::PullRequests,
					(Filter::Is(Is::Open), _) => holds != Holds::ClosedIssues,
					_ => true,
				}),
				true => !c.any.iter().any(|v| match v {
					Filter::Is(Is::Pr) => holds == Holds::PullRequests,
					Filter::Is(Is::Issue) => holds != Holds::PullRequests,
					Filter::Is(Is::Closed) => holds == Holds::ClosedIssues,
					_ => false,
				}),
			})
		};
		let (owner, repo) = source.repo.split_once('/').expect("a source's repo is `owner/repo`");
		holds(source.holds) && (source.holds == Holds::OpenIssues || self.covers(RepoInfo::new(owner, repo)))
	}

	/// Whether its `repo:` clauses leave `repo` in.
	pub fn covers(&self, repo: RepoInfo) -> bool {
		self.clauses.iter().filter(|c| c.facet() == Facet::Repo).all(|c| {
			c.any.iter().any(|v| match v {
				Filter::Repo(pattern) => names(pattern, repo),
				other => unreachable!("{other} in a repo clause"),
			}) != c.negated
		})
	}

	pub fn narrow(&mut self, within: &Terms) {
		self.clauses.extend(within.clauses.iter().cloned());
	}

	/// The values its `facet` clauses name, as they parse back, those ruled out `-`-prefixed.
	pub fn picked(&self, facet: Facet) -> Vec<String> {
		self.clauses
			.iter()
			.filter(|c| c.facet() == facet)
			.flat_map(|c| c.any.iter().map(move |v| format!("{}{v}", if c.negated { "-" } else { "" })))
			.collect()
	}

	pub fn clear(&mut self, facet: Facet) {
		self.clauses.retain(|c| c.facet() != facet);
	}

	/// Every value of `facet` the query's hits `within` take, with the facet's own clauses lifted (never
	/// `within`'s), most taken first; and each value those clauses pick, taken or not.
	pub fn tally(&self, facet: Facet, within: &Terms, cards: &[Card], me: Option<&str>) -> Result<Vec<Tally>, QueryError> {
		let mut rest = self.clone();
		rest.clear(facet);
		rest.narrow(within);
		let hits = rest.run(cards, me)?;
		let open = open(cards);
		let mut counted: Vec<(Filter, usize)> = Vec::new();
		for &i in &hits {
			for value in takes(&cards[i], facet, &open) {
				debug_assert!(value.holds(&cards[i], me, &open), "{value} is offered for {}, so picking it keeps it", cards[i].link);
				match counted.iter_mut().find(|(v, _)| *v == value) {
					Some((_, n)) => *n += 1,
					None => counted.push((value, 1)),
				}
			}
		}
		let own: Vec<&Clause> = self.clauses.iter().filter(|c| c.facet() == facet).collect();
		for value in own.iter().flat_map(|c| &c.any) {
			if !counted.iter().any(|(v, _)| v == value) {
				counted.push((value.clone(), hits.iter().filter(|&&i| value.holds(&cards[i], me, &open)).count()));
			}
		}
		counted.sort_by(|(a, an), (b, bn)| bn.cmp(an).then_with(|| a.cmp(b)));
		Ok(counted
			.into_iter()
			.map(|(value, count)| Tally {
				picked: own.iter().find(|c| c.any.contains(&value)).map(|c| !c.negated),
				value: value.to_string(),
				count,
			})
			.collect())
	}
}

/// A value of a facet as a quick filter offers it: how many of the query's hits take it, the
/// facet's own clauses aside, and whether those include it (`Some(true)`) or exclude it.
#[derive(Clone, Debug, PartialEq)]
pub struct Tally {
	pub value: String,
	pub count: usize,
	pub picked: Option<bool>,
}

/// A query of clauses alone.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize, derive_more::Deref)]
#[serde(try_from = "String", into = "String")]
pub struct Terms(Query);
impl FromStr for Terms {
	type Err = QueryError;

	fn from_str(s: &str) -> Result<Self, QueryError> {
		let query: Query = s.parse()?;
		match query.text.is_empty() {
			true => Ok(Terms(query)),
			false => Err(QueryError::Text(query.text)),
		}
	}
}
impl std::fmt::Display for Terms {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	}
}

/// How a view splits its cards: by `state:<workflow>`, `repo`, `assignee`, `type`, `importance` or `nothing`.
#[derive(Clone, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(try_from = "String", into = "String")]
pub enum GroupBy {
	/// Into that workflow's leaves, every one present even when empty: the kanban, laid out as a board.
	State(String),
	Repo,
	Assignee,
	Type,
	Importance,
	Nothing,
}
/// `importance`, or `-importance` reversed. Each key's own order: importance high→low, hours low→high,
/// a workflow's states in its order, numbers low→high, the latest updated and the newest first; a card lacking the key last
/// either way. Only what a card can be ranked by is a key: a name or a kind groups, it doesn't order.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct Sort {
	pub by: SortKey,
	pub reversed: bool,
}
impl Sort {
	/// A stable sort of `hits` by this key alone; `place` is each card's place in the query's order.
	fn apply(&self, cards: &[Card], hits: &mut [usize], workflows: &Trees, place: &[usize]) -> Result<(), WorkflowError> {
		use std::cmp::Reverse;
		fn by<K: Ord>(hits: &mut [usize], reversed: bool, key: impl Fn(usize) -> Option<K>) {
			let mut keyed: Vec<(Option<K>, usize)> = hits.iter().map(|&i| (key(i), i)).collect();
			keyed.sort_by(|(a, _), (b, _)| match (a, b) {
				(Some(a), Some(b)) if reversed => b.cmp(a),
				(Some(a), Some(b)) => a.cmp(b),
				_ => a.is_none().cmp(&b.is_none()),
			});
			for (slot, (_, i)) in hits.iter_mut().zip(keyed) {
				*slot = i;
			}
		}
		let r = self.reversed;
		match &self.by {
			SortKey::Rank if r => by(hits, r, |i| Some((cards[i].rank.is_some(), place[i]))), // the unranked stay last
			SortKey::Rank => by(hits, r, |i| Some(place[i])),
			SortKey::Number => by(hits, r, |i| Some(cards[i].link.number())),
			SortKey::State(w) => {
				let workflow = workflows.get(w).ok_or_else(|| WorkflowError::Unknown(w.clone()))?;
				by(hits, r, |i| Some(workflow.order(&cards[i].states[w])))
			}
			SortKey::Importance => by(hits, r, |i| cards[i].importance.map(Reverse)),
			SortKey::Hours => by(hits, r, |i| cards[i].hours),
			SortKey::Updated => by(hits, r, |i| cards[i].updated.map(Reverse)),
			SortKey::Created => by(hits, r, |i| cards[i].created.map(Reverse)),
		}
		Ok(())
	}
}

/// Sorts applied in turn, each ordering only what those before it tie on: `importance,hours`.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct Order(Vec<Sort>);
impl Order {
	pub fn keys(&self) -> &[Sort] {
		&self.0
	}

	/// Reorders `hits` (as `Query::run` ordered them); ties keep that order.
	pub fn apply(&self, cards: &[Card], hits: &mut [usize], workflows: &Trees) -> Result<(), WorkflowError> {
		let mut place = vec![usize::MAX; cards.len()];
		for (at, &i) in hits.iter().enumerate() {
			place[i] = at;
		}
		for sort in self.0.iter().rev() {
			sort.apply(cards, hits, workflows, &place)?;
		}
		Ok(())
	}
}
impl From<Vec<Sort>> for Order {
	fn from(keys: Vec<Sort>) -> Self {
		assert!(!keys.is_empty(), "an order sorts by at least one key");
		Order(keys)
	}
}
impl std::fmt::Display for Order {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(&self.0.iter().map(ToString::to_string).collect::<Vec<_>>().join(","))
	}
}
impl FromStr for Order {
	type Err = strum::ParseError;

	fn from_str(s: &str) -> Result<Self, strum::ParseError> {
		let keys = s.split(',').map(str::parse).collect::<Result<Vec<Sort>, _>>()?;
		assert!(!keys.is_empty(), "`split` yields at least one item");
		Ok(Order(keys))
	}
}
impl TryFrom<String> for Order {
	type Error = strum::ParseError;

	fn try_from(s: String) -> Result<Self, strum::ParseError> {
		s.parse()
	}
}
impl From<Order> for String {
	fn from(order: Order) -> String {
		order.to_string()
	}
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SortKey {
	/// As the query ordered them: by free-text relevance when there is any, else by rank.
	Rank,
	Number,
	/// `state:<workflow>`
	State(String),
	Importance,
	Hours,
	Updated,
	Created,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub enum Shown {
	Number,
	Repo,
	Assignees,
	/// `state:<workflow>`
	State(String),
	Importance,
	Hours,
	Type,
	Labels,
	Blockers,
	Prs,
	/// The open sprints holding it.
	Sprint,
	Updated,
}
/// A named starting point: the terms narrowing to its own `repos` (`owner/repo`, `owner/*`) with those of every scope
/// inside it, and to its own `terms`.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
pub struct Scope {
	#[serde(default, skip_serializing_if = "Vec::is_empty")] // a scope may only group nested ones, or only hold terms
	pub repos: Vec<String>,
	#[serde(default, skip_serializing_if = "unset")] // a scope may only cover repos
	pub terms: Terms,
	#[serde(flatten)]
	pub children: BTreeMap<String, Scope>,
}
impl Scope {
	pub fn filter(&self) -> Terms {
		fn collect<'a>(scope: &'a Scope, out: &mut Vec<&'a str>) {
			out.extend(scope.repos.iter().map(String::as_str));
			for child in scope.children.values() {
				collect(child, out);
			}
		}
		let mut repos = Vec::new();
		collect(self, &mut repos);
		let mut filter = Terms::default();
		if !repos.is_empty() {
			filter.0.clauses.push(Clause {
				negated: false,
				any: repos.into_iter().map(|r| Filter::Repo(r.to_string())).collect(),
			});
		}
		filter.0.narrow(&self.terms);
		filter
	}

	/// What it lacks to be one: it holds neither repos, nor terms, nor another scope.
	pub fn is_empty(&self) -> bool {
		self.repos.is_empty() && unset(&self.terms) && self.children.is_empty()
	}
}
/// Its first `repo:` clause as repos, what else it narrows by as terms.
impl From<Terms> for Scope {
	fn from(Terms(mut query): Terms) -> Self {
		let repos = query.clauses.iter().position(|c| c.facet() == Facet::Repo && !c.negated).map(|at| query.clauses.remove(at));
		Scope {
			repos: repos.map_or_else(Vec::new, |c| c.any.iter().map(ToString::to_string).collect()), // it narrows by no repo
			terms: Terms(query),
			children: BTreeMap::new(),
		}
	}
}

/// The scope at `path` (names ` / `-joined).
pub fn scope_at<'a>(scopes: &'a BTreeMap<String, Scope>, path: &str) -> Option<&'a Scope> {
	let (mut level, mut at) = (scopes, None);
	for name in path.split(" / ") {
		let scope = level.get(name)?;
		(level, at) = (&scope.children, Some(scope));
	}
	at
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Placed {
	/// Its index among the cards.
	pub card: usize,
	/// Not where the query, the order or its state put it, for blocking a card there.
	pub lifted: bool,
}

/// `hits` (as an `Order` left them) split into groups, each keeping that order, but an open card blocking one
/// above it in its group goes right above that one, and so does one outside `hits`. A workflow's states come
/// in its order, every leaf present even when empty, and a further step counts as above: a blocker in an
/// earlier one than what it blocks goes over to that one's. Other keys sort by name, the ungrouped last.
pub fn group(cards: &[Card], hits: &[usize], by: &GroupBy, workflows: &Trees) -> Result<Vec<(String, Vec<Placed>)>, WorkflowError> {
	let open: HashMap<&IssueLink, usize> = cards.iter().enumerate().filter(|(_, c)| c.open()).map(|(i, c)| (&c.link, i)).collect();
	let blockers: Vec<Vec<usize>> = cards
		.iter()
		.map(|c| {
			if c.open() {
				c.blocked_by.iter().filter_map(|b| open.get(b).copied()).collect()
			} else {
				Vec::new()
			}
		})
		.collect();
	// `from`, then whatever blocks it
	let reach = |from: &[usize]| -> Vec<usize> {
		let mut out = from.to_vec();
		let mut seen = vec![false; cards.len()];
		for &i in from {
			seen[i] = true;
		}
		let mut next = 0;
		while let Some(&i) = out.get(next) {
			out.extend(blockers[i].iter().filter(|&&b| !std::mem::replace(&mut seen[b], true)));
			next += 1;
		}
		out
	};
	if let GroupBy::State(w) = by {
		let workflow = workflows.get(w).ok_or_else(|| WorkflowError::Unknown(w.clone()))?;
		let all = reach(hits);
		let mut step = vec![0; cards.len()];
		for &i in &all {
			step[i] = workflow.order(&cards[i].states[w]);
		}
		let held = step.clone();
		let mut raising = all.clone();
		while let Some(i) = raising.pop() {
			for &b in &blockers[i] {
				if step[b] < step[i] {
					step[b] = step[i];
					raising.push(b);
				}
			}
		}
		let (hits, brought) = all.split_at(hits.len());
		return Ok(workflow
			.leaves()
			.iter()
			.enumerate()
			.map(|(n, leaf)| {
				let (own, raised): (Vec<usize>, Vec<usize>) = hits.iter().copied().filter(|&i| step[i] == n).partition(|&i| held[i] == n);
				let lifted = raised.into_iter().chain(brought.iter().copied().filter(|&i| step[i] == n)).collect();
				(leaf.to_string(), lift(&blockers, own, lifted))
			})
			.collect());
	}
	let key = |c: &Card| -> Vec<String> {
		match by {
			GroupBy::State(_) => unreachable!("returned above"),
			GroupBy::Repo => vec![c.link.project().to_string()],
			GroupBy::Assignee if c.assignees.is_empty() => vec!["unassigned".into()],
			GroupBy::Assignee => c.assignees.iter().map(|a| format!("@{a}")).collect(),
			GroupBy::Type => vec![c.kind.map_or_else(|| "untyped".into(), |k| k.to_string())],
			GroupBy::Importance => vec![c.importance.map_or_else(|| "i:?".into(), |i| format!("i:{i}"))],
			GroupBy::Nothing => vec![String::new()],
		}
	};
	let mut groups: BTreeMap<String, Vec<usize>> = BTreeMap::new();
	for &i in hits {
		for k in key(&cards[i]) {
			groups.entry(k).or_default().push(i);
		}
	}
	let hit: HashSet<usize> = hits.iter().copied().collect();
	Ok(groups
		.into_iter()
		.map(|(key, own)| {
			let brought = reach(&own).into_iter().filter(|i| !hit.contains(i)).collect();
			(key, lift(&blockers, own, brought))
		})
		.collect())
}

fn unset(terms: &Terms) -> bool {
	terms.clauses.is_empty()
}

/// `own` in its order, each card right below whichever of `own` and `lifted` block it, through others too;
/// `lifted` blocking none of `own` go last. A cycle has no such order: it is entered where `own` first does.
fn lift(blockers: &[Vec<usize>], own: Vec<usize>, lifted: Vec<usize>) -> Vec<Placed> {
	let first_lifted = own.len();
	let members = [own, lifted].concat();
	let mut place = vec![None; blockers.len()];
	for (at, &i) in members.iter().enumerate() {
		place[i] = Some(at);
	}
	let mut seen = vec![false; blockers.len()];
	let mut out = Vec::with_capacity(members.len());
	for &root in &members {
		let mut stack = vec![(root, false)]; // a card, and whether its blockers are out
		while let Some((i, cleared)) = stack.pop() {
			if cleared {
				if let Some(at) = place[i] {
					out.push(Placed {
						card: i,
						lifted: i != root || at >= first_lifted,
					});
				}
				continue;
			}
			if std::mem::replace(&mut seen[i], true) {
				continue;
			}
			stack.push((i, true));
			let mut above = blockers[i].clone();
			above.sort_by_key(|&b| std::cmp::Reverse((place[b].is_none(), place[b]))); // the last is popped first
			stack.extend(above.into_iter().map(|b| (b, false)));
		}
	}
	assert_eq!(out.len(), members.len(), "each member is out once");
	out
}

#[derive(Clone, Debug, PartialEq)]
struct Clause {
	negated: bool,
	/// Of one facet, never empty.
	any: Vec<Filter>,
}
impl Clause {
	fn facet(&self) -> Facet {
		Facet::from(&self.any[0])
	}

	fn holds(&self, card: &Card, me: Option<&str>, open: &HashSet<&IssueLink>) -> bool {
		self.any.iter().any(|v| v.holds(card, me, open)) != self.negated
	}
}

/// One value of one facet.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, strum::EnumDiscriminants)]
#[strum_discriminants(name(Facet), vis(pub), derive(Hash, strum::Display, strum::EnumIter, strum::EnumString), strum(serialize_all = "snake_case"))]
enum Filter {
	/// `owner/repo`, `owner/*`, or a bare repo name.
	Repo(String),
	Assignee(Assignee),
	/// A bucket of a workflow, holding its sub-buckets.
	State {
		workflow: String,
		under: Vec<String>,
	},
	#[strum_discriminants(strum(serialize = "t"))]
	Type(Option<Type>),
	#[strum_discriminants(strum(serialize = "i"))]
	Importance(Option<(Cmp, u8)>),
	#[strum_discriminants(strum(serialize = "h"))]
	Hours(Option<(Cmp, u16)>),
	Label(String),
	Is(Is),
}
impl Filter {
	fn parse(facet: Facet, v: &str) -> Result<Self, QueryError> {
		let none = v == "none";
		Ok(match facet {
			Facet::Repo => Filter::Repo(v.to_string()),
			Facet::Assignee => Filter::Assignee(match v {
				"@me" => Assignee::Me,
				"none" => Assignee::Nobody,
				login => Assignee::Login(login.trim_start_matches('@').to_string()),
			}),
			Facet::State => match v.split_once('/') {
				Some((workflow, under)) if !workflow.is_empty() && !under.split('/').any(str::is_empty) => Filter::State {
					workflow: workflow.into(),
					under: under.split('/').map(str::to_string).collect(),
				},
				_ => return Err(QueryError::State(v.into())),
			},
			Facet::Type if none => Filter::Type(None),
			Facet::Type => Filter::Type(Some(Type::from_str(&format!("t:{v}")).map_err(|_| QueryError::Type(v.into()))?)),
			Facet::Importance if none => Filter::Importance(None),
			Facet::Importance => {
				let (cmp, n) = Cmp::split(v);
				Filter::Importance(Some((cmp, n.parse().map_err(|_| QueryError::Importance(v.into()))?)))
			}
			Facet::Hours if none => Filter::Hours(None),
			Facet::Hours => {
				let (cmp, n) = Cmp::split(v);
				Filter::Hours(Some((cmp, n.parse().map_err(|_| QueryError::Hours(v.into()))?)))
			}
			Facet::Label => Filter::Label(v.to_string()),
			Facet::Is => Filter::Is(Is::from_str(v).map_err(|_| QueryError::Is(v.into()))?),
		})
	}

	fn holds(&self, card: &Card, me: Option<&str>, open: &HashSet<&IssueLink>) -> bool {
		match self {
			Filter::Repo(pattern) => names(pattern, card.link.project()),
			Filter::Assignee(Assignee::Me) => me.is_some_and(|me| card.assignees.iter().any(|a| a == me)),
			Filter::Importance(Some((cmp, n))) => card.importance.is_some_and(|i| cmp.holds(i, *n)),
			Filter::Hours(Some((cmp, n))) => card.hours.is_some_and(|h| cmp.holds(h, *n)),
			Filter::State { workflow, under } => card.states.get(workflow).is_some_and(|s| s.is_under(under)),
			exact => takes(card, exact.into(), open).contains(exact),
		}
	}
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Assignee {
	Me,
	Nobody,
	Login(String),
}
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, strum::Display, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
enum Is {
	Open,
	Closed,
	/// By an open issue on the board.
	Blocked,
	Conflicted,
	Issue,
	Pr,
}
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd, strum::Display)]
enum Cmp {
	#[strum(to_string = "<")]
	Lt,
	#[strum(to_string = "<=")]
	Le,
	#[strum(to_string = "")]
	Eq,
	#[strum(to_string = ">=")]
	Ge,
	#[strum(to_string = ">")]
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
			let (negated, clause) = match token.strip_prefix('-') {
				Some(clause) if clause.contains(':') => (true, clause),
				_ => (false, token),
			};
			let Some((key, value)) = clause.split_once(':') else {
				text.push(token);
				continue;
			};
			let any = match Facet::from_str(key) {
				Ok(facet) => {
					let any = items(value).iter().map(|v| Filter::parse(facet, v)).collect::<Result<Vec<_>, _>>()?;
					if any.is_empty() {
						return Err(QueryError::Empty(facet));
					}
					any
				}
				// `c:ci`, `ext:hack` and any other label read as typed
				Err(_) => vec![Filter::Label(clause.to_string())],
			};
			query.clauses.push(Clause { negated, any });
		}
		query.text = text.join(" ");
		Ok(query)
	}
}

impl TryFrom<String> for Query {
	type Error = QueryError;

	fn try_from(s: String) -> Result<Self, QueryError> {
		s.parse()
	}
}
impl From<Query> for String {
	fn from(query: Query) -> String {
		query.to_string()
	}
}
impl TryFrom<String> for Terms {
	type Error = QueryError;

	fn try_from(s: String) -> Result<Self, QueryError> {
		s.parse()
	}
}
impl From<Terms> for String {
	fn from(terms: Terms) -> String {
		terms.to_string()
	}
}

/// Canonical: what parses back to the same query.
impl std::fmt::Display for Query {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let mut parts: Vec<String> = self.clauses.iter().map(ToString::to_string).collect();
		if !self.text.is_empty() {
			parts.push(self.text.clone());
		}
		f.write_str(&parts.join(" "))
	}
}
impl std::fmt::Display for Clause {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let values: Vec<String> = self.any.iter().map(|v| v.to_string().replace(',', "\\,")).collect();
		write!(f, "{}{}:{}", if self.negated { "-" } else { "" }, self.facet(), values.join(","))
	}
}
impl std::fmt::Display for Filter {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Filter::Repo(s) | Filter::Label(s) | Filter::Assignee(Assignee::Login(s)) => f.write_str(s),
			Filter::Assignee(Assignee::Me) => f.write_str("@me"),
			Filter::Assignee(Assignee::Nobody) | Filter::Type(None) | Filter::Importance(None) | Filter::Hours(None) => f.write_str("none"),
			Filter::State { workflow, under } => write!(f, "{workflow}/{}", under.join("/")),
			Filter::Type(Some(k)) => f.write_str(&k.to_string()["t:".len()..]),
			Filter::Importance(Some((cmp, n))) => write!(f, "{cmp}{n}"),
			Filter::Hours(Some((cmp, n))) => write!(f, "{cmp}{n}"),
			Filter::Is(is) => write!(f, "{is}"),
		}
	}
}

/// The values `card` takes on `facet`: what a tally counts it under, and what an exact value holds on.
fn takes(card: &Card, facet: Facet, open: &HashSet<&IssueLink>) -> Vec<Filter> {
	match facet {
		Facet::Repo => vec![Filter::Repo(card.link.project().to_string())],
		Facet::Assignee if card.assignees.is_empty() => vec![Filter::Assignee(Assignee::Nobody)],
		Facet::Assignee => card.assignees.iter().map(|a| Filter::Assignee(Assignee::Login(a.clone()))).collect(),
		Facet::State => card
			.states
			.iter()
			.map(|(workflow, s)| Filter::State {
				workflow: workflow.clone(),
				under: s.names().to_vec(),
			})
			.collect(),
		Facet::Type => vec![Filter::Type(card.kind)],
		Facet::Importance => vec![Filter::Importance(card.importance.map(|i| (Cmp::Eq, i)))],
		Facet::Hours => vec![Filter::Hours(card.hours.map(|h| (Cmp::Eq, h)))],
		Facet::Label => card.labels.iter().cloned().map(Filter::Label).collect(),
		Facet::Is => [
			(card.open(), Is::Open),
			(!card.open(), Is::Closed),
			(card.blocked_by.iter().any(|b| open.contains(b)), Is::Blocked),
			(card.conflicted, Is::Conflicted),
			(card.pr.is_none(), Is::Issue),
			(card.pr.is_some(), Is::Pr),
		]
		.into_iter()
		.filter_map(|(holds, is)| holds.then_some(Filter::Is(is)))
		.collect(),
	}
}

/// A clause's `,`-separated values; `\\,` is a comma inside one.
fn items(value: &str) -> Vec<String> {
	let mut items = vec![String::new()];
	let mut chars = value.chars().peekable();
	while let Some(c) = chars.next() {
		let item = items.last_mut().expect("starts with one");
		match c {
			'\\' if chars.peek() == Some(&',') => item.push(chars.next().expect("peeked")),
			',' => items.push(String::new()),
			c => item.push(c),
		}
	}
	items.retain(|i| !i.is_empty());
	items
}

/// Whether a `repo:` value (`owner/repo`, `owner/*`, a bare name) names `project`.
fn names(pattern: &str, project: RepoInfo) -> bool {
	match pattern.split_once('/') {
		// GitHub names are case-insensitive, and links store them lowercased
		Some((owner, "*")) => project.owner().is_some_and(|o| o.eq_ignore_ascii_case(owner)),
		Some(_) => project.to_string().eq_ignore_ascii_case(pattern),
		None => project.repo().eq_ignore_ascii_case(pattern),
	}
}

fn open(cards: &[Card]) -> HashSet<&IssueLink> {
	cards.iter().filter(|c| c.open()).map(|c| &c.link).collect()
}

impl TryFrom<Shown> for Facet {
	type Error = Shown;

	fn try_from(shown: Shown) -> Result<Self, Shown> {
		Ok(match shown {
			Shown::Repo => Facet::Repo,
			Shown::Assignees => Facet::Assignee,
			Shown::State(_) => Facet::State,
			Shown::Importance => Facet::Importance,
			Shown::Hours => Facet::Hours,
			Shown::Type => Facet::Type,
			Shown::Labels => Facet::Label,
			Shown::Blockers | Shown::Prs | Shown::Number | Shown::Sprint | Shown::Updated => return Err(shown),
		})
	}
}

/// `state:<workflow>`, or one of `plain` by its name.
fn parse_keyed<T: Clone>(s: &str, plain: &[(&str, T)], state: impl FnOnce(String) -> T) -> Result<T, strum::ParseError> {
	match s.strip_prefix("state:") {
		Some(workflow) if !workflow.is_empty() => Ok(state(workflow.into())),
		Some(_) => Err(strum::ParseError::VariantNotFound),
		None => plain.iter().find(|(name, _)| *name == s).map(|(_, v)| v.clone()).ok_or(strum::ParseError::VariantNotFound),
	}
}
macro_rules! keyed {
	($t:ident { $($v:ident = $name:literal),* $(,)? }) => {
		impl std::fmt::Display for $t {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				match self {
					$t::State(workflow) => write!(f, "state:{workflow}"),
					$($t::$v => f.write_str($name),)*
				}
			}
		}
		impl FromStr for $t {
			type Err = strum::ParseError;

			fn from_str(s: &str) -> Result<Self, strum::ParseError> {
				parse_keyed(s, &[$(($name, $t::$v)),*], $t::State)
			}
		}
		impl TryFrom<String> for $t {
			type Error = strum::ParseError;

			fn try_from(s: String) -> Result<Self, strum::ParseError> {
				s.parse()
			}
		}
		impl From<$t> for String {
			fn from(v: $t) -> String {
				v.to_string()
			}
		}
	};
}
keyed!(GroupBy {
	Repo = "repo",
	Assignee = "assignee",
	Type = "type",
	Importance = "importance",
	Nothing = "nothing",
});
keyed!(Shown {
	Repo = "repo",
	Assignees = "assignees",
	Importance = "importance",
	Hours = "hours",
	Type = "type",
	Labels = "labels",
	Blockers = "blockers",
	Prs = "prs",
	Number = "number",
	Sprint = "sprint",
	Updated = "updated",
});
keyed!(SortKey {
	Rank = "rank",
	Number = "number",
	Importance = "importance",
	Hours = "hours",
	Updated = "updated",
	Created = "created",
});
impl std::fmt::Display for Sort {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}{}", if self.reversed { "-" } else { "" }, self.by)
	}
}
impl FromStr for Sort {
	type Err = strum::ParseError;

	fn from_str(s: &str) -> Result<Self, strum::ParseError> {
		let (reversed, by) = match s.strip_prefix('-') {
			Some(by) => (true, by),
			None => (false, s),
		};
		Ok(Sort { by: by.parse()?, reversed })
	}
}
impl TryFrom<String> for Sort {
	type Error = strum::ParseError;

	fn try_from(s: String) -> Result<Self, strum::ParseError> {
		s.parse()
	}
}
impl From<Sort> for String {
	fn from(sort: Sort) -> String {
		sort.to_string()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::labels::Scheme;

	fn kanban() -> Trees {
		let buckets = ["icebox", "triage", "selected", "in_progress", "in_review", "done", "wont_do", "backlog"]
			.map(|name| crate::workflow::Bucket {
				name: name.into(),
				on_enter: Vec::new(),
				buckets: Vec::new(),
			})
			.to_vec();
		let workflow = crate::workflow::Workflow {
			intake: "triage".parse().unwrap(),
			buckets,
			rules: Vec::new(),
		};
		BTreeMap::from([("kanban".to_string(), workflow)])
	}

	fn card(n: u64, title: &str, repo: &str, state: &str, labels: &[&str], assignees: &[&str], blocked_by: &[u64]) -> Card {
		let link = |n: u64| IssueLink::parse(&format!("https://github.com/o/{repo}/issues/{n}")).expect("a github issue url");
		let labels: Vec<String> = labels.iter().map(|l| l.to_string()).collect();
		let scheme = Scheme::from(labels.as_slice());
		let open = !["done", "wont_do", "backlog"].contains(&state);
		Card {
			link: link(n),
			title: title.into(),
			assignees: assignees.iter().map(|a| a.to_string()).collect(),
			state: if open { CloseState::Open } else { CloseState::Closed },
			closed_at: None,
			updated: None,
			created: None,
			states: BTreeMap::from([("kanban".to_string(), state.parse().unwrap())]),
			rank: None,
			importance: scheme.importance,
			hours: scheme.hours,
			kind: scheme.kind,
			labels,
			prs: Vec::new(),
			blocked_by: blocked_by.iter().map(|&b| link(b)).collect(),
			parent: None,
			body: String::new(),
			conflicted: false,
			pr: None,
		}
	}

	fn cards() -> Vec<Card> {
		vec![
			card(1, "Crash on save", "app", "triage", &["t:bug", "i:8", "h:3"], &["ann"], &[]),
			card(2, "Dark mode", "app", "selected", &["t:feature", "i:5"], &["bob"], &[1]),
			card(3, "bump deps", "lib", "triage", &["t:chore", "h:1", "c:ci"], &[], &[]),
			card(4, "Typo in README", "lib", "done", &["t:bug", "i:2", "h:1"], &["ann", "bob"], &[]),
			card(5, "Untyped idea", "app", "icebox", &[], &[], &[1, 2]),
		]
	}

	fn titles(cards: &[Card], hits: &[usize]) -> String {
		hits.iter().map(|&i| cards[i].title.as_str()).collect::<Vec<_>>().join(" · ")
	}

	#[test]
	fn display_parses_back_to_the_query() {
		let corpus = [
			"is:open state:kanban/icebox,kanban/triage,kanban/in_progress",
			"assignee:@me,none,@ann is:open,blocked",
			"is:pr,issue -is:conflicted",
			"-t:chore,none t:bug i:>=5,none,=3 h:<=8,<2",
			"repo:o/*,o/app,lib -repo:o/lib",
			"c:ci -x:wip label:x,y",
			r"label:a\,b odd:x,y",
			"state:kanban/triage free  text -not:a:flag -plain",
		];
		let out: Vec<String> = corpus
			.iter()
			.map(|q| {
				let parsed: Query = q.parse().expect("the corpus parses");
				let shown = parsed.to_string();
				assert_eq!(shown.parse::<Query>().expect("display parses"), parsed, "{q}");
				format!("{q}\n  → {shown}")
			})
			.collect();
		insta::assert_snapshot!(out.join("\n"), @r"
		is:open state:kanban/icebox,kanban/triage,kanban/in_progress
		  → is:open state:kanban/icebox,kanban/triage,kanban/in_progress
		assignee:@me,none,@ann is:open,blocked
		  → assignee:@me,none,ann is:open,blocked
		is:pr,issue -is:conflicted
		  → is:pr,issue -is:conflicted
		-t:chore,none t:bug i:>=5,none,=3 h:<=8,<2
		  → -t:chore,none t:bug i:>=5,none,3 h:<=8,<2
		repo:o/*,o/app,lib -repo:o/lib
		  → repo:o/*,o/app,lib -repo:o/lib
		c:ci -x:wip label:x,y
		  → label:c:ci -label:x:wip label:x,y
		label:a\,b odd:x,y
		  → label:a\,b label:odd:x\,y
		state:kanban/triage free  text -not:a:flag -plain
		  → state:kanban/triage -label:not:a:flag free text -plain
		");
	}

	#[test]
	fn toggling_rewrites_one_clause() {
		let steps: [(Facet, &str, bool); 8] = [
			(Facet::State, "kanban/triage", false),
			(Facet::State, "kanban/selected", false),
			(Facet::Type, "chore", true),
			(Facet::Type, "chore", false),            // flips polarity
			(Facet::State, "kanban/triage", false),   // again: dropped
			(Facet::State, "kanban/selected", false), // last value: clause dropped
			(Facet::Assignee, "@ann", true),
			(Facet::Importance, "none", false),
		];
		let mut q: Query = "is:open some text".parse().unwrap();
		let mut out = vec![q.to_string()];
		for (facet, value, exclude) in steps {
			q.toggle(facet, value, exclude).unwrap();
			out.push(format!("{}{facet}:{value} → {q}", if exclude { "alt " } else { "" }));
		}
		q.clear(Facet::Assignee);
		out.push(format!("clear assignee → {q}"));
		insta::assert_snapshot!(out.join("\n"), @"
		is:open some text
		state:kanban/triage → is:open state:kanban/triage some text
		state:kanban/selected → is:open state:kanban/triage,kanban/selected some text
		alt t:chore → is:open state:kanban/triage,kanban/selected -t:chore some text
		t:chore → is:open state:kanban/triage,kanban/selected t:chore some text
		state:kanban/triage → is:open state:kanban/selected t:chore some text
		state:kanban/selected → is:open t:chore some text
		alt assignee:@ann → is:open t:chore -assignee:ann some text
		i:none → is:open t:chore -assignee:ann i:none some text
		clear assignee → is:open t:chore i:none some text
		");
	}

	#[test]
	fn tally_lifts_the_facets_own_clauses() {
		let cards = cards();
		let out: Vec<String> = [
			("state:kanban/triage -t:chore", Facet::State),
			("state:kanban/triage -t:chore", Facet::Type),
			("i:>=5", Facet::Importance),
			("is:open", Facet::Assignee),
			("", Facet::Is),
		]
		.into_iter()
		.map(|(q, facet)| {
			let tallies = q.parse::<Query>().unwrap().tally(facet, &Terms::default(), &cards, None).unwrap();
			let shown: Vec<String> = tallies
				.iter()
				.map(|t| {
					format!(
						"{}{} {}",
						match t.picked {
							Some(true) => "+",
							Some(false) => "-",
							None => "",
						},
						t.value,
						t.count
					)
				})
				.collect();
			format!("{q} | {facet}: {}", shown.join(", "))
		})
		.collect();
		insta::assert_snapshot!(out.join("\n"), @"
		state:kanban/triage -t:chore | state: kanban/done 1, kanban/icebox 1, kanban/selected 1, +kanban/triage 1
		state:kanban/triage -t:chore | t: bug 1, -chore 1
		i:>=5 | i: none 2, +>=5 2, 2 1, 5 1, 8 1
		is:open | assignee: none 2, ann 1, bob 1
		 | is: issue 5, open 4, blocked 2, closed 1
		");
	}

	#[test]
	fn tally_never_lifts_the_scope() {
		let cards = cards();
		let within: Terms = "repo:o/app".parse().unwrap();
		let out: Vec<String> = [("", Facet::Repo), ("repo:o/lib", Facet::Repo), ("", Facet::Is)]
			.into_iter()
			.map(|(q, facet)| {
				let tallies = q.parse::<Query>().unwrap().tally(facet, &within, &cards, None).unwrap();
				let shown: Vec<String> = tallies
					.iter()
					.map(|t| format!("{}{} {}", if t.picked == Some(true) { "+" } else { "" }, t.value, t.count))
					.collect();
				format!("{q} | {facet}: {}", shown.join(", "))
			})
			.collect();
		insta::assert_snapshot!(out.join("\n"), @"
		 | repo: o/app 3
		repo:o/lib | repo: o/app 3, +o/lib 0
		 | is: open 3, issue 3, blocked 2
		");
	}

	#[test]
	fn a_tallied_value_picks_what_it_counted() {
		let cards = cards();
		for facet in <Facet as strum::IntoEnumIterator>::iter() {
			for t in Query::default().tally(facet, &Terms::default(), &cards, None).unwrap() {
				let mut q = Query::default();
				q.toggle(facet, &t.value, false).unwrap();
				assert_eq!(q.run(&cards, None).unwrap().len(), t.count, "{q}");
			}
		}
	}

	#[test]
	fn rank_ties_put_a_set_importance_first() {
		let mut cards = vec![
			card(1, "defaulted", "app", "triage", &[], &[], &[]),
			card(2, "set to the default", "app", "triage", &["i:3"], &[], &[]),
		];
		for c in &mut cards {
			c.rank = Some(30_003); // `i:3` and an unset `i:` (ranked as 3) tie
		}
		let hits = Query::from_str("is:open").unwrap().run(&cards, None).unwrap();
		assert_eq!(titles(&cards, &hits), "set to the default · defaulted");
	}

	#[test]
	fn a_blocker_is_lifted_right_above_what_it_blocks() {
		let cards = [
			card(1, "a", "app", "triage", &[], &["ann"], &[]),
			card(2, "b", "app", "selected", &[], &["bob"], &[1]),
			card(3, "c", "app", "triage", &[], &["ann"], &[2, 4]),
			card(4, "d", "app", "triage", &[], &["ann"], &[]),
			card(5, "e", "app", "triage", &[], &["ann"], &[6]),
			card(6, "done", "app", "done", &[], &["ann"], &[1]),
			card(7, "f", "app", "triage", &[], &["ann"], &[8]),
			card(8, "g", "app", "triage", &[], &["ann"], &[7]),
			card(9, "h", "app", "triage", &[], &["bob"], &[10]),
			card(10, "i", "app", "in_review", &[], &["ann"], &[11]),
			card(11, "j", "app", "icebox", &[], &["ann"], &[]),
		];
		let out: Vec<String> = [
			[0, 1, 2].as_slice(),
			&[3, 2, 0, 1],
			&[4, 2, 3, 0],
			&[3, 0, 2],
			&[4, 2],
			&[1],
			&[4, 5, 0],
			&[6, 7],
			&[7],
			&[10, 8, 9],
			&[8, 3],
		]
		.into_iter()
		.flat_map(|hits| {
			[GroupBy::Nothing, GroupBy::State("kanban".into()), GroupBy::Assignee].map(|by| {
				let groups = group(&cards, hits, &by, &kanban()).unwrap();
				let shown = groups.iter().filter(|(_, m)| !m.is_empty()).map(|(key, members)| {
					let members = members.iter().map(|p| format!("{}{}", if p.lifted { "^" } else { "" }, cards[p.card].title));
					format!("{key}[{}]", members.collect::<Vec<_>>().join(" "))
				});
				format!("{}: {}", titles(&cards, hits), shown.collect::<Vec<_>>().join(" "))
			})
		})
		.collect();
		insta::assert_snapshot!(out.join("\n"), @"
		a · b · c: [a b ^d c]
		a · b · c: triage[^d c] selected[^a b]
		a · b · c: @ann[a ^d c] @bob[b]
		d · c · a · b: [d ^a ^b c]
		d · c · a · b: triage[d c] selected[^a b]
		d · c · a · b: @ann[d ^a c] @bob[b]
		e · c · d · a: [e ^d ^a ^b c]
		e · c · d · a: triage[e ^d c] selected[^a ^b]
		e · c · d · a: @ann[e ^d ^a ^b c]
		d · a · c: [d a ^b c]
		d · a · c: triage[d c] selected[^a ^b]
		d · a · c: @ann[d a ^b c]
		e · c: [e ^a ^b ^d c]
		e · c: triage[e ^d c] selected[^a ^b]
		e · c: @ann[e ^a ^b ^d c]
		b: [^a b]
		b: selected[^a b]
		b: @bob[^a b]
		e · done · a: [e done a]
		e · done · a: triage[e a] done[done]
		e · done · a: @ann[e done a]
		f · g: [^g f]
		f · g: triage[^g f]
		f · g: @ann[^g f]
		g: [^f g]
		g: triage[^f g]
		g: @ann[^f g]
		j · h · i: [j ^i h]
		j · h · i: triage[h] in_review[^j i]
		j · h · i: @ann[j i] @bob[h]
		h · d: [^j ^i h d]
		h · d: triage[h d] in_review[^j ^i]
		h · d: @ann[d] @bob[^j ^i h]
		");
	}

	#[test]
	fn sorts_unset_last_either_way() {
		let mut cards = cards();
		for (card, created) in cards.iter_mut().zip(["2026-01-03", "2026-01-01", "2026-01-04", "2026-01-02"]) {
			card.created = Some(format!("{created}T00:00:00Z").parse().unwrap());
		}
		let hits: Vec<usize> = (0..cards.len()).collect();
		let out: Vec<String> = [
			"rank",
			"-rank",
			"importance",
			"-importance",
			"hours",
			"-hours",
			"state:kanban",
			"-number",
			"updated",
			"created",
			"-created",
			"state:kanban,-importance",
			"hours,rank",
			"-hours,-rank",
			"state:kanban,hours",
		]
		.into_iter()
		.map(|s| {
			let order = Order::try_from(s.to_string()).expect("an order as views store it");
			assert_eq!(order.to_string(), s);
			let mut sorted = hits.clone();
			order.apply(&cards, &mut sorted, &kanban()).unwrap();
			format!("{s}: {}", titles(&cards, &sorted))
		})
		.collect();
		insta::assert_snapshot!(out.join("\n"), @"
		rank: Crash on save · Dark mode · bump deps · Typo in README · Untyped idea
		-rank: Untyped idea · Typo in README · bump deps · Dark mode · Crash on save
		importance: Crash on save · Dark mode · Typo in README · bump deps · Untyped idea
		-importance: Typo in README · Dark mode · Crash on save · bump deps · Untyped idea
		hours: bump deps · Typo in README · Crash on save · Dark mode · Untyped idea
		-hours: Crash on save · bump deps · Typo in README · Dark mode · Untyped idea
		state:kanban: Untyped idea · Crash on save · bump deps · Dark mode · Typo in README
		-number: Untyped idea · Typo in README · bump deps · Dark mode · Crash on save
		updated: Crash on save · Dark mode · bump deps · Typo in README · Untyped idea
		created: bump deps · Crash on save · Typo in README · Dark mode · Untyped idea
		-created: Dark mode · Typo in README · Crash on save · bump deps · Untyped idea
		state:kanban,-importance: Untyped idea · Crash on save · bump deps · Dark mode · Typo in README
		hours,rank: bump deps · Typo in README · Crash on save · Dark mode · Untyped idea
		-hours,-rank: Crash on save · Typo in README · bump deps · Untyped idea · Dark mode
		state:kanban,hours: Untyped idea · bump deps · Crash on save · Dark mode · Typo in README
		");
	}
}
