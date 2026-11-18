//! The board: every stored issue as a card, a query over them, and views — named queries with a
//! layout, a grouping and an order. Pure over cards; reading them from the store and editing them
//! through the sync are the operations' business.

use std::{
	collections::{BTreeMap, HashSet},
	str::FromStr,
};

use serde::{Deserialize, Serialize};

use crate::{
	CloseState, IssueLink, LinkedPr, PrState,
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
	/// `Some` on a pull request: read-only, with no blockers, children or pins of its own.
	pub pr: Option<Pr>,
}
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Pr {
	pub state: PrState,
	pub author: String,
	pub closes: Vec<IssueLink>,
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
	/// Per `owner/repo`, its labels' colours as last read from GitHub; a label missing here is uncoloured.
	pub colors: BTreeMap<String, BTreeMap<String, crate::labels::Color>>,
}

#[derive(Debug, thiserror::Error)]
pub enum QueryError {
	#[error("no stage {0:?}")]
	Stage(String),
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
	#[error(transparent)]
	Search(#[from] tedi_utils::search::SearchError),
}

/// `repo:EV-Service-Arb/*,EV-Main/tilers assignee:@me stage:triage,selected -t:chore i:>=5,none h:<=8 is:blocked label:x free text`.
/// Clauses AND, the values of one OR, a `-` clause holds none of them; `none` is a value unset.
/// What isn't a clause is free text, ranked by `tedi_utils::search`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Query {
	clauses: Vec<Clause>,
	text: String,
}
#[derive(Clone, Debug, PartialEq)]
struct Clause {
	negated: bool,
	/// Of one facet, never empty.
	any: Vec<Filter>,
}
/// One value of one facet.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, strum::EnumDiscriminants)]
#[strum_discriminants(name(Facet), vis(pub), derive(Hash, strum::Display, strum::EnumIter, strum::EnumString), strum(serialize_all = "snake_case"))]
enum Filter {
	/// `owner/repo`, `owner/*`, or a bare repo name.
	Repo(String),
	Assignee(Assignee),
	Stage(Stage),
	#[strum_discriminants(strum(serialize = "t"))]
	Type(Option<Type>),
	#[strum_discriminants(strum(serialize = "i"))]
	Importance(Option<(Cmp, u8)>),
	#[strum_discriminants(strum(serialize = "h"))]
	Hours(Option<(Cmp, u16)>),
	Label(String),
	Is(Is),
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

/// A value of a facet as a quick filter offers it: how many of the query's hits take it, the
/// facet's own clauses aside, and whether those include it (`Some(true)`) or exclude it.
#[derive(Clone, Debug, PartialEq)]
pub struct Tally {
	pub value: String,
	pub count: usize,
	pub picked: Option<bool>,
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
				// `c:ci`, `ext:hack`, `s:wip` and any other label read as typed
				Err(_) => vec![Filter::Label(clause.to_string())],
			};
			query.clauses.push(Clause { negated, any });
		}
		query.text = text.join(" ");
		Ok(query)
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
			Filter::Stage(s) => write!(f, "{s}"),
			Filter::Type(Some(k)) => f.write_str(&k.to_string()["t:".len()..]),
			Filter::Importance(Some((cmp, n))) => write!(f, "{cmp}{n}"),
			Filter::Hours(Some((cmp, n))) => write!(f, "{cmp}{n}"),
			Filter::Is(is) => write!(f, "{is}"),
		}
	}
}

impl Query {
	/// Indices into `cards` that match, best first: by free text when there is any, else by rank.
	pub fn run(&self, cards: &[Card], me: Option<&str>) -> Result<Vec<usize>, QueryError> {
		let open = open(cards);
		let passing: Vec<usize> = (0..cards.len()).filter(|&i| self.clauses.iter().all(|c| c.holds(&cards[i], me, &open))).collect();
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

	pub fn clear(&mut self, facet: Facet) {
		self.clauses.retain(|c| c.facet() != facet);
	}

	/// Every value of `facet` the query's hits take, with the facet's own clauses lifted, most taken
	/// first; and each value those clauses pick, taken or not.
	pub fn tally(&self, facet: Facet, cards: &[Card], me: Option<&str>) -> Result<Vec<Tally>, QueryError> {
		let mut rest = self.clone();
		rest.clear(facet);
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

impl Clause {
	fn facet(&self) -> Facet {
		Facet::from(&self.any[0])
	}

	fn holds(&self, card: &Card, me: Option<&str>, open: &HashSet<&IssueLink>) -> bool {
		self.any.iter().any(|v| v.holds(card, me, open)) != self.negated
	}
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
			Facet::Stage => Filter::Stage(Stage::from_str(v).map_err(|_| QueryError::Stage(v.into()))?),
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
			Filter::Repo(pattern) => {
				let project = card.link.project();
				match pattern.split_once('/') {
					// GitHub names are case-insensitive, and links store them lowercased
					Some((owner, "*")) => project.owner().is_some_and(|o| o.eq_ignore_ascii_case(owner)),
					Some(_) => project.to_string().eq_ignore_ascii_case(pattern),
					None => project.repo().eq_ignore_ascii_case(pattern),
				}
			}
			Filter::Assignee(Assignee::Me) => me.is_some_and(|me| card.assignees.iter().any(|a| a == me)),
			Filter::Importance(Some((cmp, n))) => card.importance.is_some_and(|i| cmp.holds(i, *n)),
			Filter::Hours(Some((cmp, n))) => card.hours.is_some_and(|h| cmp.holds(h, *n)),
			exact => takes(card, exact.into(), open).contains(exact),
		}
	}
}

/// The values `card` takes on `facet`: what a tally counts it under, and what an exact value holds on.
fn takes(card: &Card, facet: Facet, open: &HashSet<&IssueLink>) -> Vec<Filter> {
	match facet {
		Facet::Repo => vec![Filter::Repo(card.link.project().to_string())],
		Facet::Assignee if card.assignees.is_empty() => vec![Filter::Assignee(Assignee::Nobody)],
		Facet::Assignee => card.assignees.iter().map(|a| Filter::Assignee(Assignee::Login(a.clone()))).collect(),
		Facet::Stage => vec![Filter::Stage(card.stage)],
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

fn open(cards: &[Card]) -> HashSet<&IssueLink> {
	cards.iter().filter(|c| c.open()).map(|c| &c.link).collect()
}

impl TryFrom<Shown> for Facet {
	type Error = Shown;

	fn try_from(shown: Shown) -> Result<Self, Shown> {
		Ok(match shown {
			Shown::Repo => Facet::Repo,
			Shown::Assignees => Facet::Assignee,
			Shown::Stage => Facet::Stage,
			Shown::Importance => Facet::Importance,
			Shown::Hours => Facet::Hours,
			Shown::Type => Facet::Type,
			Shown::Labels => Facet::Label,
			Shown::Blockers | Shown::Prs => return Err(shown),
		})
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
#[derive(Clone, Copy, Debug, Deserialize, Eq, Ord, PartialEq, PartialOrd, Serialize, strum::Display, strum::EnumIter, strum::EnumString)]
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
/// `importance`, or `-importance` reversed. Each key's own order: importance high→low, hours low→high,
/// stage board order, names A→Z, most blockers first; a card lacking the key last either way.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct Sort {
	pub by: SortKey,
	pub reversed: bool,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq, strum::Display, strum::EnumIter, strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum SortKey {
	/// As the query ordered them: by free-text relevance when there is any, else by rank.
	Rank,
	Title,
	Repo,
	Stage,
	Assignee,
	Type,
	Importance,
	Hours,
	Blockers,
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

impl Sort {
	/// Reorders `hits` (as `Query::run` ordered them); ties keep that order.
	pub fn apply(self, cards: &[Card], hits: &mut [usize]) {
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
		match self.by {
			SortKey::Rank if r => {
				hits.reverse();
				hits.sort_by_key(|&i| cards[i].rank.is_none());
			}
			SortKey::Rank => {}
			SortKey::Title => by(hits, r, |i| Some(cards[i].title.to_lowercase())),
			SortKey::Repo => by(hits, r, |i| Some(cards[i].link.project().to_string())),
			SortKey::Stage => by(hits, r, |i| Some(cards[i].stage)),
			SortKey::Assignee => by(hits, r, |i| cards[i].assignees.iter().map(|a| a.to_lowercase()).min()),
			SortKey::Type => by(hits, r, |i| cards[i].kind.map(|k| k.to_string())),
			SortKey::Importance => by(hits, r, |i| cards[i].importance.map(Reverse)),
			SortKey::Hours => by(hits, r, |i| cards[i].hours),
			SortKey::Blockers => {
				let open = open(cards);
				by(hits, r, |i| Some(Reverse(cards[i].blocked_by.iter().filter(|b| open.contains(b)).count())))
			}
		}
	}
}

impl TryFrom<Shown> for SortKey {
	type Error = Shown;

	fn try_from(shown: Shown) -> Result<Self, Shown> {
		Ok(match shown {
			Shown::Repo => SortKey::Repo,
			Shown::Assignees => SortKey::Assignee,
			Shown::Stage => SortKey::Stage,
			Shown::Importance => SortKey::Importance,
			Shown::Hours => SortKey::Hours,
			Shown::Type => SortKey::Type,
			Shown::Blockers => SortKey::Blockers,
			Shown::Labels | Shown::Prs => return Err(shown),
		})
	}
}

/// A named slice of the team's repos, nesting narrower ones: it covers its own `repos` (`owner/repo`,
/// `owner/*`) and those of every scope inside it.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Scope {
	#[serde(default)] // a scope may only group nested ones
	pub repos: Vec<String>,
	/// The team views it shows, by name; none: its parent's.
	#[serde(default)]
	pub views: Vec<String>,
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

/// Each scope's views by path (names ` / `-joined): the `team` views it names, narrowed to it and named `{path} · {view}`.
pub fn scoped_views(scopes: &BTreeMap<String, Scope>, team: &[View]) -> BTreeMap<String, Vec<View>> {
	fn walk(prefix: &str, scopes: &BTreeMap<String, Scope>, inherited: &[String], team: &[View], out: &mut BTreeMap<String, Vec<View>>) {
		for (name, scope) in scopes {
			let path = format!("{prefix}{name}");
			let names = if scope.views.is_empty() { inherited } else { &scope.views };
			let views = names
				.iter()
				.map(|n| {
					let view = team
						.iter()
						.find(|v| &v.name == n)
						.unwrap_or_else(|| panic!("the scope {path} shows the view {n}, which no team view is named"));
					View {
						name: format!("{path} · {n}"),
						query: format!("{} {}", scope.filter(), view.query),
						..view.clone()
					}
				})
				.collect();
			out.insert(path.clone(), views);
			walk(&format!("{path} / "), &scope.children, names, team, out);
		}
	}
	let mut out = BTreeMap::new();
	walk("", scopes, &[], team, &mut out);
	out
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

#[cfg(test)]
mod tests {
	use super::*;
	use crate::labels::Scheme;

	fn card(n: u64, title: &str, repo: &str, stage: Stage, labels: &[&str], assignees: &[&str], blocked_by: &[u64]) -> Card {
		let link = |n: u64| IssueLink::parse(&format!("https://github.com/o/{repo}/issues/{n}")).expect("a github issue url");
		let labels: Vec<String> = labels.iter().map(|l| l.to_string()).collect();
		let scheme = Scheme::from(labels.as_slice());
		let open = !matches!(stage, Stage::Done | Stage::WontDo | Stage::Backlog);
		Card {
			link: link(n),
			title: title.into(),
			assignees: assignees.iter().map(|a| a.to_string()).collect(),
			state: if open { CloseState::Open } else { CloseState::Closed },
			stage,
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
			card(1, "Crash on save", "app", Stage::Triage, &["t:bug", "i:8", "h:3"], &["ann"], &[]),
			card(2, "Dark mode", "app", Stage::Selected, &["t:feature", "i:5"], &["bob"], &[1]),
			card(3, "bump deps", "lib", Stage::Triage, &["t:chore", "h:1", "c:ci"], &[], &[]),
			card(4, "Typo in README", "lib", Stage::Done, &["t:bug", "i:2", "h:1"], &["ann", "bob"], &[]),
			card(5, "Untyped idea", "app", Stage::Icebox, &[], &[], &[1, 2]),
		]
	}

	fn titles(cards: &[Card], hits: &[usize]) -> String {
		hits.iter().map(|&i| cards[i].title.as_str()).collect::<Vec<_>>().join(" · ")
	}

	#[test]
	fn display_parses_back_to_the_query() {
		let corpus = [
			"is:open stage:icebox,triage,selected,in_progress,in_review",
			"assignee:@me,none,@ann is:open,blocked",
			"is:pr,issue -is:conflicted",
			"-t:chore,none t:bug i:>=5,none,=3 h:<=8,<2",
			"repo:o/*,o/app,lib -repo:o/lib",
			"c:ci -s:wip label:x,y",
			r"label:a\,b odd:x,y",
			"stage:triage free  text -not:a:flag -plain",
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
		is:open stage:icebox,triage,selected,in_progress,in_review
		  → is:open stage:icebox,triage,selected,in_progress,in_review
		assignee:@me,none,@ann is:open,blocked
		  → assignee:@me,none,ann is:open,blocked
		-t:chore,none t:bug i:>=5,none,=3 h:<=8,<2
		  → -t:chore,none t:bug i:>=5,none,3 h:<=8,<2
		repo:o/*,o/app,lib -repo:o/lib
		  → repo:o/*,o/app,lib -repo:o/lib
		c:ci -s:wip label:x,y
		  → label:c:ci -label:s:wip label:x,y
		label:a\,b odd:x,y
		  → label:a\,b label:odd:x\,y
		stage:triage free  text -not:a:flag -plain
		  → stage:triage -label:not:a:flag free text -plain
		");
	}

	#[test]
	fn toggling_rewrites_one_clause() {
		let steps: [(Facet, &str, bool); 8] = [
			(Facet::Stage, "triage", false),
			(Facet::Stage, "selected", false),
			(Facet::Type, "chore", true),
			(Facet::Type, "chore", false),     // flips polarity
			(Facet::Stage, "triage", false),   // again: dropped
			(Facet::Stage, "selected", false), // last value: clause dropped
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
		stage:triage → is:open stage:triage some text
		stage:selected → is:open stage:triage,selected some text
		alt t:chore → is:open stage:triage,selected -t:chore some text
		t:chore → is:open stage:triage,selected t:chore some text
		stage:triage → is:open stage:selected t:chore some text
		stage:selected → is:open t:chore some text
		alt assignee:@ann → is:open t:chore -assignee:ann some text
		i:none → is:open t:chore -assignee:ann i:none some text
		clear assignee → is:open t:chore i:none some text
		");
	}

	#[test]
	fn tally_lifts_the_facets_own_clauses() {
		let cards = cards();
		let out: Vec<String> = [
			("stage:triage -t:chore", Facet::Stage),
			("stage:triage -t:chore", Facet::Type),
			("i:>=5", Facet::Importance),
			("is:open", Facet::Assignee),
			("", Facet::Is),
		]
		.into_iter()
		.map(|(q, facet)| {
			let tallies = q.parse::<Query>().unwrap().tally(facet, &cards, None).unwrap();
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
		stage:triage -t:chore | stage: icebox 1, +triage 1, selected 1, done 1
		stage:triage -t:chore | t: bug 1, -chore 1
		i:>=5 | i: none 2, +>=5 2, 2 1, 5 1, 8 1
		is:open | assignee: none 2, ann 1, bob 1
		 | is: open 4, blocked 2, closed 1
		");
	}

	#[test]
	fn a_tallied_value_picks_what_it_counted() {
		let cards = cards();
		for facet in <Facet as strum::IntoEnumIterator>::iter() {
			for t in Query::default().tally(facet, &cards, None).unwrap() {
				let mut q = Query::default();
				q.toggle(facet, &t.value, false).unwrap();
				assert_eq!(q.run(&cards, None).unwrap().len(), t.count, "{q}");
			}
		}
	}

	#[test]
	fn sorts_unset_last_either_way() {
		let cards = cards();
		let hits: Vec<usize> = (0..cards.len()).collect();
		let out: Vec<String> = [
			"rank", "-rank", "title", "importance", "-importance", "hours", "-hours", "stage", "assignee", "-assignee", "type", "blockers", "-repo",
		]
		.into_iter()
		.map(|s| {
			let sort = Sort::try_from(s.to_string()).expect("a sort as views store it");
			assert_eq!(sort.to_string(), s);
			let mut sorted = hits.clone();
			sort.apply(&cards, &mut sorted);
			format!("{s}: {}", titles(&cards, &sorted))
		})
		.collect();
		insta::assert_snapshot!(out.join("\n"), @"
		rank: Crash on save · Dark mode · bump deps · Typo in README · Untyped idea
		-rank: Untyped idea · Typo in README · bump deps · Dark mode · Crash on save
		title: bump deps · Crash on save · Dark mode · Typo in README · Untyped idea
		importance: Crash on save · Dark mode · Typo in README · bump deps · Untyped idea
		-importance: Typo in README · Dark mode · Crash on save · bump deps · Untyped idea
		hours: bump deps · Typo in README · Crash on save · Dark mode · Untyped idea
		-hours: Crash on save · bump deps · Typo in README · Dark mode · Untyped idea
		stage: Untyped idea · Crash on save · bump deps · Dark mode · Typo in README
		assignee: Crash on save · Typo in README · Dark mode · bump deps · Untyped idea
		-assignee: Dark mode · Crash on save · Typo in README · bump deps · Untyped idea
		type: Crash on save · Typo in README · bump deps · Dark mode · Untyped idea
		blockers: Untyped idea · Dark mode · Crash on save · bump deps · Typo in README
		-repo: bump deps · Typo in README · Crash on save · Dark mode · Untyped idea
		");
	}
}
