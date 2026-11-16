//! Ranked search over titled documents; grammar and ranking rule in docs/ARCHITECTURE.md#search.

#[cfg(feature = "picker")]
mod picker;
mod query;

use std::{cell::RefCell, cmp::Reverse};

use query::{Prepared, Query};

/// One searchable item. Selection maps back by its index in the input, which is also the final tiebreak.
#[derive(Clone, Debug)]
pub struct Candidate {
	pub title: String,
	/// Containing scopes, outermost first; the row shows them before the title.
	pub context: Vec<String>,
	pub body: String,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum SearchKind {
	#[default]
	Fuzzy,
	Exact,
	/// One regex over the displayed row.
	Regex,
	/// [`Self::Regex`], anchored to the whole row.
	RegexLine,
}
impl TryFrom<u8> for SearchKind {
	type Error = &'static str;

	fn try_from(count: u8) -> Result<Self, Self::Error> {
		match count {
			0 => Ok(Self::Fuzzy),
			1 => Ok(Self::Exact),
			2 => Ok(Self::Regex),
			3 => Ok(Self::RegexLine),
			_ => Err("--exact / -e can be specified at most 3 times"),
		}
	}
}

#[derive(Debug, thiserror::Error)]
pub enum SearchError {
	#[error("invalid regex: {0}")]
	InvalidRegex(#[from] regex::Error),
	#[cfg(feature = "picker")]
	#[error("selection aborted")]
	Aborted,
	#[cfg(feature = "picker")]
	#[error("picker failed: {0}")]
	Picker(Box<dyn std::error::Error + Send + Sync>),
}

/// Indices of the candidates matching `query`, best first.
pub fn rank(candidates: &[Candidate], kind: SearchKind, query: &str) -> Result<Vec<usize>, SearchError> {
	let query = Query::parse(kind, query)?;
	let prepared: Vec<Prepared> = candidates.iter().enumerate().map(|(i, c)| Prepared::new(i, c)).collect();
	Ok(ranked(&prepared, &query).into_iter().map(|(index, _)| index).collect())
}

/// 0 matches → `None`; 1, or 1 with every term contiguous in its row → it; else interactive picker over all candidates, prefilled with `query`.
#[cfg(feature = "picker")]
pub async fn pick(candidates: Vec<Candidate>, kind: SearchKind, query: Option<&str>) -> Result<Option<usize>, SearchError> {
	let prepared: Vec<Prepared> = candidates.iter().enumerate().map(|(i, c)| Prepared::new(i, c)).collect();
	let Some(q) = query else {
		return picker::run(prepared, kind, "").await;
	};
	let shortlist = ranked(&prepared, &Query::parse(kind, q)?);
	let in_row: Vec<usize> = shortlist.iter().filter_map(|&(index, in_row)| in_row.then_some(index)).collect();
	match (&shortlist[..], &in_row[..]) {
		([], _) => Ok(None),
		([(only, _)], _) | (_, [only]) => Ok(Some(*only)),
		_ => picker::run(prepared, kind, q).await,
	}
}

/// `(index, in_row)` of the matches, best first.
fn ranked(prepared: &[Prepared], query: &Query) -> Vec<(usize, bool)> {
	let mut scored: Vec<(i32, usize, bool)> = with_matcher(|m| prepared.iter().filter_map(|p| query.score(p, m).map(|hit| (hit.score, p.index, hit.in_row))).collect());
	scored.sort_by_key(|&(score, index, _)| (Reverse(score), index));
	scored.into_iter().map(|(_, index, in_row)| (index, in_row)).collect()
}

thread_local! {
	static MATCHER: RefCell<nucleo_matcher::Matcher> = RefCell::new(nucleo_matcher::Matcher::default());
}
/// `nucleo_matcher::Matcher` needs `&mut` and is costly to build, so each thread keeps one.
fn with_matcher<R>(f: impl FnOnce(&mut nucleo_matcher::Matcher) -> R) -> R {
	MATCHER.with(|m| f(&mut m.borrow_mut()))
}
