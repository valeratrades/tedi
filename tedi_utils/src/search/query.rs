use std::ops::Range;

use nucleo_matcher::{
	Matcher, Utf32Str, Utf32String,
	pattern::{Atom, AtomKind, CaseMatching, Normalization},
};
use regex::Regex;

use super::{Candidate, SearchKind};

const SEPARATOR: &str = " > ";

/// A candidate in the shape matching runs on.
pub(super) struct Prepared {
	pub index: usize,
	/// The displayed row: context, then title.
	pub line: String,
	pub body: String,
	title: Utf32String,
	context: Utf32String,
	body32: Utf32String,
	/// Char offset of the title within `line`.
	title_offset: usize,
}
impl Prepared {
	pub fn new(index: usize, c: &Candidate) -> Self {
		let context = c.context.join(SEPARATOR);
		let line = match context.is_empty() {
			true => c.title.clone(),
			false => format!("{context}{SEPARATOR}{}", c.title),
		};
		let title_offset = line.chars().count() - c.title.chars().count();
		Self {
			index,
			body: c.body.clone(),
			title: c.title.as_str().into(),
			context: context.as_str().into(),
			body32: c.body.as_str().into(),
			line,
			title_offset,
		}
	}

	fn field(&self, field: Field) -> Utf32Str<'_> {
		match field {
			Field::Title => self.title.slice(..),
			Field::Context => self.context.slice(..),
			Field::Body => self.body32.slice(..),
		}
	}
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Field {
	Body,
	Context,
	Title,
}
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Quality {
	Fuzzy,
	Substring,
	WordPrefix,
	WholeWord,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Kind {
	Fuzzy,
	Substring,
	Word,
	Prefix,
	Suffix,
	Equal,
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Exclude {
	OwnTitle,
	Anywhere,
}

pub(super) struct Term {
	kind: Kind,
	exclude: Option<Exclude>,
	/// Case-folded unless the term is case-sensitive.
	needle: Vec<char>,
	ignore_case: bool,
	atom: Atom,
	/// Body is never fuzzy-matched: over long text a scatter hits almost everything.
	body_atom: Atom,
}

pub(super) enum Query {
	/// AND of OR-groups.
	Terms(Vec<Vec<Term>>),
	Regex(Regex),
}

pub(super) struct Hit {
	pub score: i32,
	/// Char indices into the displayed row.
	pub line_indices: Vec<usize>,
}

struct TermHit {
	field: Field,
	quality: Quality,
	score: u16,
	indices: Vec<u32>,
}
impl TermHit {
	fn key(&self) -> (Field, Quality, u16) {
		(self.field, self.quality, self.score)
	}
}

impl Query {
	pub fn parse(kind: SearchKind, query: &str) -> Result<Self, regex::Error> {
		let query = query.trim();
		match kind {
			SearchKind::Regex | SearchKind::RegexLine if !query.is_empty() => {
				let anchored = match kind {
					SearchKind::RegexLine => format!("^(?:{query})$"),
					_ => query.to_owned(),
				};
				let smartcase = match query.chars().any(char::is_uppercase) {
					true => anchored,
					false => format!("(?i){anchored}"),
				};
				Ok(Self::Regex(Regex::new(&smartcase)?))
			}
			_ => Ok(Self::Terms(parse_terms(query, kind == SearchKind::Exact))),
		}
	}

	pub fn score(&self, c: &Prepared, m: &mut Matcher) -> Option<Hit> {
		let terms = match self {
			Self::Regex(re) => {
				let found = re.find(&c.line)?;
				let start = c.line[..found.start()].chars().count();
				return Some(Hit {
					score: 0,
					line_indices: (start..start + found.as_str().chars().count()).collect(),
				});
			}
			Self::Terms(groups) => groups,
		};
		let mut fields = [0u32; 3];
		let (mut quality, mut nucleo) = (0u32, 0u32);
		let mut line_indices = Vec::new();
		for group in terms {
			let mut satisfied = false;
			let mut best: Option<TermHit> = None;
			for term in group {
				match term.exclude {
					Some(scope) => {
						let scope: &[Field] = match scope {
							Exclude::OwnTitle => &[Field::Title],
							Exclude::Anywhere => &[Field::Title, Field::Context, Field::Body],
						};
						satisfied |= scope.iter().all(|&f| term.hit_in(c, f, m).is_none());
					}
					None =>
						if let Some(hit) = term.hit(c, m) {
							satisfied = true;
							if best.as_ref().is_none_or(|b| hit.key() > b.key()) {
								best = Some(hit);
							}
						},
				}
			}
			if !satisfied {
				return None;
			}
			let Some(hit) = best else { continue };
			fields[hit.field as usize] += 1;
			quality += hit.quality as u32;
			nucleo += u32::from(hit.score);
			match hit.field {
				Field::Title => line_indices.extend(hit.indices.iter().map(|&i| i as usize + c.title_offset)),
				Field::Context => line_indices.extend(hit.indices.iter().map(|&i| i as usize)),
				Field::Body => {}
			}
		}
		line_indices.sort_unstable();
		line_indices.dedup();
		let [body, context, title] = fields;
		Some(Hit {
			score: pack(title, context, body, quality, nucleo),
			line_indices,
		})
	}

	/// Char ranges of `body` hit by the query's positive terms, in order.
	pub fn body_highlights(&self, body: &str) -> Vec<Range<usize>> {
		let Self::Terms(groups) = self else { return Vec::new() };
		let body32 = Utf32String::from(body);
		let mut ranges: Vec<Range<usize>> = groups
			.iter()
			.flatten()
			.filter(|t| t.exclude.is_none())
			.flat_map(|t| {
				let kind = match t.kind {
					Kind::Fuzzy => Kind::Substring,
					k => k,
				};
				t.occurrences(body32.slice(..), kind)
					.filter(move |&(_, q)| kind != Kind::Word || q == Quality::WholeWord)
					.map(|(start, _)| start..start + t.needle.len())
					.collect::<Vec<_>>()
			})
			.collect();
		ranges.sort_unstable_by_key(|r| r.start);
		ranges
	}
}

/// Item score: per-field hit counts (title first), then summed quality, then summed nucleo score.
/// shortcut: components saturate (15 terms per field, 63 quality, 4095 nucleo), far past any typed query.
fn pack(title: u32, context: u32, body: u32, quality: u32, nucleo: u32) -> i32 {
	let sat = |v: u32, bits: u32| v.min((1 << bits) - 1);
	let packed = sat(title, 4) << 26 | sat(context, 4) << 22 | sat(body, 4) << 18 | sat(quality, 6) << 12 | sat(nucleo, 12);
	i32::try_from(packed).expect("30 bits fit")
}

impl Term {
	/// Best hit across fields; field dominates, so the first field that hits wins.
	fn hit(&self, c: &Prepared, m: &mut Matcher) -> Option<TermHit> {
		[Field::Title, Field::Context, Field::Body].into_iter().find_map(|f| self.hit_in(c, f, m))
	}

	fn hit_in(&self, c: &Prepared, field: Field, m: &mut Matcher) -> Option<TermHit> {
		let haystack = c.field(field);
		let (atom, kind) = match (field, self.kind) {
			(Field::Body, Kind::Fuzzy) => (&self.body_atom, Kind::Substring),
			(_, kind) => (&self.atom, kind),
		};
		let mut indices = Vec::new();
		let score = atom.indices(haystack, m, &mut indices)?;
		let mut quality = classify(haystack, &indices);
		if let Some((start, q)) = self.occurrences(haystack, kind).max_by_key(|&(start, q)| (q, std::cmp::Reverse(start)))
			&& q > quality
		{
			quality = q;
			indices = (start as u32..(start + self.needle.len()) as u32).collect();
		}
		if kind == Kind::Word && quality != Quality::WholeWord {
			return None;
		}
		Some(TermHit { field, quality, score, indices })
	}

	/// Contiguous occurrences of the needle allowed by `kind`'s anchoring, with their quality.
	fn occurrences<'a>(&'a self, haystack: Utf32Str<'a>, kind: Kind) -> impl Iterator<Item = (usize, Quality)> + 'a {
		let (len, n) = (haystack.len(), self.needle.len());
		let starts = match kind {
			Kind::Prefix => 0..usize::from(n <= len),
			Kind::Suffix => len.saturating_sub(n)..len.saturating_sub(n) + usize::from(n <= len),
			Kind::Equal => 0..usize::from(n == len),
			Kind::Fuzzy | Kind::Substring | Kind::Word => 0..(len + 1).saturating_sub(n),
		};
		starts
			.filter(move |&s| n > 0 && (0..n).all(|j| fold(haystack.get((s + j) as u32), self.ignore_case) == self.needle[j]))
			.map(move |s| {
				let indices: Vec<u32> = (s as u32..(s + n) as u32).collect();
				(s, classify(haystack, &indices))
			})
	}
}

/// Word boundaries are the string edges and non-alphanumeric neighbours.
fn classify(haystack: Utf32Str<'_>, indices: &[u32]) -> Quality {
	let (Some(&first), Some(&last)) = (indices.first(), indices.last()) else {
		return Quality::Fuzzy;
	};
	if (last - first) as usize + 1 != indices.len() {
		return Quality::Fuzzy;
	}
	let boundary = |i: Option<u32>| i.is_none_or(|i| i as usize >= haystack.len() || !haystack.get(i).is_alphanumeric());
	match (boundary(first.checked_sub(1)), boundary(Some(last + 1))) {
		(true, true) => Quality::WholeWord,
		(true, false) => Quality::WordPrefix,
		_ => Quality::Substring,
	}
}

fn fold(c: char, ignore_case: bool) -> char {
	match ignore_case {
		true => c.to_lowercase().next().expect("to_lowercase yields at least one char"),
		false => c,
	}
}

/// Whitespace separates ANDed terms (`\ ` escapes it); a lone `|` ORs its neighbours.
fn parse_terms(query: &str, exact_mode: bool) -> Vec<Vec<Term>> {
	let mut tokens: Vec<String> = Vec::new();
	let mut current = String::new();
	let mut chars = query.chars().peekable();
	while let Some(c) = chars.next() {
		match c {
			'\\' if chars.peek() == Some(&' ') => current.push(chars.next().expect("peeked")),
			c if c.is_whitespace() =>
				if !current.is_empty() {
					tokens.push(std::mem::take(&mut current));
				},
			c => current.push(c),
		}
	}
	if !current.is_empty() {
		tokens.push(current);
	}

	let mut groups: Vec<Vec<Term>> = Vec::new();
	let mut join_next = false;
	for token in tokens {
		if token == "|" {
			join_next = !groups.is_empty();
			continue;
		}
		let Some(term) = Term::parse(&token, exact_mode) else { continue };
		match (join_next, groups.last_mut()) {
			(true, Some(group)) => group.push(term),
			_ => groups.push(vec![term]),
		}
		join_next = false;
	}
	groups
}

impl Term {
	/// `None` for a term with nothing left to match (`!`, `'`, `^` while typing).
	fn parse(token: &str, exact_mode: bool) -> Option<Self> {
		let (exclude, rest) = if let Some(rest) = token.strip_prefix("!!") {
			(Some(Exclude::Anywhere), rest)
		} else if let Some(rest) = token.strip_prefix('!') {
			(Some(Exclude::OwnTitle), rest)
		} else {
			(None, token)
		};
		let (kind, needle) = if let Some(inner) = rest.strip_prefix('\'').and_then(|r| r.strip_suffix('\'')).filter(|i| !i.is_empty()) {
			(Kind::Word, inner)
		} else if let Some(inner) = rest.strip_prefix('\'') {
			(if exact_mode { Kind::Fuzzy } else { Kind::Substring }, inner)
		} else if let Some(inner) = rest.strip_prefix('^').and_then(|r| r.strip_suffix('$')) {
			(Kind::Equal, inner)
		} else if let Some(inner) = rest.strip_prefix('^') {
			(Kind::Prefix, inner)
		} else if let Some(inner) = rest.strip_suffix('$') {
			(Kind::Suffix, inner)
		} else if exact_mode || exclude.is_some() {
			(Kind::Substring, rest)
		} else {
			(Kind::Fuzzy, rest)
		};
		if needle.is_empty() {
			return None;
		}
		let ignore_case = !needle.chars().any(char::is_uppercase);
		let case = match ignore_case {
			true => CaseMatching::Ignore,
			false => CaseMatching::Respect,
		};
		let atom_of = |kind: Kind| {
			let atom_kind = match kind {
				Kind::Fuzzy => AtomKind::Fuzzy,
				Kind::Substring | Kind::Word => AtomKind::Substring,
				Kind::Prefix => AtomKind::Prefix,
				Kind::Suffix => AtomKind::Postfix,
				Kind::Equal => AtomKind::Exact,
			};
			Atom::new(needle, case, Normalization::Never, atom_kind, false)
		};
		Some(Self {
			kind,
			exclude,
			needle: needle.chars().map(|c| fold(c, ignore_case)).collect(),
			ignore_case,
			atom: atom_of(kind),
			body_atom: atom_of(if kind == Kind::Fuzzy { Kind::Substring } else { kind }),
		})
	}
}

#[cfg(test)]
mod tests {
	use crate::search::{Candidate, SearchKind, rank};

	/// `(title, context, body)` candidates → titles of matches, best first.
	fn check(kind: SearchKind, query: &str, candidates: &[(&str, &str, &str)]) -> Vec<String> {
		let candidates: Vec<Candidate> = candidates
			.iter()
			.map(|(title, context, body)| Candidate {
				title: title.to_string(),
				context: context.split(" > ").filter(|s| !s.is_empty()).map(str::to_owned).collect(),
				body: body.to_string(),
			})
			.collect();
		rank(&candidates, kind, query).unwrap().into_iter().map(|i| candidates[i].title.clone()).collect()
	}
	use SearchKind::*;

	#[test]
	fn whole_word_beats_word_prefix() {
		assert_eq!(check(Fuzzy, "fix", &[("fixtures", "", ""), ("fix_button", "", "")]), ["fix_button", "fixtures"]);
	}

	#[test]
	fn own_title_beats_context_beats_body() {
		let items = [("child", "o/r > fix crash", ""), ("unrelated", "", "we fix it"), ("fix crash", "o/r", "")];
		assert_eq!(check(Fuzzy, "fix", &items), ["fix crash", "child", "unrelated"]);
	}

	#[test]
	fn more_terms_in_title_wins() {
		let items = [("fix a", "", "button"), ("fix b", "button", ""), ("fix button", "", "")];
		assert_eq!(check(Fuzzy, "fix button", &items), ["fix button", "fix b", "fix a"]);
	}

	#[test]
	fn ties_keep_input_order() {
		assert_eq!(check(Fuzzy, "a", &[("a 2", "", ""), ("a 1", "", "")]), ["a 2", "a 1"]);
		assert_eq!(check(Fuzzy, "", &[("x", "", ""), ("y", "", "")]), ["x", "y"]);
	}

	#[test]
	fn word_term() {
		assert_eq!(check(Fuzzy, "'fix'", &[("fixtures", "", ""), ("a fix-up", "", "")]), ["a fix-up"]);
	}

	#[test]
	fn fuzzy_never_scatters_over_body() {
		assert_eq!(check(Fuzzy, "fbr", &[("x", "", "foo bar"), ("foobar", "", "")]), ["foobar"]);
		assert_eq!(check(Fuzzy, "bar", &[("x", "", "foo bar")]), ["x"]);
	}

	#[test]
	fn exclusion_scopes() {
		let items = [("crash", "", ""), ("other", "", "a crash"), ("plain", "", "")];
		assert_eq!(check(Fuzzy, "!crash", &items), ["other", "plain"]);
		assert_eq!(check(Fuzzy, "!!crash", &items), ["plain"]);
	}

	#[test]
	fn or_groups() {
		let items = [("alpha", "", ""), ("beta", "", ""), ("gamma", "", "")];
		assert_eq!(check(Fuzzy, "alpha | gamma", &items), ["alpha", "gamma"]);
		assert_eq!(check(Fuzzy, "| alpha |", &items), ["alpha"]);
	}

	#[test]
	fn anchors_and_escapes() {
		let items = [("fix button", "", ""), ("quick fix", "", ""), ("fix", "", "")];
		assert_eq!(check(Fuzzy, "^fix", &items), ["fix button", "fix"]);
		assert_eq!(check(Fuzzy, "fix$", &items), ["quick fix", "fix"]);
		assert_eq!(check(Fuzzy, "^fix$", &items), ["fix"]);
		assert_eq!(check(Fuzzy, "fix\\ b", &items), ["fix button"]);
	}

	#[test]
	fn smartcase() {
		let items = [("Fix", "", ""), ("fix", "", "")];
		assert_eq!(check(Fuzzy, "fix", &items).len(), 2);
		assert_eq!(check(Fuzzy, "Fix", &items), ["Fix"]);
	}

	#[test]
	fn exact_mode_flips_quote_to_fuzzy() {
		let items = [("foo bar", "", ""), ("fbr", "", "")];
		assert_eq!(check(Exact, "fbr", &items), ["fbr"]);
		assert_eq!(check(Exact, "'fbr", &items), ["fbr", "foo bar"]);
	}

	#[test]
	fn regex_over_displayed_row() {
		let items = [("#2 beta", "o/r", "alpha"), ("#1 alpha", "o/r", "")];
		assert_eq!(check(Regex, "r > #\\d alpha", &items), ["#1 alpha"]);
		assert_eq!(check(RegexLine, "#1 alpha", &items), Vec::<String>::new());
		assert_eq!(check(RegexLine, "o/r > #1.*", &items), ["#1 alpha"]);
		assert!(rank(&[], Regex, "(").is_err());
	}
}
