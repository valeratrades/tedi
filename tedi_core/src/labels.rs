//! The label scheme every repo gets from `v_flakes/github/labels.nix`: a typed view over
//! `IssueContents.labels`, which stay the strings GitHub holds.
//!
//! Repos are half-migrated, so the old names `c:rewrite` and `claude` read as `c:cq` and `ext:claude`.

use std::{collections::BTreeSet, str::FromStr};

/// `h:` values: Fibonacci hours, the estimate at 68 % certainty.
pub const HOURS: [u16; 12] = [1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233];

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, strum::Display, strum::EnumString)]
pub enum Type {
	#[strum(serialize = "t:bug")]
	Bug,
	#[strum(serialize = "t:enhancement")]
	Enhancement,
	#[strum(serialize = "t:feature")]
	Feature,
	#[strum(serialize = "t:chore")]
	Chore,
	#[strum(serialize = "t:question")]
	Question,
	#[strum(serialize = "t:epic")]
	Epic,
	#[strum(serialize = "t:architectural")]
	Architectural,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, strum::Display, strum::EnumString)]
pub enum Category {
	#[strum(serialize = "c:test")]
	Test,
	#[strum(serialize = "c:perf")]
	Perf,
	#[strum(serialize = "c:ci")]
	Ci,
	#[strum(serialize = "c:docs")]
	Docs,
	#[strum(to_string = "c:cq", serialize = "c:rewrite")]
	Cq,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, strum::Display, strum::EnumString)]
pub enum Ext {
	#[strum(to_string = "ext:claude", serialize = "claude")]
	Claude,
	#[strum(serialize = "ext:from_todo")]
	FromTodo,
	#[strum(serialize = "ext:breaking")]
	Breaking,
	#[strum(serialize = "ext:hack")]
	Hack,
	#[strum(serialize = "ext:help_wanted")]
	HelpWanted,
	#[strum(serialize = "ext:good_first_issue")]
	GoodFirstIssue,
	#[strum(serialize = "ext:invalid")]
	Invalid,
}

/// The stages GitHub can't tell: everything else about a stage is derived (`crate::Stage`).
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd, strum::Display, strum::EnumString)]
pub enum Pin {
	#[strum(serialize = "s:icebox")]
	Icebox,
	#[strum(serialize = "s:selected")]
	Selected,
	#[strum(serialize = "s:wip")]
	Wip,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
enum Label {
	Type(Type),
	/// 0…9
	Importance(u8),
	Hours(u16),
	Category(Category),
	Ext(Ext),
	Pin(Pin),
}
impl Label {
	fn parse(name: &str) -> Option<Self> {
		// `i:07` or `h:+3` is not the scheme's label
		fn canonical<N: FromStr + ToString>(digits: &str) -> Option<N> {
			digits.parse::<N>().ok().filter(|n| n.to_string() == digits)
		}
		Type::from_str(name)
			.map(Label::Type)
			.or_else(|_| Category::from_str(name).map(Label::Category))
			.or_else(|_| Ext::from_str(name).map(Label::Ext))
			.or_else(|_| Pin::from_str(name).map(Label::Pin))
			.ok()
			.or_else(|| name.strip_prefix("i:").and_then(canonical::<u8>).filter(|i| *i <= 9).map(Label::Importance))
			.or_else(|| name.strip_prefix("h:").and_then(canonical::<u16>).filter(|h| HOURS.contains(h)).map(Label::Hours))
	}

	/// The field a second value of would clash, for the single-valued ones.
	fn single(self) -> Option<&'static str> {
		match self {
			Label::Type(_) => Some("t:"),
			Label::Importance(_) => Some("i:"),
			Label::Hours(_) => Some("h:"),
			Label::Pin(_) => Some("s:"),
			Label::Category(_) | Label::Ext(_) => None,
		}
	}
}

/// What the labels say in the scheme's terms. Labels outside it are not here.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Scheme {
	pub kind: Option<Type>,
	pub importance: Option<u8>,
	pub hours: Option<u16>,
	pub categories: BTreeSet<Category>,
	pub ext: BTreeSet<Ext>,
	pub pin: Option<Pin>,
}
/// Lenient, as GitHub is: of two values for a single-valued field the first by name is read. An edit can't
/// leave two (`check`).
impl<S: AsRef<str>> From<&[S]> for Scheme {
	fn from(labels: &[S]) -> Self {
		let mut names: Vec<&str> = labels.iter().map(AsRef::as_ref).collect();
		names.sort_unstable();
		let mut scheme = Scheme::default();
		for label in names.into_iter().filter_map(Label::parse) {
			match label {
				Label::Type(t) => _ = scheme.kind.get_or_insert(t),
				Label::Importance(i) => _ = scheme.importance.get_or_insert(i),
				Label::Hours(h) => _ = scheme.hours.get_or_insert(h),
				Label::Pin(p) => _ = scheme.pin.get_or_insert(p),
				Label::Category(c) => _ = scheme.categories.insert(c),
				Label::Ext(e) => _ = scheme.ext.insert(e),
			}
		}
		scheme
	}
}

/// At most one `t:`, `i:`, `h:` and `s:`. Errors naming each field holding more.
pub fn check<S: AsRef<str>>(labels: &[S]) -> Result<(), String> {
	let mut held: Vec<(&'static str, &str)> = labels.iter().map(AsRef::as_ref).filter_map(|name| Some((Label::parse(name)?.single()?, name))).collect();
	held.sort_unstable();
	let clashes: Vec<String> = held
		.chunk_by(|a, b| a.0 == b.0)
		.filter(|group| group.len() > 1)
		.map(|group| format!("more than one {}* label: {}", group[0].0, group.iter().map(|(_, n)| *n).collect::<Vec<_>>().join(", ")))
		.collect();
	match clashes.is_empty() {
		true => Ok(()),
		false => Err(clashes.join("\n")),
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[track_caller]
	fn read(labels: &[&str], expected: Scheme) {
		assert_eq!(Scheme::from(labels), expected);
	}

	#[test]
	fn reads_the_scheme() {
		read(
			&["t:bug", "i:0", "c:cq", "c:test", "h:13", "ext:claude", "s:wip", "wontfix"],
			Scheme {
				kind: Some(Type::Bug),
				importance: Some(0),
				hours: Some(13),
				categories: BTreeSet::from([Category::Cq, Category::Test]),
				ext: BTreeSet::from([Ext::Claude]),
				pin: Some(Pin::Wip),
			},
		);
	}

	#[test]
	fn old_names_read_as_new() {
		read(
			&["c:rewrite", "claude"],
			Scheme {
				categories: BTreeSet::from([Category::Cq]),
				ext: BTreeSet::from([Ext::Claude]),
				..Default::default()
			},
		);
		assert_eq!(Category::Cq.to_string(), "c:cq");
	}

	#[test]
	fn lookalikes_are_outside_the_scheme() {
		read(&["i:07", "i:10", "h:4", "c:foo", "s:low"], Scheme::default());
	}

	#[test]
	fn a_second_single_value_is_read_leniently_and_rejected_on_edit() {
		read(
			&["t:chore", "t:bug", "i:3"],
			Scheme {
				kind: Some(Type::Bug),
				importance: Some(3),
				..Default::default()
			},
		);
		assert_eq!(
			check(&["t:chore", "t:bug", "i:3", "h:1", "h:2", "c:ci", "c:docs"]),
			Err("more than one h:* label: h:1, h:2\nmore than one t:* label: t:bug, t:chore".into())
		);
		assert_eq!(check(&["t:bug", "c:ci", "c:docs", "anything"]), Ok(()));
	}
}
