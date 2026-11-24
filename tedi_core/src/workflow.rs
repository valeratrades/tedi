//! A team's workflow: an ordered tree of buckets an issue sits in, what entering one does, and how a
//! pull moves issues between them. Where each issue sits is tedi's own (an extended property), never
//! GitHub's: absent, it sits in the intake. Hooks are named here only; running them is `tedi_hooks`'.

use std::{collections::BTreeMap, path::PathBuf};

use serde::{Deserialize, Serialize};

/// The workflows cards sit in, by name.
pub type Trees = BTreeMap<String, Workflow>;

/// Where an issue sits: bucket names from a root down to a leaf, written `/`-joined.
#[derive(Clone, Debug, Deserialize, Eq, Hash, Ord, PartialEq, PartialOrd, Serialize)]
#[serde(try_from = "String", into = "String")]
pub struct State(Vec<String>);
impl State {
	pub fn names(&self) -> &[String] {
		&self.0
	}

	/// `under` is this or one of its ancestors.
	pub fn is_under(&self, under: &[String]) -> bool {
		self.0.starts_with(under)
	}
}
impl std::fmt::Display for State {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(&self.0.join("/"))
	}
}
impl std::str::FromStr for State {
	type Err = WorkflowError;

	fn from_str(s: &str) -> Result<Self, WorkflowError> {
		let names: Vec<String> = s.split('/').map(str::to_string).collect();
		match names.iter().any(String::is_empty) {
			true => Err(WorkflowError::Path(s.into())),
			false => Ok(State(names)),
		}
	}
}
impl TryFrom<String> for State {
	type Error = WorkflowError;

	fn try_from(s: String) -> Result<Self, WorkflowError> {
		s.parse()
	}
}
impl From<State> for String {
	fn from(state: State) -> String {
		state.to_string()
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Workflow {
	/// Where an issue sits until something moves it.
	pub intake: State,
	/// In order: a leaf's place in the preorder is what "forward" compares.
	pub buckets: Vec<Bucket>,
	/// Tried in order on each pull of an issue; the first yielding a move wins.
	#[serde(default)] // a workflow may be moved by hand only
	pub rules: Vec<Hook>,
}
impl Workflow {
	/// The tree is well-formed and the intake a leaf of it.
	pub fn check(&self) -> Result<(), WorkflowError> {
		fn names(buckets: &[Bucket]) -> Result<(), WorkflowError> {
			for (i, b) in buckets.iter().enumerate() {
				if b.name.is_empty() || b.name.contains('/') {
					return Err(WorkflowError::Name(b.name.clone()));
				}
				if buckets[..i].iter().any(|o| o.name == b.name) {
					return Err(WorkflowError::Twice(b.name.clone()));
				}
				names(&b.buckets)?;
			}
			Ok(())
		}
		if self.buckets.is_empty() {
			return Err(WorkflowError::Empty);
		}
		names(&self.buckets)?;
		self.leaf(&self.intake)
	}

	/// `state` names a leaf of this tree.
	pub fn leaf(&self, state: &State) -> Result<(), WorkflowError> {
		match self.path(state.names()) {
			None => Err(WorkflowError::Missing(state.clone())),
			Some(nodes) if !nodes.last().expect("a state names at least one bucket").buckets.is_empty() => Err(WorkflowError::NotLeaf(state.clone())),
			Some(_) => Ok(()),
		}
	}

	/// Every leaf, in order.
	pub fn leaves(&self) -> Vec<State> {
		fn walk(buckets: &[Bucket], at: &mut Vec<String>, out: &mut Vec<State>) {
			for b in buckets {
				at.push(b.name.clone());
				match b.buckets.is_empty() {
					true => out.push(State(at.clone())),
					false => walk(&b.buckets, at, out),
				}
				at.pop();
			}
		}
		let mut out = Vec::new();
		walk(&self.buckets, &mut Vec::new(), &mut out);
		out
	}

	/// `state`'s place among the leaves: what "forward" compares.
	pub fn order(&self, state: &State) -> usize {
		self.leaves()
			.iter()
			.position(|l| l == state)
			.unwrap_or_else(|| panic!("{state} is checked a leaf before it is ordered"))
	}

	/// The buckets along `names`, root first; `None` if one is missing.
	pub fn path(&self, names: &[String]) -> Option<Vec<&Bucket>> {
		let mut level = &self.buckets;
		let mut out = Vec::new();
		for name in names {
			let b = level.iter().find(|b| &b.name == name)?;
			out.push(b);
			level = &b.buckets;
		}
		Some(out)
	}
}

#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct Bucket {
	pub name: String,
	/// Run on every move entering it, its sub-buckets' included.
	#[serde(default)] // entering may do nothing
	pub on_enter: Vec<Hook>,
	#[serde(default)] // a leaf has none
	pub buckets: Vec<Bucket>,
}
/// A hook by name, with the parameters the workflow file gives it.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
#[serde(untagged)]
pub enum Hook {
	Builtin {
		builtin: String,
		#[serde(flatten)]
		params: serde_json::Map<String, serde_json::Value>,
	},
	/// A symbol of a shared library speaking the C ABI of `tedi.h`.
	Dylib {
		lib: PathBuf,
		symbol: String,
		#[serde(flatten)]
		params: serde_json::Map<String, serde_json::Value>,
	},
}

#[derive(Debug, thiserror::Error)]
pub enum WorkflowError {
	#[error("{0:?} is no bucket path: names `/`-joined, none empty")]
	Path(String),
	#[error("a workflow holds at least one bucket")]
	Empty,
	#[error("{0:?} is no bucket name: non-empty, without `/`")]
	Name(String),
	#[error("two buckets named {0} side by side")]
	Twice(String),
	#[error("no bucket {0}")]
	Missing(State),
	#[error("{0} holds buckets: an issue sits in a leaf")]
	NotLeaf(State),
	#[error("no workflow {0}")]
	Unknown(String),
}

#[cfg(test)]
mod tests {
	use super::*;

	fn bucket(name: &str, buckets: Vec<Bucket>) -> Bucket {
		Bucket {
			name: name.into(),
			on_enter: Vec::new(),
			buckets,
		}
	}

	#[test]
	fn leaves_in_preorder() {
		let w = Workflow {
			intake: "todo".parse().unwrap(),
			buckets: vec![
				bucket("todo", vec![]),
				bucket("doing", vec![bucket("code", vec![]), bucket("test", vec![])]),
				bucket("done", vec![]),
			],
			rules: Vec::new(),
		};
		w.check().unwrap();
		let leaves: Vec<String> = w.leaves().iter().map(ToString::to_string).collect();
		assert_eq!(leaves, ["todo", "doing/code", "doing/test", "done"]);
		assert!(matches!(w.leaf(&"doing".parse().unwrap()), Err(WorkflowError::NotLeaf(_))));
		assert!(matches!(w.leaf(&"doing/nope".parse().unwrap()), Err(WorkflowError::Missing(_))));
	}
}
