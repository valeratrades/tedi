#![doc = include_str!("../README.md")]

mod abi;
mod builtin;
mod dylib;

use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use jiff::Timestamp;
use serde::{Deserialize, Serialize};
use tedi_core::{
	board::Card,
	conflict::{Field, FieldEdit, list},
	workflow::{Bucket, Hook as Named, State, Workflow, WorkflowError},
};

const PRESETS: [(&str, &str); 2] = [
	("kanban", include_str!("../presets/kanban.toml")),
	("pull_requests", include_str!("../presets/pull_requests.toml")),
];

/// Who moves a card.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum Mover {
	Person(String),
	/// A workflow's rule, on a pull.
	Rule,
}

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
	#[error("no preset {0}: tedi ships {shipped}", shipped = PRESETS.map(|(name, _)| name).join(", "))]
	Preset(String),
	#[error("reading {}", .0.display())]
	Read(PathBuf, #[source] std::io::Error),
	#[error(transparent)]
	Toml(#[from] toml::de::Error),
	#[error(transparent)]
	Workflow(#[from] WorkflowError),
	#[error("{at}: {why}")]
	Hook { at: String, why: String },
}

/// A hook asked for what its call doesn't allow; the move it was part of lands nothing.
#[derive(Debug, thiserror::Error)]
#[error("{at}: {why}")]
pub struct HookError {
	at: String,
	why: String,
}

/// What entering a bucket comes to.
#[derive(Debug, PartialEq)]
pub enum Entered {
	/// Every edit asked for that changes something, each made against the card the hooks were shown.
	Edits(Vec<FieldEdit>),
	/// The first refusal, in its hook's words.
	Rejected(String),
}

/// A workflow with its hooks loaded. Only the instance's operator configures one: a library is code run in their process.
pub struct Loaded {
	pub workflow: Workflow,
	/// By bucket path.
	on_enter: HashMap<Vec<String>, Vec<Loaded1>>,
	rules: Vec<Loaded1>,
}
/// A hook with its parameters, and where the workflow names it, for errors.
struct Loaded1 {
	at: String,
	params: serde_json::Map<String, serde_json::Value>,
	hook: Box<dyn Hook>,
}

trait Hook: Send + Sync {
	fn call(&self, ctx: &Ctx, out: &mut Buffer) -> Result<(), String>;
}

struct Ctx<'a> {
	workflow: &'a Workflow,
	card: &'a Card,
	params: &'a serde_json::Map<String, serde_json::Value>,
	now: Timestamp,
	at: Moment<'a>,
}
enum Moment<'a> {
	Entering { from: &'a State, to: &'a State, mover: &'a Mover },
	Pulled { current: &'a State },
}
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Role {
	OnEnter,
	Rule,
}

#[derive(Default)]
struct Buffer {
	edits: Vec<(Field, String)>,
	rejected: Option<String>,
	move_to: Option<State>,
}

impl Loaded {
	/// `preset:<name>`, or a workflow file; a library it names is found relative to the file.
	pub fn load(spec: &str) -> Result<Self, LoadError> {
		match spec.strip_prefix("preset:") {
			Some(name) => {
				let (_, src) = PRESETS.iter().find(|(n, _)| *n == name).ok_or_else(|| LoadError::Preset(name.into()))?;
				Self::parse(src, None)
			}
			None => {
				let path = Path::new(spec);
				let src = std::fs::read_to_string(path).map_err(|e| LoadError::Read(path.into(), e))?;
				Self::parse(&src, path.parent())
			}
		}
	}

	fn parse(src: &str, dir: Option<&Path>) -> Result<Self, LoadError> {
		let workflow: Workflow = toml::from_str(src)?;
		workflow.check()?;
		let load = |named: &Named, role: Role, at: String| -> Result<Loaded1, LoadError> {
			let hook: Box<dyn Hook> = match named {
				Named::Builtin { builtin, params } => Box::new(builtin::Builtin::load(builtin, params, role, &workflow).map_err(|why| LoadError::Hook { at: at.clone(), why })?),
				Named::Dylib { lib, symbol, .. } => {
					let lib = match (lib.is_relative(), dir) {
						(true, Some(dir)) => dir.join(lib),
						(true, None) => unreachable!("presets name no libraries"),
						(false, _) => lib.clone(),
					};
					Box::new(dylib::Dylib::load(&lib, symbol, role).map_err(|why| LoadError::Hook { at: at.clone(), why })?)
				}
			};
			let (Named::Builtin { params, .. } | Named::Dylib { params, .. }) = named;
			Ok(Loaded1 { at, params: params.clone(), hook })
		};
		fn walk(buckets: &[Bucket], at: &mut Vec<String>, out: &mut Vec<(Vec<String>, Vec<Named>)>) {
			for b in buckets {
				at.push(b.name.clone());
				out.push((at.clone(), b.on_enter.clone()));
				walk(&b.buckets, at, out);
				at.pop();
			}
		}
		let mut nodes = Vec::new();
		walk(&workflow.buckets, &mut Vec::new(), &mut nodes);
		let mut on_enter = HashMap::new();
		for (path, hooks) in nodes {
			let loaded = hooks
				.iter()
				.enumerate()
				.map(|(i, h)| load(h, Role::OnEnter, format!("{}.on_enter[{i}]", path.join("/"))))
				.collect::<Result<Vec<_>, _>>()?;
			on_enter.insert(path, loaded);
		}
		let rules = workflow
			.rules
			.iter()
			.enumerate()
			.map(|(i, h)| load(h, Role::Rule, format!("rules[{i}]")))
			.collect::<Result<Vec<_>, _>>()?;
		Ok(Self { workflow, on_enter, rules })
	}

	/// Every `on_enter` hook of the buckets a move from `from` to `to` enters (those on `to`'s path below
	/// where the two part), outermost first, each shown the same card.
	pub fn enter(&self, card: &Card, from: &State, to: &State, mover: &Mover, now: Timestamp) -> Result<Entered, HookError> {
		let mut edits = Vec::new();
		for bucket in self.entered(from, to) {
			for loaded in &self.on_enter[&bucket] {
				let mut out = Buffer::default();
				self.call(loaded, card, now, Moment::Entering { from, to, mover }, &mut out)?;
				if let Some(why) = out.rejected {
					return Ok(Entered::Rejected(why));
				}
				edits.extend(out.edits.into_iter().map(|(field, new)| FieldEdit {
					field,
					seen: read(field, card),
					new,
				}));
			}
		}
		edits.retain(|e| e.new != e.seen);
		Ok(Entered::Edits(edits))
	}

	/// Where the first rule moving `card` off `current` sends it.
	pub fn rule(&self, card: &Card, current: &State, now: Timestamp) -> Result<Option<State>, HookError> {
		for loaded in &self.rules {
			let mut out = Buffer::default();
			self.call(loaded, card, now, Moment::Pulled { current }, &mut out)?;
			if let Some(to) = out.move_to
				&& &to != current
			{
				self.workflow.leaf(&to).map_err(|e| HookError {
					at: loaded.at.clone(),
					why: e.to_string(),
				})?;
				return Ok(Some(to));
			}
		}
		Ok(None)
	}

	fn entered(&self, from: &State, to: &State) -> Vec<Vec<String>> {
		let shared = from.names().iter().zip(to.names()).take_while(|(a, b)| a == b).count();
		(shared + 1..=to.names().len()).map(|n| to.names()[..n].to_vec()).collect()
	}

	fn call(&self, loaded: &Loaded1, card: &Card, now: Timestamp, at: Moment, out: &mut Buffer) -> Result<(), HookError> {
		let ctx = Ctx {
			workflow: &self.workflow,
			card,
			params: &loaded.params,
			now,
			at,
		};
		loaded.hook.call(&ctx, out).map_err(|why| HookError { at: loaded.at.clone(), why })
	}
}

/// What `Field::read` renders of the stored issue a card was made from.
fn read(field: Field, card: &Card) -> String {
	match field {
		Field::Title => card.title.clone(),
		Field::Labels => list(&card.labels),
		Field::Assignees => list(&card.assignees),
		Field::Body => card.body.clone(),
		Field::State => card.state.to_checkbox_contents(),
		Field::Comments | Field::SubIssues => unreachable!("buffered only once checked writable"),
	}
}

impl Buffer {
	fn edit(&mut self, field: Field, value: String) -> Result<(), String> {
		match field {
			Field::Title | Field::Labels | Field::Assignees | Field::Body | Field::State => {
				self.edits.push((field, value));
				Ok(())
			}
			Field::Comments | Field::SubIssues => Err(format!("{field} is not edited as a field")),
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn presets_load() {
		for (name, _) in PRESETS {
			Loaded::load(&format!("preset:{name}")).unwrap_or_else(|e| panic!("{name}: {e}"));
		}
	}
}
