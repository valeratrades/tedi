//! What its owner has picked up, as text: `selected.md` beside a repo's sprints, sections of a workflow's
//! leaves, and `.selected.json`, the answer of whoever holds the workflow it was rendered from. What the two
//! disagree on is moves not sent yet; consensus can't be git's, whose every commit takes the whole store.
//!
//! r[local.sink-only-mutation]

use std::collections::HashMap;

use color_eyre::eyre::{Result, WrapErr, eyre};
use serde::{Deserialize, Serialize};
use tedi_core::{
	IssueLink, NodeLink, TaskView,
	workflow::{State, Workflow},
};

use super::{Local, LocalFs};
use crate::{RepoInfo, sink::Sink};

const DOC_FILE: &str = "selected.md";
const ANSWER_FILE: &str = ".selected.json";

#[derive(Clone)]
pub struct Picked {
	/// The sprints' repo it sits beside.
	pub repo: RepoInfo,
	pub doc: TaskView,
	pub answer: Placements,
}
impl Picked {
	/// The issues listed at the bucket or ahead of it (`started`), or all of them.
	pub fn links(&self, started: bool) -> Result<Vec<NodeLink>> {
		let mut out = Vec::new();
		for (header, nodes) in self.doc.sections() {
			if !started || self.answer.tree.order(&self.answer.leaf(header)?) >= self.answer.tree.order(&self.answer.bucket) {
				out.extend(nodes);
			}
		}
		Ok(out)
	}

	/// Point every issue held in `moved` at its new place.
	pub fn relink(&mut self, moved: &HashMap<IssueLink, IssueLink>) {
		self.doc.relink(moved);
		for (link, _) in &mut self.answer.placed {
			if let Some(new) = moved.get(link) {
				*link = new.clone();
			}
		}
	}
}

/// Where the home last said each issue sits.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize)]
pub struct Placements {
	pub workflow: String,
	pub tree: Workflow,
	/// The bucket of it that reads as picked up: the lines under no header.
	pub bucket: State,
	/// In rank order.
	pub placed: Vec<(IssueLink, State)>,
}
impl Placements {
	/// The leaf a section of the document is of.
	pub fn leaf(&self, header: &[String]) -> Result<State> {
		let [name] = header else {
			return match header.is_empty() {
				true => Ok(self.bucket.clone()),
				false => Err(eyre!("no bucket `{}` in {}: a header is one of {}", header.join(" / "), self.workflow, self.leaves())),
			};
		};
		let state: State = name.parse()?;
		self.tree.leaf(&state).wrap_err_with(|| format!("a header is one of {}'s: {}", self.workflow, self.leaves()))?;
		Ok(state)
	}

	fn leaves(&self) -> String {
		self.tree.leaves().iter().map(ToString::to_string).collect::<Vec<_>>().join(", ")
	}

	pub fn at(&self, link: &IssueLink) -> Option<&State> {
		self.placed.iter().find(|(l, _)| l == link).map(|(_, s)| s)
	}

	/// The document as it stands right after the answer: the bucket's own lines, then every other leaf holding one, those ahead of the bucket first.
	pub fn document(&self) -> TaskView {
		let mut leaves = self.tree.leaves();
		let here = self.tree.order(&self.bucket);
		leaves.rotate_left(here); // the bucket, what is ahead of it, then what is behind
		let mut out = String::new();
		for leaf in leaves {
			let mut links = self.placed.iter().filter(|(_, s)| *s == leaf).map(|(l, _)| l).peekable();
			if links.peek().is_some() && leaf != self.bucket {
				out.push_str(&format!("\n# {leaf}\n"));
			}
			for link in links {
				out.push_str(&format!("- {link}\n"));
			}
		}
		TaskView::parse(&out)
	}
}

impl Local {
	/// `None`: never fetched. One of its two files alone says as little.
	pub fn load_picked(repo: RepoInfo) -> Result<Option<Picked>> {
		let dir = Self::milestone_project_dir(repo);
		let read = |name: &str| match std::fs::read_to_string(dir.join(name)) {
			Ok(content) => Ok(Some(content)),
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
			Err(e) => Err(e),
		};
		let (Some(doc), Some(answer)) = (read(DOC_FILE)?, read(ANSWER_FILE)?) else {
			return Ok(None);
		};
		Ok(Some(Picked {
			repo,
			doc: TaskView::parse(&doc),
			answer: serde_json::from_str(&answer).wrap_err_with(|| format!("corrupted {}", dir.join(ANSWER_FILE).display()))?,
		}))
	}

	/// The repos with a `selected.md`.
	pub fn picked_repos() -> Result<Vec<RepoInfo>> {
		Ok(Self::milestone_repos()?
			.into_iter()
			.filter(|repo| Self::milestone_project_dir(*repo).join(DOC_FILE).exists())
			.collect())
	}
}

impl Sink<LocalFs> for Picked {
	type Error = std::io::Error;

	async fn sink(&mut self, old: Option<&Self>) -> Result<bool, std::io::Error> {
		assert!(super::store_lock::store_locked(), "what is picked up is written under `lock_store`");
		let dir = Local::milestone_project_dir(self.repo);
		std::fs::create_dir_all(&dir)?;
		let mut changed = false;
		let mut put = |name: &str, content: String, held: Option<String>| -> std::io::Result<()> {
			if held.as_ref() != Some(&content) {
				let tmp = dir.join(format!("{name}.tmp"));
				std::fs::write(&tmp, content)?;
				std::fs::rename(&tmp, dir.join(name))?; // whole or not at all
				changed = true;
			}
			Ok(())
		};
		let json = |p: &Placements| serde_json::to_string_pretty(p).expect("placements serialize");
		let text = |doc: &TaskView| format!("{}\n", doc.serialize());
		put(DOC_FILE, text(&self.doc), old.map(|o| text(&o.doc)))?; // first: an answer newer than the document would read as moves undone
		put(ANSWER_FILE, json(&self.answer), old.map(|o| json(&o.answer)))?;
		Ok(changed)
	}
}
