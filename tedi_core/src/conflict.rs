//! A clash is data: what each side holds of a field both changed since they last agreed.

use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::{CloseState, Events, Issue, IssueLink, split_blockers};

/// A sync that can't settle: each field both sides changed since consensus, every clashing node recorded
/// to a file of its own for resolution. Blocks only the line of issues those nodes sit on.
#[derive(Clone, Debug, Deserialize, PartialEq, Serialize, miette::Diagnostic, thiserror::Error)]
#[diagnostic(code(tedi::conflict))]
pub struct Conflict {
	pub link: IssueLink,
	pub clashes: Vec<Clash>,
	pub files: Vec<PathBuf>,
}
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct Clash {
	pub node: IssueLink,
	pub field: Field,
	/// `None` when there is no consensus: never synced from here.
	pub consensus: Option<String>,
	pub local: String,
	pub remote: String,
}
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize, strum::Display, strum::EnumString)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum Field {
	Title,
	Labels,
	Assignees,
	Body,
	State,
	Comments,
	SubIssues,
}
/// A clash file: the whole node as each side holds it, between markers. Removing them resolves it.
pub fn markers(local: &str, consensus: Option<&str>, remote: &str) -> String {
	let side = |s: &str| match s.ends_with('\n') {
		true => s.to_string(),
		false => format!("{s}\n"),
	};
	format!(
		"<<<<<<< local\n{}||||||| consensus\n{}=======\n{}>>>>>>> remote\n",
		side(local),
		consensus.map(side).unwrap_or_default(),
		side(remote)
	)
}

/// `(local, remote)` of a file [`markers`] wrote; `None` once it was edited past that shape.
pub fn sides(file: &str) -> Option<(String, String)> {
	let rest = file.strip_prefix("<<<<<<< local\n")?;
	let (local, rest) = rest.split_once("||||||| consensus\n")?;
	let (_, rest) = rest.split_once("=======\n")?;
	let remote = rest.strip_suffix(">>>>>>> remote\n")?;
	Some((local.to_string(), remote.to_string()))
}

/// How [`Field::read`] renders a field holding several names (labels, assignees).
pub fn list(items: &[String]) -> String {
	let mut items = items.to_vec();
	items.sort_unstable();
	items.join(", ")
}

/// `field` set to `new` by someone who saw it hold `seen` (as [`Field::read`] renders both).
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct FieldEdit {
	pub field: Field,
	pub seen: String,
	pub new: String,
}

#[derive(Debug, thiserror::Error)]
pub enum FieldError {
	#[error("{0} is not edited as a field")]
	NotWritable(Field),
	#[error("{0:?} is no state: ` `, `x`, `-`, a progress mark or a duplicate's number")]
	State(String),
}

impl Field {
	/// The one rendering a clash shows and a field edit is made against.
	pub fn read(self, issue: &Issue) -> String {
		match self {
			Field::Title => issue.contents.title.clone(),
			Field::Labels => list(&issue.contents.labels),
			Field::Assignees => list(&issue.contents.assignees),
			Field::Body => issue.body().to_string(),
			Field::State => issue.contents.state.to_checkbox_contents(),
			Field::Comments => issue.contents.comments.iter().skip(1).map(|c| format!("{:?}: {}", c.id(), c.body)).collect::<Vec<_>>().join("\n"),
			Field::SubIssues => {
				let mut titles: Vec<String> = issue.children.values().map(|c| c.contents.title.clone()).collect();
				titles.sort();
				titles.join("\n")
			}
		}
	}

	/// Inverse of [`Self::read`] for the fields a single value sets.
	pub fn write(self, issue: &mut Issue, value: &str) -> Result<(), FieldError> {
		let names = || value.split(", ").filter(|v| !v.is_empty()).map(str::to_string).collect();
		match self {
			Field::Title => issue.contents.title = value.to_string(),
			Field::Labels => issue.contents.labels = names(),
			Field::Assignees => issue.contents.assignees = names(),
			Field::State => issue.contents.state = CloseState::from_checkbox(value).map_err(FieldError::State)?,
			Field::Body => {
				let (body, blockers) = split_blockers(value);
				issue.contents.comments.first_mut().expect("`Comments::new` asserts the body leads").body = Events::parse(&body);
				issue.contents.blockers = blockers;
			}
			Field::Comments | Field::SubIssues => return Err(FieldError::NotWritable(self)),
		}
		Ok(())
	}
}

impl std::fmt::Display for Conflict {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let home = self.link.project();
		writeln!(f, "{home}#{} changed on both sides since the last sync:", self.link.number())?;
		for c in &self.clashes {
			// the first line the sides differ on
			let at = c.local.lines().zip(c.remote.lines()).take_while(|(l, r)| l == r).count();
			let line = |v: &str| v.lines().nth(at).map_or_else(|| "nothing".to_string(), |l| format!("{:?}", l.trim()));
			write!(f, "  {} {}: {} here, {} on GitHub", c.node.shorthand(home), c.field, line(&c.local), line(&c.remote))?;
			match &c.consensus {
				Some(was) => writeln!(f, " (was {})", line(was))?,
				None => writeln!(f)?,
			}
		}
		writeln!(f, "Resolve the markers (<<<<<<< ||||||| ======= >>>>>>>) in:")?;
		for file in &self.files {
			writeln!(f, "  {}", file.display())?;
		}
		write!(f, "then re-run.")
	}
}
