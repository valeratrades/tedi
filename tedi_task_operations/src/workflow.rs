//! Moving a card between a workflow's buckets: the one way every move goes, by hand (the board, `tedi
//! move`) or by a rule on a pull. A move runs the `on_enter` hooks of each bucket it enters; their edits
//! sync as any field edit does, and where the card sits is written once they landed.

use std::collections::BTreeMap;

use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use jiff::Timestamp;
use tedi_core::{
	board::Card,
	workflow::{State, Trees},
};
pub use tedi_hooks::Mover;
use tedi_hooks::{Entered, Hooks};

use crate::{
	Issue, IssueLink, LazyIssue,
	local::{FsReader, Local, LocalFs, LocalIssueSource, LocalPath, lock_store},
	open_interactions::{SyncMode, fetch_and_store_missing},
	reach::{Reach, Reached},
	sink::Sink,
};

/// The workflows the config names, loaded once at startup.
#[derive(Default)]
pub struct Workflows {
	pub trees: Trees,
	hooks: BTreeMap<String, Hooks>,
}
impl Workflows {
	/// Each by name, from what `specs` say it is: `preset:<name>`, or a workflow file.
	pub fn load(specs: &BTreeMap<String, String>) -> Result<Self> {
		let mut workflows = Self::default();
		for (name, spec) in specs {
			let (tree, hooks) = Hooks::load(spec).wrap_err_with(|| format!("the workflow {name} ({spec})"))?;
			workflows.trees.insert(name.clone(), tree);
			workflows.hooks.insert(name.clone(), hooks);
		}
		Ok(workflows)
	}
}

/// A move nothing of which landed: a hook refused it, or it would edit a pull request.
#[derive(Debug, thiserror::Error)]
#[error("{0}")]
pub struct Rejected(pub String);

/// The card left the state the move was made against: nothing landed.
#[derive(Debug, thiserror::Error)]
#[error("{link} sits at {workflow}/{held} now, not {workflow}/{seen} as seen: move it from there")]
pub struct MovedOn {
	pub link: IssueLink,
	pub workflow: String,
	pub seen: State,
	pub held: State,
}

/// `link`'s card to `to` of `workflow`, made against it sitting at `seen` (`None`: wherever it sits).
#[expect(clippy::too_many_arguments, reason = "each is the move's own; none derives from another")]
pub async fn move_card(link: &Reached<IssueLink>, workflow: &str, to: &State, seen: Option<&State>, mover: &Mover, workflows: &Workflows, mode: SyncMode, now: Timestamp) -> Result<()> {
	let tree = workflows.trees.get(workflow).ok_or_else(|| {
		eyre!(
			"no workflow {workflow}: the config's `board.workflows` names {}",
			workflows.trees.keys().cloned().collect::<Vec<_>>().join(", ")
		)
	})?;
	tree.leaf(to)?;
	if mode != SyncMode::Offline && !is_stored(link)? {
		fetch_and_store_missing(std::slice::from_ref(link)).await; // created since the last pull, say
	}
	let card = stored_card(link, &workflows.trees).await?;
	let held = &card.states[workflow];
	let seen = seen.unwrap_or(held);
	if held != seen {
		return Err(MovedOn {
			link: (**link).clone(),
			workflow: workflow.into(),
			seen: seen.clone(),
			held: held.clone(),
		}
		.into());
	}
	if to == held {
		return Ok(());
	}
	let edits = match workflows.hooks[workflow].enter(tree, &card, held, to, mover, now)? {
		Entered::Rejected(why) => return Err(Rejected(why).into()),
		Entered::Edits(edits) => edits,
	};
	if !edits.is_empty() {
		if card.pr.is_some() {
			let fields: Vec<String> = edits.iter().map(|e| e.field.to_string()).collect();
			return Err(Rejected(format!("entering {workflow}/{to} edits {}, and tedi never edits a pull request", fields.join(", "))).into());
		}
		crate::board::edit(link, edits, mode).await?;
	}

	let _store = lock_store().await?;
	let repo = link.project();
	let old = Local::load_states(repo)?;
	let now_held = old.held.get(workflow).and_then(|issues| issues.get(&link.number())).unwrap_or(&tree.intake);
	if now_held != seen {
		// another move landed between ours reading and writing: its state stands, our edits landed all the same
		return Err(MovedOn {
			link: (**link).clone(),
			workflow: workflow.into(),
			seen: seen.clone(),
			held: now_held.clone(),
		}
		.into());
	}
	let mut new = old.clone();
	new.held.entry(workflow.into()).or_default().insert(link.number(), to.clone());
	<crate::local::States as Sink<LocalFs>>::sink(&mut new, Some(&old)).await?;
	Ok(())
}

/// Each workflow's rules over `link`'s card as stored, the first move each yields made as any move is;
/// `moved` holds what this pull moved already, never moved twice.
pub(crate) async fn apply_rules(link: &IssueLink, workflows: &Workflows, moved: &mut std::collections::HashSet<(IssueLink, String)>, now: Timestamp) -> Result<()> {
	if workflows.trees.is_empty() {
		return Ok(());
	}
	let card = stored_card(link, &workflows.trees).await?;
	for (name, tree) in &workflows.trees {
		if moved.contains(&(link.clone(), name.clone())) {
			continue;
		}
		let current = &card.states[name];
		if let Some(to) = workflows.hooks[name].rule(tree, &card, current, now)? {
			moved.insert((link.clone(), name.clone()));
			let link = Reach::All.issue(link.clone())?; // a rule is the instance's own
			move_card(&link, name, &to, Some(current), &Mover::Rule, workflows, SyncMode::Full, now).await?;
		}
	}
	Ok(())
}

fn is_stored(link: &IssueLink) -> Result<bool> {
	let repo = link.project();
	Ok(Local::find_by_number(repo, link.number(), FsReader).is_some() || Local::stored_prs(repo)?.contains_key(&link.number()))
}

/// `link` as the board cards it: a stored issue's node, or a mirrored pull request.
async fn stored_card(link: &IssueLink, trees: &Trees) -> Result<Card> {
	let repo = link.project();
	let states = Local::load_states(repo)?;
	if Local::find_by_number(repo, link.number(), FsReader).is_some() {
		let issue = Issue::load(LocalIssueSource::<FsReader>::build(LocalPath::new(crate::board::stored_index(link)?)).await?).await?;
		let parent = issue.identity.git_lineage()?.last().map(|&n| IssueLink::in_project(repo, n));
		return crate::board::card(&issue, parent, false, &states, trees);
	}
	match Local::stored_prs(repo)?.remove(&link.number()) {
		Some(pr) => crate::board::pr_card(repo, pr, &states, trees),
		None => bail!("{link} is not stored here: pull it first"),
	}
}
