//! `selected.md`: what the owner of a sprint series has picked up, a text over the kanban the home holds. A save is the
//! diff of its sections against the answer it was rendered from, sent as moves; nothing else of it is stored there.

use std::collections::{HashMap, HashSet};

use color_eyre::eyre::{Result, WrapErr as _, bail};
use tedi_adapters::home::{Home, HomeError};
use tedi_core::{NodeLink, board::Answer, conflict::Edited, workflow::State};

use crate::{
	IssueLink, MilestoneLink, RepoInfo, TaskView,
	local::{Local, LocalFs, Picked, Placements, lock_store},
	open_interactions::SyncMode,
	selection::{link_local, lock_sprint},
	sink::Sink,
	sprints::{Expanded, expand_and_refresh, materialize_new_tasks, pull_stored},
};

/// The bucket that reads as picked up, and who holds it.
pub struct Picking {
	pub home: Home,
	pub workflow: String,
	pub bucket: State,
}
impl Picking {
	/// Where the home has `owner`'s open issues.
	async fn ask(&self, owner: &str) -> Result<Placements, HomeError> {
		let Answer { login, mut trees, hits } = self.home.query(&format!("assignee:{owner} is:issue is:open")).await?;
		self.known_as(owner, login)?;
		let refuse = |why: String| HomeError::Refused(format!("`milestones.selected` is {}/{}: {why}", self.workflow, self.bucket));
		let tree = trees.remove(&self.workflow).ok_or_else(|| refuse("the home holds no such workflow".into()))?;
		tree.leaf(&self.bucket).map_err(|e| refuse(e.to_string()))?;
		Ok(Placements {
			placed: hits
				.into_iter()
				.map(|mut hit| (hit.link, hit.states.remove(&self.workflow).expect("a card sits in each of the trees")))
				.collect(),
			workflow: self.workflow.clone(),
			tree,
			bucket: self.bucket.clone(),
		})
	}

	fn known_as(&self, owner: &str, login: Option<String>) -> Result<(), HomeError> {
		match login {
			Some(login) if login.eq_ignore_ascii_case(owner) => Ok(()),
			Some(login) => Err(HomeError::Refused(format!("the home knows you as {login}, and these sprints are {owner}'s"))),
			None => Err(HomeError::Refused("the home doesn't know who you are on GitHub: bring your account there a key".into())),
		}
	}

	//#[codestyle::skip(swappable-args)]
	/// Every move `picked` differs from its answer by, made; then rendered from a new answer, what is no issue yet kept.
	/// A refused one is told, and its line goes where the home has it. `Err`: the home is away, and what it didn't take stays a difference.
	async fn settle(&self, mut picked: Picked, owner: &str, raw: &str) -> Result<Picked> {
		let old = picked.clone();
		let moves = moves(&picked)?;
		if !moves.is_empty() {
			self.known_as(owner, self.home.query("is:issue is:pr").await?.login)?; // nothing is both: asked for who asks
		}
		let mut touched = Vec::new();
		let mut refused = Vec::new();
		for Move { link, seen, to } in moves {
			let sent = match &link {
				IssueLink::Owned(_) => self.home.move_card(&link, &picked.answer.workflow, &to, seen.as_ref()).await,
				IssueLink::Virtual(_) => Err(HomeError::Refused("a local-only issue sits in no workflow".into())),
			};
			match sent {
				Ok(Edited::Landed) => {
					println!("{link} → {to}");
					picked.answer.placed.retain(|(l, _)| *l != link);
					picked.answer.placed.push((link.clone(), to));
					touched.push(link);
				}
				Ok(Edited::Rejected(why)) => refused.push((link, why)),
				Ok(Edited::Conflict(conflict)) => refused.push((link, conflict.to_string())),
				Err(e) if e.is_away() => {
					store(&mut picked, Some(&old)).await?;
					return Err(e.into());
				}
				Err(e) => refused.push((link, e.to_string())),
			}
		}
		if !refused.is_empty() {
			crate::utils::persist_rejected_changes(&[raw.to_string()])?;
			for (link, why) in &refused {
				eprintln!("warning: {link} stays where the home has it: {why}");
			}
			eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		}
		let answer = match self.ask(owner).await {
			Ok(answer) => answer,
			Err(e) => {
				store(&mut picked, Some(&old)).await?;
				return Err(e.into());
			}
		};
		let mut text = picked.doc;
		text.remove_nodes(&text.issue_links().into_iter().map(NodeLink::Issue).collect::<Vec<_>>()); // what is no issue yet
		let mut settled = Picked {
			repo: picked.repo,
			doc: answer.document(),
			answer,
		};
		settled.doc.append(text);
		store(&mut settled, Some(&old)).await?;
		pull_stored(&touched).await?; // hooks edit what they are entered with
		Ok(settled)
	}
}

/// `selected.md` open in the editor.
pub(crate) struct PickedTab {
	pub(crate) expanded: Expanded,
	/// As stored when it opened.
	opened: Picked,
	owner: String,
	/// Whose repo a task typed into it lands in.
	ambient: MilestoneLink,
	_lock: std::fs::File,
}
impl PickedTab {
	/// The one beside `sprint`.
	pub(crate) async fn render(picking: &Picking, sprint: &MilestoneLink, mode: SyncMode) -> Result<Self> {
		let lock = lock_sprint("selected")?;
		let repo = sprint.repo_info();
		let owner = repo.owner().expect("milestones are github-only").to_string();
		let stored = Local::load_picked(repo)?;
		let opened = match (mode, stored) {
			(SyncMode::Offline, None) => bail!("never fetched while online"),
			(SyncMode::OnExit | SyncMode::Offline, Some(stored)) => stored,
			(SyncMode::Full | SyncMode::OnExit, None) => {
				let answer = picking.ask(&owner).await?;
				let mut fresh = Picked {
					repo,
					doc: answer.document(),
					answer,
				};
				store(&mut fresh, None).await?;
				fresh
			}
			(SyncMode::Full, Some(stored)) => {
				let raw = stored.doc.serialize();
				match picking.settle(stored, &owner, &raw).await {
					Ok(settled) => settled,
					Err(e) => {
						eprintln!("warning: selected.md opens as last fetched: {e:#}");
						Local::load_picked(repo)?.expect("loaded above")
					}
				}
			}
		};
		Ok(Self {
			expanded: expand_and_refresh(opened.doc.clone(), mode == SyncMode::Full).await?,
			opened,
			owner,
			ambient: sprint.clone(),
			_lock: lock,
		})
	}

	pub(crate) fn repo(&self) -> RepoInfo {
		self.opened.repo
	}

	pub(crate) async fn land(self, picking: &Picking, raw: &str, mut doc: TaskView, moved: &HashMap<IssueLink, IssueLink>, mode: SyncMode) -> Result<()> {
		let mut picked = self.opened.clone();
		let untouched = raw == self.expanded.text && (mode == SyncMode::Offline || doc.homeless_tasks().is_empty());
		match untouched {
			true => picked.doc.relink(moved),
			false => {
				let milestones = doc.milestone_links();
				if !milestones.is_empty() {
					let named = milestones.iter().map(|l| l.as_str()).collect::<Vec<_>>().join(", ");
					bail!("selected.md lists issues, and a milestone sits in no workflow: {named}");
				}
				for (header, _) in doc.sections() {
					picked.answer.leaf(header).wrap_err("selected.md is not saved")?;
				}
				materialize_new_tasks(&mut doc, Some(self.ambient), mode).await?;
				doc.collapse_to_links();
				picked.doc = doc;
			}
		}
		picked.answer.placed.retain(|(link, _)| !moved.contains_key(link)); // placed again, at its new link
		moves(&picked)?; // what can't be read as moves is not saved
		let (listed, intake) = (picked.doc.issue_links(), &picked.answer.tree.intake);
		for (link, _) in picked.answer.placed.iter().filter(|(l, s)| s == intake && !listed.contains(l)) {
			println!("{link} sits in {intake} as it is: taking it out of selected.md changes nothing");
		}
		store(&mut picked, Some(&self.opened)).await?;
		if mode == SyncMode::Offline || (mode == SyncMode::Full && untouched && moves(&picked)?.is_empty()) {
			return Ok(());
		}
		match picking.settle(picked, &self.owner, raw).await {
			Ok(_) => Ok(()),
			Err(e) if e.downcast_ref::<HomeError>().is_some() => {
				eprintln!("warning: selected.md is saved here, its moves to be sent with the next online edit: {e:#}");
				Ok(())
			}
			Err(e) => Err(e),
		}
	}
}

struct Move {
	link: IssueLink,
	/// `None`: wherever it sits.
	seen: Option<State>,
	to: State,
}

/// What `picked`'s document says otherwise than its answer.
fn moves(picked: &Picked) -> Result<Vec<Move>> {
	let mut out = Vec::new();
	let mut listed = HashSet::new();
	for (header, nodes) in picked.doc.sections() {
		let to = picked.answer.leaf(header)?;
		for node in nodes {
			let NodeLink::Issue(link) = node else { bail!("selected.md lists issues, not {node}") };
			if !listed.insert(link.clone()) {
				bail!("{link} is listed twice in selected.md");
			}
			let seen = picked.answer.at(&link).cloned();
			if seen.as_ref() != Some(&to) {
				out.push(Move { link, seen, to: to.clone() });
			}
		}
	}
	let intake = &picked.answer.tree.intake;
	for (link, seen) in picked.answer.placed.iter().filter(|(l, _)| !listed.contains(l)) {
		if link_local(link).is_some_and(|vi| vi.contents.state.is_closed()) {
			continue; // closing is the home's own rule
		}
		if seen != intake {
			out.push(Move {
				link: link.clone(),
				seen: Some(seen.clone()),
				to: intake.clone(),
			});
		}
	}
	Ok(out)
}

async fn store(picked: &mut Picked, old: Option<&Picked>) -> Result<()> {
	let _store = lock_store().await?;
	<Picked as Sink<LocalFs>>::sink(picked, old).await?;
	Ok(())
}
