//! Three-way sync for milestones — the milestone mirror of `sync.rs`.
//!
//! Three-way merge (local ⇄ consensus ⇄ remote) and the same recorded-conflict flow,
//! over the simpler `Milestone` primitive (no children, no comments).

use std::collections::HashMap;

use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use tedi_core::{
	Sprint,
	chain::{self, ChainError, Edge},
	issue_file_name,
};

use super::sync::{MergeMode, RejectedEdit, Side, SyncMode, SyncOptions, clash, fetch_and_store_missing, is_transient_sync_error};
use crate::{
	IssueLink, Milestone, MilestoneBody, MilestoneLink, NodeLink,
	conflict_resolve::{check_for_existing_milestone_conflict, record_conflict},
	local::{Consensus, FsReader, GitReader, Local, LocalFs, conflict_detect::milestone_conflict_path, lock_store},
	remote::{Remote, load_remote_milestone},
	sink::Sink,
	workflow::Rejected,
};

/// How to modify a milestone before syncing.
#[derive(Debug)]
pub enum MilestoneModifier {
	/// Open the milestone's body in `$EDITOR`.
	Editor,
	BodyWrite(BodyWrite),
}

/// `body` as edited out of `rendered`: it lands over whatever is stored by then, only what the edit changed.
#[derive(Clone, Debug)]
pub struct BodyWrite {
	pub rendered: MilestoneBody,
	pub body: MilestoneBody,
}
impl BodyWrite {
	/// Whether `stored` changed. Refused, it didn't.
	fn apply(self, stored: &mut Milestone) -> Result<bool> {
		let Self { rendered, body } = self;
		if body == rendered {
			return Ok(false);
		}
		allowed(stored, &added(&rendered, &body))?;
		let old = stored.clone();
		match stored.body == rendered {
			true => stored.body = body,
			false => {
				let base = Milestone { body: rendered, ..old.clone() };
				let mut edited = Milestone { body, ..old.clone() };
				edited.post_update(&base);
				if concurrent_edit(&edited, &base, &old) {
					return Err(Rejected(format!("{} ({}) changed while it was edited, in the same place", old.identity.title, old.link().as_str())).into());
				}
				let (edited_nodes, old_nodes) = (edited.body.hosted(), old.body.hosted());
				let dropped: Vec<NodeLink> = base.body.hosted().into_iter().filter(|n| !edited_nodes.contains(n) || !old_nodes.contains(n)).collect();
				edited.merge(&old, Some(&base), false);
				edited.body.0.remove_nodes(&dropped);
				*stored = edited;
			}
		}
		stored.post_update(&old);
		Ok(stored.body != old.body)
	}
}

/// Reconcile `link` with GitHub, storing it if it isn't yet, and everything it contains — the milestone mirror of
/// `pull_issues`, and the same reason: a body rendered into the editor from the local cache alone is a body the
/// user edits blind.
pub async fn pull_milestone(link: &MilestoneLink, mode: MergeMode) -> Result<Milestone> {
	let mut pull = Pull::default();
	let milestone = pull.link(link, mode).await?;
	pull.finish().await?;
	Ok(milestone)
}

/// Modify a milestone, then sync back to GitHub (parity with `modify_and_sync_issue`).
/// Returns the milestone as it ends up stored.
pub async fn modify_and_sync_milestone(link: &MilestoneLink, mode: SyncMode, modifier: MilestoneModifier, sync_opts: SyncOptions) -> Result<Milestone> {
	let edit = match modifier {
		MilestoneModifier::BodyWrite(edit) => edit,
		MilestoneModifier::Editor => {
			let milestone = match mode {
				SyncMode::Full => pull_milestone(link, sync_opts.take_merge_mode()).await?,
				SyncMode::OnExit | SyncMode::Offline => stored(link)?,
			};
			let path = tempfile::tempdir()?.keep().join(issue_file_name(Some(link.number()), &milestone.identity.title, false)); // kept: an edit outlives a crash
			std::fs::write(&path, format!("{milestone}\n"))?;
			crate::utils::open_files(&[&path], None).await?;
			let typed = std::fs::read_to_string(&path)?;
			let body = MilestoneBody::parse(&typed, link.repo_info());
			// Unpulled, the sync below is the only reconciliation — callers read the synced state even when nothing was typed.
			if body == milestone.body && mode != SyncMode::OnExit {
				v_utils::log!("Aborted (no milestone changes made)");
				return Ok(milestone);
			}
			return match land(link, mode, BodyWrite { rendered: milestone.body, body }, sync_opts).await {
				Err(e) if e.downcast_ref::<ChainError>().is_some() || e.downcast_ref::<Unshown>().is_some() => {
					crate::utils::persist_rejected_changes(&[typed])?;
					Err(RejectedEdit(e.into()).into())
				}
				landed => landed,
			};
		}
	};
	land(link, mode, edit, sync_opts).await
}

async fn land(link: &MilestoneLink, mode: SyncMode, mut edit: BodyWrite, sync_opts: SyncOptions) -> Result<Milestone> {
	if mode == SyncMode::Offline {
		return save(link, edit).await;
	}
	let mut pull = Pull { seen: vec![link.clone()] };
	by_now(&mut edit).await?;
	pull.contained(added(&edit.rendered, &edit.body)).await; // what they are, and own, decides whether they can be owned
	match pull.sync(link, Some(edit.clone()), sync_opts.take_merge_mode()).await {
		Err(e) if is_transient_sync_error(&e) => {
			tracing::warn!("GitHub transient during milestone sync — deferring to local: {e}");
			eprintln!("GitHub unreachable; milestone edit saved locally (will sync next online edit).");
			save(link, edit).await
		}
		synced => synced,
	}
}

async fn save(link: &MilestoneLink, edit: BodyWrite) -> Result<Milestone> {
	let _store = lock_store().await?;
	let mut milestone = stored(link)?;
	if edit.apply(&mut milestone)? {
		<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
		println!("Milestone saved locally.");
	}
	Ok(milestone)
}

fn stored(link: &MilestoneLink) -> Result<Milestone> {
	Local::load_milestone(link, &FsReader)?.ok_or_else(|| eyre!("milestone {} is not stored locally", link.as_str()))
}

/// What `edit` adds named as GitHub names it by now: a renamed owner redirects, and a stale name would hide a loop.
async fn by_now(edit: &mut BodyWrite) -> Result<()> {
	let mut renamed = HashMap::new();
	for link in added(&edit.rendered, &edit.body) {
		match crate::github::client::get()?.get_milestone(link.repo_info(), link.number()).await {
			Ok(held) => {
				let now = MilestoneLink::parse(&held.html_url).ok_or_else(|| eyre!("{} is no milestone", held.html_url))?;
				if now != link {
					renamed.insert(link, now);
				}
			}
			Err(e) if e.is_transient() => tracing::warn!("GitHub transient while reading {} — judged as stored: {e}", link.as_str()),
			Err(e) => return Err(e).wrap_err_with(|| Unshown(link)),
		}
	}
	edit.body.0.relink(&renamed);
	Ok(())
}

#[derive(Debug, thiserror::Error)]
#[error("{} can't be owned: GitHub doesn't show it", .0.as_str())]
struct Unshown(MilestoneLink);

/// The milestones `after` owns and `before` doesn't.
fn added(before: &MilestoneBody, after: &MilestoneBody) -> Vec<MilestoneLink> {
	let before = before.0.milestone_links();
	after.0.milestone_links().into_iter().filter(|l| !before.contains(l)).collect()
}

/// The chain allows `owner` to own each of `new`.
fn allowed(owner: &Milestone, new: &[MilestoneLink]) -> Result<()> {
	if !new.is_empty() {
		chain::check(&Local::stored_milestones()?, owner, new, Edge::Added)?;
	}
	Ok(())
}

/// Milestones pulled together, each once.
#[derive(Default)]
pub(crate) struct Pull {
	seen: Vec<MilestoneLink>,
}
impl Pull {
	pub(crate) fn pulled(&self, link: &MilestoneLink) -> bool {
		self.seen.contains(link)
	}

	/// `link` alone, stored if it isn't. An edge of it the chain came to forbid is told, and its local body stays unsynced.
	pub(crate) async fn link(&mut self, link: &MilestoneLink, mode: MergeMode) -> Result<Milestone> {
		if !self.pulled(link) {
			self.seen.push(link.clone());
		}
		match self.sync(link, None, mode).await {
			Ok(synced) => Ok(synced),
			Err(e) => match (Local::load_milestone(link, &FsReader)?, e.downcast_ref::<ChainError>()) {
				(Some(stored), _) if is_transient_sync_error(&e) => {
					tracing::warn!("GitHub transient during milestone sync — proceeding with local: {e}");
					Ok(stored)
				}
				(Some(stored), Some(refused)) => {
					tell_unsynced(link, refused);
					Ok(stored)
				}
				_ => Err(e),
			},
		}
	}

	/// Each of `links`, and everything they contain. What can't be pulled is told, and stays as it is.
	pub(crate) async fn contained(&mut self, links: Vec<MilestoneLink>) {
		let mut level = links;
		//LOOP each level holds only milestones not seen on those before
		loop {
			let mut fresh: Vec<MilestoneLink> = Vec::new();
			for link in level {
				if !self.pulled(&link) {
					self.seen.push(link.clone());
					fresh.push(link);
				}
			}
			if fresh.is_empty() {
				return;
			}
			let remotes = futures::future::join_all(fresh.iter().map(load_remote_milestone)).await;
			level = Vec::new();
			for (link, remote) in fresh.iter().zip(remotes) {
				let merged = match remote {
					Ok(remote) => self.merge(link, remote, None, MergeMode::Normal).await,
					Err(e) => Err(e.into()),
				};
				if let Err(e) = merged {
					match e.downcast_ref::<ChainError>() {
						Some(refused) => tell_unsynced(link, refused),
						None => eprintln!("warning: {} stays as stored: {e:#}", link.as_str()),
					}
				}
				match Local::load_milestone(link, &FsReader) {
					Ok(held) => level.extend(held.iter().flat_map(|m| m.body.0.milestone_links())), // one GitHub won't show us isn't stored
					Err(e) => eprintln!("{}: {e:#}", link.as_str()),
				}
			}
		}
	}

	async fn sync(&mut self, link: &MilestoneLink, edit: Option<BodyWrite>, mode: MergeMode) -> Result<Milestone> {
		tracing::info!("Syncing milestone {}", link.as_str());
		no_conflict(link).await?;
		let remote = load_remote_milestone(link).await?;
		self.merge(link, remote, edit, mode).await
	}

	async fn merge(&mut self, link: &MilestoneLink, remote: Milestone, edit: Option<BodyWrite>, mode: MergeMode) -> Result<Milestone> {
		no_conflict(link).await?;
		if let Some(local) = Local::load_milestone(link, &FsReader)? {
			// judged against what they contain by now, not as of when the edge was made
			Box::pin(self.contained(added(&remote.body, &local.body))).await;
		}
		let _store = lock_store().await?;
		let consensus = Local::load_milestone(link, &GitReader)?;
		let mut milestone = Local::load_milestone(link, &FsReader)?.unwrap_or_else(|| remote.clone()); // not stored yet: GitHub's is what there is
		let edited = match edit {
			Some(edit) => edit.apply(&mut milestone)?,
			None => false,
		};
		let reconciled = reconciled(&milestone, consensus.as_ref(), &remote, mode);
		if let Ok(resolved) = &reconciled {
			allowed(resolved, &added(&remote.body, &resolved.body))?;
		}
		if edited {
			<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?; // whatever comes of the merge, the edit is kept
		}
		let mut resolved = match reconciled {
			Ok(resolved) => resolved,
			Err((local_side, remote_side)) => return Err(diverged(&milestone, consensus.as_ref(), &remote, &local_side, &remote_side)?),
		};
		<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?;
		if mode != (MergeMode::Reset { prefer: Side::Remote }) {
			<Milestone as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
		}
		<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?; // what it is assigned by now
		<Milestone as Sink<Consensus>>::sink(&mut resolved, None).await?;
		Ok(resolved)
	}

	/// What was pulled, as stored, once everything it contains is too, with the issues they host; the edges
	/// the chain forbids are told.
	pub(crate) async fn finish(mut self) -> Result<Vec<Milestone>> {
		let stored = |links: &[MilestoneLink]| -> Result<Vec<Milestone>> {
			let held: Vec<Option<Milestone>> = links.iter().map(|l| Local::load_milestone(l, &FsReader)).collect::<Result<_>>()?;
			Ok(held.into_iter().flatten().collect()) // one GitHub won't show us isn't stored
		};
		let contained = stored(&self.seen)?.iter().flat_map(|m| m.body.0.milestone_links()).collect();
		self.contained(contained).await;

		let pulled = stored(&self.seen)?;
		let hosted: Vec<IssueLink> = pulled.iter().flat_map(|m| m.body.0.issue_links()).filter(|l| !matches!(l, IssueLink::Virtual(_))).collect();
		fetch_and_store_missing(&hosted).await;

		// as of consensus: an edit not synced yet is nothing GitHub holds
		let known: Vec<Milestone> = Local::stored_milestones()?
			.iter()
			.filter_map(|m| Local::load_milestone(m.link(), &GitReader).transpose())
			.collect::<Result<_>>()?;
		let mut told: Vec<String> = Vec::new();
		for milestone in known.iter().filter(|m| self.pulled(m.link())) {
			if let Err(misplaced) = Sprint::check(milestone.identity.link.repo_info(), &milestone.identity.title) {
				eprintln!("{} is taken for an ordinary milestone: {misplaced}", milestone.link().as_str());
			}
			for child in milestone.body.0.milestone_links() {
				if let Err(e) = chain::check(&known, milestone, &[child], Edge::Held) {
					let drawing = e.to_string();
					if !told.contains(&drawing) {
						eprintln!("{drawing}");
						told.push(drawing);
					}
				}
			}
		}
		Ok(pulled)
	}
}

fn tell_unsynced(link: &MilestoneLink, refused: &ChainError) {
	eprintln!("{} stays as it is here, unsynced, until its edit is fixed:\n{refused}", link.as_str());
}

async fn no_conflict(link: &MilestoneLink) -> Result<()> {
	if let Some(conflict_file) = check_for_existing_milestone_conflict(link).await? {
		bail!("Unresolved milestone conflict in {}. Resolve the markers, then re-run.", conflict_file.display());
	}
	Ok(())
}

/// Three-way resolution; a divergence is both sides, as text.
fn reconciled(local: &Milestone, consensus: Option<&Milestone>, remote: &Milestone, mode: MergeMode) -> Result<Milestone, (String, String)> {
	let (force_local_wins, force_remote_wins) = match mode {
		MergeMode::Reset { prefer: Side::Local } => return Ok(local.clone()),
		MergeMode::Reset { prefer: Side::Remote } => return Ok(remote.clone()),
		MergeMode::Force { prefer: Side::Local } => (true, false),
		MergeMode::Force { prefer: Side::Remote } => (false, true),
		MergeMode::Normal => (false, false),
	};

	// `merge` unions the hosted set, which alone can't tell "the other side added it" from "I
	// deleted it". Against the base, a node one side dropped is a deletion; the losing side of a
	// `--force` gets no vote. Without a base (never committed) every node reads as an addition.
	let clash = match (mode, consensus) {
		(MergeMode::Normal, Some(base)) => concurrent_edit(local, base, remote),
		_ => false,
	};

	let dropped: Vec<NodeLink> = match consensus {
		Some(base) => {
			// a description still names what Github unassigned
			let unassigned = |node: &NodeLink| match node {
				NodeLink::Issue(l) => l.project() == base.identity.link.repo_info() && base.assigned.contains(&l.number()) && !remote.assigned.contains(&l.number()),
				NodeLink::Milestone(_) => false, // the field is an issue's
			};
			let in_local = local.body.hosted();
			let in_remote: Vec<NodeLink> = remote.body.hosted().into_iter().filter(|n| !unassigned(n)).collect();
			base.body
				.hosted()
				.into_iter()
				.filter(|n| (!force_remote_wins && !in_local.contains(n)) || (!force_local_wins && !in_remote.contains(n)))
				.collect()
		}
		None => Vec::new(),
	};

	let mut local_merged = local.clone();
	local_merged.merge(remote, consensus, force_remote_wins);
	let mut remote_merged = remote.clone();
	remote_merged.merge(local, consensus, force_local_wins);

	local_merged.body.0.remove_nodes(&dropped);
	remote_merged.body.0.remove_nodes(&dropped);

	match (clash, milestones_agree(&local_merged, &remote_merged)) {
		(false, true) => Ok(local_merged),
		(true, _) => Err((local.to_string(), remote.to_string())),
		(false, false) => Err((local_merged.to_string(), remote_merged.to_string())),
	}
}

/// The divergence recorded, to be resolved in its own conflict file.
fn diverged(local: &Milestone, consensus: Option<&Milestone>, remote: &Milestone, local_side: &str, remote_side: &str) -> Result<color_eyre::Report> {
	let repo = local.identity.link.repo_info();
	let number = local.number();
	if local_side == remote_side {
		let differ: Vec<&str> = [
			("title", local.identity.title != remote.identity.title),
			("state", local.identity.state != remote.identity.state),
			("due date", local.identity.due_on != remote.identity.due_on),
		]
		.into_iter()
		.filter_map(|(field, differs)| differs.then_some(field))
		.collect();
		return Ok(eyre!(
			"Milestone {repo}#{number}: {} changed both locally and on GitHub since the last sync, and the conflict file holds only the body. Make {} agree with GitHub (or GitHub with it), then re-run.",
			differ.join(", "),
			Local::milestone_project_dir(repo).join(".meta.json").display()
		));
	}
	let path = milestone_conflict_path(&local.identity.link);
	record_conflict(&path, local_side, consensus.map(Milestone::to_string).as_deref(), remote_side)?;
	Ok(eyre!("Conflict detected for milestone {repo}#{number}.\nResolve the markers in {}, then re-run.", path.display()))
}

/// A field both sides edited since `base`: neither edit was made knowing of the other, so no
/// timestamp can order them.
///
/// The body is one timestamped unit whose hosted nodes merge as a set, so only prose can be lost:
/// the newer body wins whole, and the older side's prose with it.
fn concurrent_edit(local: &Milestone, base: &Milestone, remote: &Milestone) -> bool {
	let older = match local.identity.timestamps.description > remote.identity.timestamps.description {
		true => remote,
		false => local,
	};
	clash(&local.identity.title, &base.identity.title, &remote.identity.title)
		|| clash(&local.identity.state, &base.identity.state, &remote.identity.state)
		|| clash(local.identity.due_on, base.identity.due_on, remote.identity.due_on)
		|| (local.body != base.body && remote.body != base.body && prose(older) != prose(base) && prose(local) != prose(remote))
}

/// The body with what it hosts taken out.
fn prose(m: &Milestone) -> String {
	let mut doc = m.body.0.clone();
	doc.strip_nodes();
	doc.serialize()
}

/// Merge equality that treats `hosted` as an order-independent set — the two merge sides
/// build the union in opposite orders, so a Vec `==` would spuriously flag a conflict.
fn milestones_agree(a: &Milestone, b: &Milestone) -> bool {
	if a.identity != b.identity || prose(a) != prose(b) {
		return false;
	}
	let mut ah: Vec<String> = a.body.hosted().iter().map(|l| l.to_string()).collect();
	let mut bh: Vec<String> = b.body.hosted().iter().map(|l| l.to_string()).collect();
	ah.sort_unstable();
	bh.sort_unstable();
	ah == bh
}