//! Three-way sync for milestones — the milestone mirror of `sync.rs`.
//!
//! Three-way merge (local ⇄ consensus ⇄ remote) and the same recorded-conflict flow,
//! over the simpler `Milestone` primitive (no children, no comments).

use std::{collections::HashMap, path::PathBuf};

use color_eyre::eyre::{Result, WrapErr as _, bail, eyre};
use tedi_core::chain::{self, ChainError, Edge, Kind};

use super::sync::{MergeMode, RejectedEdit, Side, SyncMode, SyncOptions, clash, fetch_and_store_missing, is_transient_sync_error};
use crate::{
	IssueLink, Milestone, MilestoneBody, MilestoneLink, NodeLink,
	conflict_resolve::{check_for_existing_milestone_conflict, record_conflict},
	local::{Consensus, FsReader, GitReader, Local, LocalFs, conflict_detect::milestone_conflict_path, lock_store},
	remote::{Remote, load_remote_milestone},
	sink::Sink,
};

/// How to modify a milestone before syncing.
#[derive(Debug)]
pub enum MilestoneModifier {
	/// Open the milestone's local body file in `$EDITOR`.
	Editor,
	/// Replace the milestone body wholesale (used by inline-edit propagation).
	BodyWrite { body: MilestoneBody },
}

/// Reconcile a milestone with GitHub, and everything it contains — the milestone mirror of `pull_issues`, and
/// the same reason: a body rendered into the editor from the local cache alone is a body the user edits blind.
pub async fn pull_milestone(milestone: &mut Milestone, mode: MergeMode) -> Result<()> {
	let mut pull = Pull::default();
	pull.milestone(milestone, mode).await?;
	pull.finish().await?;
	Ok(())
}

/// Modify a local milestone, then sync back to GitHub (parity with `modify_and_sync_issue`).
/// Returns the milestone as it ends up stored.
pub async fn modify_and_sync_milestone(mut milestone: Milestone, mode: SyncMode, modifier: MilestoneModifier, sync_opts: SyncOptions) -> Result<Milestone> {
	// `BodyWrite` applies a buffer rendered earlier: a pull between that render and the apply reads as the user reverting whatever the remote gained meanwhile.
	let pulled = mode == SyncMode::Full && matches!(modifier, MilestoneModifier::Editor);
	if pulled {
		pull_milestone(&mut milestone, sync_opts.take_merge_mode()).await?;
	}

	let before = milestone.clone();
	let typed = apply_modifier(&mut milestone, modifier).await?;
	// Unpulled, the sync below is the only reconciliation — callers read the synced state even when nothing was typed.
	if milestone.body == before.body && (pulled || mode == SyncMode::Offline) {
		v_utils::log!("Aborted (no milestone changes made)");
		return Ok(milestone);
	}

	let mut pull = Pull {
		seen: vec![milestone.link().clone()],
		..Default::default()
	};
	let admitted = admit(&mut milestone, &before, mode, &mut pull).await;
	let refused = admitted.is_err();
	let landed = match admitted {
		Ok(()) => land(&mut milestone, mode, sync_opts, &mut pull).await,
		Err(e) => Err(e),
	};
	match (landed, typed) {
		(Ok(()), _) => Ok(milestone),
		(Err(e), Some(typed)) if refused || e.downcast_ref::<ChainError>().is_some() => {
			typed.reject()?;
			Err(RejectedEdit(e.into()).into())
		}
		(Err(e), _) => Err(e),
	}
}

/// A body typed into the milestone's own file.
struct InPlace {
	path: PathBuf,
	/// What the file held when it opened; `None`: it was made for the editor.
	held: Option<String>,
	typed: String,
}
impl InPlace {
	fn reject(self) -> std::io::Result<()> {
		crate::utils::persist_rejected_changes(&self.typed)?;
		match self.held {
			Some(held) => std::fs::write(&self.path, held),
			None => std::fs::remove_file(&self.path),
		}
	}
}

async fn apply_modifier(milestone: &mut Milestone, modifier: MilestoneModifier) -> Result<Option<InPlace>> {
	let old = milestone.clone();
	let typed = match modifier {
		MilestoneModifier::Editor => {
			let repo = milestone.identity.link.repo_info();
			let path = Local::milestone_file_path(repo, milestone.number(), &milestone.identity.title);
			let existed = path.exists();
			std::fs::create_dir_all(path.parent().expect("milestone file always has a parent"))?;
			let held = format!("{milestone}\n");
			std::fs::write(&path, &held)?;
			crate::utils::open_file(&path, None).await?;
			let typed = std::fs::read_to_string(&path)?;
			milestone.body = MilestoneBody::parse(&typed, repo);
			Some(InPlace {
				path,
				held: existed.then_some(held),
				typed,
			})
		}
		MilestoneModifier::BodyWrite { body } => {
			milestone.body = body;
			None
		}
	};
	if milestone.body != old.body {
		milestone.post_update(&old);
	}
	Ok(typed)
}

/// What the edit made of `before` owns is known, named as GitHub names it by now, and allowed by the chain.
async fn admit(milestone: &mut Milestone, before: &Milestone, mode: SyncMode, pull: &mut Pull) -> Result<()> {
	let new = introduced(milestone, before);
	if mode != SyncMode::Offline && !new.is_empty() {
		let gh = crate::github::client::get()?;
		let mut renamed = HashMap::new();
		for link in &new {
			match gh.get_milestone(link.repo_info(), link.number()).await {
				Ok(held) => {
					let by_now = MilestoneLink::parse(&held.html_url).ok_or_else(|| eyre!("{} is no milestone", held.html_url))?;
					if by_now != *link {
						renamed.insert(link.clone(), by_now);
					}
				}
				Err(e) if e.is_transient() => tracing::warn!("GitHub transient while reading {} — judged as stored: {e}", link.as_str()),
				Err(e) => return Err(e).wrap_err_with(|| format!("{} can't be owned: GitHub doesn't show it", link.as_str())),
			}
		}
		milestone.body.0.relink(&renamed);
		pull.contained(introduced(milestone, before)).await;
	}
	allowed(milestone, before)
}

async fn land(milestone: &mut Milestone, mode: SyncMode, sync_opts: SyncOptions, pull: &mut Pull) -> Result<()> {
	match mode {
		SyncMode::Offline => {
			<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
			println!("Offline: milestone saved locally.");
		}
		SyncMode::OnExit | SyncMode::Full =>
			if let Err(e) = pull.sync(milestone, sync_opts.take_merge_mode()).await {
				if !is_transient_sync_error(&e) {
					return Err(e);
				}
				tracing::warn!("GitHub transient during milestone sync — deferring to local: {e}");
				eprintln!("GitHub unreachable; milestone edit saved locally (will sync next online edit).");
				<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
			},
	}
	Ok(())
}

/// The milestones `after` owns and `before` doesn't.
fn introduced(after: &Milestone, before: &Milestone) -> Vec<MilestoneLink> {
	let held = before.body.0.milestone_links();
	let mut new = after.body.0.milestone_links();
	new.retain(|l| !held.contains(l));
	new
}

/// The chain allows what `after` owns and `before` doesn't.
fn allowed(after: &Milestone, before: &Milestone) -> Result<()> {
	let new = introduced(after, before);
	if !new.is_empty() {
		chain::check(&Local::stored_milestones()?, after, &new, Edge::Added)?;
	}
	Ok(())
}

async fn store(milestone: &mut Milestone) -> Result<()> {
	let store = lock_store().await?;
	<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
	<Milestone as Sink<Consensus>>::sink(milestone, None).await?;
	drop(store);
	Ok(())
}

/// Milestones pulled together, each once.
#[derive(Default)]
pub(crate) struct Pull {
	seen: Vec<MilestoneLink>,
	/// For a reader: a local copy past consensus is left as it is.
	reading: bool,
}
impl Pull {
	pub(crate) fn reading() -> Self {
		Self {
			reading: true,
			..Default::default()
		}
	}

	/// `link` alone, stored first if it isn't.
	pub(crate) async fn link(&mut self, link: &MilestoneLink) -> Result<()> {
		if self.seen.contains(link) {
			return Ok(());
		}
		match Local::load_milestone(link, &FsReader)? {
			Some(mut local) => self.milestone(&mut local, MergeMode::Normal).await,
			None => {
				let mut remote = load_remote_milestone(link).await?;
				store(&mut remote).await?;
				self.seen.push(link.clone());
				Ok(())
			}
		}
	}

	/// `milestone` alone. An edge of it the chain came to forbid is told, and its local body stays unsynced.
	pub(crate) async fn milestone(&mut self, milestone: &mut Milestone, mode: MergeMode) -> Result<()> {
		let link = milestone.link().clone();
		if !self.seen.contains(&link) {
			self.seen.push(link.clone());
		}
		match self.sync(milestone, mode).await {
			Err(e) if is_transient_sync_error(&e) => {
				tracing::warn!("GitHub transient during milestone sync — proceeding with local: {e}");
				Ok(())
			}
			Err(e) => match e.downcast_ref::<ChainError>() {
				Some(refused) => {
					tell_unsynced(&link, refused);
					Ok(())
				}
				None => Err(e),
			},
			Ok(()) => Ok(()),
		}
	}

	/// Each of `links`, and everything they contain. What can't be pulled is told, and stays as it is.
	pub(crate) async fn contained(&mut self, links: Vec<MilestoneLink>) {
		let mut level = links;
		//LOOP each level holds only milestones not seen on those before
		loop {
			let mut fresh: Vec<MilestoneLink> = Vec::new();
			for link in level {
				if !self.seen.contains(&link) {
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
				let taken = match remote {
					Ok(remote) => self.take(link, remote).await,
					Err(e) => Err(e.into()),
				};
				if let Err(e) = taken {
					match e.downcast_ref::<ChainError>() {
						Some(refused) => tell_unsynced(link, refused),
						None => eprintln!("{} was not pulled: {e:#}", link.as_str()),
					}
				}
				match Local::load_milestone(link, &FsReader) {
					Ok(Some(m)) if Kind::from((link, m.identity.title.as_str())) == Kind::Sprint => {} // its own flow's to pull
					Ok(held) => level.extend(held.iter().flat_map(|m| m.body.0.milestone_links())),
					Err(e) => eprintln!("{}: {e:#}", link.as_str()),
				}
			}
		}
	}

	async fn take(&mut self, link: &MilestoneLink, mut remote: Milestone) -> Result<()> {
		let Some(mut local) = Local::load_milestone(link, &FsReader)? else {
			return store(&mut remote).await;
		};
		if self.reading || Kind::from((link, remote.identity.title.as_str())) == Kind::Sprint {
			let consensus = load_consensus_milestone(link)?;
			if consensus.is_some_and(|c| c.to_string() == local.to_string()) {
				store(&mut remote).await?;
			}
			return Ok(());
		}
		no_conflict(link).await?;
		self.merge(&mut local, remote, MergeMode::Normal).await
	}

	async fn sync(&mut self, milestone: &mut Milestone, mode: MergeMode) -> Result<()> {
		let link = milestone.link().clone();
		tracing::info!("Syncing milestone {}", link.as_str());
		no_conflict(&link).await?;
		let remote = load_remote_milestone(&link).await?;
		self.merge(milestone, remote, mode).await
	}

	async fn merge(&mut self, milestone: &mut Milestone, remote: Milestone, mode: MergeMode) -> Result<()> {
		// judged against what they contain by now, not as of when the edge was made
		Box::pin(self.contained(introduced(milestone, &remote))).await;
		let store = lock_store().await?;
		let consensus = load_consensus_milestone(milestone.link())?;
		*milestone = resolve_merge(milestone.clone(), consensus, remote, mode).await?;
		<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
		<Milestone as Sink<Consensus>>::sink(milestone, None).await?;
		drop(store);
		Ok(())
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
			.filter_map(|m| load_consensus_milestone(m.link()).transpose())
			.collect::<Result<_>>()?;
		let mut told: Vec<String> = Vec::new();
		for milestone in known.iter().filter(|m| self.seen.contains(m.link())) {
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

/// The consensus (git HEAD) milestone, or `None` if not yet committed.
fn load_consensus_milestone(link: &MilestoneLink) -> Result<Option<Milestone>> {
	Local::load_milestone(link, &GitReader)
}

/// Three-way resolution; a divergence is recorded for resolution in its own conflict file.
async fn resolve_merge(local: Milestone, consensus: Option<Milestone>, remote: Milestone, mode: MergeMode) -> Result<Milestone> {
	if let MergeMode::Reset { prefer } = mode {
		return match prefer {
			Side::Local => {
				allowed(&local, &remote)?;
				let mut resolved = local;
				<Milestone as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
				Ok(resolved)
			}
			Side::Remote => {
				let mut resolved = remote;
				<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?;
				Ok(resolved)
			}
		};
	}

	let (force_local_wins, force_remote_wins) = match mode {
		MergeMode::Force { prefer: Side::Local } => (true, false),
		MergeMode::Force { prefer: Side::Remote } => (false, true),
		_ => (false, false),
	};

	// `merge` unions the hosted set, which alone can't tell "the other side added it" from "I
	// deleted it". Against the base, a link one side dropped is a deletion; the losing side of a
	// `--force` gets no vote. Without a base (never committed) every link reads as an addition.
	let clash = match (mode, consensus.as_ref()) {
		(MergeMode::Normal, Some(base)) => concurrent_edit(&local, base, &remote),
		_ => false,
	};

	let dropped: Vec<NodeLink> = match consensus.as_ref() {
		Some(base) => {
			// a description still names what Github unassigned
			let unassigned = |node: &NodeLink| match node {
				NodeLink::Issue(l) => l.project() == base.identity.link.repo_info() && base.assigned.contains(&l.number()) && !remote.assigned.contains(&l.number()),
				NodeLink::Milestone(_) => false, // the field is an issue's
			};
			let in_local = local.body.hosted();
			let in_remote: Vec<NodeLink> = remote.body.hosted().into_iter().filter(|l| !unassigned(l)).collect();
			base.body
				.hosted()
				.into_iter()
				.filter(|l| (!force_remote_wins && !in_local.contains(l)) || (!force_local_wins && !in_remote.contains(l)))
				.collect()
		}
		None => Vec::new(),
	};

	let mut local_merged = local.clone();
	local_merged.merge(&remote, consensus.as_ref(), force_remote_wins);
	let mut remote_merged = remote.clone();
	remote_merged.merge(&local, consensus.as_ref(), force_local_wins);

	local_merged.body.0.remove_nodes(&dropped);
	remote_merged.body.0.remove_nodes(&dropped);

	if !clash && milestones_agree(&local_merged, &remote_merged) {
		let mut resolved = local_merged;
		allowed(&resolved, &remote)?;
		<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?;
		<Milestone as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
		return Ok(resolved);
	}

	let repo = local.identity.link.repo_info();
	let number = local.number();
	let (local_side, remote_side) = match clash {
		true => (local.to_string(), remote.to_string()),
		false => (local_merged.to_string(), remote_merged.to_string()),
	};
	if local_side == remote_side {
		let differ: Vec<&str> = [
			("title", local.identity.title != remote.identity.title),
			("state", local.identity.state != remote.identity.state),
			("due date", local.identity.due_on != remote.identity.due_on),
		]
		.into_iter()
		.filter_map(|(field, differs)| differs.then_some(field))
		.collect();
		bail!(
			"Milestone {repo}#{number}: {} changed both locally and on GitHub since the last sync, and the conflict file holds only the body. Make {} agree with GitHub (or GitHub with it), then re-run.",
			differ.join(", "),
			Local::milestone_project_dir(repo).join(".meta.json").display()
		);
	}
	let path = milestone_conflict_path(&local.identity.link);
	record_conflict(&path, &local_side, consensus.as_ref().map(Milestone::to_string).as_deref(), &remote_side)?;
	bail!("Conflict detected for milestone {repo}#{number}.\nResolve the markers in {}, then re-run.", path.display())
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
