//! Three-way sync for milestones — the milestone mirror of `sync.rs`.
//!
//! Three-way merge (local ⇄ consensus ⇄ remote) and the same recorded-conflict flow,
//! over the simpler `Milestone` primitive (no children, no comments).

use color_eyre::eyre::{Result, bail, eyre};
use tedi_core::{Sprint, issue_file_name};

use super::sync::{MergeMode, Side, SyncMode, SyncOptions, clash};
use crate::{
	IssueLink, Milestone, MilestoneBody, MilestoneLink,
	conflict_resolve::{check_for_existing_milestone_conflict, record_conflict},
	local::{Consensus, FsReader, GitReader, Local, LocalFs, conflict_detect::milestone_conflict_path},
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
	/// Whether `stored` changed.
	fn apply(self, stored: &mut Milestone) -> Result<bool> {
		let Self { rendered, body } = self;
		if body == rendered {
			return Ok(false);
		}
		holds(stored, &rendered, &body)?;
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
				let (edited_links, old_links) = (edited.body.hosted(), old.body.hosted());
				let dropped: Vec<IssueLink> = base.body.hosted().into_iter().filter(|l| !edited_links.contains(l) || !old_links.contains(l)).collect();
				edited.merge(&old, Some(&base), false);
				edited.body.0.remove_issue_links(&dropped);
				*stored = edited;
			}
		}
		stored.post_update(&old);
		Ok(stored.body != old.body)
	}
}

/// Reconcile `link` with GitHub, storing it if it isn't yet — the milestone mirror of `pull_issues`, and the same
/// reason: a body rendered into the editor from the local cache alone is a body the user edits blind.
pub async fn pull_milestone(link: &MilestoneLink, mode: MergeMode) -> Result<Milestone> {
	match sync(link, None, mode).await {
		Err(e) if super::sync::is_transient_sync_error(&e) => match Local::load_milestone(link, &FsReader)? {
			Some(stored) => {
				tracing::warn!("GitHub transient during pre-open milestone sync — proceeding with local: {e}");
				Ok(stored)
			}
			None => Err(e),
		},
		synced => synced,
	}
}

/// Modify a milestone, then sync back to GitHub (parity with `modify_and_sync_issue`).
/// Returns the milestone as it ends up stored.
pub async fn modify_and_sync_milestone(link: &MilestoneLink, mode: SyncMode, modifier: MilestoneModifier, sync_opts: SyncOptions) -> Result<Milestone> {
	let stored = || Local::load_milestone(link, &FsReader)?.ok_or_else(|| eyre!("milestone {} is not stored locally", link.as_str()));
	let edit = match modifier {
		MilestoneModifier::BodyWrite(edit) => edit,
		MilestoneModifier::Editor => {
			let milestone = match mode {
				SyncMode::Full => pull_milestone(link, sync_opts.take_merge_mode()).await?,
				SyncMode::OnExit | SyncMode::Offline => stored()?,
			};
			let path = tempfile::tempdir()?.keep().join(issue_file_name(Some(link.number()), &milestone.identity.title, false)); // kept: an edit outlives a crash
			std::fs::write(&path, format!("{milestone}\n"))?;
			crate::utils::open_files(&[&path], None).await?;
			let body = MilestoneBody::parse(&std::fs::read_to_string(&path)?, link.repo_info());
			// Unpulled, the sync below is the only reconciliation — callers read the synced state even when nothing was typed.
			if body == milestone.body && mode != SyncMode::OnExit {
				v_utils::log!("Aborted (no milestone changes made)");
				return Ok(milestone);
			}
			BodyWrite { rendered: milestone.body, body }
		}
	};

	if mode == SyncMode::Offline {
		return save(&mut stored()?, edit).await;
	}
	for added in added(&edit.rendered, &edit.body) {
		if Local::find_milestone_path(added.repo_info(), added.number(), &FsReader).is_none() {
			pull_milestone(&added, MergeMode::Normal).await?; // what it is decides whether it can be held
		}
	}
	match sync(link, Some(edit.clone()), sync_opts.take_merge_mode()).await {
		Err(e) if super::sync::is_transient_sync_error(&e) => {
			tracing::warn!("GitHub transient during milestone sync — deferring to local: {e}");
			eprintln!("GitHub unreachable; milestone edit saved locally (will sync next online edit).");
			save(&mut stored()?, edit).await
		}
		synced => synced,
	}
}

async fn save(milestone: &mut Milestone, edit: BodyWrite) -> Result<Milestone> {
	let _store = crate::local::lock_store().await?;
	if edit.apply(milestone)? {
		<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
		println!("Milestone saved locally.");
	}
	Ok(milestone.clone())
}

/// Only a sprint holds a sprint, only Bottlenecks are held, and never round. What an edit didn't add stands, whatever it is.
fn holds(holder: &Milestone, rendered: &MilestoneBody, body: &MilestoneBody) -> Result<()> {
	for link in added(rendered, body) {
		let Some(held) = Local::load_milestone(&link, &FsReader)? else { continue }; // unfetched offline: what it is isn't known here
		let refuse = |why: &str| Err(Rejected(format!("{} can't hold {}: {why}", holder.identity.title, held.identity.title)).into());
		match (held.sprint(), holder.sprint()) {
			(None, _) => {}
			(Some(Sprint::Planning(_)), _) => return refuse("a planning sprint is held by nothing"),
			(Some(Sprint::Bottlenecks), None) => return refuse("only a sprint holds a sprint"),
			(Some(Sprint::Bottlenecks), Some(_)) => {
				let (mut seen, mut next) = (Vec::new(), vec![link]);
				while let Some(at) = next.pop() {
					if at == *holder.link() {
						return refuse("it holds it, itself or through others");
					}
					if !seen.contains(&at) {
						next.extend(Local::load_milestone(&at, &FsReader)?.into_iter().flat_map(|m| m.body.0.milestone_links()));
						seen.push(at);
					}
				}
			}
		}
	}
	Ok(())
}

fn added(rendered: &MilestoneBody, body: &MilestoneBody) -> Vec<MilestoneLink> {
	let before = rendered.0.milestone_links();
	body.0.milestone_links().into_iter().filter(|l| !before.contains(l)).collect()
}

async fn sync(link: &MilestoneLink, edit: Option<BodyWrite>, mode: MergeMode) -> Result<Milestone> {
	tracing::info!("Syncing milestone {}", link.as_str());
	if let Some(conflict_file) = check_for_existing_milestone_conflict(link).await? {
		bail!("Unresolved milestone conflict in {}. Resolve the markers, then re-run.", conflict_file.display());
	}
	let remote = load_remote_milestone(link).await?;

	let _store = crate::local::lock_store().await?;
	let consensus = Local::load_milestone(link, &GitReader)?;
	let mut milestone = Local::load_milestone(link, &FsReader)?.unwrap_or_else(|| remote.clone()); // not stored yet: GitHub's is what there is
	if let Some(edit) = edit
		&& edit.apply(&mut milestone)?
	{
		<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?; // whatever comes of the merge, the edit is kept
	}
	let mut milestone = resolve_merge(milestone, consensus, remote, mode).await?;
	<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
	<Milestone as Sink<Consensus>>::sink(&mut milestone, None).await?;
	Ok(milestone)
}

/// Three-way resolution; a divergence is recorded for resolution in its own conflict file.
async fn resolve_merge(local: Milestone, consensus: Option<Milestone>, remote: Milestone, mode: MergeMode) -> Result<Milestone> {
	if let MergeMode::Reset { prefer } = mode {
		return match prefer {
			Side::Local => {
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

	let dropped: Vec<IssueLink> = match consensus.as_ref() {
		Some(base) => {
			// a description still names what Github unassigned
			let unassigned = |l: &IssueLink| l.project() == base.identity.link.repo_info() && base.assigned.contains(&l.number()) && !remote.assigned.contains(&l.number());
			let in_local = local.body.hosted();
			let in_remote: Vec<IssueLink> = remote.body.hosted().into_iter().filter(|l| !unassigned(l)).collect();
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

	local_merged.body.0.remove_issue_links(&dropped);
	remote_merged.body.0.remove_issue_links(&dropped);

	if !clash && milestones_agree(&local_merged, &remote_merged) {
		let mut resolved = local_merged;
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
/// The body is one timestamped unit whose hosted links merge as a set, so only prose can be lost:
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

/// The body with its issue links taken out.
fn prose(m: &Milestone) -> String {
	let mut doc = m.body.0.clone();
	doc.remove_issues();
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
