//! Three-way sync for milestones — the milestone mirror of `sync.rs`.
//!
//! Three-way merge (local ⇄ consensus ⇄ remote) and the same git-branch conflict flow,
//! over the simpler `Milestone` primitive (no children, no comments).

use color_eyre::eyre::{Result, bail};

use super::sync::{MergeMode, Side, SyncMode, SyncOptions, clash};
use crate::{
	IssueLink, Milestone, MilestoneBody, MilestoneLink,
	conflict_resolve::{check_for_existing_milestone_conflict, initiate_conflict_merge},
	local::{
		Consensus, GitReader, Local, LocalFs,
		conflict_detect::{ConflictOutcome, milestone_conflict_file_path},
	},
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

/// Load the consensus (git HEAD) milestone, or `None` if not yet committed.
pub async fn load_consensus_milestone(link: &MilestoneLink) -> Result<Option<Milestone>> {
	Local::load_milestone(link, &GitReader).map_err(Into::into)
}

/// Reconcile a milestone with GitHub — the milestone mirror of `pull_issues`, and the same reason:
/// a body rendered into the editor from the local cache alone is a body the user edits blind.
pub async fn pull_milestone(milestone: &mut Milestone, mode: MergeMode) -> Result<()> {
	let consensus = load_consensus_milestone(&milestone.identity.link).await?;
	if let Err(e) = sync(milestone, consensus, mode).await {
		if !super::sync::is_transient_sync_error(&e) {
			return Err(e);
		}
		tracing::warn!("GitHub transient during pre-open milestone sync — proceeding with local: {e}");
	}
	Ok(())
}

/// Modify a local milestone, then sync back to GitHub (parity with `modify_and_sync_issue`).
/// Returns the milestone as it ends up stored.
pub async fn modify_and_sync_milestone(mut milestone: Milestone, mode: SyncMode, modifier: MilestoneModifier, sync_opts: SyncOptions) -> Result<Milestone> {
	let link = milestone.identity.link.clone();

	// `BodyWrite` applies a buffer rendered earlier: a pull between that render and the apply reads as the user reverting whatever the remote gained meanwhile.
	let pulled = mode == SyncMode::Full && matches!(modifier, MilestoneModifier::Editor);
	if pulled {
		pull_milestone(&mut milestone, sync_opts.take_merge_mode()).await?;
	}

	// Unpulled, the sync below is the only reconciliation — callers read the synced state even when nothing was typed.
	if !apply_modifier(&mut milestone, modifier).await? && (pulled || mode == SyncMode::Offline) {
		v_utils::log!("Aborted (no milestone changes made)");
		return Ok(milestone);
	}

	match mode {
		SyncMode::Offline => {
			<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
			println!("Offline: milestone saved locally.");
			Ok(milestone)
		}
		SyncMode::OnExit | SyncMode::Full => {
			let mode = sync_opts.take_merge_mode();
			let consensus = load_consensus_milestone(&link).await?;
			if let Err(e) = sync(&mut milestone, consensus, mode).await {
				if !super::sync::is_transient_sync_error(&e) {
					return Err(e);
				}
				tracing::warn!("GitHub transient during milestone sync — deferring to local: {e}");
				eprintln!("GitHub unreachable; milestone edit saved locally (will sync next online edit).");
				<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
			}
			Ok(milestone)
		}
	}
}

async fn apply_modifier(milestone: &mut Milestone, modifier: MilestoneModifier) -> Result<bool> {
	let old = milestone.clone();
	let modified = match modifier {
		MilestoneModifier::Editor => {
			let repo = milestone.identity.link.repo_info();
			let path = Local::milestone_file_path(repo, milestone.number(), &milestone.identity.title);
			std::fs::create_dir_all(path.parent().expect("milestone file always has a parent"))?;
			std::fs::write(&path, format!("{milestone}\n"))?;
			crate::utils::open_file(&path, None).await?;
			let content = std::fs::read_to_string(&path)?;
			let new_body = MilestoneBody::parse(&content, repo);
			let changed = new_body != milestone.body;
			milestone.body = new_body;
			changed
		}
		MilestoneModifier::BodyWrite { body } => {
			let changed = milestone.body != body;
			milestone.body = body;
			changed
		}
	};
	if modified {
		milestone.post_update(&old);
	}
	Ok(modified)
}

async fn sync(milestone: &mut Milestone, consensus: Option<Milestone>, mode: MergeMode) -> Result<()> {
	println!("Syncing milestone...");
	let link = milestone.identity.link.clone();
	if let Some(conflict_file) = check_for_existing_milestone_conflict(&link).await? {
		bail!("Unresolved milestone conflict in {}. Resolve with git tools, then re-run.", conflict_file.display());
	}
	let remote = load_remote_milestone(&link).await?;

	let (resolved, changed) = resolve_merge(milestone.clone(), consensus, remote, mode).await?;
	*milestone = resolved;

	match changed {
		true => {
			<Milestone as Sink<LocalFs>>::sink(milestone, None).await?;
			<Milestone as Sink<Consensus>>::sink(milestone, None).await?;
		}
		false => println!("No milestone changes."),
	}
	Ok(())
}

/// Three-way resolution; a divergence produces a git-branch conflict in `__milestone_conflict.md`.
async fn resolve_merge(local: Milestone, consensus: Option<Milestone>, remote: Milestone, mode: MergeMode) -> Result<(Milestone, bool)> {
	if let MergeMode::Reset { prefer } = mode {
		return match prefer {
			Side::Local => {
				let mut resolved = local;
				<Milestone as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
				Ok((resolved, true))
			}
			Side::Remote => {
				let mut resolved = remote;
				<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?;
				Ok((resolved, true))
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
			let (in_local, in_remote) = (local.body.hosted(), remote.body.hosted());
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
		return Ok((resolved, true));
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
	match initiate_conflict_merge(repo, number, &local_side, &remote_side, milestone_conflict_file_path(repo.owner().expect("github project")))? {
		ConflictOutcome::AutoMerged => unreachable!("divergent states cannot auto-merge; the conflict must be recorded for manual resolution"),
		ConflictOutcome::NeedsResolution => bail!("Conflict detected for milestone {repo}#{number}.\nResolve using standard git tools, then re-run."),
		ConflictOutcome::NoChanges => Ok((local_merged, false)),
	}
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
