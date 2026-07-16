//! Three-way sync for milestones — the milestone mirror of `sync.rs`.
//!
//! Same four-merge algorithm (local ⇄ consensus ⇄ remote) and git-branch conflict flow,
//! over the simpler `Milestone` primitive (no children, no comments).

use color_eyre::eyre::{Result, bail};

use super::sync::{MergeMode, Side, SyncOptions};
use crate::{
	Milestone, MilestoneBody, MilestoneLink,
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

/// Modify a local milestone, then sync back to GitHub (parity with `modify_and_sync_issue`).
pub async fn modify_and_sync_milestone(mut milestone: Milestone, offline: bool, modifier: MilestoneModifier, sync_opts: SyncOptions) -> Result<bool> {
	let link = milestone.identity.link.clone();

	// Pre-open sync: pull remote and reconcile if the local file diverges from consensus.
	if !offline {
		if let Some(conflict_file) = check_for_existing_milestone_conflict(&link).await? {
			bail!("Unresolved milestone conflict in {}. Resolve with git tools, then re-run.", conflict_file.display());
		}
		let consensus = load_consensus_milestone(&link).await?;
		let local_differs = consensus.as_ref().map(|c| *c != milestone).unwrap_or(false);
		if sync_opts.pull || local_differs {
			sync(&mut milestone, consensus, sync_opts.take_merge_mode()).await?;
		}
	}

	if !apply_modifier(&mut milestone, modifier).await? {
		v_utils::log!("Aborted (no milestone changes made)");
		return Ok(false);
	}

	match offline {
		true => {
			<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
			println!("Offline: milestone saved locally.");
			Ok(true)
		}
		false => {
			let mode = sync_opts.take_merge_mode();
			let consensus = load_consensus_milestone(&link).await?;
			sync(&mut milestone, consensus, mode).await?;
			Ok(true)
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
			let new_body = MilestoneBody::parse(&content);
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

/// Four-merge resolution; a divergence produces a git-branch conflict in `__milestone_conflict.md`.
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

	let mut local_merged = local.clone();
	let mut remote_merged = remote.clone();

	if let Some(ref consensus) = consensus {
		local_merged.merge(consensus, false);
	}
	local_merged.merge(&remote, force_remote_wins);

	if let Some(consensus) = consensus {
		remote_merged.merge(&consensus, false);
	}
	remote_merged.merge(&local, force_local_wins);

	if milestones_agree(&local_merged, &remote_merged) {
		let mut resolved = local_merged;
		<Milestone as Sink<LocalFs>>::sink(&mut resolved, None).await?;
		<Milestone as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
		return Ok((resolved, true));
	}

	let repo = local.identity.link.repo_info();
	let number = local.number();
	match initiate_conflict_merge(
		repo,
		number,
		&local_merged.to_string(),
		&remote_merged.to_string(),
		milestone_conflict_file_path(repo.owner().expect("github project")),
	)? {
		ConflictOutcome::AutoMerged => unreachable!("divergent states cannot auto-merge; the conflict must be recorded for manual resolution"),
		ConflictOutcome::NeedsResolution => bail!("Conflict detected for milestone {repo}#{number}.\nResolve using standard git tools, then re-run."),
		ConflictOutcome::NoChanges => Ok((local_merged, false)),
	}
}

/// Merge equality that treats `hosted` as an order-independent set — the two merge sides
/// build the union in opposite orders, so a Vec `==` would spuriously flag a conflict.
fn milestones_agree(a: &Milestone, b: &Milestone) -> bool {
	if a.identity != b.identity {
		return false;
	}
	// Compare prose skeleton (issues removed) so the order-independent hosted set is judged separately.
	let skeleton = |m: &Milestone| {
		let mut doc = m.body.0.clone();
		doc.remove_issues();
		doc.serialize()
	};
	if skeleton(a) != skeleton(b) {
		return false;
	}
	let mut ah: Vec<String> = a.body.hosted().iter().map(|l| l.to_string()).collect();
	let mut bh: Vec<String> = b.body.hosted().iter().map(|l| l.to_string()).collect();
	ah.sort_unstable();
	bh.sort_unstable();
	ah == bh
}
