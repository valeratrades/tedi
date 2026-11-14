//! Sync local issue changes with Github.
//!
//! ## Sync Algorithm
//!
//! The sync uses a four-merge approach:
//! ```text
//! local.merge(consensus, false);  local.merge(remote, force);
//! remote.merge(consensus, false); remote.merge(local, force);
//!
//! if local == remote { resolved } else { conflict }
//! ```
//!
//! ## MergeMode Semantics
//!
//! - `Normal`: Use timestamp-based resolution. Conflict if timestamps equal.
//! - `Force { prefer }`: On conflicts, take preferred side.
//! - `Reset { prefer }`: Take preferred side entirely.
//!
//! ## MergeMode Consumption
//!
//! The merge mode is consumed after first use (pre-editor sync), so post-editor
//! sync always uses `Normal`. This prevents accidental data loss.

pub(crate) use core::fetch_remote;

use color_eyre::eyre::{Result, bail};
use tracing::instrument;
pub use types::*;

use super::merge::Merge;
use crate::{
	BlockerSetState, Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, RepoInfo, VirtualIssue,
	conflict_resolve::{Clash, Conflict, Field},
	local::{Consensus, FsReader, Local, LocalFs, LocalIssueSource, LocalPath, consensus::load_consensus_issue},
	remote::{Remote, RemoteSource},
	selection::Selected,
	sink::Sink,
};

/// Is a bailed sync error a transient GitHub outage (5xx/429/network)? Walks the error chain so it
/// catches a `GithubError` surfaced directly (e.g. `load_remote_*`) or nested in a `RemoteSinkError`.
/// A `true` means the online sync can safely defer to a local save, leaving the divergence for the
/// pre-open sync to reflush on the next online edit.
pub(crate) fn is_transient_sync_error(e: &color_eyre::Report) -> bool {
	e.chain()
		.any(|cause| cause.downcast_ref::<crate::github::GithubError>().map(|g| g.is_transient()).unwrap_or(false))
}

/// Reconcile linked issues with Github, leaving local, consensus and remote in agreement.
///
/// Every read of an issue that a user is about to reason against goes through here first: an edit
/// applied on top of a body Github has moved past folds back as a deletion of everything it gained
/// since. A transient outage degrades to the local copy; a genuine divergence is a loud conflict.
///
/// Remotes are fetched concurrently; merges run one at a time — the consensus sink commits through
/// the one git index, and a conflict is recorded on a git branch once per run, so the first failed
/// merge stops the batch. An issue nested under another one in the batch is synced as part of that
/// ancestor's tree, then re-read from disk.
pub async fn pull_issues(issues: &mut [Issue], mode: MergeMode) -> Result<()> {
	let indices: Vec<IssueIndex> = issues.iter().map(Issue::full_index).collect();
	let nested: Vec<bool> = indices
		.iter()
		.map(|i| {
			indices
				.iter()
				.any(|a| a.repo_info() == i.repo_info() && a.index().len() < i.index().len() && i.index().starts_with(a.index()))
		})
		.collect();
	let remotes: Vec<Option<Result<Issue>>> = futures::future::join_all(issues.iter().zip(&nested).map(|(issue, nested)| async move {
		match !nested && issue.is_linked() && !issue.identity.is_virtual {
			true => Some(core::fetch_remote(issue).await),
			false => None,
		}
	}))
	.await;

	let mut linked = Vec::new();
	for (issue, remote) in issues.iter_mut().zip(remotes) {
		let Some(remote) = remote else { continue };
		let result = match remote {
			Ok(remote) => {
				let consensus = load_consensus_issue(IssueIndex::from(&*issue)).await?;
				core::sync(issue, consensus, remote, mode).await
			}
			Err(e) => Err(e),
		};
		match result {
			Ok(links) => linked.extend(links),
			Err(e) if is_transient_sync_error(&e) => tracing::warn!("GitHub transient during pre-open issue sync — proceeding with local: {e}"),
			Err(e) => return Err(e),
		}
	}
	fetch_and_store_missing(&linked).await;
	for (issue, _) in issues.iter_mut().zip(&nested).filter(|(_, nested)| **nested) {
		*issue = Issue::load(LocalIssueSource::<FsReader>::build(LocalPath::new(issue.full_index())).await?).await?;
	}
	Ok(())
}

/// Modify a local issue, then sync changes back to Github.
///
/// Caller is responsible for loading the issue (via `Issue::load(LocalIssueSource)`).
#[instrument(skip_all, fields(
	repo = ?issue.identity.repo_info(),
	issue_id = ?issue.git_id(),
	title = %issue.contents.title,
	?mode,
	modifier = ?modifier,
))]
pub async fn modify_and_sync_issue(mut issue: Issue, mode: SyncMode, modifier: Modifier, sync_opts: SyncOptions) -> Result<ModifyResult> {
	let repo_info = issue.identity.repo_info();
	let issue_index = IssueIndex::from(&issue);

	// `Write` applies a buffer rendered earlier: a pull between that render and the apply reads as the user reverting whatever the remote gained meanwhile.
	let pre_pull = !matches!(modifier, Modifier::Write { .. })
		&& match mode {
			SyncMode::Offline => false,
			SyncMode::OnExit => sync_opts.merge_mode_set(), // a forced side is a resolution asked for before editing
			SyncMode::Full => true,
		};
	if pre_pull {
		pull_issues(std::slice::from_mut(&mut issue), sync_opts.take_merge_mode()).await?;
	}

	// expose for modification (by user or procedural)
	let before = issue.clone();
	let new_modified = {
		let result = modifier.apply(&mut issue).await?;
		match result.file_modified {
			true => {
				rehome_detached(&before, &issue).await?;
				// Record this issue as the last modified
				let cache_path = crate::paths::cache_file("last_modified_issue");
				std::fs::write(&cache_path, issue.full_index().to_string()).ok();
			}
			false if mode != SyncMode::Offline && unsynced(&issue).await? => println!("No changes made; resuming an unfinished sync."),
			false => {
				v_utils::log!("Aborted (no changes made)");
				return Ok(result);
			}
		}
		result
	};

	if mode != SyncMode::Offline
		&& !repo_info.is_virtual()
		&& let Err(missing) = check_repo_labels(&before, &issue).await?
	{
		crate::utils::persist_rejected_changes(&issue.to_string())?;
		<Issue as Sink<LocalFs>>::sink(&mut before.clone(), Some(&issue)).await?;
		return Err(RejectedEdit(missing.into()).into());
	}

	match mode == SyncMode::Offline || repo_info.is_virtual() {
		true => {
			<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
			persist_blocker_selection(&mut issue);
			println!("Offline: saved locally and exiting.");
			return Ok(new_modified);
		}
		false => {
			// Post-editor sync
			let mode = sync_opts.take_merge_mode();
			match issue.is_linked() {
				true => {
					let consensus = load_consensus_issue(issue_index).await?;
					let synced = match core::fetch_remote(&issue).await {
						Ok(remote) => core::sync(&mut issue, consensus, remote, mode).await,
						Err(e) => Err(e),
					};
					match synced {
						Ok(linked) => {
							fetch_and_store_missing(&linked).await;
						}
						Err(e) if is_transient_sync_error(&e) => {
							tracing::warn!("GitHub transient during issue sync — deferring to local: {e}");
							eprintln!("GitHub unreachable; edit saved locally (will sync next online edit).");
							<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
						}
						Err(e) => return Err(e),
					}
				}
				false => {
					// New issue - check if parent needs syncing first
					let parent_index = issue.identity.parent_index;
					if let Some((i, _)) = parent_index
						.index()
						.iter()
						.enumerate()
						.find(|(_, s)| matches!(s, IssueSelector::Title(_) | IssueSelector::Exact(_)))
					{
						// 1. Sink current issue to local so ancestor can find it
						<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;

						// 2. Load ancestor up to first Title selector
						let ancestor_index = IssueIndex::with_index(repo_info, parent_index.index()[..=i].to_vec());
						let ancestor_source = LocalIssueSource::<FsReader>::build(LocalPath::new(ancestor_index)).await?;
						let mut ancestor = Issue::load(ancestor_source).await?;
						let old_ancestor = ancestor.clone();

						// 3. Sink ancestor to Remote, then Local (with old state for cleanup)
						<Issue as Sink<Remote>>::sink(&mut ancestor, None).await?;
						<Issue as Sink<LocalFs>>::sink(&mut ancestor, Some(&old_ancestor)).await?;

						// 4. Commit
						<Issue as Sink<Consensus>>::sink(&mut ancestor, None).await?;
					} else {
						<Issue as Sink<Remote>>::sink(&mut issue, None).await?;
						<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
						<Issue as Sink<Consensus>>::sink(&mut issue, None).await?;
					}
				}
			}

			persist_blocker_selection(&mut issue);
			Ok(new_modified)
		}
	}
}

/// Store every link not held locally, each with the whole tree it lives in: fetched from GitHub
/// concurrently, stored one at a time — the consensus sink commits through the one git index. A tree
/// already stored is synced instead, so its local edits survive. Returns the links still not stored.
pub(crate) async fn fetch_and_store_missing(links: &[IssueLink]) -> Vec<IssueLink> {
	let stored = |link: &IssueLink| match link {
		IssueLink::Virtual(p) => p.exists(),
		IssueLink::Owned(_) => Local::find_by_number(link.project(), link.number(), FsReader).is_some(),
	};
	let mut missing: Vec<&IssueLink> = Vec::new();
	for link in links.iter().filter(|l| !stored(l)) {
		if !missing.contains(&link) {
			missing.push(link);
		}
	}
	let roots: Vec<Result<Option<IssueLink>>> = futures::future::join_all(missing.iter().map(|link| async move {
		// virtual issues have no remote: they must never be fetched
		if matches!(link, IssueLink::Virtual(_)) {
			bail!("virtual issue file is missing");
		}
		if crate::github::client::get()?.fetch_issue(link.project(), link.number()).await?.pull_request.is_some() {
			return Ok(None); // a blocker may name a pull request; there's no issue to store
		}
		let lineage = RemoteSource::build((*link).clone(), None)?
			.resolve_parent_index()
			.await?
			.expect("a remote source always resolves its parent index");
		Ok(Some(match lineage.index().first() {
			Some(IssueSelector::GitId(root)) => IssueLink::in_project(link.project(), *root),
			Some(other) => unreachable!("lineage read from GitHub is numbered, got {other:?}"),
			None => (*link).clone(),
		}))
	}))
	.await;

	let mut trees: Vec<IssueLink> = Vec::new();
	let mut pull_requests: Vec<&IssueLink> = Vec::new();
	for (link, root) in missing.iter().zip(roots) {
		match root {
			Ok(Some(root)) if !trees.contains(&root) => trees.push(root),
			Ok(Some(_)) => {}
			Ok(None) => pull_requests.push(link),
			Err(e) => tracing::warn!("failed to fetch {link}: {e}"),
		}
	}
	let (held, fresh): (Vec<IssueLink>, Vec<IssueLink>) = trees.into_iter().partition(|root| stored(root));
	let fetched: Vec<Result<Issue>> = futures::future::join_all(fresh.iter().map(|root| async move { Ok(Issue::load(RemoteSource::build(root.clone(), Some(&[]))?).await?) })).await;
	for (root, tree) in fresh.iter().zip(fetched) {
		let landed = match tree {
			Ok(mut tree) =>
				async {
					<Issue as Sink<LocalFs>>::sink(&mut tree, None).await?;
					<Issue as Sink<Consensus>>::sink(&mut tree, None).await?;
					Ok::<_, color_eyre::Report>(())
				}
				.await,
			Err(e) => Err(e),
		};
		if let Err(e) = landed {
			tracing::warn!("failed to fetch {root}: {e}");
		}
	}
	// a sub-issue new on GitHub under a tree stored here
	for root in held {
		let synced = async {
			let mut tree = crate::sprints::load_local_issue(&root).await?;
			let remote = core::fetch_remote(&tree).await?;
			let consensus = load_consensus_issue(IssueIndex::from(&tree)).await?;
			core::sync(&mut tree, consensus, remote, MergeMode::Normal).await
		}
		.await;
		if let Err(e) = synced {
			tracing::warn!("failed to sync {root}: {e}");
		}
	}

	let unavailable: Vec<IssueLink> = missing.into_iter().filter(|l| !stored(l) && !pull_requests.contains(l)).cloned().collect();
	for link in &unavailable {
		tracing::warn!("{link} is not stored locally"); // its ref stays a bare link
	}
	unavailable
}

/// Local moved past consensus with no sync landing it: a run killed mid-sync, or an outage deferred to local.
pub(crate) async fn unsynced(issue: &Issue) -> Result<bool> {
	Ok(issue.is_linked() && load_consensus_issue(IssueIndex::from(issue)).await?.as_ref() != Some(issue))
}

/// Labels the edit added that the repo lacks, as the error naming them. The cached label list is
/// refreshed once before a label counts as missing: it can only lack ones created since.
async fn check_repo_labels(before: &Issue, after: &Issue) -> Result<Result<(), String>> {
	fn added<'a>(before: Option<&Issue>, after: &'a Issue, out: &mut Vec<&'a str>) {
		out.extend(
			after
				.contents
				.labels
				.iter()
				.map(String::as_str)
				.filter(|l| !before.is_some_and(|b| b.contents.labels.iter().any(|h| h == l))),
		);
		for (selector, child) in &after.children {
			added(before.and_then(|b| b.children.get(selector)), child, out);
		}
	}
	let mut labels = Vec::new();
	added(Some(before), after, &mut labels);
	if labels.is_empty() {
		return Ok(Ok(()));
	}
	let repo = after.repo_info();
	let mut known = Local::load_project_meta(repo).labels;
	if labels.iter().any(|l| !known.iter().any(|k| k == l)) {
		known = crate::github::client::get()?.list_labels(repo).await?;
		Local::update_project_meta(repo, |m| m.labels = known.clone())?;
	}
	let mut missing: Vec<&str> = labels.into_iter().filter(|l| !known.iter().any(|k| k == l)).collect();
	missing.sort_unstable();
	missing.dedup();
	Ok(match missing.is_empty() {
		true => Ok(()),
		false => Err(format!("{repo} has no label {}: create it there first, or fix the name", missing.join(", "))),
	})
}

/// Linked children of `before` that `after` no longer holds were detached: they live on as top-level issues.
async fn rehome_detached(before: &Issue, after: &Issue) -> Result<()> {
	fn rehome(issue: &mut Issue, parent_index: IssueIndex) {
		issue.identity.parent_index = parent_index;
		let child_index = issue.identity.child_parent_index().expect("stored issues are numbered");
		for child in issue.children.values_mut() {
			rehome(child, child_index);
		}
	}

	for (selector, old) in &before.children {
		match after.children.get(selector) {
			Some(kept) => Box::pin(rehome_detached(old, kept)).await?,
			None if old.is_linked() => {
				let mut moved = old.clone();
				rehome(&mut moved, IssueIndex::repo_only(old.repo_info()));
				<Issue as Sink<LocalFs>>::sink(&mut moved, Some(old)).await?;
				println!("Detached {} from {}", moved.full_index(), before.full_index());
			}
			None => {}
		}
	}
	Ok(())
}

/// Changed on both sides since `base`, to different values.
pub(super) fn clash<T: PartialEq>(local: T, base: T, remote: T) -> bool {
	local != base && remote != base && local != remote
}

/// `!s` blocker selection: once the edited issue is sunk (and its final path known), point the
/// active-sprint selection at it. Relocated here from the `Sink<LocalFs>` impl so storage stays
/// selection-free. Best-effort — a failure to resolve the path only warns.
fn persist_blocker_selection(issue: &mut Issue) {
	if !matches!(issue.contents.blockers.set_state, Some(BlockerSetState::Pending)) {
		return;
	}
	let title = &issue.contents.title;
	let closed = issue.contents.state.is_closed();
	let has_children = !issue.children.is_empty();
	match LocalPath::from(&*issue).resolve_parent(FsReader) {
		Ok(resolved) => {
			let issue_file_path = resolved.deterministic(title, closed, has_children).path();
			match Selected::set_by_path(&issue_file_path) {
				Ok(()) => {
					tracing::info!(path = %issue_file_path.display(), "blocker selection persisted via !s");
					issue.contents.blockers.set_state = Some(BlockerSetState::Applied);
				}
				Err(e) => tracing::warn!("failed to persist blocker selection: {e}"),
			}
		}
		Err(e) => tracing::warn!("!s: failed to resolve issue path for blocker selection: {e}"),
	}
}
#[derive(Debug, thiserror::Error)]
#[error("{0}\n\nYour changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.")]
struct RejectedEdit(Box<dyn std::error::Error + Send + Sync>);

mod core {
	use super::*;
	/// Sync an issue between local and remote using the four-merge algorithm.
	///
	/// ```text
	/// local.merge(consensus, false);  local.merge(remote, force);
	/// remote.merge(consensus, false); remote.merge(local, force);
	///
	/// if local == remote { resolved } else { conflict }
	/// ```
	///
	/// Returns `(resolved_issue, changed)` where `changed` indicates if any updates were made.
	#[instrument(skip_all, fields(?mode))]
	pub(super) async fn resolve_merge(local: Issue, consensus: Option<Issue>, remote: Issue, mode: MergeMode, repo_info: RepoInfo, issue_number: u64) -> Result<(Issue, bool)> {
		// `remote` stays the sinks' `old`: it's what Github holds, so a body the view dropped refs from gets rewritten
		let mut view = remote.clone();
		unlinked_on_github(&local, &mut view);

		// Handle Reset mode - take one side entirely
		if let MergeMode::Reset { prefer } = mode {
			return match prefer {
				Side::Local => {
					let mut resolved = local;
					prs_from(&mut resolved, &remote);
					<Issue as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
					Ok((resolved, true))
				}
				Side::Remote => {
					let mut resolved = view;
					<Issue as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
					<Issue as Sink<LocalFs>>::sink(&mut resolved, None).await?;
					Ok((resolved, true))
				}
			};
		}

		// In Force mode, we pass force=true only to the merge where `other` is the preferred side.
		// merge(other, true) means	 "take other's values on conflicts".
		// So: prefer Local → force on remote_merged.merge(local)
		//     prefer Remote → force on local_merged.merge(remote)
		let (force_local_wins, force_remote_wins) = match mode {
			MergeMode::Force { prefer: Side::Local } => (true, false),
			MergeMode::Force { prefer: Side::Remote } => (false, true),
			_ => (false, false),
		};

		let mut found = Vec::new();
		if let (MergeMode::Normal, Some(base)) = (mode, &consensus) {
			clashes(&local, Some(base), &view, true, &mut found);
		}

		// Apply four-merge algorithm
		let mut local_merged = local.clone();
		let mut remote_merged = view.clone();

		if let Some(ref consensus) = consensus {
			local_merged.merge(consensus, false)?;
		}
		local_merged.merge(&view, force_remote_wins)?;

		if let Some(ref consensus) = consensus {
			remote_merged.merge(consensus, false)?;
		}
		remote_merged.merge(&local, force_local_wins)?;

		if let Some(ref consensus) = consensus {
			drop_detached(&mut local_merged, consensus, &local, &view);
			drop_detached(&mut remote_merged, consensus, &local, &view);
		}

		if found.is_empty() && local_merged != remote_merged {
			clashes(&local_merged, consensus.as_ref(), &remote_merged, false, &mut found);
			if found.is_empty() {
				let children = |i: &Issue| {
					let mut titles: Vec<String> = i.children.values().map(|c| c.contents.title.clone()).collect();
					titles.sort();
					titles.join("\n")
				};
				found.push(Found {
					index: local_merged.full_index(),
					local: local_merged.to_string(),
					consensus: consensus.as_ref().map(Issue::to_string),
					remote: remote_merged.to_string(),
					clashes: vec![Clash {
						node: IssueLink::in_project(repo_info, issue_number),
						field: Field::SubIssues,
						consensus: consensus.as_ref().map(children),
						local: children(&local_merged),
						remote: children(&remote_merged),
					}],
				});
			}
		}
		if found.is_empty() {
			let mut resolved = local_merged;
			prs_from(&mut resolved, &remote);
			<Issue as Sink<LocalFs>>::sink(&mut resolved, None).await?;
			<Issue as Sink<Remote>>::sink(&mut resolved, Some(&remote)).await?;
			return Ok((resolved, true));
		}
		let mut files = Vec::new();
		for f in &found {
			let path = crate::local::conflict_detect::conflict_path(f.index);
			crate::conflict_resolve::record_conflict(&path, &f.local, f.consensus.as_deref(), &f.remote)?;
			files.push(path);
		}
		Err(Conflict {
			link: IssueLink::in_project(repo_info, issue_number),
			clashes: found.into_iter().flat_map(|f| f.clashes).collect(),
			files,
		}
		.into())
	}

	struct Found {
		index: IssueIndex,
		local: String,
		consensus: Option<String>,
		remote: String,
		clashes: Vec<Clash>,
	}

	/// Each node's fields that differ between `local` and `remote`: changed on both sides since `base` when
	/// `since_base` (neither edit was made knowing of the other, so no timestamp can order them), else at all.
	fn clashes(local: &Issue, base: Option<&Issue>, remote: &Issue, since_base: bool, out: &mut Vec<Found>) {
		if since_base && base.is_none() {
			return;
		}
		let fields: [(Field, fn(&Issue) -> String); 6] = [
			(Field::Title, |i| i.contents.title.clone()),
			(Field::Labels, |i| {
				let mut labels = i.contents.labels.clone();
				labels.sort_unstable();
				labels.join(", ")
			}),
			(Field::Assignees, |i| i.contents.assignees.join(", ")),
			(Field::Body, |i| i.body().to_string()),
			(Field::State, |i| format!("{:?}", i.contents.state)),
			(Field::Comments, |i| {
				i.contents.comments.iter().skip(1).map(|c| format!("{:?}: {}", c.id(), c.body)).collect::<Vec<_>>().join("\n")
			}),
		];
		let node = local.identity.link().expect("on the remote, so linked").clone();
		let found: Vec<Clash> = fields
			.into_iter()
			.filter_map(|(field, read)| {
				let (l, b, r) = (read(local), base.map(read), read(remote));
				let differs = match since_base {
					true => clash(&l, b.as_ref().expect("checked above"), &r),
					false => l != r,
				};
				differs.then(|| Clash {
					node: node.clone(),
					field,
					consensus: b,
					local: l,
					remote: r,
				})
			})
			.collect();
		if !found.is_empty() {
			out.push(Found {
				index: local.full_index(),
				local: local.to_string(),
				consensus: base.map(Issue::to_string),
				remote: remote.to_string(),
				clashes: found,
			});
		}
		for (selector, child) in &local.children {
			if let Some(remote) = remote.children.get(selector) {
				clashes(child, base.and_then(|b| b.children.get(selector)), remote, since_base, out);
			}
		}
	}

	/// Pull requests are read-only, so Github's view of them is the only one.
	fn prs_from(issue: &mut Issue, remote: &Issue) {
		if let (Some(meta), Some(read)) = (issue.identity.mut_linked_issue_meta(), remote.identity.as_linked()) {
			meta.prs = read.prs.clone();
		}
		for (selector, child) in &mut issue.children {
			if let Some(remote) = remote.children.get(selector) {
				prs_from(child, remote);
			}
		}
	}

	/// Drops each blocker ref whose link Github held at our last sync (the set `local` carries) but no longer does.
	fn unlinked_on_github(local: &Issue, remote: &mut Issue) {
		let home = remote.repo_info();
		if let (Some(was), Some(now)) = (local.identity.as_linked(), remote.identity.as_linked()) {
			remote.contents.blockers.drop_unlinked(home, &now.blocked_by, &was.blocked_by);
		}
		for (selector, child) in &mut remote.children {
			if let Some(local) = local.children.get(selector) {
				unlinked_on_github(local, child);
			}
		}
	}

	/// A child `base` held that either side no longer does was detached there; the merges must not bring it back.
	fn drop_detached(merged: &mut Issue, base: &Issue, local: &Issue, remote: &Issue) {
		merged
			.children
			.retain(|s, _| !base.children.contains_key(s) || (local.children.contains_key(s) && remote.children.contains_key(s)));
		for (s, child) in &mut merged.children {
			if let (Some(b), Some(l), Some(r)) = (base.children.get(s), local.children.get(s), remote.children.get(s)) {
				drop_detached(child, b, l, r);
			}
		}
	}

	/// Network half of a sync: reads Github only, so any number may run at once.
	pub(crate) async fn fetch_remote(current_issue: &Issue) -> Result<Issue> {
		let issue_number = current_issue.git_id().expect(
			"can't be linked and not have number associated\nunless we die in a weird moment I guess. If this ever triggers, should fix it to set issue as pending (not linked) and sink",
		);
		let link = IssueLink::in_project(current_issue.repo_info(), issue_number);
		let remote_source = RemoteSource::build(link, Some(&current_issue.identity.git_lineage()?))?; //DEPENDS: git_lineage() will error if any parent is not synced. //Q: should I move the logic for traversing IssueIndex in search of pending parents right here?
		Ok(Issue::load(remote_source).await?)
	}

	/// Returns every issue the synced tree's blocker links reach, on either end.
	#[instrument(skip_all, fields(?mode, has_consensus = consensus.is_some()))]
	pub(super) async fn sync(current_issue: &mut Issue, consensus: Option<Issue>, remote: Issue, mode: MergeMode) -> Result<Vec<IssueLink>> {
		fn linked(issue: &Issue, out: &mut Vec<IssueLink>) {
			out.extend(issue.contents.blockers.issue_links(issue.repo_info()));
			out.extend(issue.identity.as_linked().and_then(|m| m.blocking.clone()).into_iter().flatten());
			for child in issue.children.values() {
				linked(child, out);
			}
		}

		println!("Syncing...");
		let issue_number = current_issue.git_id().expect("fetched remote, so linked");
		let repo_info = current_issue.repo_info();
		let mut links = Vec::new();
		linked(&remote, &mut links);

		let (resolved, changed) = core::resolve_merge(current_issue.clone(), consensus, remote, mode, repo_info, issue_number).await?;
		super::rehome_detached(current_issue, &resolved).await?;
		*current_issue = resolved;

		match changed {
			true => {
				// Re-sink local in case issue numbers changed
				<Issue as Sink<LocalFs>>::sink(current_issue, None).await?;
				<Issue as Sink<Consensus>>::sink(current_issue, None).await?;
			}
			false => println!("No changes."),
		}
		linked(current_issue, &mut links);
		Ok(links)
	}
}

mod types {
	use super::*;
	/// Which side to prefer in merge operations.
	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum Side {
		Local,
		Remote,
	}

	/// How to merge local and remote states.
	#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
	pub enum MergeMode {
		/// Timestamp-based resolution. Conflict if can't auto-resolve.
		#[default]
		Normal,
		/// Force preferred side on conflicts, keep non-conflicting from both.
		Force { prefer: Side },
		/// Take preferred side entirely.
		Reset { prefer: Side },
	}

	/// When a run may reach GitHub, relative to the editor.
	#[derive(Clone, Copy, Debug, Eq, PartialEq)]
	pub enum SyncMode {
		Offline,
		/// Only once the editor has closed: it opens on local state.
		OnExit,
		/// Before the editor too, so it opens on reconciled state.
		Full,
	}

	/// Options for controlling sync behavior.
	///
	/// The `merge_mode` is consumed after first use (pre-editor sync),
	/// so post-editor sync runs with `MergeMode::Normal`.
	#[derive(Debug, Default)]
	pub struct SyncOptions {
		merge_mode: std::cell::Cell<Option<MergeMode>>,
	}

	impl SyncOptions {
		pub fn new(merge_mode: Option<MergeMode>) -> Self {
			Self {
				merge_mode: std::cell::Cell::new(merge_mode),
			}
		}

		pub(super) fn merge_mode_set(&self) -> bool {
			self.merge_mode.get().is_some()
		}

		/// Take the merge mode, returning Normal if already taken or not set.
		pub fn take_merge_mode(&self) -> MergeMode {
			self.merge_mode.take().unwrap_or_default()
		}
	}

	/// Result of applying a modifier to an issue.
	pub struct ModifyResult {
		pub output: Option<String>,
		pub file_modified: bool,
	}

	/// Fold an edited parent buffer back into the in-memory issue.
	///
	/// The buffer carries this node's own contents plus its children as one-line links (shallow —
	/// state/title/labels only). The children's real subtrees stay as loaded; we apply link-level
	/// changes, drop links the user removed, and add newly-typed pending children. Errors if the
	/// resulting tree breaks `Issue::check_edit`.
	pub(crate) fn apply_edited_buffer(issue: &mut Issue, old: &Issue, edited: VirtualIssue) -> Result<(), crate::IssueError> {
		use std::collections::HashMap;

		issue.contents = edited.contents;
		let child_parent_idx = IssueIndex::from(&*issue);

		let mut children = HashMap::new();
		for (selector, edited_child) in edited.children {
			let child = match old.children.get(&selector) {
				Some(existing) => {
					let mut child = existing.clone();
					child.contents.title = edited_child.contents.title;
					child.contents.labels = edited_child.contents.labels;
					child.contents.assignees = edited_child.contents.assignees;
					child.contents.state = edited_child.contents.state;
					child
				}
				None => Issue {
					identity: if issue.identity.is_virtual {
						crate::IssueIdentity::virtual_issue(child_parent_idx)
					} else {
						crate::IssueIdentity::pending(child_parent_idx)
					},
					contents: edited_child.contents,
					children: HashMap::new(),
				},
			};
			children.insert(selector, child);
		}
		issue.children = children;
		issue.check_edit()
	}

	/// An edit pinning an unassigned issue Selected or Wip takes it: whoever made the edit is its assignee.
	fn take_on_pin(issue: &mut Issue, old: &Issue) {
		use crate::labels::{Pin, Scheme};
		let taken = |i: &Issue| matches!(Scheme::from(i.contents.labels.as_slice()).pin, Some(Pin::Selected | Pin::Wip));
		if taken(issue) && !taken(old) && issue.contents.assignees.is_empty()
			// unknown only on a first-ever offline run: nobody to assign
			&& let Some(me) = crate::current_user::get()
		{
			issue.contents.assignees.push(me);
		}
		for (selector, child) in &mut issue.children {
			if let Some(old) = old.children.get(selector) {
				take_on_pin(child, old);
			}
		}
	}

	/// A modifier that can be applied to an issue file.
	#[derive(Debug)]
	pub enum Modifier {
		Editor {
			open_at_blocker: bool,
		},
		BlockerPop {
			/// Number of parent ancestors to pop in addition to the current leaf.
			parents: usize,
		},
		BlockerAdd {
			text: String,
			nest: bool,
		},
		/// Replace the current (deepest) blocker's text in-place, preserving its position in the tree.
		BlockerSet {
			text: String,
		},
		/// Replace an issue's whole content from a buffer that rendered it — the sprint-embedded
		/// counterpart of `Editor`, which parses the issue's own file the same way.
		Write {
			edited: Box<VirtualIssue>,
		},
		/// Repoint blocker refs at issues that moved repo (`!mv`).
		Relink {
			moved: std::collections::HashMap<IssueLink, IssueLink>,
		},
		/// Mock modifier that does nothing but reports file as modified. For testing.
		MockGhostEdit,
	}

	impl Modifier {
		#[tracing::instrument(skip_all)]
		pub(super) async fn apply(&self, issue: &mut Issue) -> Result<ModifyResult> {
			let old_issue = issue.clone();

			let result = match self {
				Modifier::Editor { open_at_blocker } => {
					// Open the issue's real source file and edit it in place; children show as links.
					let title = issue.contents.title.clone();
					let closed = issue.contents.state.is_closed();
					let has_children = !issue.children.is_empty();
					let path = LocalPath::from(&*issue).resolve_parent(FsReader)?.deterministic(&title, closed, has_children).path();
					let pre_existed = path.exists();
					std::fs::create_dir_all(path.parent().expect("issue file path always has a parent"))?;
					std::fs::write(&path, issue.to_string())?;

					let mtime_before = std::fs::metadata(&path)?.modified()?;

					let position = open_at_blocker.then(|| {
						let (line, col) = issue.editor_position();
						crate::utils::Position::new(line, Some(col))
					});

					crate::utils::open_file(&path, position).await?;

					let mtime_after = std::fs::metadata(&path)?.modified()?;
					let file_modified = mtime_after != mtime_before;

					let content = std::fs::read_to_string(&path)?;

					// `!u` on its own last line means "undo": treat as if no changes were made
					let trimmed = content.trim_end();
					let undo = matches!(
						trimmed.strip_suffix("!u").or_else(|| trimmed.strip_suffix("!U")),
						Some(before) if before.is_empty() || before.ends_with('\n')
					);

					// If the file existed before, restore it to mirror the issue (an undo or a rejected edit
					// leaves raw editor text on disk). If we created it just to open the editor for a
					// brand-new issue, remove it — an aborted touch must not create anything.
					let restore = || -> std::io::Result<()> {
						if pre_existed {
							std::fs::write(&path, old_issue.to_string())
						} else {
							std::fs::remove_file(&path)?;
							if let Some(parent) = path.parent() {
								let _ = std::fs::remove_dir(parent); // only succeeds if we left it empty
							}
							Ok(())
						}
					};

					if undo || !file_modified {
						restore()?;
						ModifyResult { output: None, file_modified: false }
					} else {
						let applied: Result<(), Box<dyn std::error::Error + Send + Sync>> = match VirtualIssue::parse(&content, path.clone()) {
							Ok(edited) => apply_edited_buffer(issue, &old_issue, edited).map_err(Into::into),
							Err(e) => Err(e.into()),
						};
						if let Err(cause) = applied {
							crate::utils::persist_rejected_changes(&content)?;
							restore()?;
							return Err(RejectedEdit(cause).into());
						}
						ModifyResult { output: None, file_modified: true }
					}
				}
				Modifier::BlockerPop { parents } => {
					let popped = issue
						.contents
						.blockers
						.pop(*parents)
						.ok_or_else(|| color_eyre::eyre::eyre!("Cannot pop {parents} parents — blocker chain is shorter"))?;
					ModifyResult {
						output: Some(format!("Popped: {popped}")),
						file_modified: true,
					}
				}
				Modifier::BlockerAdd { text, nest } => {
					if *nest {
						issue.contents.blockers.add_child(text);
					} else {
						issue.contents.blockers.add(text);
					}
					ModifyResult { output: None, file_modified: true }
				}
				Modifier::BlockerSet { text } => {
					let old = issue.contents.blockers.set(text);
					ModifyResult {
						output: old.map(|prev| format!("Replaced: {prev} -> {text}")),
						file_modified: true,
					}
				}
				Modifier::Write { edited } => {
					apply_edited_buffer(issue, &old_issue, (**edited).clone())?;
					ModifyResult {
						output: None,
						file_modified: *issue != old_issue,
					}
				}
				Modifier::Relink { moved } => ModifyResult {
					output: None,
					file_modified: issue.contents.blockers.relink(issue.repo_info(), moved, issue.repo_info()),
				},
				Modifier::MockGhostEdit => ModifyResult { output: None, file_modified: true },
			};

			if result.file_modified {
				take_on_pin(issue, &old_issue);
				issue.post_update(&old_issue);
			}

			Ok(result)
		}
	}
}
