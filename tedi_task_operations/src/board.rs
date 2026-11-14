//! The board as the store holds it: every stored issue carded (the model is `tedi_core::board`).

use std::{collections::BTreeMap, path::PathBuf};

use color_eyre::eyre::{Result, eyre};
use jiff::{SignedDuration, Timestamp};
pub use tedi_core::board::*;

use crate::{
	Issue, IssueIndex, IssueLink, LazyIssue,
	conflict_resolve::{FieldEdit, check_for_existing_conflict},
	labels::Scheme,
	local::{FsReader, GitReader, Local, LocalError, LocalIssueSource, LocalPath},
	open_interactions::{Modifier, SyncMode, SyncOptions, modify_and_sync_issue},
	rank::{Task, rank},
	stage::stage,
};

/// What an issue without `i:` counts as (`labels.nix`: "`i:3` is the default").
const DEFAULT_IMPORTANCE: u8 = 3;

/// Every issue in the store, one card per node, ranked over their `blocked by` links.
pub async fn load(archive_after: SignedDuration, now: Timestamp) -> Result<Board> {
	let mut cards = Vec::new();
	let mut unreadable = Vec::new();
	for rel in Local::issue_file_relpaths()? {
		let path = Local::issues_dir().join(&rel);
		let index = Local::extract_index_from_path(&path)?;
		if index.index().len() != 1 || index.repo_info().is_virtual() {
			continue; // a child is carded with its root's tree; virtual issues have no board
		}
		let loaded = match LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await {
			Ok(source) => Issue::load(source).await.map(|i| (i, false)),
			Err(LocalError::ConflictBlocked(_)) => Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::new(index))?).await.map(|i| (i, true)),
			Err(e) => Err(e),
		};
		match loaded {
			Ok((issue, conflicted)) => card_tree(&issue, None, conflicted, archive_after, now, &mut cards),
			Err(e) => unreadable.push((path, e.to_string())),
		}
	}

	let tasks: BTreeMap<String, Task> = cards
		.iter()
		.map(|c| {
			let task = Task {
				importance: c.importance.unwrap_or(DEFAULT_IMPORTANCE),
				open: c.open(),
			};
			(c.link.to_string(), task)
		})
		.collect();
	let blocks: Vec<(String, String)> = cards.iter().flat_map(|c| c.blocked_by.iter().map(|b| (b.to_string(), c.link.to_string()))).collect();
	let ranking = rank(&tasks, &blocks);
	for card in &mut cards {
		card.rank = ranking.ranks.get(&card.link.to_string()).copied();
	}
	Ok(Board { cards, unreadable })
}

fn card_tree(issue: &Issue, parent: Option<&IssueLink>, conflicted: bool, archive_after: SignedDuration, now: Timestamp, out: &mut Vec<Card>) {
	let meta = issue.identity.as_linked().expect("stored github issues are linked");
	let scheme = Scheme::from(issue.contents.labels.as_slice());
	let link = meta.link().clone();
	out.push(Card {
		link: link.clone(),
		title: issue.contents.title.clone(),
		labels: issue.contents.labels.clone(),
		assignees: issue.contents.assignees.clone(),
		state: issue.contents.state.clone(),
		stage: stage(scheme.pin, &issue.contents.state, &meta.prs, meta.timestamps.state, archive_after, now),
		rank: None,
		importance: scheme.importance,
		hours: scheme.hours,
		kind: scheme.kind,
		prs: meta.prs.clone(),
		blocked_by: meta.blocked_by.clone(),
		parent: parent.cloned(),
		body: issue.body().to_string(),
		conflicted,
	});
	for child in issue.children.values() {
		card_tree(child, Some(&link), conflicted, archive_after, now, out);
	}
}

/// `edits` applied to `link`'s stored issue and synced, without an editor: a clash pending on its line
/// is the error, to be resolved through [`pending_conflict`].
pub async fn edit(link: &IssueLink, edits: Vec<FieldEdit>, mode: SyncMode) -> Result<()> {
	let source = LocalIssueSource::<FsReader>::build(LocalPath::new(stored_index(link)?)).await?;
	modify_and_sync_issue(Issue::load(source).await?, mode, Modifier::Field { edits }, SyncOptions::default()).await?;
	Ok(())
}

/// The clash file waiting on `link`'s line, once every file already freed of its markers has landed.
/// Writing a marker-free resolution there and asking again lands it.
pub async fn pending_conflict(link: &IssueLink) -> Result<Option<PathBuf>> {
	check_for_existing_conflict(stored_index(link)?).await
}

fn stored_index(link: &IssueLink) -> Result<IssueIndex> {
	let path = Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("{link} is not stored here: pull it first"))?;
	Local::extract_index_from_path(&path)
}
