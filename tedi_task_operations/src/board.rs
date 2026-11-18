//! The board as the store holds it: every stored issue and mirrored pull request carded (the model is `tedi_core::board`).

use std::{collections::BTreeMap, path::PathBuf};

use color_eyre::eyre::{Result, bail, eyre};
pub use tedi_core::board::*;
use tedi_core::{PrState, PullRequest, workflow::State};

use crate::{
	CloseState, Issue, IssueIndex, IssueLink, LazyIssue,
	conflict_resolve::{FieldEdit, check_for_existing_conflict},
	labels::Scheme,
	local::{FsReader, GitReader, Local, LocalError, LocalIssueSource, LocalPath, States},
	open_interactions::{Modifier, SyncMode, SyncOptions, modify_and_sync_issue},
	rank::{Task, rank},
	workflow::Workflows,
};

/// What an issue without `i:` counts as (`labels.nix`: "`i:3` is the default").
const DEFAULT_IMPORTANCE: u8 = 3;

/// Every issue in the store, one card per node, and every pull request, ranked over their `blocked by` links;
/// each placed in every one of `workflows`.
pub async fn load(workflows: &Workflows) -> Result<Board> {
	let mut held: std::collections::HashMap<crate::RepoInfo, States> = std::collections::HashMap::new();
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
			Ok((issue, conflicted)) => {
				let repo = index.repo_info();
				if let std::collections::hash_map::Entry::Vacant(e) = held.entry(repo) {
					e.insert(Local::load_states(repo)?);
				}
				let states = &held[&repo];
				card_tree(&issue, None, conflicted, states, workflows, &mut cards)?
			}
			Err(e) => unreadable.push((path, e.to_string())),
		}
	}
	for (repo, pr) in Local::stored_prs()? {
		if let std::collections::hash_map::Entry::Vacant(e) = held.entry(repo) {
			e.insert(Local::load_states(repo)?);
		}
		let states = &held[&repo];
		cards.push(pr_card(repo, pr, states, workflows)?);
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
	let repos: std::collections::HashSet<_> = cards.iter().map(|c| c.link.project()).collect();
	let colors = repos.into_iter().map(|repo| (repo.to_string(), Local::load_project_meta(repo).label_colors)).collect();
	Ok(Board { cards, unreadable, colors })
}

fn card_tree(issue: &Issue, parent: Option<&IssueLink>, conflicted: bool, states: &States, workflows: &Workflows, out: &mut Vec<Card>) -> Result<()> {
	let link = issue.identity.as_linked().expect("stored github issues are linked").link().clone();
	out.push(card(issue, parent.cloned(), conflicted, states, workflows)?);
	for child in issue.children.values() {
		card_tree(child, Some(&link), conflicted, states, workflows, out)?;
	}
	Ok(())
}

/// `issue`'s own node, carded.
pub(crate) fn card(issue: &Issue, parent: Option<IssueLink>, conflicted: bool, states: &States, workflows: &Workflows) -> Result<Card> {
	let meta = issue.identity.as_linked().expect("stored github issues are linked");
	let scheme = Scheme::from(issue.contents.labels.as_slice());
	let link = meta.link().clone();
	Ok(Card {
		states: placed(states, link.number(), workflows)?,
		link,
		title: issue.contents.title.clone(),
		labels: issue.contents.labels.clone(),
		assignees: issue.contents.assignees.clone(),
		state: issue.contents.state.clone(),
		closed_at: meta.timestamps.state.filter(|_| issue.contents.state.is_closed()),
		rank: None,
		importance: scheme.importance,
		hours: scheme.hours,
		kind: scheme.kind,
		prs: meta.prs.clone(),
		blocked_by: meta.blocked_by.clone(),
		parent,
		body: issue.body().to_string(),
		conflicted,
		pr: None,
	})
}

pub(crate) fn pr_card(repo: crate::RepoInfo, pr: PullRequest, states: &States, workflows: &Workflows) -> Result<Card> {
	let scheme = Scheme::from(pr.labels.as_slice());
	Ok(Card {
		link: IssueLink::in_project(repo, pr.number),
		states: placed(states, pr.number, workflows)?,
		title: pr.title,
		state: match pr.state {
			PrState::Draft | PrState::Open => CloseState::Open,
			PrState::Merged => CloseState::Closed,
			PrState::Closed => CloseState::NotPlanned,
		},
		closed_at: pr.closed_at,
		labels: pr.labels,
		assignees: pr.assignees,
		rank: None,
		importance: scheme.importance,
		hours: scheme.hours,
		kind: scheme.kind,
		prs: Vec::new(),
		blocked_by: Vec::new(),
		parent: None,
		body: pr.body,
		conflicted: false,
		pr: Some(Pr {
			state: pr.state,
			author: pr.author,
			closes: pr.closes,
		}),
	})
}

/// Where issue `number` of `states`' project sits in each workflow: as held, else the intake.
fn placed(states: &States, number: u64, workflows: &Workflows) -> Result<BTreeMap<String, State>> {
	workflows
		.iter()
		.map(|(name, loaded)| {
			let at = match states.held.get(name).and_then(|issues| issues.get(&number)) {
				Some(held) => held.clone(),
				None => loaded.workflow.intake.clone(),
			};
			if let Err(e) = loaded.workflow.leaf(&at) {
				bail!(
					"{}#{number} sits at {at} in {name}: {e}. Move it to a bucket {name} has (`tedi move`), or name its old one again",
					states.repo
				);
			}
			Ok((name.clone(), at))
		})
		.collect()
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

pub(crate) fn stored_index(link: &IssueLink) -> Result<IssueIndex> {
	let path = Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("{link} is not stored here: pull it first"))?;
	Local::extract_index_from_path(&path)
}
