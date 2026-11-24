//! The board as the store holds it: every stored issue and mirrored pull request carded (the model is `tedi_core::board`).

use std::{
	collections::{BTreeMap, HashMap, HashSet, VecDeque, hash_map::Entry},
	path::PathBuf,
	sync::Arc,
};

use color_eyre::eyre::{Result, bail, eyre};
pub use tedi_core::board::*;
use tedi_core::{NodeLink, PrState, PullRequest, chain::Kind, conflict::CommentEdit, workflow::State};

use crate::{
	CloseState, Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, Milestone, MilestoneLink, RepoInfo,
	conflict_resolve::{FieldEdit, check_for_existing_conflict},
	labels::Scheme,
	local::{FsReader, GitReader, Local, LocalError, LocalIssueSource, LocalPath, ProjectMeta, States, lock_store},
	open_interactions::{MergeMode, MilestoneModifier, Modifier, Pull, SyncMode, SyncOptions, modify_and_sync_issue, modify_and_sync_milestone},
	pull::pull_milestones,
	rank::{Task, rank},
	workflow::{Rejected, Workflows},
};

/// What an issue without `i:` counts as (`labels.nix`: "`i:3` is the default").
const DEFAULT_IMPORTANCE: u8 = 3;

/// Pull requests are read a repo at a time, and shown so many.
const PRS_A_PART: usize = 25;

/// Every issue in the store, one card per node, and every pull request, ranked over their `blocked by` links;
/// each placed in every one of `workflows`.
pub fn load(workflows: &Workflows) -> Result<Reading<'_>> {
	let mut roots = Vec::new();
	let mut trees: HashMap<(RepoInfo, IssueSelector), u64> = HashMap::new();
	for rel in Local::issue_file_relpaths()? {
		let path = Local::issues_dir().join(&rel);
		let index = Local::extract_index_from_path(&path)?;
		let Some(&root) = index.index().first().filter(|_| !index.repo_info().is_virtual()) else {
			continue; // virtual issues have no board
		};
		*trees.entry((index.repo_info(), root)).or_default() += std::fs::metadata(&path)?.len();
		if index.index().len() == 1 {
			roots.push((path, index.repo_info(), root)); // a child is carded with its root's tree
		}
	}
	let mut steps: Vec<Step> = roots
		.into_iter()
		.map(|(path, repo, root)| Step::Issue {
			bytes: trees.remove(&(repo, root)).unwrap_or(0), // the same root's second file (`.md`, `.md.bak`): counted with the first
			path,
		})
		.collect();
	steps.push(Step::Rest { whole: false });
	steps.extend(Local::pr_repos()?.into_iter().map(|(repo, bytes)| Step::Prs { repo, bytes }));
	steps.push(Step::Rest { whole: true });
	let (mut issues, mut prs) = (0, 0);
	for step in &steps {
		match step {
			Step::Issue { bytes, .. } => issues += bytes,
			Step::Prs { bytes, .. } => prs += bytes,
			Step::Rest { .. } => {}
		}
	}
	Ok(Reading {
		workflows,
		steps,
		at: 0,
		ready: [Part::Sizes { issues, prs }].into(),
		held: HashMap::new(),
		metas: HashMap::new(),
		tasks: BTreeMap::new(),
		blocks: Vec::new(),
		repos: HashSet::new(),
	})
}

/// Which milestone of `repo` hosts `link`, from `seen` to `new`: an issue sits in at most one a repo.
pub async fn set_milestone(link: &IssueLink, repo: RepoInfo, seen: Option<&MilestoneLink>, new: Option<&MilestoneLink>, mode: SyncMode) -> Result<()> {
	if let Some(new) = new
		&& new.repo_info() != repo
	{
		bail!("{} is no milestone of {repo}", new.as_str());
	}
	stored_index(link)?;
	if mode != SyncMode::Offline {
		pull_milestones(repo, &mut Pull::default()).await?; // whatever of the repo hosts it by now
	}
	let (hosting, rest): (Vec<Milestone>, Vec<Milestone>) = Local::stored_milestones()?
		.into_iter()
		.filter(|m| offered(m) && m.identity.link.repo_info() == repo)
		.partition(|m| m.body.0.issue_links().contains(link));
	let is_in = |at: Option<&MilestoneLink>| match at {
		Some(at) => hosting.iter().any(|m| m.link() == at),
		None => hosting.is_empty(),
	};
	if !is_in(seen) && !is_in(new) {
		let held = hosting.first().map_or("none", |m| &m.identity.title);
		return Err(Rejected(format!("its milestone in {repo} changed since it was seen: it is {held} now")).into());
	}
	let into = match new {
		Some(new) if !is_in(Some(new)) => Some(
			rest.into_iter()
				.find(|m| m.link() == new)
				.ok_or_else(|| Rejected(format!("{} is no longer an open milestone", new.as_str())))?,
		),
		_ => None,
	};
	for mut left in hosting.into_iter().filter(|m| Some(m.link()) != new) {
		left.body.0.remove_nodes(&[NodeLink::Issue(link.clone())]);
		write(left, mode).await?;
	}
	let Some(mut into) = into else { return Ok(()) };
	match mode == SyncMode::Offline || link.project() != repo {
		true => {
			into.body.0.push_nodes(&[NodeLink::Issue(link.clone())]);
			write(into, mode).await
		}
		false => {
			crate::github::client::get()?.set_issue_milestone(repo, link.number(), Some(into.number())).await?;
			Pull::default().milestone(&mut into, MergeMode::Normal).await
		}
	}
}

/// What is said on `link`, as stored: its author, then its comments, oldest first.
pub async fn thread(link: &IssueLink) -> Result<Thread> {
	let issue = Issue::load(LocalIssueSource::<FsReader>::build(LocalPath::new(stored_index(link)?)).await?).await?;
	let meta = issue.identity.as_linked().expect("stored github issues are linked");
	Ok(Thread {
		author: meta.user.clone(),
		comments: issue
			.contents
			.comments
			.iter()
			.skip(1)
			.enumerate()
			.map(|(i, c)| Said {
				id: c.id(),
				by: c.user().map(str::to_string),
				at: meta.timestamps.comments.get(i).copied(),
				body: c.body.to_string(),
			})
			.collect(),
	})
}

pub async fn comment(link: &IssueLink, edit: CommentEdit, mode: SyncMode) -> Result<()> {
	let source = LocalIssueSource::<FsReader>::build(LocalPath::new(stored_index(link)?)).await?;
	modify_and_sync_issue(Issue::load(source).await?, mode, Modifier::Comment(edit), SyncOptions::default()).await?;
	Ok(())
}

/// `reviewer`'s review of the mirrored pull request `link` asked for, or the ask withdrawn, straight on GitHub.
pub async fn request_review(link: &IssueLink, reviewer: &str, on: bool) -> Result<()> {
	let repo = link.project();
	if !Local::stored_prs(repo)?.contains_key(&link.number()) {
		bail!("{link} is no pull request mirrored here");
	}
	crate::github::client::get()?.request_review(repo, link.number(), reviewer, on).await?;
	let _store = lock_store().await?;
	Local::update_prs(repo, |prs| {
		let pr = prs.get_mut(&link.number()).expect("found above, and a pull only adds");
		pr.requested.retain(|r| r != reviewer);
		if on {
			pr.requested.push(reviewer.to_string());
		}
	})?;
	Ok(())
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

/// A board being read.
pub struct Reading<'a> {
	workflows: &'a Workflows,
	steps: Vec<Step>,
	at: usize,
	ready: VecDeque<Part>,
	held: HashMap<RepoInfo, States>,
	metas: HashMap<RepoInfo, Arc<ProjectMeta>>,
	tasks: BTreeMap<String, Task>,
	blocks: Vec<(String, String)>,
	repos: HashSet<RepoInfo>,
}
impl Reading<'_> {
	/// Reads on; `None` once whole. Dropped midway, the next call reads the same again.
	pub async fn next(&mut self) -> Result<Option<Part>> {
		while self.ready.is_empty() && self.at < self.steps.len() {
			self.step().await?;
		}
		Ok(self.ready.pop_front())
	}

	async fn step(&mut self) -> Result<()> {
		match self.steps[self.at].clone() {
			Step::Issue { path, bytes } => {
				let index = Local::extract_index_from_path(&path)?;
				let meta = self
					.metas
					.entry(index.repo_info())
					.or_insert_with(|| Arc::new(Local::load_project_meta(index.repo_info())))
					.clone();
				let loaded = match LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await {
					Ok(source) => Issue::load(LocalIssueSource { meta: Some(meta), ..source }).await.map(|i| (i, false)),
					Err(LocalError::ConflictBlocked(_)) => Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::new(index))?).await.map(|i| (i, true)),
					Err(e) => Err(e),
				};
				let (mut cards, mut unreadable) = (Vec::new(), Vec::new());
				match loaded {
					Ok((issue, conflicted)) => {
						let workflows = self.workflows;
						card_tree(&issue, None, conflicted, self.states(index.repo_info())?, workflows, &mut cards)?
					}
					Err(e) => unreadable.push((path, e.to_string())),
				}
				self.read(bytes, cards, unreadable);
			}
			Step::Prs { repo, bytes } => {
				let workflows = self.workflows;
				let states = self.states(repo)?;
				let mut cards = Local::stored_prs(repo)?
					.into_values()
					.map(|pr| pr_card(repo, pr, states, workflows))
					.collect::<Result<Vec<_>>>()?;
				let (n, mut left) = (cards.len() as u64, bytes);
				while !cards.is_empty() {
					let rest = cards.split_off(PRS_A_PART.min(cards.len()));
					let share = if rest.is_empty() { left } else { (bytes * cards.len() as u64).div_euclid(n) };
					left -= share;
					self.read(share, std::mem::replace(&mut cards, rest), Vec::new());
				}
				if n == 0 {
					self.read(bytes, Vec::new(), Vec::new());
				}
			}
			Step::Rest { whole } => self.ready.push_back(self.rest(whole)?),
		}
		self.at += 1;
		Ok(())
	}

	fn states(&mut self, repo: RepoInfo) -> Result<&States> {
		Ok(match self.held.entry(repo) {
			Entry::Occupied(e) => e.into_mut(),
			Entry::Vacant(e) => e.insert(Local::load_states(repo)?),
		})
	}

	fn read(&mut self, bytes: u64, cards: Vec<Card>, unreadable: Vec<(PathBuf, String)>) {
		for c in &cards {
			let task = Task {
				importance: c.importance.unwrap_or(DEFAULT_IMPORTANCE),
				open: c.open(),
			};
			self.tasks.insert(c.link.to_string(), task);
			self.blocks.extend(c.blocked_by.iter().map(|b| (b.to_string(), c.link.to_string())));
			self.repos.insert(c.link.project());
		}
		self.ready.push_back(Part::Read { bytes, cards, unreadable });
	}

	fn rest(&self, whole: bool) -> Result<Part> {
		let (mut colors, mut assignable) = (BTreeMap::new(), BTreeMap::new());
		for &repo in &self.repos {
			let meta = Local::load_project_meta(repo);
			colors.insert(repo.to_string(), meta.label_colors);
			assignable.insert(repo.to_string(), meta.assignable);
		}
		let targets = Local::stored_milestones()?
			.into_iter()
			.filter(offered)
			.map(|m| Target {
				hosted: m.body.0.issue_links(),
				contains: m.body.0.milestone_links(),
				link: m.identity.link,
				title: m.identity.title,
				due_on: m.identity.due_on,
			})
			.collect();
		Ok(Part::Rest {
			whole,
			ranks: rank(&self.tasks, &self.blocks).ranks,
			targets,
			colors,
			assignable,
		})
	}
}

#[derive(Clone)]
enum Step {
	Issue { path: PathBuf, bytes: u64 },
	Prs { repo: RepoInfo, bytes: u64 },
	Rest { whole: bool },
}

/// A milestone the board shows: open, and no sprint's.
fn offered(m: &Milestone) -> bool {
	!m.is_closed() && Kind::from((m.link(), m.identity.title.as_str())) != Kind::Sprint
}

async fn write(milestone: Milestone, mode: SyncMode) -> Result<()> {
	let stored = Local::load_milestone(milestone.link(), &FsReader)?.expect("edited from the stored one");
	modify_and_sync_milestone(stored, mode, MilestoneModifier::BodyWrite { body: milestone.body }, SyncOptions::default()).await?;
	Ok(())
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
		updated: {
			let t = &meta.timestamps;
			[t.title, t.description, t.labels, t.state, t.assignees]
				.into_iter()
				.flatten()
				.chain(t.comments.iter().copied())
				.max()
		},
		created: meta.created,
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
		updated: Some(pr.updated_at),
		created: pr.created_at,
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
			requested: pr.requested,
			reviews: pr.reviews,
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

pub(crate) fn stored_index(link: &IssueLink) -> Result<IssueIndex> {
	let path = Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("{link} is not stored here: pull it first"))?;
	Local::extract_index_from_path(&path)
}
