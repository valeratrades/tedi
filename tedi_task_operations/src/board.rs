//! The board as the store holds it: every stored issue and mirrored pull request carded (the model is `tedi_core::board`).

use std::{
	collections::{BTreeMap, HashMap, HashSet, VecDeque, hash_map::Entry},
	path::PathBuf,
	sync::Arc,
};

use color_eyre::eyre::{Result, bail, eyre};
pub use tedi_core::board::*;
use tedi_core::{
	PrState, PullRequest, Sprint,
	conflict::CommentEdit,
	workflow::{State, Trees},
};

use crate::{
	CloseState, Issue, IssueIndex, IssueLink, IssueSelector, LazyIssue, Milestone, MilestoneLink, RepoInfo, TaskView,
	conflict_resolve::{FieldEdit, check_for_existing_conflict},
	labels::Scheme,
	local::{FsReader, GitReader, Local, LocalError, LocalIssueSource, LocalPath, ProjectMeta, States, lock_store},
	open_interactions::{BodyWrite, MergeMode, MilestoneModifier, Modifier, SyncMode, SyncOptions, modify_and_sync_issue, modify_and_sync_milestone, pull_milestone},
	pull::pull_milestones,
	rank::{Task, rank},
	reach::{Reach, Reached},
	workflow::Rejected,
};

/// What an issue without `i:` counts as (`labels.nix`: "`i:3` is the default").
const DEFAULT_IMPORTANCE: u8 = 3;

/// Pull requests are read a repo at a time, and shown so many.
const PRS_A_PART: usize = 25;

/// Every issue in the store within `reach`, one card per node, and every pull request, ranked over their
/// `blocked by` links; each placed in every one of `trees`.
pub fn load(reach: Reach, trees: &Trees) -> Result<Reading<'_>> {
	let mut roots = Vec::new();
	let mut sizes: HashMap<(RepoInfo, IssueSelector), u64> = HashMap::new();
	for rel in Local::issue_file_relpaths()? {
		let path = Local::issues_dir().join(&rel);
		let index = Local::extract_index_from_path(&path)?;
		let Some(&root) = index.index().first().filter(|_| !index.repo_info().is_virtual() && reach.covers(index.repo_info())) else {
			continue; // virtual issues have no board, and what is out of reach isn't on this one
		};
		*sizes.entry((index.repo_info(), root)).or_default() += std::fs::metadata(&path)?.len();
		if index.index().len() == 1 {
			roots.push((path, index.repo_info(), root)); // a child is carded with its root's tree
		}
	}
	let mut steps: Vec<Step> = roots
		.into_iter()
		.map(|(path, repo, root)| Step::Issue {
			bytes: sizes.remove(&(repo, root)).unwrap_or(0), // the same root's second file (`.md`, `.md.bak`): counted with the first
			path,
		})
		.collect();
	steps.push(Step::Rest { whole: false });
	steps.extend(
		Local::pr_repos()?
			.into_iter()
			.filter(|(repo, _)| reach.covers(*repo))
			.map(|(repo, bytes)| Step::Prs { repo, bytes }),
	);
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
		reach,
		trees,
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

/// `named` is one of `views`, or a query; `group` stands over the view's own grouping.
pub fn listing(board: &Board, named: &str, group: Option<GroupBy>, views: &[View], trees: &Trees, me: Option<&str>) -> Result<Listing> {
	let (query, group) = match views.iter().find(|v| v.name == named) {
		Some(view) => (view.query.clone(), group.unwrap_or_else(|| view.group_by.clone())),
		None => (named.to_string(), group.unwrap_or(GroupBy::Nothing)),
	};
	let mut hits = query.parse::<Query>()?.run(&board.cards, me)?;
	let all = hits.len();
	hits.retain(|&i| board.cards[i].pr.is_none());
	let groups = tedi_core::board::group(&board.cards, &hits, &group, trees)?
		.into_iter()
		.filter(|(_, members)| !members.is_empty())
		.map(|(key, members)| {
			let links = members.into_iter().map(|p| board.cards[p.card].link.clone()).collect();
			((group != GroupBy::Nothing).then_some(key), links)
		})
		.collect();
	Ok(Listing {
		query,
		groups,
		pull_requests: all - hits.len(),
	})
}

/// Which milestone of `repo` hosts `link`, from `seen` to `new`: an issue sits in at most one a repo.
pub async fn set_milestone(link: &Reached<IssueLink>, repo: &Reached<RepoInfo>, seen: Option<&MilestoneLink>, new: Option<&MilestoneLink>, mode: SyncMode) -> Result<()> {
	let (link, repo) = (&**link, **repo);
	if let Some(new) = new
		&& new.repo_info() != repo
	{
		bail!("{} is no milestone of {repo}", new.as_str());
	}
	stored_index(link)?;
	if mode != SyncMode::Offline {
		pull_milestones(repo).await?; // whatever of the repo hosts it by now
	}
	let (hosting, rest): (Vec<Milestone>, Vec<Milestone>) = Local::stored_milestones()?
		.into_iter()
		.filter(|m| !m.is_closed() && !matches!(m.sprint(), Some(Sprint::Planning(_))) && m.identity.link.repo_info() == repo) // what is planned is its own file's to say
		.partition(|m| m.body.hosted().contains(link));
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
	let write = async |milestone: &Milestone, change: &dyn Fn(&mut TaskView)| {
		let mut body = milestone.body.clone();
		change(&mut body.0);
		let write = BodyWrite {
			rendered: milestone.body.clone(),
			body,
		};
		modify_and_sync_milestone(milestone.link(), mode, MilestoneModifier::BodyWrite(write), SyncOptions::default()).await
	};
	for left in hosting.iter().filter(|m| Some(m.link()) != new) {
		write(left, &|body| body.remove_issue_links(std::slice::from_ref(link))).await?;
	}
	let Some(into) = into else { return Ok(()) };
	match mode == SyncMode::Offline || link.project() != repo {
		true => write(&into, &|body| body.push_issue_links(std::slice::from_ref(link))).await?,
		false => {
			crate::github::client::get()?.set_issue_milestone(repo, link.number(), Some(into.number())).await?;
			pull_milestone(into.link(), MergeMode::Normal).await?
		}
	};
	Ok(())
}

/// What is said on `link`, as stored: its author, then its comments, oldest first.
pub async fn thread(link: &Reached<IssueLink>) -> Result<Thread> {
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

pub async fn comment(link: &Reached<IssueLink>, edit: CommentEdit, mode: SyncMode) -> Result<()> {
	let source = LocalIssueSource::<FsReader>::build(LocalPath::new(stored_index(link)?)).await?;
	modify_and_sync_issue(Issue::load(source).await?, mode, Modifier::Comment(edit), SyncOptions::default()).await?;
	Ok(())
}

/// `reviewer`'s review of the mirrored pull request `link` asked for, or the ask withdrawn, straight on GitHub.
pub async fn request_review(link: &Reached<IssueLink>, reviewer: &str, on: bool) -> Result<()> {
	let repo = link.project();
	if !Local::stored_prs(repo)?.contains_key(&link.number()) {
		bail!("{} is no pull request mirrored here", **link);
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
pub async fn edit(link: &Reached<IssueLink>, edits: Vec<FieldEdit>, mode: SyncMode) -> Result<()> {
	let source = LocalIssueSource::<FsReader>::build(LocalPath::new(stored_index(link)?)).await?;
	modify_and_sync_issue(Issue::load(source).await?, mode, Modifier::Field { edits }, SyncOptions::default()).await?;
	Ok(())
}

/// The clash file waiting on `link`'s line, once every file already freed of its markers has landed.
/// Writing a marker-free resolution there and asking again lands it.
pub async fn pending_conflict(link: &Reached<IssueLink>) -> Result<Option<PathBuf>> {
	check_for_existing_conflict(stored_index(link)?).await
}

/// A board being read.
pub struct Reading<'a> {
	reach: Reach,
	trees: &'a Trees,
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
						let trees = self.trees;
						card_tree(&issue, None, conflicted, self.states(index.repo_info())?, trees, &mut cards)?
					}
					Err(e) => unreadable.push((path, e.to_string())),
				}
				self.read(bytes, cards, unreadable);
			}
			Step::Prs { repo, bytes } => {
				let trees = self.trees;
				let states = self.states(repo)?;
				let mut cards = Local::stored_prs(repo)?.into_values().map(|pr| pr_card(repo, pr, states, trees)).collect::<Result<Vec<_>>>()?;
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
			.filter(|m| !m.is_closed() && self.reach.covers(m.link().repo_info()))
			.map(|m| Target {
				nodes: m.body.0.nodes(),
				description: m.body.0.description(),
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

fn card_tree(issue: &Issue, parent: Option<&IssueLink>, conflicted: bool, states: &States, trees: &Trees, out: &mut Vec<Card>) -> Result<()> {
	let link = issue.identity.as_linked().expect("stored github issues are linked").link().clone();
	out.push(card(issue, parent.cloned(), conflicted, states, trees)?);
	for child in issue.children.values() {
		card_tree(child, Some(&link), conflicted, states, trees, out)?;
	}
	Ok(())
}

/// `issue`'s own node, carded.
pub(crate) fn card(issue: &Issue, parent: Option<IssueLink>, conflicted: bool, states: &States, trees: &Trees) -> Result<Card> {
	let meta = issue.identity.as_linked().expect("stored github issues are linked");
	let scheme = Scheme::from(issue.contents.labels.as_slice());
	let link = meta.link().clone();
	Ok(Card {
		states: placed(states, link.number(), trees)?,
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

pub(crate) fn pr_card(repo: crate::RepoInfo, pr: PullRequest, states: &States, trees: &Trees) -> Result<Card> {
	let scheme = Scheme::from(pr.labels.as_slice());
	Ok(Card {
		link: IssueLink::in_project(repo, pr.number),
		states: placed(states, pr.number, trees)?,
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
fn placed(states: &States, number: u64, trees: &Trees) -> Result<BTreeMap<String, State>> {
	trees
		.iter()
		.map(|(name, tree)| {
			let at = match states.held.get(name).and_then(|issues| issues.get(&number)) {
				Some(held) => held.clone(),
				None => tree.intake.clone(),
			};
			if let Err(e) = tree.leaf(&at) {
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
