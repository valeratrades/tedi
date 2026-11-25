//! The board as the store holds it: every stored issue and mirrored pull request carded (the model is `tedi_core::board`).

use std::{
	collections::{BTreeMap, HashMap, hash_map::Entry},
	path::PathBuf,
	sync::{Arc, Mutex},
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

/// Issue trees, and pull requests, are read and shown so many at a time.
const TREES_A_UNIT: usize = 4;
const PRS_A_PART: usize = 25;

/// Every issue in the store within `reach`, one card per node, and every pull request, ranked over their
/// `blocked by` links; each placed in every one of `trees`.
pub async fn load(reach: &Reach, trees: &Trees) -> Result<Board> {
	let reading = Reading::begin(trees)?;
	let mut board = Board::default();
	board.extend([reading.head(reach)]);
	for (unit, _) in reading.units(reach) {
		board.extend(Box::pin(reading.read(unit)).await?); // a command polls this whole future off main's stack
	}
	board.extend([reading.ranks(reach)]);
	Ok(board)
}

pub fn listing(board: &Board, query: String, group: &GroupBy, trees: &Trees, me: Option<&str>) -> Result<Listing> {
	let mut hits = query.parse::<Query>()?.run(&board.cards, me)?;
	let all = hits.len();
	hits.retain(|&i| board.cards[i].pr.is_none());
	let groups = tedi_core::board::group(&board.cards, &hits, group, trees)?
		.into_iter()
		.filter(|(_, members)| !members.is_empty())
		.map(|(key, members)| {
			let links = members.into_iter().map(|p| board.cards[p.card].link.clone()).collect();
			((*group != GroupBy::Nothing).then_some(key), links)
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

/// The store as a board is read from it, a unit at a time: those of open issues first, which the ranks then come of.
pub struct Reading<'a> {
	units: Vec<Unit>,
	milestones: Vec<Milestone>,
	trees: &'a Trees,
	metas: HashMap<RepoInfo, Arc<ProjectMeta>>,
	states: HashMap<RepoInfo, States>,
	/// By the link of an open issue read.
	open: Mutex<HashMap<String, Open>>,
}
impl<'a> Reading<'a> {
	/// All of it: what a caller gets of it is what their reach asks for.
	pub fn begin(trees: &'a Trees) -> Result<Self> {
		struct Tree {
			roots: Vec<PathBuf>,
			bytes: u64,
			open: bool,
		}
		let mut stored: BTreeMap<(String, IssueSelector), (RepoInfo, Tree)> = BTreeMap::new();
		for rel in Local::issue_file_relpaths()? {
			let path = Local::issues_dir().join(&rel);
			let index = Local::extract_index_from_path(&path)?;
			let Some(&root) = index.index().first().filter(|_| !index.repo_info().is_virtual()) else {
				continue; // virtual issues have no board
			};
			let repo = index.repo_info();
			let none = Tree {
				roots: Vec::new(),
				bytes: 0,
				open: false,
			};
			let (_, tree) = stored.entry((repo.to_string(), root)).or_insert((repo, none));
			tree.bytes += std::fs::metadata(&path)?.len();
			tree.open |= path.extension().is_some_and(|e| e == "md"); // closed, it is `.md.bak`
			if index.index().len() == 1 {
				tree.roots.push(path); // a child is carded with its root's tree
			}
		}
		let mut units: Vec<Unit> = Vec::new();
		for ((name, _), (repo, tree)) in stored {
			let source = Source {
				repo: name,
				holds: if tree.open { Holds::OpenIssues } else { Holds::ClosedIssues },
			};
			match units.iter_mut().rfind(|u| u.source == source && u.roots.len() < TREES_A_UNIT) {
				Some(unit) => {
					unit.bytes += tree.bytes;
					unit.roots.extend(tree.roots);
				}
				None => units.push(Unit {
					source,
					repo,
					bytes: tree.bytes,
					roots: tree.roots,
				}),
			}
		}
		units.extend(Local::pr_repos()?.into_iter().map(|(repo, bytes)| Unit {
			source: Source {
				repo: repo.to_string(),
				holds: Holds::PullRequests,
			},
			repo,
			bytes,
			roots: Vec::new(),
		}));
		units.sort_by(|a, b| (a.source.holds, &a.source.repo).cmp(&(b.source.holds, &b.source.repo)));

		let (mut metas, mut states) = (HashMap::new(), HashMap::new());
		for unit in &units {
			if let Entry::Vacant(e) = states.entry(unit.repo) {
				e.insert(Local::load_states(unit.repo)?);
				metas.insert(unit.repo, Arc::new(Local::load_project_meta(unit.repo)));
			}
		}
		Ok(Self {
			units,
			milestones: Local::stored_milestones()?.into_iter().filter(|m| !m.is_closed()).collect(),
			trees,
			metas,
			states,
			open: Mutex::new(HashMap::new()),
		})
	}

	pub fn head(&self, reach: &Reach) -> Part {
		let mut sizes: BTreeMap<Source, u64> = BTreeMap::new();
		for unit in self.units.iter().filter(|u| reach.covers(u.repo)) {
			*sizes.entry(unit.source.clone()).or_default() += unit.bytes;
		}
		let targets = self
			.milestones
			.iter()
			.filter(|m| reach.covers(m.link().repo_info()))
			.map(|m| Target {
				nodes: m.body.0.nodes(),
				description: m.body.0.description(),
				link: m.identity.link.clone(),
				title: m.identity.title.clone(),
				due_on: m.identity.due_on,
			})
			.collect();
		let metas = || self.metas.iter().filter(|(repo, _)| reach.covers(**repo));
		Part::Head {
			sizes: sizes.into_iter().collect(),
			targets,
			colors: metas().map(|(repo, m)| (repo.to_string(), m.label_colors.clone())).collect(),
			assignable: metas().map(|(repo, m)| (repo.to_string(), m.assignable.clone())).collect(),
		}
	}

	/// Each by its place, those of open issues first.
	pub fn units(&self, reach: &Reach) -> impl Iterator<Item = (usize, &Source)> {
		self.units.iter().enumerate().filter(|(_, u)| reach.covers(u.repo)).map(|(i, u)| (i, &u.source))
	}

	/// What the unit at `i` holds; the same however often it is read.
	pub async fn read(&self, i: usize) -> Result<Vec<Part>> {
		let unit = &self.units[i];
		let states = &self.states[&unit.repo];
		let part = |bytes, cards, unreadable| Part::Read {
			source: unit.source.clone(),
			bytes,
			cards,
			unreadable,
		};
		if unit.source.holds == Holds::PullRequests {
			let mut cards = Local::stored_prs(unit.repo)?
				.into_values()
				.map(|pr| pr_card(unit.repo, pr, states, self.trees))
				.collect::<Result<Vec<_>>>()?;
			for card in &mut cards {
				let link = card.link.to_string();
				card.rank = rank(&[(link.clone(), task(card))].into(), &[]).ranks.remove(&link); // it blocks and is blocked by nothing
			}
			let (n, mut left) = (cards.len() as u64, unit.bytes);
			let mut parts = Vec::new();
			while !cards.is_empty() {
				let rest = cards.split_off(PRS_A_PART.min(cards.len()));
				let share = if rest.is_empty() { left } else { (unit.bytes * cards.len() as u64).div_euclid(n) };
				left -= share;
				parts.push(part(share, std::mem::replace(&mut cards, rest), Vec::new()));
			}
			if parts.is_empty() {
				parts.push(part(unit.bytes, Vec::new(), Vec::new()));
			}
			return Ok(parts);
		}
		let (mut cards, mut unreadable) = (Vec::new(), Vec::new());
		for path in &unit.roots {
			let index = Local::extract_index_from_path(path)?;
			let loaded = match LocalIssueSource::<FsReader>::build(LocalPath::new(index)).await {
				Ok(source) => {
					let meta = Some(self.metas[&unit.repo].clone());
					Issue::load(LocalIssueSource { meta, ..source }).await.map(|i| (i, false))
				}
				Err(LocalError::ConflictBlocked(_)) => Issue::load(LocalIssueSource::<GitReader>::build(LocalPath::new(index))?).await.map(|i| (i, true)),
				Err(e) => Err(e),
			};
			match loaded {
				Ok((issue, conflicted)) => card_tree(&issue, None, conflicted, states, self.trees, &mut cards)?,
				Err(e) => unreadable.push((path.clone(), e.to_string())),
			}
		}
		let mut open = self.open.lock().expect("nothing panics holding it");
		open.extend(cards.iter().map(|c| {
			let held = Open {
				repo: unit.repo,
				task: task(c),
				blocked_by: c.blocked_by.iter().map(ToString::to_string).collect(),
			};
			(c.link.to_string(), held)
		}));
		Ok(vec![part(unit.bytes, cards, unreadable)])
	}

	/// Of the issues read so far.
	pub fn ranks(&self, reach: &Reach) -> Part {
		let open = self.open.lock().expect("nothing panics holding it");
		let (mut tasks, mut blocks) = (BTreeMap::new(), Vec::new());
		for (link, held) in open.iter().filter(|(_, held)| reach.covers(held.repo)) {
			tasks.insert(link.clone(), held.task);
			blocks.extend(held.blocked_by.iter().map(|b| (b.clone(), link.clone())));
		}
		Part::Ranks(rank(&tasks, &blocks).ranks)
	}
}

struct Open {
	repo: RepoInfo,
	task: Task,
	blocked_by: Vec<String>,
}

struct Unit {
	source: Source,
	repo: RepoInfo,
	bytes: u64,
	/// The files of the trees' roots; none: the repo's pull requests.
	roots: Vec<PathBuf>,
}

fn task(card: &Card) -> Task {
	Task {
		importance: card.importance.unwrap_or(DEFAULT_IMPORTANCE),
		open: card.open(),
	}
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
