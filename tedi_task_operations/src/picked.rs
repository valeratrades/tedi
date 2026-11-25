//! `selected.md`: what the owner of a sprint series has picked up, a text over the kanban the home holds. A save is the
//! diff of its sections against the answer it was rendered from, sent as moves; nothing else of it is stored there.

use std::collections::{BTreeSet, HashMap, HashSet};

use color_eyre::eyre::{Result, WrapErr as _, bail};
use tedi_adapters::home::{Home, HomeError};
use tedi_core::{NodeLink, Sprint, board::Answer, conflict::Edited, workflow::State};

use crate::{
	IssueLink, MilestoneLink, RepoInfo, TaskView,
	local::{FsReader, Local, LocalFs, Picked, Placements, lock_store},
	open_interactions::{SyncMode, fetch_and_store_missing},
	selection::{link_local, lock_sprint, urgent_path},
	sink::Sink,
	sprints::{Expanded, expand_and_refresh, materialize_new_tasks, pull_stored},
};

/// The bucket that reads as picked up, and who holds it.
pub struct Picking {
	pub home: Home,
	pub workflow: String,
	pub bucket: State,
}
impl Picking {
	/// Where the home has the open issues of `repo`'s owner.
	async fn ask(&self, repo: RepoInfo) -> Result<Placements, HomeError> {
		let owner = repo.owner().expect("milestones are github-only");
		let Answer { login, mut trees, hits } = self.home.query(&format!("assignee:{owner} is:issue is:open")).await?;
		self.known_as(repo, login)?;
		let refuse = |why: String| HomeError::Refused(format!("`milestones.selected` is {}/{}: {why}", self.workflow, self.bucket));
		let tree = trees.remove(&self.workflow).ok_or_else(|| refuse("the home holds no such workflow".into()))?;
		tree.leaf(&self.bucket).map_err(|e| refuse(e.to_string()))?;
		Ok(Placements {
			placed: hits
				.into_iter()
				.map(|mut hit| (hit.link, hit.states.remove(&self.workflow).expect("a card sits in each of the trees")))
				.collect(),
			workflow: self.workflow.clone(),
			tree,
			bucket: self.bucket.clone(),
		})
	}

	fn known_as(&self, repo: RepoInfo, login: Option<String>) -> Result<(), HomeError> {
		let owner = repo.owner().expect("milestones are github-only");
		match login {
			Some(login) if login.eq_ignore_ascii_case(owner) => Ok(()),
			Some(login) => Err(HomeError::Refused(format!("the home knows you as {login}, and these sprints are {owner}'s"))),
			None => Err(HomeError::Refused("the home doesn't know who you are on GitHub: bring your account there a key".into())),
		}
	}

	//#[codestyle::skip(swappable-args)]
	/// Every move `picked` differs from its answer by, made; then rendered from a new answer, what is no issue yet kept.
	/// A refused one is told, and its line goes where the home has it. `Err`: the home is away, and what it didn't take stays a difference.
	async fn settle(&self, mut picked: Picked, raw: &str) -> Result<Picked> {
		let old = picked.clone();
		let moves = moves(&picked, &musts(picked.repo)?)?;
		if !moves.is_empty() {
			self.known_as(picked.repo, self.home.query("is:issue is:pr").await?.login)?; // nothing is both: asked for who asks
		}
		let Sent { landed, refused, away } = self.send(moves).await;
		let mut touched = Vec::new();
		for (link, to) in landed {
			picked.answer.placed.retain(|(l, _)| *l != link);
			picked.answer.placed.push((link.clone(), to));
			touched.push(link);
		}
		if let Some(e) = away {
			store(&mut picked, Some(&old)).await?;
			return Err(e.into());
		}
		if !refused.is_empty() {
			crate::utils::persist_rejected_changes(&[raw.to_string()])?;
			tell(&refused);
			eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
		}
		let (answer, raised) = match self.fetch(picked.repo).await {
			Ok(fetched) => fetched,
			Err(e) => {
				store(&mut picked, Some(&old)).await?;
				return Err(e);
			}
		};
		touched.extend(raised);
		let mut text = picked.doc;
		text.remove_issues(); // what is no issue yet
		let mut settled = Picked {
			repo: picked.repo,
			doc: answer.document(),
			answer,
		};
		settled.doc.append(text);
		store(&mut settled, Some(&old)).await?;
		pull_stored(&touched).await?; // hooks edit what they are entered with
		Ok(settled)
	}

	/// The home's answer once every must sits at the bucket or ahead of it, and those moved for that.
	async fn fetch(&self, repo: RepoInfo) -> Result<(Placements, Vec<IssueLink>)> {
		let answer = self.ask(repo).await?;
		let musts = musts(repo)?;
		let elsewhere: BTreeSet<String> = musts.iter().filter(|l| answer.at(l).is_none()).map(|l| l.project().to_string()).collect();
		let mut unassigned = HashMap::new();
		if !elsewhere.is_empty() {
			let of = elsewhere.into_iter().collect::<Vec<_>>().join(",");
			for mut hit in self.home.query(&format!("is:issue is:open repo:{of}")).await?.hits {
				let at = hit.states.remove(&self.workflow).expect("a card sits in each of the trees");
				unassigned.insert(hit.link, at);
			}
		}
		let moves = musts
			.into_iter()
			.map(|link| Move {
				seen: answer.at(&link).cloned().or_else(|| unassigned.remove(&link)),
				to: self.bucket.clone(),
				workflow: self.workflow.clone(),
				link,
			})
			.filter(|m| m.seen.as_ref().is_none_or(|seen| answer.tree.order(seen) < answer.tree.order(&self.bucket)))
			.collect();
		let Sent { landed, refused, away } = self.send(moves).await;
		tell(&refused);
		match away {
			Some(e) => Err(e.into()),
			None if landed.is_empty() => Ok((answer, Vec::new())),
			None => Ok((self.ask(repo).await?, landed.into_iter().map(|(link, _)| link).collect())),
		}
	}

	async fn send(&self, moves: Vec<Move>) -> Sent {
		let mut sent = Sent::default();
		for Move { link, workflow, seen, to } in moves {
			let answer = match &link {
				IssueLink::Owned(_) => self.home.move_card(&link, &workflow, &to, seen.as_ref()).await,
				IssueLink::Virtual(_) => Err(HomeError::Refused("a local-only issue sits in no workflow".into())),
			};
			match answer {
				Ok(Edited::Landed) => {
					println!("{link} → {to}");
					sent.landed.push((link, to));
				}
				Ok(Edited::Rejected(why)) => sent.refused.push((link, why)),
				Ok(Edited::Conflict(conflict)) => sent.refused.push((link, conflict.to_string())),
				Err(e) if e.is_away() => {
					sent.away = Some(e);
					break;
				}
				Err(e) => sent.refused.push((link, e.to_string())),
			}
		}
		sent
	}

	/// What is stored of `repo`'s, settled; never fetched, the home's answer.
	async fn current(&self, repo: RepoInfo) -> Result<Picked> {
		match Local::load_picked(repo)? {
			Some(stored) => {
				let raw = stored.doc.serialize();
				self.settle(stored, &raw).await
			}
			None => {
				let (answer, raised) = self.fetch(repo).await?;
				let mut fresh = Picked {
					repo,
					doc: answer.document(),
					answer,
				};
				store(&mut fresh, None).await?;
				pull_stored(&raised).await?;
				Ok(fresh)
			}
		}
	}

	/// `selected.md` beside `repo`'s sprints brought to where the home is, its started issues stored.
	pub async fn refresh(&self, repo: RepoInfo) -> Result<()> {
		let _lock = lock_sprint("selected")?;
		let picked = self.current(repo).await?;
		let started: Vec<IssueLink> = picked.links(true)?.into_iter().filter_map(|n| if let NodeLink::Issue(l) = n { Some(l) } else { None }).collect();
		fetch_and_store_missing(&started).await;
		Ok(())
	}
}

/// `selected.md` open in the editor.
pub(crate) struct PickedTab {
	pub(crate) expanded: Expanded,
	/// As stored when it opened.
	opened: Picked,
	/// Whose repo a task typed into it lands in.
	ambient: MilestoneLink,
	_lock: std::fs::File,
}
impl PickedTab {
	/// The one beside `sprint`.
	pub(crate) async fn render(picking: &Picking, sprint: &MilestoneLink, mode: SyncMode) -> Result<Self> {
		let lock = lock_sprint("selected")?;
		let repo = sprint.repo_info();
		let opened = match (mode, Local::load_picked(repo)?) {
			(SyncMode::Offline, None) => bail!("never fetched while online"),
			(SyncMode::OnExit | SyncMode::Offline, Some(stored)) => stored,
			(SyncMode::Full | SyncMode::OnExit, None) => picking.current(repo).await?,
			(SyncMode::Full, Some(_)) => match picking.current(repo).await {
				Ok(settled) => settled,
				Err(e) => {
					eprintln!("warning: selected.md opens as last fetched: {e:#}");
					Local::load_picked(repo)?.expect("loaded above")
				}
			},
		};
		Ok(Self {
			expanded: expand_and_refresh(opened.doc.clone(), mode == SyncMode::Full).await?,
			opened,
			ambient: sprint.clone(),
			_lock: lock,
		})
	}

	pub(crate) fn repo(&self) -> RepoInfo {
		self.opened.repo
	}

	pub(crate) async fn land(self, picking: &Picking, raw: &str, mut doc: TaskView, moved: &HashMap<IssueLink, IssueLink>, mode: SyncMode) -> Result<()> {
		let mut picked = self.opened.clone();
		let untouched = raw == self.expanded.text && (mode == SyncMode::Offline || doc.homeless_tasks().is_empty());
		match untouched {
			true => picked.doc.relink(moved),
			false => {
				let milestones = doc.milestone_links();
				if !milestones.is_empty() {
					let named = milestones.iter().map(|l| l.as_str()).collect::<Vec<_>>().join(", ");
					bail!("selected.md lists issues, and a milestone sits in no workflow: {named}");
				}
				for (header, _) in doc.sections() {
					picked.answer.leaf(header).wrap_err("selected.md is not saved")?;
				}
				materialize_new_tasks(&mut doc, Some(self.ambient), mode).await?;
				doc.collapse_to_links();
				picked.doc = doc;
			}
		}
		picked.answer.placed.retain(|(link, _)| !moved.contains_key(link)); // placed again, at its new link
		let musts = musts(picked.repo)?;
		let sent = moves(&picked, &musts)?; // what can't be read as moves is not saved
		let (listed, started) = (picked.doc.issue_links(), picked.links(true)?);
		let (tree, bucket) = (&picked.answer.tree, &picked.answer.bucket);
		for (link, at) in picked.answer.placed.iter().filter(|(l, _)| !sent.iter().any(|m| m.link == *l)) {
			if *at == tree.intake && !listed.contains(link) {
				println!("{link} sits in {at} as it is: taking it out of selected.md changes nothing");
			}
			if at == bucket && musts.contains(link) && !started.contains(&NodeLink::Issue(link.clone())) {
				println!("{link} stays in {at}: today's sprint or urgent holds it");
			}
		}
		let raised = musts.iter().all(|l| started.contains(&NodeLink::Issue(l.clone())));
		store(&mut picked, Some(&self.opened)).await?;
		if mode == SyncMode::Offline || (mode == SyncMode::Full && untouched && raised && sent.is_empty()) {
			return Ok(());
		}
		match picking.settle(picked, raw).await {
			Ok(_) => Ok(()),
			Err(e) if e.downcast_ref::<HomeError>().is_some() => {
				eprintln!("warning: selected.md is saved here, its moves to be sent with the next online edit: {e:#}");
				Ok(())
			}
			Err(e) => Err(e),
		}
	}
}

struct Move {
	link: IssueLink,
	workflow: String,
	/// `None`: wherever it sits.
	seen: Option<State>,
	to: State,
}

#[derive(Default)]
struct Sent {
	landed: Vec<(IssueLink, State)>,
	refused: Vec<(IssueLink, String)>,
	/// Why the rest was not sent.
	away: Option<HomeError>,
}

fn tell(refused: &[(IssueLink, String)]) {
	for (link, why) in refused {
		eprintln!("warning: {link} stays where the home has it: {why}");
	}
}

/// The open issues `repo`'s sprint of the day and urgent hold: at least picked up.
fn musts(repo: RepoInfo) -> Result<Vec<IssueLink>> {
	let mut out = match Local::stored_sprint(repo, Sprint::DAY) {
		Some(day) => Local::load_milestone(&day, &FsReader)?.expect("listed as stored").body.0.issue_links(),
		None => Vec::new(),
	};
	match std::fs::read_to_string(urgent_path()) {
		Ok(raw) => {
			let mut urgent = TaskView::parse(&raw);
			urgent.resolve_bare_refs(None);
			out.extend(urgent.issue_links());
		}
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
		Err(e) => return Err(e.into()),
	}
	let mut seen = HashSet::new();
	out.retain(|l| matches!(l, IssueLink::Owned(_)) && seen.insert(l.clone()) && link_local(l).is_none_or(|vi| !vi.contents.state.is_closed()));
	Ok(out)
}

/// What `picked`'s document says otherwise than its answer; a must goes no further back than the bucket.
fn moves(picked: &Picked, musts: &[IssueLink]) -> Result<Vec<Move>> {
	let Placements { tree, bucket, workflow, .. } = &picked.answer;
	let mut out = Vec::new();
	let mut push = |link: &IssueLink, seen: Option<&State>, to: &State| {
		let to = match seen != Some(to) && musts.contains(link) && tree.order(to) < tree.order(bucket) {
			true => bucket,
			false => to,
		};
		if seen != Some(to) {
			out.push(Move {
				link: link.clone(),
				workflow: workflow.clone(),
				seen: seen.cloned(),
				to: to.clone(),
			});
		}
	};
	let mut listed = HashSet::new();
	for (header, nodes) in picked.doc.sections() {
		let to = picked.answer.leaf(header)?;
		for node in nodes {
			let NodeLink::Issue(link) = node else { bail!("selected.md lists issues, not {node}") };
			if !listed.insert(link.clone()) {
				bail!("{link} is listed twice in selected.md");
			}
			push(&link, picked.answer.at(&link), &to);
		}
	}
	for (link, seen) in picked.answer.placed.iter().filter(|(l, _)| !listed.contains(l)) {
		if link_local(link).is_some_and(|vi| vi.contents.state.is_closed()) {
			continue; // closing is the home's own rule
		}
		push(link, Some(seen), &tree.intake);
	}
	Ok(out)
}

async fn store(picked: &mut Picked, old: Option<&Picked>) -> Result<()> {
	let _store = lock_store().await?;
	<Picked as Sink<LocalFs>>::sink(picked, old).await?;
	Ok(())
}
