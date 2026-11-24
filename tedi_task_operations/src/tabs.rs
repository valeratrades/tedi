//! Task views opened in the editor together, a tab each, and folded back as one: the issues they expand
//! first, whichever tab each was edited in, then every tab's own document.

use std::collections::HashMap;

use color_eyre::eyre::{Report, Result, bail};
use jiff::{SignedDuration, Timestamp};
use tedi_core::Sprint;

use crate::{
	IssueLink, MilestoneBody, MilestoneLink, RepoInfo, TaskView,
	conflict_resolve::check_for_existing_milestone_conflict,
	local::{FsReader, Local},
	mv::{Editing, move_issues},
	open_interactions::{BodyWrite, MergeMode, MilestoneModifier, SyncMode, SyncOptions, modify_and_sync_milestone, pull_milestone},
	picked::{PickedTab, Picking},
	selection::{URGENT_KEY, closed_links, lock_sprint, urgent_path},
	sprints::{Expanded, NoSprint, expand_and_refresh, find_sprint, materialize_new_tasks, sync_embedded_issue_changes, sync_milestone_changes},
};

/// Edit `repo`'s `sprint`; a planning one with what is picked up and its org's Bottlenecks beside it, rolled
/// over once `grace` past its due date.
pub async fn edit_sprint(repo: RepoInfo, sprint: Sprint, picking: Option<&Picking>, mode: SyncMode, grace: SignedDuration) -> Result<MilestoneLink> {
	// GitHub unreachable → degrade the whole run to offline: every edit persists to LocalFs as a
	// divergence from Consensus, reconciled by the next online sync of it.
	let mode = match mode == SyncMode::Full && !crate::github::reachable().await {
		true => SyncMode::Offline,
		false => mode,
	};
	let find = async |sprint: Sprint| match (mode, Local::stored_sprint(repo, sprint)) {
		(SyncMode::OnExit, Some(stored)) => Ok(stored),
		_ => find_sprint(repo, sprint, mode).await,
	};
	let link = find(sprint).await?;

	let own = MilestoneTab::render(&link, sprint, mode).await?;
	let opened = own.opened.0.serialize();
	let mut tabs = vec![Tab::Milestone(Box::new(own))];
	if let Sprint::Planning(_) = sprint {
		let beside = |what: &str, e: Report| eprintln!("warning: {what} stays closed: {e:#}");
		if let Some(picking) = picking {
			match PickedTab::render(picking, &link, mode).await {
				Ok(tab) => tabs.push(Tab::Picked(Box::new(tab), picking)),
				Err(e) => beside("selected.md", e),
			}
		}
		let bottlenecks = async { MilestoneTab::render(&find(Sprint::Bottlenecks).await?, Sprint::Bottlenecks, mode).await };
		match bottlenecks.await {
			Ok(tab) => tabs.push(Tab::Milestone(Box::new(tab))),
			Err(e) if e.downcast_ref::<NoSprint>().is_some() => {} // nor is there one to open
			Err(e) => beside("Bottlenecks", e),
		}
	}
	let buffers: Vec<(String, &str)> = tabs.iter().map(|t| (t.name(), t.text())).collect();
	let raws = open(&buffers).await?;
	let _syncing = v_utils::io::Spinner::start(); // until the edit is synced

	let mode = match mode == SyncMode::OnExit && !crate::github::reachable().await {
		true => SyncMode::Offline,
		false => mode,
	};
	let editing: Vec<Editing> = tabs.iter().map(Tab::editing).collect();
	let landed = async {
		// Untouched too: an issue a killed run left unsynced has nothing else to carry it.
		let Folded { docs, moved } = fold_back(&raws, &editing, mode).await?;
		let mut failed = None;
		for ((tab, raw), doc) in tabs.into_iter().zip(&raws).zip(docs) {
			if let Err(e) = tab.land(raw, doc, &moved, mode).await {
				eprintln!("warning: a tab was not saved: {e:#}");
				failed = failed.or(Some(e));
			}
		}
		match failed {
			Some(e) => Err(e),
			None => Ok(()),
		}
	};
	if let Err(e) = landed.await {
		return reject(&raws, e);
	}
	println!("Updated milestone '{sprint}'");

	let milestone = Local::load_milestone(&link, &FsReader)?.expect("landed above");
	// Read off the synced due date: another machine may have bumped it already.
	if let Sprint::Planning(tf) = sprint
		&& mode != SyncMode::Offline
		&& milestone.identity.due_on.is_none_or(|due| due + grace < Timestamp::now())
		&& let Err(e) = roll_over(&link, tf.duration(), &milestone.to_string(), &opened).await
	{
		eprintln!("warning: '{sprint}' is past its due date and did not roll over (the next online edit retries): {e:#}");
	}
	Ok(link)
}

/// Edit the local urgent list (creating it if absent): the same cycle, stored at `urgent_path()` — no
/// GitHub milestone involved.
pub async fn edit_urgent(mode: SyncMode) -> Result<()> {
	let _lock = lock_sprint(URGENT_KEY)?;

	let path = urgent_path();
	let original = match std::fs::read_to_string(&path) {
		Ok(c) => c,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => String::new(),
		Err(e) => return Err(e.into()),
	};
	let expanded = expand_and_refresh(TaskView::parse(&original), mode == SyncMode::Full).await?.text;
	let raws = open(&[(URGENT_KEY.to_string(), &expanded)]).await?;
	let _syncing = v_utils::io::Spinner::start(); // until the edit is synced
	if raws[0] == expanded {
		sync_embedded_issue_changes(&raws, mode).await?; // an issue a killed run left unsynced has nothing else to carry it
		println!("No changes made to urgent sprint");
		return Ok(());
	}
	let landed = async {
		// urgent is local-only: milestone refs live on GitHub and have no meaning here
		let milestones = TaskView::parse(&raws[0]).milestone_links();
		if !milestones.is_empty() {
			let named = milestones.iter().map(|l| l.as_str()).collect::<Vec<_>>().join(", ");
			bail!("urgent sprint cannot contain milestone refs: {named}");
		}
		let mut doc = fold_back(&raws, &[Editing::Urgent], mode).await?.docs.remove(0);
		materialize_new_tasks(&mut doc, None, mode).await?;
		Ok(doc)
	};
	let mut doc = match landed.await {
		Ok(doc) => doc,
		Err(e) => return reject(&raws, e),
	};

	doc.collapse_to_links();
	doc.remove_nodes(&closed_links(&doc)); // an issue closed in the buffer leaves urgent with the same save
	let stored = doc.serialize();
	// urgent is section-less by design (ARCHITECTURE.md); a header here is inert, not an error
	let headers: Vec<&str> = stored.lines().filter(|l| l.starts_with('#')).collect();
	if !headers.is_empty() {
		eprintln!("warning: urgent is a flat list — these headers carry no meaning in it: {}", headers.join(", "));
	}

	if stored.trim().is_empty() {
		if path.exists() {
			std::fs::remove_file(&path)?;
		}
		println!("Urgent sprint is empty; removed");
		return Ok(());
	}
	std::fs::create_dir_all(path.parent().expect("urgent_path is always nested under data dir"))?;
	std::fs::write(&path, stored)?;
	println!("Updated urgent sprint");
	Ok(())
}

/// Edit what `content` lists. It is stored nowhere: only the issues it expands change.
pub async fn edit_view(content: &str, mode: SyncMode) -> Result<()> {
	let expanded = expand_and_refresh(TaskView::parse(content), mode == SyncMode::Full).await?.text;
	let raws = open(&[("view".to_string(), &expanded)]).await?;
	let _syncing = v_utils::io::Spinner::start(); // until the edit is synced
	if raws[0] == expanded {
		return sync_embedded_issue_changes(&raws, mode).await; // an issue a killed run left unsynced has nothing else to carry it
	}
	let landed = async {
		let mut doc = fold_back(&raws, &[], mode).await?.docs.remove(0);
		materialize_new_tasks(&mut doc, None, mode).await?;
		Ok(())
	};
	match landed.await {
		Ok(()) => Ok(()),
		Err(e) => reject(&raws, e),
	}
}

/// Persist buffers that failed a genuine (non-transient) sync/parse, and surface the path.
fn reject<T>(raws: &[String], e: Report) -> Result<T> {
	crate::utils::persist_rejected_changes(raws)?;
	eprintln!("Your changes were saved to /tmp/tedi/rejected-changes.md — you can recover them from there.");
	Err(e)
}

/// Each buffer as the editor left it.
async fn open(buffers: &[(String, &str)]) -> Result<Vec<String>> {
	let dir = tempfile::tempdir()?.keep(); // kept: the buffers outlive a crash
	let mut paths = Vec::new();
	for (name, text) in buffers {
		let path = dir.join(format!("{name}.md"));
		std::fs::write(&path, text)?;
		eprintln!("[{name}] tmp_path: {}", path.display());
		paths.push(path);
	}
	crate::utils::open_files(&paths, None).await?;
	Ok(paths.iter().map(std::fs::read_to_string).collect::<std::io::Result<_>>()?)
}

struct Folded {
	/// Per buffer.
	docs: Vec<TaskView>,
	moved: HashMap<IssueLink, IssueLink>,
}

/// The issue blocks of all `raws` landed and their `!mv`s made; `editing`: what the buffers are stored as.
async fn fold_back(raws: &[String], editing: &[Editing], mode: SyncMode) -> Result<Folded> {
	let (mut texts, mut moves) = (Vec::new(), Vec::new());
	for raw in raws {
		let (text, named) = tedi_core::take_moves(raw)?;
		texts.push(text);
		moves.extend(named.into_iter().filter(|m| !moves.contains(m)).collect::<Vec<_>>());
	}
	if mode == SyncMode::Offline && !moves.is_empty() {
		bail!("`!mv` needs GitHub, and this edit runs offline");
	}
	sync_embedded_issue_changes(&texts, mode).await?;
	let moved = match moves.is_empty() {
		true => HashMap::new(),
		false => move_issues(&moves, editing, mode).await?,
	};
	let mut docs: Vec<TaskView> = texts.iter().map(|t| TaskView::parse(t)).collect();
	for doc in &mut docs {
		doc.relink(&moved);
	}
	Ok(Folded { docs, moved })
}

enum Tab<'a> {
	Milestone(Box<MilestoneTab>),
	Picked(Box<PickedTab>, &'a Picking),
}
impl Tab<'_> {
	fn name(&self) -> String {
		match self {
			Tab::Milestone(tab) => tab.sprint.to_string(),
			Tab::Picked(..) => "selected".to_string(),
		}
	}

	fn text(&self) -> &str {
		match self {
			Tab::Milestone(tab) => &tab.expanded.text,
			Tab::Picked(tab, _) => &tab.expanded.text,
		}
	}

	fn editing(&self) -> Editing {
		match self {
			Tab::Milestone(tab) => Editing::Milestone(tab.link.clone()),
			Tab::Picked(tab, _) => Editing::Picked(tab.repo()),
		}
	}

	async fn land(self, raw: &str, doc: TaskView, moved: &HashMap<IssueLink, IssueLink>, mode: SyncMode) -> Result<()> {
		match self {
			Tab::Milestone(tab) => tab.land(raw, doc, moved, mode).await,
			Tab::Picked(tab, picking) => tab.land(picking, raw, doc, moved, mode).await,
		}
	}
}

/// A sprint open in the editor.
struct MilestoneTab {
	sprint: Sprint,
	link: MilestoneLink,
	/// As stored when it opened.
	opened: MilestoneBody,
	/// That, without what closed by then.
	body: MilestoneBody,
	expanded: Expanded,
	_lock: std::fs::File,
}
impl MilestoneTab {
	async fn render(link: &MilestoneLink, sprint: Sprint, mode: SyncMode) -> Result<Self> {
		let lock = lock_sprint(&sprint.to_string())?;
		// The buffer is what the edit is reasoned against, so reconcile before rendering it — not on the
		// way out, where whatever Github gained since would already have been typed over.
		let stored = Local::load_milestone(link, &FsReader)?;
		let milestone = match (mode, stored) {
			(SyncMode::Full, _) | (SyncMode::OnExit, None) => pull_milestone(link, MergeMode::Normal).await?,
			(SyncMode::OnExit, Some(stored)) => match check_for_existing_milestone_conflict(link).await? {
				Some(conflict_file) => bail!("Unresolved milestone conflict in {}. Resolve the markers, then re-run.", conflict_file.display()),
				None => stored,
			},
			(SyncMode::Offline, Some(stored)) => stored,
			(SyncMode::Offline, None) => bail!("Cannot edit '{sprint}' offline — it was never fetched while online."),
		};
		let mut body = milestone.body.clone();
		body.0.remove_nodes(&closed_links(&body.0)); // closed by the time it's opened: they leave the sprint
		Ok(Self {
			sprint,
			link: link.clone(),
			opened: milestone.body,
			expanded: expand_and_refresh(body.0.clone(), mode == SyncMode::Full).await?,
			body,
			_lock: lock,
		})
	}

	async fn land(self, raw: &str, mut doc: TaskView, moved: &HashMap<IssueLink, IssueLink>, mode: SyncMode) -> Result<()> {
		// Against what was expanded, not stored: expansion itself is not a user edit, nor is what collapsing it back reformats.
		let body = match raw == self.expanded.text {
			true => {
				let mut body = self.body;
				body.0.relink(moved);
				body
			}
			false => {
				// Top-level new tasks belong to this milestone → materialize as real issues in its repo.
				materialize_new_tasks(&mut doc, Some(self.link.clone()), mode).await?;
				sync_milestone_changes(&self.expanded, &doc, mode).await?;
				doc.collapse_to_links();
				MilestoneBody::parse(&doc.serialize(), self.link.repo_info())
			}
		};
		let write = BodyWrite { rendered: self.opened, body };
		modify_and_sync_milestone(&self.link, mode, MilestoneModifier::BodyWrite(write), SyncOptions::default()).await?;
		Ok(())
	}
}

//#[codestyle::skip(swappable-args)]
/// Another period for `link`, and the one that ended archived as it opened.
async fn roll_over(link: &MilestoneLink, period: std::time::Duration, description: &str, opened: &str) -> Result<()> {
	let gh = crate::github::client::get()?;
	let now = Timestamp::now();
	let due = now + period;
	println!("Milestone was outdated, updating due date to {}", due.strftime("%Y-%m-%d"));
	gh.update_milestone(link.repo_info(), link.number(), description, Some(due)).await?;
	let title = Local::load_milestone(link, &FsReader)?.expect("rolls over once landed").identity.title;
	gh.create_milestone(link.repo_info(), &format!("{}_{title}", now.strftime("%Y/%m/%d")), opened, true).await?;
	pull_milestone(link, MergeMode::Normal).await?;
	Ok(())
}
