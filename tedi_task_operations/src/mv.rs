//! `!mv`: an issue tree changes repo, and every reference tedi stores to it follows.
//!
//! GitHub transfers issues only between repos of one owner; across owners the tree is recreated in
//! the target and the originals are closed as duplicates pointing at it.

use std::collections::HashMap;

use color_eyre::eyre::{Result, bail, eyre};

use crate::{
	CloseState, CommentIdentity, Events, Issue, IssueIdentity, IssueIndex, IssueLink, IssueSelector, LazyIssue, MilestoneLink, RepoInfo, VirtualIssue, clockify_tracking,
	local::{Consensus, FsReader, Local, LocalFs},
	open_interactions::{MilestoneModifier, Modifier, SyncMode, SyncOptions, modify_and_sync_issue, modify_and_sync_milestone},
	remote::{Remote, RemoteSource},
	selection::{Selected, try_lock_urgent, urgent_path},
	sink::Sink,
	sprints::load_local_issue,
};

/// Move each issue, subtree included, to its target repo; then repoint every store except the
/// buffer being edited — that of milestone `editing`, or of urgent when `None` — which is the
/// caller's to relink. Returns where each moved issue went.
pub async fn move_issues(moves: &[(IssueLink, RepoInfo)], editing: Option<&MilestoneLink>, mode: SyncMode) -> Result<HashMap<IssueLink, IssueLink>> {
	assert_ne!(mode, SyncMode::Offline, "moves are rejected offline before reaching here");
	let _urgent = match editing {
		Some(_) => Some(try_lock_urgent()?.ok_or_else(|| eyre!("the urgent sprint is open in another editor, and a move repoints links in it — close it and retry"))?),
		None => None,
	};

	let mut moved: HashMap<IssueLink, IssueLink> = HashMap::new();
	for (link, target) in moves {
		let from = moved.get(link).unwrap_or(link).clone(); // carried along by an ancestor moved earlier in this save
		let step = move_issue(&from, *target).await?;
		for to in moved.values_mut() {
			if let Some(next) = step.get(to) {
				*to = next.clone();
			}
		}
		moved.extend(step);
	}

	if editing.is_some() {
		let path = urgent_path();
		match std::fs::read_to_string(&path) {
			Ok(content) => {
				let mut view = crate::TaskView::parse(&content);
				if view.issue_links().iter().any(|l| moved.contains_key(l)) {
					view.relink(&moved);
					std::fs::write(&path, view.serialize())?;
				}
			}
			Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
			Err(e) => return Err(e.into()),
		}
	}
	relink_milestones(&moved, editing, mode).await?;
	Selected::relink(&moved)?;
	relink_tracked(&moved)?;
	relink_last_modified(&moved)?;
	relink_blockers(&moved, mode).await?;
	Ok(moved)
}

async fn move_issue(link: &IssueLink, target: RepoInfo) -> Result<HashMap<IssueLink, IssueLink>> {
	let source = link.project();
	if source == target {
		bail!("{link} is already in {target}");
	}
	let gh = crate::github::client::get()?;
	if !gh.repo_exists(target).await? {
		bail!("can't move {link} to {target}: no such repo, or your token can't see it");
	}
	let old = load_local_issue(link).await?;
	if let Some(pending) = nodes(&old).into_iter().find(|n| !n.is_linked()) {
		bail!("{link} holds {:?}, which isn't on GitHub yet — sync it before moving the tree", pending.contents.title);
	}

	let target_labels = gh.list_labels(target).await?;
	for node in nodes(&old) {
		let missing: Vec<&str> = node.contents.labels.iter().filter(|l| !target_labels.contains(l)).map(String::as_str).collect();
		if !missing.is_empty() {
			eprintln!(
				"warning: {target} has no {} label — {} moves without it",
				missing.iter().map(|l| format!("`{l}`")).collect::<Vec<_>>().join(", "),
				linked(node)
			);
		}
	}

	// one repo per tree: a moved child leaves its parent
	let parent = match old.identity.git_lineage()?.last() {
		Some(&n) => {
			let before = load_local_issue(&IssueLink::in_project(source, n)).await?;
			let mut after = before.clone();
			after.children.remove(&old.selector()).expect("a child is held by its parent");
			<Issue as Sink<Remote>>::sink(&mut after, Some(&before)).await?;
			Some((after, before))
		}
		None => None,
	};

	let moved = match source.owner() == target.owner() {
		true => transfer(&old, target).await?,
		false => recreate(&old, target, &target_labels).await?,
	};

	let pulled = Issue::load(RemoteSource::build(moved[link].clone(), Some(&[]))?).await?;
	let was: HashMap<IssueLink, &Issue> = nodes(&old).into_iter().map(|n| (moved[linked(n)].clone(), n)).collect();
	let mut placed = pulled.clone();
	settle(&mut placed, &was, &moved, &target_labels, source);
	<Issue as Sink<Remote>>::sink(&mut placed, Some(&pulled)).await?;
	<Issue as Sink<LocalFs>>::sink(&mut placed, Some(&old)).await?;
	if let Some((mut after, before)) = parent {
		<Issue as Sink<LocalFs>>::sink(&mut after, Some(&before)).await?;
	}
	<Issue as Sink<Consensus>>::sink(&mut placed, None).await?;
	println!("Moved {link} → {}", moved[link]);
	Ok(moved)
}

/// Same owner: GitHub's own transfer, which keeps comments, authorship and assignees.
async fn transfer(old: &Issue, target: RepoInfo) -> Result<HashMap<IssueLink, IssueLink>> {
	let gh = crate::github::client::get()?;
	let source = old.repo_info();
	let mut moved = HashMap::new();
	// leaves first: whether a parent's transfer takes its sub-issues along is undocumented, a leaf's is unambiguous
	for node in nodes(old).into_iter().rev() {
		let n = linked(node).number();
		moved.insert(linked(node).clone(), IssueLink::in_project(target, gh.transfer_issue(source, n, target).await?));
	}
	// nor is it documented whether sub-issue links survive
	for parent in nodes(old) {
		let p = moved[linked(parent)].number();
		let held = gh.fetch_sub_issues(target, p).await?;
		for child in parent.children.values() {
			let c = moved[linked(child)].number();
			if !held.iter().any(|i| i.number == c) {
				let id = gh.fetch_issue(target, c).await?.id;
				gh.add_sub_issue(target, p, id).await?;
			}
		}
	}
	Ok(moved)
}

/// Across owners: each node is created anew in `target`, its authorship and assignees carried by
/// hand, and the original closed as a duplicate pointing at it.
async fn recreate(old: &Issue, target: RepoInfo, target_labels: &[String]) -> Result<HashMap<IssueLink, IssueLink>> {
	let gh = crate::github::client::get()?;
	let source = old.repo_info();
	let me = gh.fetch_authenticated_user().await?;
	let signed = |events: &Events, line: String| Events::parse(&format!("{}\n\n{line}", events.to_string().trim_end()));

	let mut moved = HashMap::new();
	let mut originals = Vec::new();
	let mut queue = vec![(old, IssueIndex::repo_only(target))];
	while let Some((node, parent_index)) = queue.pop() {
		let n = linked(node).number();
		let original = gh.fetch_issue(source, n).await?;
		let mut copy = Issue {
			identity: IssueIdentity::pending(parent_index),
			contents: node.contents.clone(),
			children: HashMap::new(),
		};
		copy.contents.labels.retain(|l| target_labels.contains(l));
		for comment in copy.contents.comments.iter_mut() {
			match &comment.identity {
				CommentIdentity::Body =>
					if original.user.login != me {
						comment.body = signed(&comment.body, format!("Opened by @{}", original.user.login));
					},
				CommentIdentity::Created { user, .. } => {
					if *user != me {
						comment.body = signed(&comment.body, format!("— @{user}"));
					}
					comment.identity = CommentIdentity::Pending;
				}
				CommentIdentity::Pending => {}
			}
		}
		<Issue as Sink<Remote>>::sink(&mut copy, None).await?;
		let new = copy.identity.link().expect("linked by the remote sink").clone();

		let assignees: Vec<String> = original.assignees.into_iter().map(|a| a.login).collect();
		if !assignees.is_empty() {
			gh.add_assignees(target, new.number(), &assignees).await?;
		}
		originals.push((n, new.clone(), assignees));
		moved.insert(linked(node).clone(), new);

		let child_index = copy.identity.child_parent_index().expect("linked by the remote sink");
		queue.extend(node.children.values().map(|child| (child, child_index)));
	}

	for (n, new, assignees) in originals {
		// a mention notifies even an assignee the target repo couldn't take
		let cc = match assignees.is_empty() {
			true => String::new(),
			false => format!("\n\ncc {}", assignees.iter().map(|a| format!("@{a}")).collect::<Vec<_>>().join(" ")),
		};
		gh.create_comment(source, n, &format!("Moved to {new}{cc}")).await?;
		gh.update_issue_state(source, n, &CloseState::Duplicate(new.number())).await?;
	}
	Ok(moved)
}

/// What the moved tree must hold beyond what the move carried: each node's own state, its labels
/// the target knows, and blockers that no longer read relative to the repo they were written in.
fn settle(placed: &mut Issue, was: &HashMap<IssueLink, &Issue>, moved: &HashMap<IssueLink, IssueLink>, target_labels: &[String], source: RepoInfo) {
	let old = was[linked(placed)];
	placed.contents.state = old.contents.state.clone();
	placed.contents.labels = old.contents.labels.iter().filter(|l| target_labels.contains(l)).cloned().collect();
	placed.contents.blockers = old.contents.blockers.clone();
	placed.contents.blockers.relink(source, moved, true);
	for child in placed.children.values_mut() {
		settle(child, was, moved, target_labels, source);
	}
}

/// Pre-order.
fn nodes(issue: &Issue) -> Vec<&Issue> {
	std::iter::once(issue).chain(issue.children.values().flat_map(nodes)).collect()
}

fn linked(issue: &Issue) -> &IssueLink {
	issue.identity.link().expect("checked linked before moving")
}

/// Open milestones hosting a moved issue (other sprints, bottlenecks). Archived ones are frozen history.
async fn relink_milestones(moved: &HashMap<IssueLink, IssueLink>, editing: Option<&MilestoneLink>, mode: SyncMode) -> Result<()> {
	let root = Local::milestones_dir();
	let owners = match std::fs::read_dir(&root) {
		Ok(owners) => owners,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
		Err(e) => return Err(e.into()),
	};
	for owner in owners {
		let owner = owner?;
		if !owner.file_type()?.is_dir() {
			continue;
		}
		for repo in std::fs::read_dir(owner.path())? {
			let repo = repo?;
			if !repo.file_type()?.is_dir() {
				continue;
			}
			let repo = RepoInfo::new(&owner.file_name().to_string_lossy(), &repo.file_name().to_string_lossy());
			for number in Local::load_milestone_project_meta(repo, &FsReader).milestones.into_keys() {
				let link = MilestoneLink::parse(&format!("https://github.com/{repo}/milestone/{number}")).expect("well-formed milestone url");
				if editing == Some(&link) {
					continue;
				}
				let milestone = Local::load_milestone(&link, &FsReader)?.expect("listed in its project's meta");
				if milestone.is_closed() || !milestone.body.hosted().iter().any(|l| moved.contains_key(l)) {
					continue;
				}
				let mut body = milestone.body.clone();
				body.0.relink(moved);
				modify_and_sync_milestone(milestone, mode, MilestoneModifier::BodyWrite { body }, SyncOptions::default()).await?;
			}
		}
	}
	Ok(())
}

/// Blockers of other issues pointing at a moved one.
async fn relink_blockers(moved: &HashMap<IssueLink, IssueLink>, mode: SyncMode) -> Result<()> {
	for (path, candidate) in Local::issue_candidates()? {
		let index = Local::extract_index_from_path(&path)?;
		let mut blockers = VirtualIssue::parse(&candidate.body, path.clone())?.contents.blockers;
		if !blockers.relink(index.repo_info(), moved, false) {
			continue;
		}
		if index.issue_number().is_none() {
			eprintln!("warning: {} points at a moved issue but isn't on GitHub yet — repoint it by hand", path.display());
			continue;
		}
		let issue = Issue::load(crate::conflict_resolve::build_resolving_from_path(&path).await?).await?;
		modify_and_sync_issue(issue, mode, Modifier::Relink { moved: moved.clone() }, SyncOptions::default()).await?;
	}
	Ok(())
}

/// The Clockify timer follows its issue rather than restarting on it.
fn relink_tracked(moved: &HashMap<IssueLink, IssueLink>) -> Result<()> {
	let key = |l: &IssueLink| format!("{}#{}", l.project(), l.number());
	if let Some(tracked) = clockify_tracking::tracked_issue()
		&& let Some((_, to)) = moved.iter().find(|(from, _)| key(from) == tracked)
	{
		clockify_tracking::set_tracked_issue(Some(&key(to)))?;
	}
	Ok(())
}

/// `open --last` follows the issue it points at.
fn relink_last_modified(moved: &HashMap<IssueLink, IssueLink>) -> Result<()> {
	let path = crate::paths::cache_file("last_modified_issue");
	let stored = match std::fs::read_to_string(&path) {
		Ok(s) => s,
		Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(()),
		Err(e) => return Err(e.into()),
	};
	let index: IssueIndex = stored.parse()?;
	let repo = index.repo_info();
	if repo.is_virtual() {
		return Ok(());
	}
	let link = |s: &IssueSelector| match s {
		IssueSelector::GitId(n) => moved.get(&IssueLink::in_project(repo, *n)),
		IssueSelector::Title(_) | IssueSelector::Exact(_) => None,
	};
	let Some(at) = index.index().iter().position(|s| link(s).is_some()) else { return Ok(()) };
	let renamed: Option<Vec<&IssueLink>> = index.index()[at..].iter().map(link).collect();
	match renamed {
		Some(renamed) => std::fs::write(
			&path,
			IssueIndex::with_index(renamed[0].project(), renamed.iter().map(|l| IssueSelector::GitId(l.number())).collect()).to_string(),
		)?,
		None => std::fs::remove_file(&path)?, // points under a moved issue at a node that didn't move with it: nothing left to open
	}
	Ok(())
}
