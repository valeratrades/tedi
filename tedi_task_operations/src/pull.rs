//! `tedi pull`: every issue of the named orgs and repos, into the store, through the same paths any
//! other sync takes; and their pull requests, mirrored read-only. Each stored, every workflow's rules
//! may move it.

use std::{
	collections::{BTreeMap, BTreeSet, HashSet},
	sync::Arc,
};

use color_eyre::eyre::{Result, bail};
use jiff::Timestamp;
use tedi_core::{chain::Kind, conflict::Conflict};

use crate::{
	CloseState, Issue, IssueLink, IssueSelector, LazyIssue, Milestone, MilestoneLink, RepoInfo,
	github::{FetchedIssue, GithubError},
	local::{Consensus, FsReader, Local, LocalError, LocalFs, LocalIssueSource, conflict_detect::milestone_conflict_path, lock_store},
	open_interactions::{MergeMode, Pull, fetch_and_store_missing, merge_with_remote, pull_issues},
	remote::RemoteSource,
	sink::Sink,
	sprints::load_local_issue,
	workflow::{MovedOn, Rejected, Workflows, apply_rules},
};

/// `targets`: an org (`owner`) or a repo (`owner/repo`) each.
pub async fn pull_repos(targets: &[String], workflows: &Workflows, now: Timestamp) -> Result<()> {
	let gh = crate::github::client::get()?;
	let mut repos = Vec::new();
	for target in targets {
		match target.split_once('/') {
			Some((owner, repo)) => repos.push(RepoInfo::new(owner, repo)),
			None => repos.extend(gh.list_org_repos(target).await?),
		}
	}

	let mut behind = Vec::new();
	let mut milestones = Pull::default();
	for &repo in &repos {
		if let Err(e) = pull_repo(repo, workflows, &mut milestones, now).await {
			eprintln!("{repo}: {e:?}");
			behind.push(repo.to_string());
		}
	}
	pull_hosted(milestones.finish().await?, &repos, workflows, now).await?;
	if !behind.is_empty() {
		bail!("not every issue was stored, so these pull again from the last page that was: {}", behind.join(", "));
	}
	Ok(())
}

/// Page by page, least recently updated first: the cursor moves past a page once all of it is stored,
/// so a pull cut short (by the rate limit, say) is picked up there.
async fn pull_repo(repo: RepoInfo, workflows: &Workflows, milestones: &mut Pull, now: Timestamp) -> Result<()> {
	let gh = crate::github::client::get()?;
	let mut moved = HashSet::new();
	let labels = gh.list_labels(repo).await?;
	let assignable = gh.list_assignable(repo).await?;
	Local::update_project_meta(repo, |m| {
		m.label_colors = labels;
		m.assignable = assignable;
		m.dated |= m.pulled_at.is_none() && m.prs_walked_to.is_none(); // all of it is about to be read
	})?;
	let since = Local::load_project_meta(repo).pulled_at;
	let mut after = None;
	//LOOP pages until the remote reports no next cursor
	loop {
		let page = gh.list_repo_issues(repo, since, after.as_deref()).await?;
		let read: Vec<Arc<FetchedIssue>> = page
			.issues
			.into_iter()
			.filter(|f| !CloseState::is_duplicate_reason(f.issue.state_reason.as_deref()))
			.map(Arc::new)
			.collect();

		let mut roots: Vec<u64> = Vec::new();
		for issue in &read {
			let lineage = RemoteSource::prefetched(IssueLink::in_project(repo, issue.issue.number), None, &read)
				.resolve_parent_index()
				.await?
				.expect("a remote source always resolves its parent index");
			let root = match lineage.index().first() {
				Some(IssueSelector::GitId(root)) => *root,
				Some(other) => unreachable!("lineage read from GitHub is numbered, got {other:?}"),
				None => issue.issue.number,
			};
			if !roots.contains(&root) {
				roots.push(root);
			}
		}

		let mut linked = Vec::new();
		for root in roots {
			let mut remote = Issue::load(RemoteSource::prefetched(IssueLink::in_project(repo, root), Some(&[]), &read)).await?;
			let Some(path) = Local::find_by_number(repo, root, FsReader) else {
				let store = lock_store().await?;
				<Issue as Sink<LocalFs>>::sink(&mut remote, None).await?;
				<Issue as Sink<Consensus>>::sink(&mut remote, None).await?;
				drop(store);
				rules(&remote, workflows, &mut moved, now).await?;
				continue;
			};
			let source = match LocalIssueSource::<FsReader>::build_from_path(&path).await {
				Ok(source) => source,
				Err(LocalError::ConflictBlocked(e)) => {
					eprintln!("{repo}#{root} waits on its conflict: {}", e.conflict_file.display());
					continue;
				}
				Err(e) => return Err(e.into()),
			};
			let mut local = Issue::load(source).await?;
			match merge_with_remote(&mut local, remote, MergeMode::Normal).await {
				Ok(links) => {
					linked.extend(links);
					rules(&local, workflows, &mut moved, now).await?;
				}
				Err(e) if e.downcast_ref::<Conflict>().is_some() => eprintln!("{repo}#{root}: {e}"), // recorded, for the user to resolve
				Err(e) => return Err(e),
			}
		}
		fetch_and_store_missing(&linked).await;

		if let Some(last) = read.last() {
			Local::update_project_meta(repo, |m| m.pulled_at = Some(last.updated_at))?;
		}
		match page.next {
			Some(next) => after = Some(next),
			None => break,
		}
	}
	pull_milestones(repo, milestones).await?;
	pull_prs(repo, workflows, &mut moved, now).await?;
	if !Local::load_project_meta(repo).dated {
		date_stored(repo).await?;
	}
	Ok(())
}

/// The issues `milestones` host outside `repos`: no listing of those brings them.
async fn pull_hosted(milestones: Vec<Milestone>, repos: &[RepoInfo], workflows: &Workflows, now: Timestamp) -> Result<()> {
	let mut links: Vec<IssueLink> = Vec::new();
	for link in milestones.iter().flat_map(|m| m.body.0.issue_links()) {
		let listed = matches!(link, IssueLink::Virtual(_)) || repos.contains(&link.project());
		let stored = !listed && Local::find_by_number(link.project(), link.number(), FsReader).is_some(); // one GitHub won't show us isn't
		if stored && !links.contains(&link) {
			links.push(link);
		}
	}
	let mut issues = Vec::new();
	for link in links {
		match load_local_issue(&link).await {
			Ok(issue) => issues.push(issue),
			Err(e) => eprintln!("{link} was not pulled: {e:#}"), // a conflict waits on it
		}
	}
	if let Err(e) = pull_issues(&mut issues, MergeMode::Normal).await {
		eprintln!("{e:#}"); // each recorded as a conflict, or pulled again by the next
	}
	let mut moved = HashSet::new();
	for issue in &issues {
		rules(issue, workflows, &mut moved, now).await?;
	}
	Ok(())
}

/// Only when each was opened: what else of them is stored was left to the pull's cursor, and no rule gets to move one.
async fn date_stored(repo: RepoInfo) -> Result<()> {
	let gh = crate::github::client::get()?;
	let mut opened: BTreeMap<u64, Timestamp> = BTreeMap::new();
	let mut after = None;
	//LOOP pages until the remote reports no next cursor
	loop {
		let page = gh.list_repo_issues(repo, None, after.as_deref()).await?;
		opened.extend(page.issues.iter().map(|f| (f.issue.number, f.issue.created_at)));
		match page.next {
			Some(next) => after = Some(next),
			None => break,
		}
	}
	let mut after = None;
	//LOOP pages until the remote reports no next cursor
	loop {
		let page = gh.list_repo_prs(repo, after.as_deref()).await?;
		opened.extend(page.prs.iter().filter_map(|pr| Some((pr.number, pr.created_at?))));
		match page.next {
			Some(next) => after = Some(next),
			None => break,
		}
	}
	let _store = lock_store().await?;
	Local::update_prs(repo, |prs| {
		for (n, pr) in prs {
			pr.created_at = pr.created_at.or(opened.get(n).copied());
		}
	})?;
	Local::update_project_meta(repo, |m| {
		for (n, issue) in &mut m.issues {
			issue.created = issue.created.or(opened.get(n).copied());
		}
		m.dated = true;
	})?;
	Ok(())
}

/// `repo`'s open milestones, and those stored as open: what the board offers. A sprint's is its own flow's to sync.
pub(crate) async fn pull_milestones(repo: RepoInfo, pull: &mut Pull) -> Result<()> {
	let sprint = |number: u64, title: &str| Kind::from((&MilestoneLink::in_project(repo, number), title)) == Kind::Sprint;
	let gh = crate::github::client::get()?;
	let listed = gh.list_milestones(repo).await?.into_iter().filter(|m| !sprint(m.number, &m.title)).map(|m| m.number);
	let held: Vec<u64> = Local::load_milestone_project_meta(repo, &FsReader)
		.milestones
		.into_iter()
		.filter(|(n, m)| !m.state.is_closed() && !sprint(*n, &m.title))
		.map(|(n, _)| n)
		.collect();
	for number in listed.chain(held).collect::<BTreeSet<_>>() {
		let link = MilestoneLink::in_project(repo, number);
		match pull.link(&link).await {
			Ok(()) => {}
			Err(e) if matches!(e.downcast_ref::<GithubError>(), Some(GithubError::Api { status, .. }) if status.as_u16() == 404) => {
				println!("{} is gone from GitHub: kept here, closed", link.as_str());
				let mut local = Local::load_milestone(&link, &FsReader)?.expect("listed, or held here");
				local.identity.state = CloseState::NotPlanned;
				let store = lock_store().await?;
				<Milestone as Sink<LocalFs>>::sink(&mut local, None).await?;
				<Milestone as Sink<Consensus>>::sink(&mut local, None).await?;
				drop(store);
			}
			Err(e) if milestone_conflict_path(&link).exists() => eprintln!("{e:#}"), // recorded, for the user to resolve
			Err(e) => return Err(e),
		}
	}
	Ok(())
}

/// Every node of `tree` through each workflow's rules. A move that can't land is told, and the pull goes on.
async fn rules(tree: &Issue, workflows: &Workflows, moved: &mut HashSet<(IssueLink, String)>, now: Timestamp) -> Result<()> {
	fn links(node: &Issue, out: &mut Vec<IssueLink>) {
		out.extend(node.identity.link().cloned());
		for child in node.children.values() {
			links(child, out);
		}
	}
	let mut all = Vec::new();
	links(tree, &mut all);
	for link in all {
		rule(&link, workflows, moved, now).await?;
	}
	Ok(())
}

async fn rule(link: &IssueLink, workflows: &Workflows, moved: &mut HashSet<(IssueLink, String)>, now: Timestamp) -> Result<()> {
	match apply_rules(link, workflows, moved, now).await {
		Err(e) if e.downcast_ref::<Conflict>().is_some() || e.downcast_ref::<Rejected>().is_some() || e.downcast_ref::<MovedOn>().is_some() => {
			eprintln!("{link}: a rule's move didn't land: {e}"); // stays where it sat; the next pull of it tries again
			Ok(())
		}
		other => other,
	}
}

/// Newest first, down to the last whole walk's newest: the cursor moves only once a walk is whole, so
/// one cut short walks again from the top.
async fn pull_prs(repo: RepoInfo, workflows: &Workflows, moved: &mut HashSet<(IssueLink, String)>, now: Timestamp) -> Result<()> {
	let gh = crate::github::client::get()?;
	let since = Local::load_project_meta(repo).prs_walked_to;
	let mut newest = since;
	let mut after = None;
	//LOOP pages until the remote reports no next cursor
	loop {
		let page = gh.list_repo_prs(repo, after.as_deref()).await?;
		let listed = page.prs.len();
		let fresh: Vec<_> = page.prs.into_iter().take_while(|pr| since.is_none_or(|s| pr.updated_at >= s)).collect();
		newest = newest.max(fresh.iter().map(|pr| pr.updated_at).max());
		{
			let _store = lock_store().await?;
			Local::update_prs(repo, |stored| stored.extend(fresh.iter().map(|pr| (pr.number, pr.clone()))))?;
		}
		for pr in &fresh {
			rule(&IssueLink::in_project(repo, pr.number), workflows, moved, now).await?;
		}
		match page.next {
			Some(next) if fresh.len() == listed => after = Some(next),
			_ => break,
		}
	}
	Local::update_project_meta(repo, |m| m.prs_walked_to = newest)?;
	Ok(())
}
