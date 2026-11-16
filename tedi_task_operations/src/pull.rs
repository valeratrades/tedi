//! `tedi pull`: every issue of the named orgs and repos, into the store, through the same paths any
//! other sync takes.

use std::sync::Arc;

use color_eyre::eyre::{Result, bail};
use tedi_core::conflict::Conflict;

use crate::{
	CloseState, Issue, IssueLink, IssueSelector, LazyIssue, RepoInfo,
	github::FetchedIssue,
	local::{Consensus, FsReader, Local, LocalError, LocalFs, LocalIssueSource, lock_store},
	open_interactions::{MergeMode, fetch_and_store_missing, merge_with_remote},
	remote::RemoteSource,
	sink::Sink,
};

/// `targets`: an org (`owner`) or a repo (`owner/repo`) each.
pub async fn pull_repos(targets: &[String]) -> Result<()> {
	let gh = crate::github::client::get()?;
	let mut repos = Vec::new();
	for target in targets {
		match target.split_once('/') {
			Some((owner, repo)) => repos.push(RepoInfo::new(owner, repo)),
			None => repos.extend(gh.list_org_repos(target).await?),
		}
	}

	let mut behind = Vec::new();
	for repo in repos {
		if let Err(e) = pull_repo(repo).await {
			eprintln!("{repo}: {e:?}");
			behind.push(repo.to_string());
		}
	}
	if !behind.is_empty() {
		bail!("not every issue was stored, so these pull again from the last page that was: {}", behind.join(", "));
	}
	Ok(())
}

/// Page by page, least recently updated first: the cursor moves past a page once all of it is stored,
/// so a pull cut short (by the rate limit, say) is picked up there.
async fn pull_repo(repo: RepoInfo) -> Result<()> {
	let gh = crate::github::client::get()?;
	let since = Local::load_project_meta(repo).pulled_at;
	let mut after = None;
	loop {
		let page = gh.list_repo_issues(repo, since, after.as_deref()).await?;
		let read: Vec<Arc<FetchedIssue>> = page.issues.into_iter().filter(|f| !CloseState::is_duplicate_reason(f.issue.state_reason.as_deref())).map(Arc::new).collect();

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
				Ok(links) => linked.extend(links),
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
			None => return Ok(()),
		}
	}
}
