//! `tedi pull`: every issue of the named orgs and repos, into the store, through the same paths any
//! other sync takes.

use color_eyre::eyre::{Result, bail};
use jiff::Timestamp;

use crate::{
	CloseState, Issue, IssueLink, LazyIssue, RepoInfo,
	local::{FsReader, Local, LocalIssueSource},
	open_interactions::{MergeMode, fetch_and_store_missing, pull_issues},
};

/// `targets`: an org (`owner`) or a repo (`owner/repo`) each. A repo's cursor only advances once
/// every issue it listed is stored; one that failed is retried by the next pull.
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
		let started = Timestamp::now();
		let listed = gh.list_repo_issues(repo, Local::load_project_meta(repo).pulled_at).await?;
		let links: Vec<IssueLink> = listed
			.iter()
			.filter(|i| !CloseState::is_duplicate_reason(i.state_reason.as_deref()))
			.map(|i| IssueLink::in_project(repo, i.number))
			.collect();
		let (stored, missing): (Vec<IssueLink>, Vec<IssueLink>) = links.into_iter().partition(|l| Local::find_by_number(repo, l.number(), FsReader).is_some());

		let mut clean = fetch_and_store_missing(&missing).await.is_empty();
		let mut loaded = Vec::new();
		for link in &stored {
			let path = Local::find_by_number(repo, link.number(), FsReader).expect("partitioned as stored");
			match async { Ok::<_, color_eyre::Report>(Issue::load(LocalIssueSource::<FsReader>::build_from_path(&path).await?).await?) }.await {
				Ok(issue) => loaded.push(issue),
				Err(e) => {
					eprintln!("{link}: {e}");
					clean = false;
				}
			}
		}
		let indices: Vec<_> = loaded.iter().map(Issue::full_index).collect();
		for mut issue in loaded {
			let index = issue.full_index();
			// pulled with its ancestor's tree
			if indices.iter().any(|a| a.index().len() < index.index().len() && index.index().starts_with(a.index())) {
				continue;
			}
			if let Err(e) = pull_issues(std::slice::from_mut(&mut issue), MergeMode::Normal).await {
				eprintln!("{index}: {e}");
				clean = false;
			}
		}

		match clean {
			true => Local::update_project_meta(repo, |m| m.pulled_at = Some(started))?,
			false => behind.push(repo.to_string()),
		}
	}
	if !behind.is_empty() {
		bail!("not every issue was stored, so these pull again from where they last completed: {}", behind.join(", "));
	}
	Ok(())
}
