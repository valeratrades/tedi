//! Sprint operations: expand/refresh a sprint's embedded issues from local state,
//! and sync edited blocker sections back to the individual issue files.
//!
//! These are the config-free halves of the sprint command — the bin owns config,
//! CLI dispatch, and the GitHub milestone-metadata calls.

use color_eyre::eyre::{Result, bail, eyre};

use crate::{
	Issue, IssueLink, LazyIssue, Milestone,
	local::{Consensus, FsReader, Local, LocalFs, LocalIssueSource},
	parse_blockers_from_embedded,
	remote::RemoteSource,
	sink::Sink,
};

/// Expand shorthand refs and refresh all embedded issue sections from local state.
///
/// Parses the content into a `Milestone`, resolves bare refs, then for each issue
/// ref (shorthand, bare URL, or embedded), loads the local issue and replaces the
/// item with the issue's fresh `Display`.
pub async fn expand_and_refresh(content: &str) -> Result<String> {
	let mut doc = Milestone::parse(content);
	doc.resolve_bare_refs();

	let links = doc.issue_links();
	let mut expansions: std::collections::HashMap<IssueLink, String> = std::collections::HashMap::new();

	for link in &links {
		if expansions.contains_key(link) {
			continue;
		}
		let issue = match load_local_issue(link).await {
			Ok(issue) => issue,
			Err(_) => match fetch_and_store_remote_issue(link).await {
				Ok(issue) => issue,
				Err(e) => {
					tracing::warn!("failed to expand {}/{}/#{}: {e}", link.owner(), link.repo(), link.number());
					continue;
				}
			},
		};
		expansions.insert(link.clone(), issue.to_string());
	}

	doc.expand_with(&expansions);
	Ok(doc.serialize())
}
/// Parse blocker changes from an edited sprint description and sync them back to issue files.
pub async fn sync_blocker_changes(content: &str, offline: bool) -> Result<()> {
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	let doc = Milestone::parse(content);

	for (link, section_text) in doc.embedded_issues() {
		let edited_blockers = parse_blockers_from_embedded(&section_text);
		if edited_blockers.had_orphans {
			bail!(
				"blocker section for {}/{}/#{} contains lines that don't belong to any blocker item — \
				fix the format (all text must be under a `- ` blocker line)",
				link.owner(),
				link.repo(),
				link.number()
			);
		}

		let issue = match load_local_issue(&link).await {
			Ok(issue) => issue,
			Err(e) => {
				tracing::warn!("failed to load {}/{}/#{} for sync: {e}", link.owner(), link.repo(), link.number());
				continue;
			}
		};

		// Only sync if the blockers actually differ.
		if issue.contents.blockers == edited_blockers {
			continue;
		}

		println!("Syncing blocker changes for {}/{}#{}", link.owner(), link.repo(), link.number());

		let modifier = Modifier::BlockerWrite { blockers: edited_blockers };
		modify_and_sync_issue(issue, offline, modifier, SyncOptions::default()).await?;
	}

	Ok(())
}
/// Load a local issue by its IssueLink.
async fn load_local_issue(link: &IssueLink) -> Result<Issue> {
	let path = Local::find_by_number(link.repo_info(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?;
	let local_source = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
	Issue::load(local_source).await.map_err(Into::into)
}

/// Fetch an issue from GitHub and store it locally (filesystem + consensus).
async fn fetch_and_store_remote_issue(link: &IssueLink) -> Result<Issue> {
	let source = RemoteSource::build(link.clone(), None)?;
	let mut issue = Issue::load(source).await?;
	<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
	<Issue as Sink<Consensus>>::sink(&mut issue, None).await?;
	Ok(issue)
}
