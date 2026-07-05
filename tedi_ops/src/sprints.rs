//! Sprint operations: expand/refresh a sprint's embedded issues, sync edited blocker
//! sections back to issue files, drive the active sprint's selection (`select`), and the
//! per-selected-issue operations (`selected add/pop/set/list/open/resume/halt`) plus `search`.
//!
//! These are the config-free halves of the sprint command — the bin owns config,
//! CLI dispatch, and the GitHub milestone-metadata calls.

use std::path::{Path, PathBuf};

use color_eyre::eyre::{Result, bail, eyre};

use crate::{
	Issue, IssueLink, LazyIssue, TaskView, VirtualIssue,
	clockify_tracking::{self, HaltArgs, ResumeArgs},
	local::{Consensus, FsReader, Local, LocalFs, LocalIssueSource, Selected},
	open_interactions::{Modifier, SyncOptions, modify_and_sync_issue},
	parse_blockers_from_embedded,
	remote::RemoteSource,
	sink::Sink,
};

/// Expand shorthand refs and refresh all embedded issue sections from local state.
///
/// Parses the content into a `TaskView`, resolves bare refs, then for each issue
/// ref (shorthand, bare URL, or embedded), loads the local issue and renders the
/// item as the issue's fresh `Display`.
pub async fn expand_and_refresh(content: &str) -> Result<String> {
	let mut doc = TaskView::parse(content);
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

	Ok(doc.render(&expansions))
}
/// Parse blocker changes from an edited sprint description and sync them back to issue files.
pub async fn sync_blocker_changes(content: &str, offline: bool) -> Result<()> {
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	let doc = TaskView::parse(content);

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
/// `sprints search <label-or-text>` — build a task view of local issues whose files
/// match `query` (case-insensitive `rg` over labels/title/body/blockers).
pub async fn search(query: &str) -> Result<()> {
	let issues_dir = Local::issues_dir();
	let output = std::process::Command::new("rg").args(["-l", "-i", "--", query]).current_dir(&issues_dir).output()?;

	let stdout = String::from_utf8_lossy(&output.stdout);
	let mut links: Vec<IssueLink> = Vec::new();
	for rel in stdout.lines().filter(|l| !l.is_empty()) {
		let path = issues_dir.join(rel);
		let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
		if name.ends_with(".bak") {
			continue;
		}
		// Directory issues match on `__main__.md`; take the issue number from the parent dir.
		let index_path = if name.starts_with(tedi_core::MAIN_ISSUE_FILENAME) {
			path.parent().map(Path::to_path_buf).unwrap_or(path.clone())
		} else {
			path.clone()
		};
		if let Ok(index) = Local::extract_index_from_path(&index_path)
			&& let Some(number) = index.issue_number()
			&& let Some(link) = IssueLink::parse(&format!("https://github.com/{}/{}/issues/{number}", index.owner(), index.repo()))
		{
			links.push(link);
		}
	}
	links.sort_by(|a, b| a.as_str().cmp(b.as_str()));
	links.dedup();

	if links.is_empty() {
		println!("No issues matching '{query}'.");
		return Ok(());
	}
	let content: String = links.iter().map(|l| format!("- {}\n", l.as_str())).collect();
	println!("{}", expand_and_refresh(&content).await?);
	Ok(())
}
/// Refresh the cached lowest-normal-sprint content (called by the bin after `edit`/`healthcheck`).
pub fn refresh_selection_cache(key: &str, content: &str) {
	if let Err(e) = Selected::refresh_normal(key, content) {
		tracing::warn!("failed to refresh selection cache: {e}");
	}
}
/// `sprints select [pattern] [--next|--prev]` — change the active sprint's selection.
pub async fn select(pattern: Option<String>, next: bool, prev: bool, yes: bool) -> Result<()> {
	let link = if next {
		Selected::select_move(1)
	} else if prev {
		Selected::select_move(-1)
	} else {
		Selected::select_pattern(pattern.as_deref())
	}
	.map_err(|e| eyre!("{e}"))?;

	println!("Selected: {}", issue_key(&link));
	retrack_if_changed(yes).await;
	Ok(())
}
/// Print the active sprint's current selection (auto-defaulting to the top open item).
pub async fn selected_show(yes: bool) -> Result<()> {
	let (link, _) = selected_link_path()?;
	println!("Selected: {}", issue_key(&link));
	retrack_if_changed(yes).await;
	Ok(())
}

/// `sprints selected open` — edit the selected issue file.
pub async fn selected_open(offline: bool, yes: bool) -> Result<()> {
	let (_, path) = selected_link_path()?;
	let local = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
	let issue = Issue::load(local).await?;
	modify_and_sync_issue(issue, offline, Modifier::Editor { open_at_blocker: true }, SyncOptions::default()).await?;
	retrack_if_changed(yes).await;
	Ok(())
}
/// `sprints selected list` — print the selected issue's blockers.
pub fn selected_list() -> Result<()> {
	let (_, path) = selected_link_path()?;
	let content = std::fs::read_to_string(&path)?;
	let blockers = VirtualIssue::parse(&content, path)?.contents.blockers;
	if blockers.is_empty() {
		println!("No blockers.");
	} else {
		println!("{}", String::from(&blockers));
	}
	Ok(())
}
pub async fn selected_add(text: String, nest: bool, offline: bool, yes: bool) -> Result<()> {
	modify_selected(offline, Modifier::BlockerAdd { text, nest }, yes).await
}
pub async fn selected_pop(parents: usize, offline: bool, yes: bool) -> Result<()> {
	modify_selected(offline, Modifier::BlockerPop { parents }, yes).await
}
pub async fn selected_set(text: String, offline: bool, yes: bool) -> Result<()> {
	modify_selected(offline, Modifier::BlockerSet { text }, yes).await
}
/// `sprints selected resume` — start a Clockify timer on the selected issue.
pub async fn selected_resume(mut resume_args: ResumeArgs, yes: bool) -> Result<()> {
	let (link, path) = selected_link_path()?;
	let info = track_info(&path)?;
	if resume_args.project.is_none() {
		resume_args.project = Some(info.project);
	}
	clockify_tracking::start_tracking_for_task(|_fq| info.description.clone(), &resume_args, None, yes).await?;
	clockify_tracking::set_tracked_issue(Some(&issue_key(&link)))?;
	println!("Tracking resumed on {}.", issue_key(&link));
	Ok(())
}
/// `sprints selected halt` — stop the Clockify timer.
pub async fn selected_halt(halt_args: HaltArgs) -> Result<()> {
	clockify_tracking::stop_current_tracking(halt_args.workspace.as_deref()).await?;
	clockify_tracking::set_tracked_issue(None)?;
	println!("Tracking halted.");
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

// ─── Selection & the `sprints selected` operations ────────────────────────────

async fn modify_selected(offline: bool, modifier: Modifier, yes: bool) -> Result<()> {
	let (_, path) = selected_link_path()?;
	let local = LocalIssueSource::<FsReader>::build_from_path(&path).await?;
	let issue = Issue::load(local).await?;
	let result = modify_and_sync_issue(issue, offline, modifier, SyncOptions::default()).await?;
	if let Some(output) = result.output {
		println!("{output}");
	}
	retrack_if_changed(yes).await;
	Ok(())
}

/// The selected issue's link and local path in the active sprint.
fn selected_link_path() -> Result<(IssueLink, PathBuf)> {
	let mut selected = Selected::load();
	let link = selected
		.current_link()
		.ok_or_else(|| eyre!("No selected issue. Edit a sprint (`todo sprints edit 1d`) or the urgent list first."))?;
	let path = Local::find_by_number(link.repo_info(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?;
	Ok((link, path))
}

struct TrackInfo {
	description: String,
	project: String,
}

/// Clockify description (issue title + current blocker) and project (`repo/title`).
fn track_info(path: &Path) -> Result<TrackInfo> {
	let content = std::fs::read_to_string(path)?;
	let vi = VirtualIssue::parse(&content, path.to_path_buf())?;
	let title = vi.contents.title.clone();
	let repo = path
		.strip_prefix(Local::issues_dir())
		.ok()
		.and_then(|rel| rel.components().nth(1).map(|c| c.as_os_str().to_string_lossy().into_owned()))
		.unwrap_or_default();
	let description = match vi.contents.blockers.current_with_context(&[]) {
		Some(current) => format!("{title}: {current}"),
		None => title.clone(),
	};
	Ok(TrackInfo {
		description,
		project: format!("{repo}/{title}"),
	})
}

fn issue_key(link: &IssueLink) -> String {
	format!("{}/{}#{}", link.owner(), link.repo(), link.number())
}

/// After a selection/edit, restart the Clockify timer on the newly-selected issue if it
/// changed (and stop entirely if nothing is selected). No-op when not tracking.
async fn retrack_if_changed(yes: bool) {
	let Some(tracked) = clockify_tracking::tracked_issue() else { return };

	let mut selected = Selected::load();
	let Some(link) = selected.current_link() else {
		let _ = clockify_tracking::stop_current_tracking(None).await;
		let _ = clockify_tracking::set_tracked_issue(None);
		return;
	};
	let key = issue_key(&link);
	if key == tracked {
		return;
	}

	let _ = clockify_tracking::stop_current_tracking(None).await;
	if let Some(path) = Local::find_by_number(link.repo_info(), link.number(), FsReader)
		&& let Ok(info) = track_info(&path)
	{
		let resume = ResumeArgs {
			project: Some(info.project.clone()),
			..Default::default()
		};
		if let Err(e) = clockify_tracking::start_tracking_for_task(|_fq| info.description.clone(), &resume, None, yes).await {
			tracing::warn!("failed to restart tracking on {key}: {e}");
		}
	}
	let _ = clockify_tracking::set_tracked_issue(Some(&key));
}
