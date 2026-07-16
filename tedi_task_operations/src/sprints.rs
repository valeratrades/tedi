//! Sprint operations: expand/refresh a sprint's embedded issues, sync edited blocker
//! sections back to issue files, drive the active sprint's selection (`select`), and the
//! per-selected-issue operations (`selected add/pop/set/list/open/resume/halt`) plus `search`.
//!
//! These are the config-free halves of the sprint command — the bin owns config,
//! CLI dispatch, and the GitHub milestone-metadata calls.

use std::path::{Path, PathBuf};

use color_eyre::eyre::{Result, bail, eyre};

use crate::{
	HollowIssue, Issue, IssueIdentity, IssueIndex, IssueLink, LazyIssue, Milestone, MilestoneLink, NodeLink, RepoInfo, TaskView, VirtualIssue,
	clockify_tracking::{self, HaltArgs, ResumeArgs},
	local::{Consensus, FsReader, GitReader, Local, LocalFs},
	open_interactions::{MilestoneModifier, Modifier, SyncOptions, modify_and_sync_issue, modify_and_sync_milestone},
	parse_blockers_from_embedded,
	remote::{Remote, RemoteSource, load_remote_milestone},
	selection::{Landing, Selected},
	sink::Sink,
};

/// Expand shorthand refs and refresh all embedded sections from local state.
///
/// Parses the content into a `TaskView`, resolves bare refs, then renders each issue
/// ref as the issue's fresh `Display`, and each *cached* milestone ref as an inline
/// block (title line + materialized content, its own issue refs expanded the same way —
/// one level, inner milestone refs stay bare links). Uncached milestones stay bare.
pub async fn expand_and_refresh(content: &str) -> Result<String> {
	let mut doc = TaskView::parse(content);
	doc.resolve_bare_refs();

	let mut milestones: Vec<(String, Milestone, TaskView)> = Vec::new();
	for link in doc.milestone_links() {
		let url = link.as_str().to_string();
		if milestones.iter().any(|(u, ..)| *u == url) {
			continue;
		}
		match Local::load_milestone(&link, &FsReader)? {
			Some(milestone) => {
				let mut inner = TaskView::parse(&milestone.to_string());
				inner.resolve_bare_refs();
				milestones.push((url, milestone, inner));
			}
			None => tracing::warn!("milestone {url} not stored locally; leaving as bare link (run `sprints get/edit` online to fetch it)"),
		}
	}

	let mut links = doc.issue_links();
	for (_, _, inner) in &milestones {
		links.extend(inner.issue_links());
	}

	let mut expansions: std::collections::HashMap<String, String> = std::collections::HashMap::new();
	for link in &links {
		let key = link.to_string();
		if expansions.contains_key(&key) {
			continue;
		}
		let issue = match load_local_issue(link).await {
			Ok(issue) => issue,
			// virtual issues have no remote: they must never be fetched
			Err(e) if matches!(link, IssueLink::Virtual(_)) => {
				tracing::warn!("failed to expand virtual issue {link}: {e}");
				continue;
			}
			Err(_) => match fetch_and_store_remote_issue(link).await {
				Ok(issue) => issue,
				Err(e) => {
					tracing::warn!("failed to expand {link}: {e}");
					continue;
				}
			},
		};
		expansions.insert(key, issue.to_string());
	}

	for (url, milestone, inner) in milestones {
		let checkbox = if milestone.is_closed() { 'x' } else { ' ' };
		// markdown-link title → `gf`-jumpable to the milestone's own local file from the edit buffer;
		// the marker still carries identity, so this collapses back to the bare URL on save.
		let abspath = Local::milestone_file_path(milestone.identity.link.repo_info(), milestone.number(), &milestone.identity.title);
		let mut block = format!("- [{checkbox}] [{}]({}) <!-- {url} -->\n", milestone.identity.title, abspath.display());
		let inner_rendered = inner.render(&expansions);
		if !inner_rendered.trim().is_empty() {
			tedi_md::indent_into(&mut block, &inner_rendered, "  ");
		}
		expansions.insert(url, block);
	}

	Ok(doc.render(&expansions))
}

/// Fetch each milestone (deduped) and store it as a durable local file, along with its
/// hosted issues, so navigation and inline expansion resolve offline. A local copy carrying
/// unsynced edits is left untouched (it syncs on the next milestone edit).
pub async fn refresh_milestone_cache(links: &[MilestoneLink]) -> Result<()> {
	let mut seen = std::collections::HashSet::new();
	for link in links {
		if !seen.insert(link.as_str().to_string()) {
			continue;
		}
		let remote = load_remote_milestone(link).await?;

		for child in &remote.body.hosted() {
			if load_local_issue(child).await.is_ok() || matches!(child, IssueLink::Virtual(_)) {
				continue;
			}
			if let Err(e) = fetch_and_store_remote_issue(child).await {
				tracing::warn!("failed to store milestone issue {child}: {e}");
			}
		}

		let local = Local::load_milestone(link, &FsReader)?;
		let consensus = Local::load_milestone(link, &GitReader)?;
		let has_unsynced_local = match (&local, &consensus) {
			(Some(l), Some(c)) => l.to_string() != c.to_string(),
			(Some(_), None) => true,
			_ => false,
		};
		if has_unsynced_local {
			tracing::debug!("milestone {} has unsynced local edits; keeping local copy until next sync", link.as_str());
		} else {
			let mut milestone = remote;
			<Milestone as Sink<LocalFs>>::sink(&mut milestone, None).await?;
			<Milestone as Sink<Consensus>>::sink(&mut milestone, None).await?;
		}
	}
	Ok(())
}

/// Materialize each unlinked open `- [ ]` item of an edited task view into a real issue,
/// replacing the item with the issue's link. Returns the created links.
///
/// A task's home decides its kind: a task belonging to a milestone (its own inlined milestone
/// ref, else `ambient` — the milestone whose body is being edited) is created upstream as a
/// pending (`!n`) Github issue, since milestones host only real issues. A task with no milestone
/// (a plain sprint item) becomes a local-only numbered virtual issue.
///
/// Offline, a milestone task can't be uploaded — it's left as text and retried on the next online
/// edit. Tasks living inside an inlined milestone leave their new link in that milestone's body;
/// persisting it upstream is `sync_milestone_changes`'s job (call it after this).
pub async fn materialize_new_tasks(doc: &mut TaskView, ambient: Option<MilestoneLink>, offline: bool) -> Result<Vec<IssueLink>> {
	let candidates = doc.homeless_tasks();
	if candidates.is_empty() {
		return Ok(Vec::new());
	}

	let mut created = Vec::new();
	for (id, block, task_milestone) in candidates {
		let link = match task_milestone.or_else(|| ambient.clone()) {
			Some(milestone) if offline => {
				tracing::warn!(
					"new milestone task can't be created offline; left as text, will upload on next online edit ({}):\n{block}",
					milestone.as_str()
				);
				continue;
			}
			Some(milestone) => match create_pending_upstream(&block, &milestone).await? {
				Some(link) => link,
				None => continue,
			},
			None => match materialize_virtual(&block).await? {
				Some(link) => link,
				None => continue,
			},
		};
		doc.assign_link(&id, link.clone());
		created.push(link);
	}

	Ok(created)
}

/// Propagate edits made inside inlined milestone blocks of a sprint back to each milestone's
/// own durable store + upstream. The general form of the old materialize-only push: every
/// inlined milestone whose body changed is routed through `modify_and_sync_milestone`, so
/// materialized tasks (and any body edit) persist and never re-materialize.
pub async fn sync_milestone_changes(doc: &TaskView, offline: bool) -> Result<()> {
	for (link, body_text) in doc.embedded_milestone_bodies() {
		let new_body = crate::MilestoneBody::parse(&body_text);

		let milestone = match Local::load_milestone(&link, &FsReader)? {
			Some(m) => m,
			None if offline => {
				tracing::warn!("milestone {} not stored locally; its inline edits will sync on the next online edit", link.as_str());
				continue;
			}
			None => load_remote_milestone(&link).await?,
		};

		if milestone.body == new_body {
			continue;
		}

		println!("Syncing milestone changes for {}", link.as_str());
		modify_and_sync_milestone(milestone, offline, MilestoneModifier::BodyWrite { body: new_body }, SyncOptions::default()).await?;
	}
	Ok(())
}
/// Parse blocker changes from an edited sprint description and sync them back to issue files.
pub async fn sync_blocker_changes(content: &str, offline: bool) -> Result<()> {
	use crate::open_interactions::{Modifier, SyncOptions, modify_and_sync_issue};

	let doc = TaskView::parse(content);

	for (link, section_text) in doc.embedded_issues() {
		let edited_blockers = parse_blockers_from_embedded(&section_text);
		if edited_blockers.had_orphans {
			bail!(
				"blocker section for {link} contains lines that don't belong to any blocker item — \
				fix the format (all text must be under a `- ` blocker line)",
			);
		}

		let issue = match load_local_issue(&link).await {
			Ok(issue) => issue,
			Err(e) => {
				tracing::warn!("failed to load {link} for sync: {e}");
				continue;
			}
		};

		// Only sync if the blockers actually differ.
		if issue.contents.blockers == edited_blockers {
			continue;
		}

		println!("Syncing blocker changes for {link}");

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
		{
			links.push(Local::issue_link(index.repo_info(), number));
		}
	}
	links.sort_by_key(|l| l.to_string());
	links.dedup();

	if links.is_empty() {
		println!("No issues matching '{query}'.");
		return Ok(());
	}
	let content: String = links.iter().map(|l| format!("- {l}\n")).collect();
	println!("{}", expand_and_refresh(&content).await?);
	Ok(())
}
/// Refresh the cached lowest-normal-sprint content (called by the bin after `edit`/`healthcheck`).
pub fn refresh_selection_cache(key: &str, content: &str) {
	if let Err(e) = Selected::refresh_normal(key, content) {
		tracing::warn!("failed to refresh selection cache: {e}");
	}
}
/// `sprints select [pattern] [--next|--prev|--down|--up]` — change the active sprint's selection.
pub async fn select(pattern: Option<String>, next: bool, prev: bool, down: bool, up: bool, yes: bool) -> Result<()> {
	let landing = if next {
		Selected::select_move(1)
	} else if prev {
		Selected::select_move(-1)
	} else if down {
		Selected::select_down()
	} else if up {
		Selected::select_up()
	} else {
		Selected::select_pattern(pattern.as_deref())
	}
	.map_err(|e| eyre!("{e}"))?;

	match landing {
		Landing::Issue(link) => println!("Selected: {}", issue_key(&link)),
		Landing::Milestone { title, resolved: Some(link) } => println!("Selected milestone: {title} → {}", issue_key(&link)),
		Landing::Milestone { title, resolved: None } => println!("Selected milestone: {title} (no workable issue)"),
	}
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
/// `sprints selected open` — edit the selected node. A milestone terminal opens the
/// milestone's own file (synced as a milestone); an issue terminal opens the issue.
pub async fn selected_open(offline: bool, yes: bool) -> Result<()> {
	match Selected::load().current_node() {
		Some(NodeLink::Milestone(ml)) => {
			let milestone = Local::load_milestone(&ml, &FsReader)?.ok_or_else(|| eyre!("milestone {} is not stored locally", ml.as_str()))?;
			modify_and_sync_milestone(milestone, offline, MilestoneModifier::Editor, SyncOptions::default()).await?;
		}
		Some(NodeLink::Issue(link)) => {
			let path = Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?;
			let local = crate::conflict_resolve::build_resolving_from_path(&path).await?;
			let issue = Issue::load(local).await?;
			modify_and_sync_issue(issue, offline, Modifier::Editor { open_at_blocker: true }, SyncOptions::default()).await?;
		}
		None => bail!("No selected issue. Edit a sprint (`todo sprints edit 1d`) or the urgent list first."),
	}
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
/// `sprints selected current` — compactly print the current (deepest) blocker.
pub fn selected_current() -> Result<()> {
	let (_, path) = selected_link_path()?;
	let content = std::fs::read_to_string(&path)?;
	let contents = VirtualIssue::parse(&content, path)?.contents;
	let current = contents.blockers.current_with_context(&[]).unwrap_or(contents.title);
	// Truncated for status-bar consumers (eww polls this).
	const MAX_LEN: usize = 70;
	if current.chars().count() <= MAX_LEN {
		println!("{current}");
	} else {
		println!("{}...", current.chars().take(MAX_LEN - 3).collect::<String>());
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
/// Mark a homeless task block with `marker` on its title line so `VirtualIssue::parse` accepts it.
fn mark_task_block(block: &str, marker: &str) -> String {
	let mut lines = block.lines();
	let first = lines.next().expect("serialized block always has a title line");
	std::iter::once(format!("{first} {marker}")).chain(lines.map(str::to_string)).collect::<Vec<_>>().join("\n")
}

/// Sprint task → local-only numbered virtual issue. `Ok(None)` if the block failed to parse
/// (already warned; the item stays as text, the burned number is harmless).
async fn materialize_virtual(block: &str) -> Result<Option<IssueLink>> {
	Local::ensure_virtual_project()?;
	let n = Local::allocate_virtual_issue_number()?;

	// parse first to learn the title, which the deterministic file path depends on
	let vi = match VirtualIssue::parse(&mark_task_block(block, "<!-- virtual -->"), PathBuf::from("<sprint item>")) {
		Ok(vi) => vi,
		Err(e) => {
			tracing::warn!("skipping materialization of sprint item:\n{block}\n{e}");
			return Ok(None);
		}
	};
	let title = vi.contents.title.clone();

	// the file path is the virtual issue's id; compute where the sink will deterministically write it
	let path = Local::project_dir(RepoInfo::Virtual).join(tedi_core::issue_file_name(Some(n), &title, false));
	let link = IssueLink::Virtual(path);

	let home = IssueIndex::repo_only(RepoInfo::Virtual);
	let identity = IssueIdentity::virtual_linked(home, link.clone());
	// hollow carries the remote so children get parent_index rooted under this issue's number
	let hollow = HollowIssue::new(identity.remote.clone(), std::collections::HashMap::new());
	let mut issue = Issue::from_combined(hollow, vi, home, true)?;
	issue.identity = identity;
	<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;

	println!("Created virtual issue virtual#{n}: {title}");
	Ok(Some(link))
}

/// Milestone task → pending (`!n`) issue created upstream in the milestone's repo, then synced
/// to local + consensus. Returns the created issue's real link. `Ok(None)` if the block failed
/// to parse (already warned; the item stays as text).
async fn create_pending_upstream(block: &str, milestone: &MilestoneLink) -> Result<Option<IssueLink>> {
	let repo = milestone.repo_info();

	let vi = match VirtualIssue::parse(&mark_task_block(block, "!n"), PathBuf::from("<milestone item>")) {
		Ok(vi) => vi,
		Err(e) => {
			tracing::warn!("skipping materialization of milestone item:\n{block}\n{e}");
			return Ok(None);
		}
	};
	let title = vi.contents.title.clone();

	let parent_index = IssueIndex::repo_only(repo);
	let mut issue = Issue::from_combined(HollowIssue::new(None, std::collections::HashMap::new()), vi, parent_index, false)?;

	<Issue as Sink<Remote>>::sink(&mut issue, None).await?;
	<Issue as Sink<LocalFs>>::sink(&mut issue, None).await?;
	<Issue as Sink<Consensus>>::sink(&mut issue, None).await?;

	let link = issue.identity.link().expect("issue is linked after remote sink").clone();
	println!("Created issue {repo}#{}: {title}", link.number());
	Ok(Some(link))
}

/// Load a local issue by its IssueLink. A virtual link's path is the file directly; a remote
/// link is resolved by number under its project.
async fn load_local_issue(link: &IssueLink) -> Result<Issue> {
	let path = match link {
		IssueLink::Virtual(p) => p.clone(),
		IssueLink::Owned(_) => Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?,
	};
	let local_source = crate::conflict_resolve::build_resolving_from_path(&path).await?;
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
	let local = crate::conflict_resolve::build_resolving_from_path(&path).await?;
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
	let path = Local::find_by_number(link.project(), link.number(), FsReader).ok_or_else(|| eyre!("issue #{} not found locally", link.number()))?;
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
	format!("{}#{}", link.project(), link.number())
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
	if let Some(path) = Local::find_by_number(link.project(), link.number(), FsReader)
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
