//! Per-sprint selection state: a path pointer into the active sprint's node tree.
//!
//! The active sprint is the lowest existing one — the local urgent sprint while it has
//! open issues, else the cached lowest normal sprint. Selection is a *path* of node URLs
//! (sprint → [milestone] → issue → [child issue …]), validated on every load: segments
//! that are closed or no longer children of the previous level truncate the path, empty
//! paths default to the top open root node. A terminal milestone auto-resolves to its
//! first open, non-delegating issue — a resolution rule, never written back into the path.
//! Milestone contents are cached here (fetched by `sprints get/edit`/healthcheck) so
//! resolution works offline. Once every issue in urgent is closed, the closed links are
//! pruned (the file is deleted when nothing else remains) — plain-text items are never
//! cleanup fodder, and cleanup defers to a running edit session via the urgent lock.

use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};
use tedi_core::{IssueLink, IssueSelector, MilestoneLink, NodeLink, TaskView, VirtualIssue};

use super::{FsReader, Local};

/// Sprint key for the local urgent sprint.
pub const URGENT_KEY: &str = "urgent";
/// `$XDG_DATA_HOME/tedi/issues/urgent.md` — the local, GitHub-unsynced urgent sprint.
pub fn urgent_path() -> PathBuf {
	Local::issues_dir().join("urgent.md")
}
/// Exclusive cross-process lock over urgent-file mutation (edit sessions and auto-cleanup).
/// `None` — another session holds it. Keep the returned handle alive for the whole session.
pub fn try_lock_urgent() -> std::io::Result<Option<std::fs::File>> {
	let file = std::fs::File::create(crate::paths::cache_file("milestones_urgent.lock"))?;
	match file.try_lock() {
		Ok(()) => Ok(Some(file)),
		Err(std::fs::TryLockError::WouldBlock) => Ok(None),
		Err(std::fs::TryLockError::Error(e)) => Err(e),
	}
}
/// Where a selection operation landed. A milestone landing keeps the milestone as the
/// stored terminal while `selected *` ops act on the auto-resolved issue.
#[derive(Clone, Debug)]
pub enum Landing {
	Issue(IssueLink),
	Milestone { title: String, resolved: Option<IssueLink> },
}
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Selected {
	/// sprint key ("urgent" or a timeframe like "1d") → selection path of node URLs.
	/// Every segment except possibly the last is a container of the next.
	#[serde(default)]
	paths: HashMap<String, Vec<String>>,
	/// Cached lowest *normal* sprint (a GitHub milestone description). Refreshed on
	/// `sprints edit`/`healthcheck`; urgent needs no cache (it is a local file).
	#[serde(default)]
	normal: Option<NormalSprint>,
}
impl Selected {
	fn cache_path() -> PathBuf {
		crate::paths::cache_file("sprints_selection.json")
	}

	pub fn load() -> Self {
		match std::fs::read_to_string(Self::cache_path()) {
			Ok(content) => serde_json::from_str(&content).unwrap_or_default(),
			Err(_) => Self::default(),
		}
	}

	fn save(&self) -> std::io::Result<()> {
		let path = Self::cache_path();
		if let Some(parent) = path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		std::fs::write(path, serde_json::to_string_pretty(self).expect("Selected serialization"))
	}

	/// The lowest existing sprint: urgent while it has an open issue, else the cached normal
	/// sprint. A text-only (or fully-closed) urgent never shadows the normal sprint.
	pub fn active(&self) -> Option<ActiveSprint> {
		let urgent = urgent_path();
		if urgent.exists() {
			let content = std::fs::read_to_string(&urgent).ok()?;
			let mut view = TaskView::parse(&content);
			view.resolve_bare_refs();
			if view.issue_links().iter().any(link_is_open) {
				return Some(ActiveSprint { key: URGENT_KEY.to_string(), view });
			}
		}
		let normal = self.normal.as_ref()?;
		let mut view = TaskView::parse(&normal.content);
		view.resolve_bare_refs();
		Some(ActiveSprint { key: normal.key.clone(), view })
	}

	/// The selected issue in the active sprint: the validated path's terminal, with a
	/// terminal milestone auto-resolving to its first open, non-delegating issue.
	pub fn current_link(&mut self) -> Option<IssueLink> {
		cleanup_urgent();
		let active = self.active()?;
		if let Err(e) = guard_urgent(&active) {
			panic!("{e}");
		}
		let path = self.validate_path(&active);
		if path.is_empty() {
			if self.paths.remove(&active.key).is_some() {
				let _ = self.save(); // best-effort persistence; next load re-derives the same state
			}
			return None;
		}
		self.persist_path(&active.key, &path);
		match path.last().expect("emptiness handled above") {
			NodeLink::Issue(link) => Some(link.clone()),
			NodeLink::Milestone(ml) => self.resolve_milestone(ml),
		}
	}

	/// The selected node itself (the validated path's terminal), *without* resolving a
	/// milestone to a child — so `selected open` can act on the milestone as a first-class node.
	pub fn current_node(&mut self) -> Option<NodeLink> {
		cleanup_urgent();
		let active = self.active()?;
		if let Err(e) = guard_urgent(&active) {
			panic!("{e}");
		}
		let path = self.validate_path(&active);
		if path.is_empty() {
			if self.paths.remove(&active.key).is_some() {
				let _ = self.save();
			}
			return None;
		}
		self.persist_path(&active.key, &path);
		path.last().cloned()
	}

	/// Move the selection by `delta` among the terminal's siblings (circular), skipping
	/// closed/delegating issues and milestones that resolve to nothing.
	pub fn select_move(delta: isize) -> Result<Landing, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		guard_urgent(&active)?;
		let mut path = sel.validate_path(&active);
		let Some(terminal) = path.last().cloned() else {
			return Err("No open items in the active sprint.".into());
		};
		let siblings = match path.len() {
			1 => active.view.nodes(),
			n => sel.children(&path[n - 2]),
		};
		if siblings.len() == 1 {
			return Err("Only one item at this level. Nothing to move to.".into());
		}
		let start = siblings.iter().position(|n| *n == terminal).expect("validate_path keeps the terminal among its siblings");
		let len = siblings.len() as isize;
		let mut idx = start;
		for _ in 0..siblings.len() {
			idx = ((idx as isize + delta).rem_euclid(len)) as usize;
			if let Some(landing) = sel.landing(&siblings[idx]) {
				*path.last_mut().expect("non-empty checked above") = siblings[idx].clone();
				sel.persist_path(&active.key, &path);
				return Ok(landing);
			}
		}
		Err("All items at this level are closed, delegating, or unresolvable. Nothing to stop at.".into())
	}

	/// Descend into the terminal: milestone → its first open non-delegating issue,
	/// issue → its top open child. Explicit descent errors loudly instead of skipping.
	pub fn select_down() -> Result<Landing, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		guard_urgent(&active)?;
		let mut path = sel.validate_path(&active);
		let Some(terminal) = path.last().cloned() else {
			return Err("No open items in the active sprint.".into());
		};
		let child = match &terminal {
			NodeLink::Milestone(ml) => {
				let title = milestone_title(ml);
				sel.resolve_milestone(ml).ok_or_else(|| format!("no open non-delegating issue in milestone '{title}'"))?
			}
			NodeLink::Issue(link) => match issue_children(link).first() {
				Some(NodeLink::Issue(l)) => l.clone(),
				_ => return Err(format!("{}/{}#{} has no open children", link.owner(), link.repo(), link.number())),
			},
		};
		path.push(NodeLink::Issue(child.clone()));
		sel.persist_path(&active.key, &path);
		Ok(Landing::Issue(child))
	}

	/// Ascend one level; error at the sprint root.
	pub fn select_up() -> Result<Landing, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		guard_urgent(&active)?;
		let mut path = sel.validate_path(&active);
		if path.len() <= 1 {
			return Err("Already at the sprint root. Nothing to move up to.".into());
		}
		path.pop();
		let landing = sel.describe(path.last().expect("len > 1 checked above"));
		sel.persist_path(&active.key, &path);
		Ok(landing)
	}

	/// Select the tree node whose display matches `pattern` (smartcase regex over the
	/// full display string). With no unique match (or no pattern) opens fzf on the whole
	/// reachable tree. Duplicates resolve to the first occurrence in document order.
	pub fn select_pattern(pattern: Option<&str>) -> Result<Landing, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		guard_urgent(&active)?;
		let candidates = sel.candidate_paths(&active);
		if candidates.is_empty() {
			return Err("No items in the active sprint.".into());
		}
		let displays = sel.candidate_displays(&candidates);
		let selected = match pattern {
			None => Local::fzf_select(&displays, "").map_err(|e| format!("fzf failed: {e}"))?,
			Some(p) => Local::resolve_pattern(&displays, p)
				.map_err(|e| format!("fzf failed: {e}"))?
				.ok_or_else(|| format!("No item matching '{p}' in the sprint."))?,
		};
		// fzf maps back by string equality
		let idx = displays.iter().position(|d| *d == selected).ok_or_else(|| format!("fzf returned unknown entry: {selected}"))?;
		let path = candidates[idx].clone();
		let landing = sel.describe(path.last().expect("candidate paths are non-empty"));
		sel.persist_path(&active.key, &path);
		Ok(landing)
	}

	/// Point the active sprint's selection at the issue at `issue_path` (the `!s` flow).
	/// Warns instead of failing — it runs mid-sink.
	pub fn set_by_path(issue_path: &Path) -> std::io::Result<()> {
		let mut sel = Self::load();
		let Some(active) = sel.active() else {
			tracing::warn!("no active sprint; !s has no effect");
			return Ok(());
		};
		if let Err(e) = guard_urgent(&active) {
			tracing::warn!("{e}; !s ignored");
			return Ok(());
		}
		let found = sel.candidate_paths(&active).into_iter().find(|path| match path.last() {
			Some(NodeLink::Issue(l)) => resolve(l).is_some_and(|p| p == issue_path),
			_ => false,
		});
		match found {
			Some(path) => {
				sel.paths.insert(active.key.clone(), path.iter().map(|n| n.as_str().to_string()).collect());
				sel.save()
			}
			None => {
				tracing::warn!(path = %issue_path.display(), "issue not in active sprint tree; !s ignored");
				Ok(())
			}
		}
	}

	/// Whether an active sprint is resolvable without a network fetch — urgent has an open
	/// issue, or a normal sprint's content is already cached.
	pub fn active_ready() -> bool {
		Self::load().active().is_some()
	}

	/// Cache the (currently-focused) normal sprint's content (called by `sprints edit`,
	/// `healthcheck`, and `sprints select <tf>`).
	pub fn refresh_normal(key: &str, content: &str) -> std::io::Result<()> {
		let mut sel = Self::load();
		sel.normal = Some(NormalSprint {
			key: key.to_string(),
			content: content.to_string(),
		});
		sel.save()
	}

	/// Milestone links in the active view that are not yet stored in the local milestone tree.
	pub fn uncached_active_milestones() -> Vec<MilestoneLink> {
		let sel = Self::load();
		let Some(active) = sel.active() else { return Vec::new() };
		active
			.view
			.milestone_links()
			.into_iter()
			.filter(|l| Local::find_milestone_path(l.repo_info(), l.number(), &FsReader).is_none())
			.collect()
	}

	// ─── Tree resolution ───────────────────────────────────────────────────────

	/// Walk the stored path from the root, truncating at the first segment that is closed
	/// or no longer a child of the previous level. Empty → the top open root node.
	fn validate_path(&self, active: &ActiveSprint) -> Vec<NodeLink> {
		let root = active.view.nodes();
		let stored = self.paths.get(&active.key).cloned().unwrap_or_default();
		let mut path: Vec<NodeLink> = Vec::new();
		let mut siblings = root.clone();
		for url in &stored {
			let Some(node) = NodeLink::parse(url) else { break };
			if !siblings.contains(&node) || !self.node_is_open(&node) {
				break;
			}
			siblings = self.children(&node);
			path.push(node);
		}
		if path.is_empty()
			&& let Some(top) = root.into_iter().find(|n| self.node_is_open(n))
		{
			path.push(top);
		}
		path
	}

	fn persist_path(&mut self, key: &str, path: &[NodeLink]) {
		let urls: Vec<String> = path.iter().map(|n| n.as_str().to_string()).collect();
		if self.paths.get(key) != Some(&urls) {
			self.paths.insert(key.to_string(), urls);
			if let Err(e) = self.save() {
				tracing::warn!("failed to save selection: {e}");
			}
		}
	}

	/// A node's children: a milestone's hosted issue nodes and any inner milestone refs
	/// (read from its durable local file), or a directory issue's open child issues.
	fn children(&self, node: &NodeLink) -> Vec<NodeLink> {
		match node {
			NodeLink::Milestone(ml) => milestone_nodes(ml),
			NodeLink::Issue(link) => issue_children(link),
		}
	}

	fn node_is_open(&self, node: &NodeLink) -> bool {
		match node {
			NodeLink::Issue(link) => link_is_open(link),
			// milestones not stored locally count as open: closed-ness is only knowable from the file
			NodeLink::Milestone(ml) => !milestone_local(ml).is_some_and(|m| m.is_closed()),
		}
	}

	/// A milestone resolves to its first open, non-delegating issue; falling back to a
	/// delegating issue is deliberately not allowed.
	fn resolve_milestone(&self, ml: &MilestoneLink) -> Option<IssueLink> {
		self.children(&NodeLink::Milestone(ml.clone())).into_iter().find_map(|n| match n {
			NodeLink::Issue(l) if link_is_open(&l) && !link_delegates(&l) => Some(l),
			_ => None,
		})
	}

	/// Whether `node` is a valid movement landing target; `None` means skip.
	fn landing(&self, node: &NodeLink) -> Option<Landing> {
		match node {
			NodeLink::Issue(l) => (link_is_open(l) && !link_delegates(l)).then(|| Landing::Issue(l.clone())),
			NodeLink::Milestone(ml) => {
				let Some(milestone) = milestone_local(ml) else {
					// hot-path movement must stay usable offline: skip, but tell the user
					tracing::warn!(milestone = ml.as_str(), "skipping milestone not stored locally during selection movement");
					eprintln!("skipping milestone {} — run `todo sprints get/edit` online to fetch it", ml.as_str());
					return None;
				};
				if milestone.is_closed() {
					return None;
				}
				self.resolve_milestone(ml).map(|resolved| Landing::Milestone {
					title: milestone.identity.title.clone(),
					resolved: Some(resolved),
				})
			}
		}
	}

	fn describe(&self, node: &NodeLink) -> Landing {
		match node {
			NodeLink::Issue(l) => Landing::Issue(l.clone()),
			NodeLink::Milestone(ml) => Landing::Milestone {
				title: milestone_title(ml),
				resolved: self.resolve_milestone(ml),
			},
		}
	}

	/// Every reachable node with its full path, in document order: root nodes,
	/// cached-milestone issue children, and local issue children, recursive.
	fn candidate_paths(&self, active: &ActiveSprint) -> Vec<Vec<NodeLink>> {
		fn walk(sel: &Selected, prefix: &mut Vec<NodeLink>, nodes: &[NodeLink], out: &mut Vec<Vec<NodeLink>>) {
			for node in nodes {
				prefix.push(node.clone());
				out.push(prefix.clone());
				let children = sel.children(node);
				walk(sel, prefix, &children, out);
				prefix.pop();
			}
		}
		let mut out = Vec::new();
		walk(self, &mut Vec::new(), &active.view.nodes(), &mut out);
		out
	}

	/// Display strings for candidate paths. Two candidates sharing a display string get
	/// their full path as context — mandatory, since fzf maps back by string equality.
	fn candidate_displays(&self, candidates: &[Vec<NodeLink>]) -> Vec<String> {
		let base: Vec<String> = candidates.iter().map(|p| self.display(p.last().expect("candidate paths are non-empty"))).collect();
		let mut counts: HashMap<&String, usize> = HashMap::new();
		for d in &base {
			*counts.entry(d).or_default() += 1;
		}
		candidates
			.iter()
			.zip(&base)
			.map(|(path, d)| {
				if counts[d] > 1 {
					path.iter().map(|n| self.display(n)).collect::<Vec<_>>().join(" > ")
				} else {
					d.clone()
				}
			})
			.collect()
	}

	fn display(&self, node: &NodeLink) -> String {
		match node {
			NodeLink::Issue(link) => display_for_link(link),
			NodeLink::Milestone(ml) => display_for_milestone(ml),
		}
	}
}

/// The active sprint: its key and resolved view.
pub struct ActiveSprint {
	pub key: String,
	pub view: TaskView,
}
/// The durable local milestone at `ml`, if stored. Milestones are first-class local
/// files now (no in-memory cache), so navigation reads them straight off disk.
fn milestone_local(ml: &MilestoneLink) -> Option<crate::Milestone> {
	Local::load_milestone(ml, &FsReader).ok().flatten()
}

/// A milestone's child nodes: its body parsed as a task view (hosted issues and any inner
/// milestone refs), in document order.
fn milestone_nodes(ml: &MilestoneLink) -> Vec<NodeLink> {
	let Some(milestone) = milestone_local(ml) else { return Vec::new() };
	let mut view = TaskView::parse(&milestone.to_string());
	view.resolve_bare_refs();
	view.nodes()
}

/// A milestone's display title: its stored title, else the bare URL.
fn milestone_title(ml: &MilestoneLink) -> String {
	milestone_local(ml).map(|m| m.identity.title).unwrap_or_else(|| ml.as_str().to_string())
}

/// The urgent sprint is local-only: a hand-edited milestone ref in it is never navigated.
fn guard_urgent(active: &ActiveSprint) -> Result<(), String> {
	if active.key == URGENT_KEY && active.view.nodes().iter().any(|n| matches!(n, NodeLink::Milestone(_))) {
		return Err("urgent sprint cannot contain milestone refs".into());
	}
	Ok(())
}

/// Once every issue in urgent is closed, prune the closed links; delete the file when
/// nothing else remains. Skipped while an edit session holds the urgent lock.
fn cleanup_urgent() {
	let path = urgent_path();
	if !path.exists() {
		return;
	}
	let Ok(Some(_lock)) = try_lock_urgent() else { return };
	let Ok(content) = std::fs::read_to_string(&path) else { return }; // deleted between exists() and here
	let mut view = TaskView::parse(&content);
	view.resolve_bare_refs();
	let links = view.issue_links();
	if links.is_empty() || links.iter().any(link_is_open) {
		return;
	}
	view.remove_issues();
	let remaining = view.serialize();
	let result = if remaining.trim().is_empty() {
		std::fs::remove_file(&path)
	} else {
		std::fs::write(&path, remaining)
	};
	if let Err(e) = result {
		tracing::warn!("failed to clean up urgent file: {e}");
	}
}

#[derive(Clone, Debug, Deserialize, Serialize)]
struct NormalSprint {
	key: String,
	content: String,
}

/// Resolve an issue link to its local path.
fn resolve(link: &IssueLink) -> Option<PathBuf> {
	Local::find_by_number(link.repo_info(), link.number(), FsReader)
}

/// Open child issues of a directory issue, ordered like `Issue::Display` (by selector).
fn issue_children(link: &IssueLink) -> Vec<NodeLink> {
	let Some(path) = resolve(link) else { return Vec::new() };
	let is_dir_issue = path.file_name().and_then(|n| n.to_str()).is_some_and(|n| n.starts_with(tedi_core::MAIN_ISSUE_FILENAME));
	if !is_dir_issue {
		return Vec::new();
	}
	let dir = path.parent().expect("__main__.md always sits inside the issue directory");
	let Ok(entries) = std::fs::read_dir(dir) else { return Vec::new() };
	let mut numbers: Vec<u64> = entries
		.flatten()
		.filter_map(|entry| {
			let name = entry.file_name().to_string_lossy().to_string();
			// `.bak` entries are closed; unnumbered (pending) children have no link to select
			if name.starts_with(tedi_core::MAIN_ISSUE_FILENAME) || name.ends_with(".bak") {
				return None;
			}
			match Local::parse_issue_selector_from_name(&name) {
				Some(IssueSelector::GitId(n)) => Some(n),
				_ => None,
			}
		})
		.collect();
	numbers.sort_unstable();
	numbers
		.into_iter()
		.map(|n| {
			let url = format!("https://github.com/{}/{}/issues/{n}", link.owner(), link.repo());
			NodeLink::Issue(IssueLink::parse(&url).expect("constructed URL must be valid"))
		})
		.collect()
}

/// Whether an issue is resolvable locally and open.
fn link_is_open(link: &IssueLink) -> bool {
	let Some(path) = resolve(link) else { return false };
	let Ok(content) = std::fs::read_to_string(&path) else { return false };
	match VirtualIssue::parse(&content, path) {
		Ok(vi) => !vi.contents.state.is_closed(),
		Err(_) => false,
	}
}

/// Whether an issue's current (deepest) blocker delegates to another issue.
fn link_delegates(link: &IssueLink) -> bool {
	let Some(path) = resolve(link) else { return false };
	let Ok(content) = std::fs::read_to_string(&path) else { return false };
	match VirtualIssue::parse(&content, path) {
		Ok(vi) => vi.contents.blockers.deepest_issue_ref().is_some(),
		Err(_) => false,
	}
}

/// Display string for a link: local relative path if resolvable, else `owner/repo#number`.
/// The path doubles as fzf's `cat {}` preview target, so it must stay relative to `issues_dir`.
fn display_for_link(link: &IssueLink) -> String {
	resolve(link)
		.and_then(|p| p.strip_prefix(Local::issues_dir()).ok().map(|rel| rel.to_string_lossy().to_string()))
		.unwrap_or_else(|| format!("{}/{}#{}", link.owner(), link.repo(), link.number()))
}

/// Display string for a milestone: its local relative file path (so fzf's `cat {}` previews it),
/// falling back to the title when the file is not stored locally.
fn display_for_milestone(ml: &MilestoneLink) -> String {
	Local::find_milestone_path(ml.repo_info(), ml.number(), &FsReader)
		.and_then(|p| p.strip_prefix(Local::issues_dir()).ok().map(|rel| rel.to_string_lossy().to_string()))
		.unwrap_or_else(|| milestone_title(ml))
}
