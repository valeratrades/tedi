//! Per-sprint selection state: which issue is "selected" in the active sprint.
//!
//! Replaces the old rotating `MilestoneBlockerCache`. The active sprint is the lowest
//! existing one — the local urgent sprint if present, else the cached lowest normal
//! sprint. Selection is stored per sprint key and by issue link (stable across edits),
//! auto-advancing to the top open item when the selection is closed or removed, and
//! clearing (deleting the urgent file) when no open issues remain.

use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};
use tedi_core::{IssueLink, TaskView, VirtualIssue};

use super::{FsReader, Local};

/// Sprint key for the local urgent sprint.
pub const URGENT_KEY: &str = "urgent";

/// `$XDG_DATA_HOME/tedi/sprints/urgent.md` — the local, GitHub-unsynced urgent sprint.
pub fn urgent_path() -> PathBuf {
	crate::paths::data_dir("sprints").join("urgent.md")
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct Selected {
	/// sprint key ("urgent" or a timeframe like "1d") → selected issue URL.
	#[serde(default)]
	selections: HashMap<String, String>,
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

	/// The lowest existing sprint: urgent (local file) if it exists, else the cached normal sprint.
	pub fn active(&self) -> Option<ActiveSprint> {
		let urgent = urgent_path();
		if urgent.exists() {
			let content = std::fs::read_to_string(&urgent).ok()?;
			let mut view = TaskView::parse(&content);
			view.resolve_bare_refs();
			return Some(ActiveSprint {
				key: URGENT_KEY.to_string(),
				view,
				urgent_file: Some(urgent),
			});
		}
		let normal = self.normal.as_ref()?;
		let mut view = TaskView::parse(&normal.content);
		view.resolve_bare_refs();
		Some(ActiveSprint {
			key: normal.key.clone(),
			view,
			urgent_file: None,
		})
	}

	/// The selected issue link in the active sprint, auto-advancing to the top open item
	/// when the selection is closed/removed, and clearing (deleting the urgent file) when
	/// no open issues remain.
	pub fn current_link(&mut self) -> Option<IssueLink> {
		let active = self.active()?;
		let open: Vec<IssueLink> = active.view.issue_links().into_iter().filter(link_is_open).collect();

		if open.is_empty() {
			self.selections.remove(&active.key);
			if let Some(file) = &active.urgent_file {
				let _ = std::fs::remove_file(file);
			}
			let _ = self.save();
			return None;
		}

		let selected = self.selections.get(&active.key).and_then(|s| IssueLink::parse(s));
		if let Some(sel) = &selected
			&& open.contains(sel)
		{
			return selected;
		}

		let top = open[0].clone();
		self.selections.insert(active.key.clone(), top.as_str().to_string());
		let _ = self.save();
		Some(top)
	}

	/// Resolve the selected issue to a local path.
	pub fn current_path(&mut self) -> Option<PathBuf> {
		self.current_link().as_ref().and_then(resolve)
	}

	/// Move the selection by `delta` (circular), skipping issues that delegate via a blocker ref.
	pub fn select_move(delta: isize) -> Result<IssueLink, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		let links = active.view.issue_links();
		if links.is_empty() {
			return Err("No issues in the active sprint.".into());
		}
		if links.len() == 1 {
			return Err("Only one issue in the sprint. Nothing to move to.".into());
		}
		let start = sel
			.selections
			.get(&active.key)
			.and_then(|s| IssueLink::parse(s))
			.and_then(|c| links.iter().position(|l| *l == c))
			.unwrap_or(0);
		let len = links.len() as isize;

		let mut idx = start;
		for _ in 0..links.len() {
			idx = ((idx as isize + delta).rem_euclid(len)) as usize;
			if !link_delegates(&links[idx]) {
				break;
			}
		}
		let link = links[idx].clone();
		if idx == start && link_delegates(&link) {
			return Err("All issues in the sprint delegate via blocker refs. Nothing to stop at.".into());
		}

		sel.selections.insert(active.key.clone(), link.as_str().to_string());
		sel.save().map_err(|e| format!("Failed to save selection: {e}"))?;
		Ok(link)
	}

	/// Select the sprint issue whose display matches `pattern` (case-insensitive substring).
	/// With no unique match (or no pattern) opens fzf on the sprint's issues.
	pub fn select_pattern(pattern: Option<&str>) -> Result<IssueLink, String> {
		let mut sel = Self::load();
		let active = sel.active().ok_or("No active sprint. Run `todo sprints edit 1d` first.")?;
		let links = active.view.issue_links();
		if links.is_empty() {
			return Err("No issues in the active sprint.".into());
		}

		let matches: Vec<IssueLink> = match pattern {
			None => links,
			Some(p) => {
				let needle = p.to_lowercase();
				links.into_iter().filter(|l| display_for_link(l).to_lowercase().contains(&needle)).collect()
			}
		};
		let link = match matches.len() {
			0 => return Err(format!("No issue matching '{}' in the sprint.", pattern.unwrap_or(""))),
			1 if pattern.is_some() => matches.into_iter().next().unwrap(),
			_ => {
				let displays: Vec<String> = matches.iter().map(display_for_link).collect();
				let selected = Local::fzf_select(&displays, pattern.unwrap_or("")).map_err(|e| format!("fzf failed: {e}"))?;
				matches
					.into_iter()
					.find(|l| display_for_link(l) == selected)
					.ok_or_else(|| format!("fzf returned unknown entry: {selected}"))?
			}
		};
		sel.selections.insert(active.key.clone(), link.as_str().to_string());
		sel.save().map_err(|e| format!("Failed to save selection: {e}"))?;
		Ok(link)
	}

	/// Point the active sprint's selection at the issue at `issue_path` (the `!s` flow).
	pub fn set_by_path(issue_path: &Path) -> std::io::Result<()> {
		let mut sel = Self::load();
		let Some(active) = sel.active() else {
			tracing::warn!("no active sprint; !s has no effect");
			return Ok(());
		};
		match active.view.issue_links().iter().find(|l| resolve(l).is_some_and(|p| p == issue_path)) {
			Some(link) => {
				sel.selections.insert(active.key.clone(), link.as_str().to_string());
				sel.save()
			}
			None => {
				tracing::warn!(path = %issue_path.display(), "issue not in active sprint; !s ignored");
				Ok(())
			}
		}
	}

	/// Whether an active sprint is resolvable without a network fetch — an urgent file
	/// exists, or a normal sprint's content is already cached.
	pub fn active_ready() -> bool {
		urgent_path().exists() || Self::load().normal.is_some()
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
}

/// The active sprint: its key and resolved view.
pub struct ActiveSprint {
	pub key: String,
	pub view: TaskView,
	/// Present only for urgent — the local file to remove when the view empties.
	urgent_file: Option<PathBuf>,
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
fn display_for_link(link: &IssueLink) -> String {
	resolve(link)
		.and_then(|p| p.strip_prefix(Local::issues_dir()).ok().map(|rel| rel.to_string_lossy().to_string()))
		.unwrap_or_else(|| format!("{}/{}#{}", link.owner(), link.repo(), link.number()))
}
