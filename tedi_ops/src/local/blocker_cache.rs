//! Milestone-derived blocker working-set state. App-state, not a primitive:
//! lives beside the fs machinery it depends on (moves to tedi_ops with it).

use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};
use tedi_core::{BlockerItem, Blockers, IssueLink, Marker, split_blockers};

use super::{FsReader, Local};
use crate::TaskView;

/// Milestone-derived blocker cache. Replaces the old `Revolver` (rotating list of paths).
///
/// The working set of issues is derived from a milestone description (default `1d`).
/// `current_index` selects which embedded issue is "current" for blocker operations.
/// The cached `milestone_description` is refreshed on `milestones edit` or `milestones healthcheck`.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct MilestoneBlockerCache {
	pub current_index: usize,
	pub milestone_description: String,
	/// Per-issue ref annotations: maps source issue URL → ref target issue URL.
	/// Issues with an entry here have a current blocker that is an issue ref,
	/// meaning they delegate work to the target. These are skipped during rotation.
	#[serde(default)]
	pub ref_targets: HashMap<String, String>,
}
impl MilestoneBlockerCache {
	fn cache_path() -> PathBuf {
		crate::paths::cache_file("milestone_blockers.json")
	}

	/// Load cache from disk. Returns None if no cache file exists.
	pub fn load() -> Option<Self> {
		let cache_path = Self::cache_path();
		let content = std::fs::read_to_string(&cache_path).ok()?;
		let mut cache: Self = serde_json::from_str(&content).ok()?;
		// Clamp index to valid range
		let count = cache.embedded_links().len();
		if count > 0 {
			cache.current_index = cache.current_index.min(count - 1);
		} else {
			cache.current_index = 0;
		}
		Some(cache)
	}

	pub fn save(&self) -> Result<(), std::io::Error> {
		let cache_path = Self::cache_path();
		if let Some(parent) = cache_path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let content = serde_json::to_string_pretty(self).expect("MilestoneBlockerCache serialization");
		std::fs::write(&cache_path, content)
	}

	/// Get all issue links from the cached milestone description.
	///
	/// Recognizes all supported formats: expanded title lines, shorthand refs (`o/r#123`),
	/// and bare issue URLs.
	pub fn embedded_links(&self) -> Vec<IssueLink> {
		let mut doc = TaskView::parse(&self.milestone_description);
		doc.resolve_bare_refs();
		doc.issue_links()
	}

	/// Get the currently selected issue link, if any.
	pub fn current_link(&self) -> Option<IssueLink> {
		let links = self.embedded_links();
		links.into_iter().nth(self.current_index)
	}

	/// Resolve an IssueLink to a local filesystem path.
	/// Uses synchronous filesystem operations (no network).
	pub fn resolve_link_to_path(link: &IssueLink) -> Option<PathBuf> {
		Local::find_by_number(link.repo_info(), link.number(), FsReader)
	}

	/// Get the current issue's local path, if resolvable.
	pub fn current_path(&self) -> Option<PathBuf> {
		let link = self.current_link()?;
		Self::resolve_link_to_path(&link)
	}

	/// Set the current index to point to the issue matching the given path.
	/// Called by `!s` flow when editing an individual issue file.
	pub fn set_by_path(issue_path: &Path) -> Result<(), std::io::Error> {
		let Some(mut cache) = Self::load() else {
			tracing::warn!("no milestone blocker cache exists; !s has no effect");
			return Ok(());
		};

		let links = cache.embedded_links();
		let found = links
			.iter()
			.enumerate()
			.find(|(_, link)| Self::resolve_link_to_path(link).map(|p| p == issue_path).unwrap_or(false));

		match found {
			Some((idx, _)) => {
				cache.current_index = idx;
				cache.save()
			}
			None => {
				tracing::warn!(
					path = %issue_path.display(),
					"issue not found in milestone cache; !s ignored"
				);
				Ok(())
			}
		}
	}

	/// Move current index by `delta` positions (circular), skipping ref-annotated issues.
	/// Returns the new current link.
	pub fn move_by(delta: isize) -> Result<IssueLink, String> {
		let mut cache = Self::load().ok_or("No milestone blocker cache. Run `todo milestones edit` first.")?;
		let links = cache.embedded_links();
		if links.is_empty() {
			return Err("No issues in milestone. Add issues to the milestone first.".into());
		}
		if links.len() == 1 {
			return Err("Only one issue in milestone. Nothing to move to.".into());
		}
		let len = links.len() as isize;

		// Advance, skipping issues that have ref_targets (they delegate to another issue).
		// Stop after at most `len` steps to avoid infinite loop if all are refs.
		let start = cache.current_index;
		for _ in 0..links.len() {
			cache.current_index = ((cache.current_index as isize + delta).rem_euclid(len)) as usize;
			let candidate = &links[cache.current_index];
			if !cache.ref_targets.contains_key(candidate.as_str()) {
				break;
			}
		}

		// If we looped back to start, everything is ref-annotated
		let link = links[cache.current_index].clone();
		if cache.ref_targets.contains_key(link.as_str()) && cache.current_index == start {
			return Err("All issues in milestone have blocker refs. Nothing to stop at.".into());
		}

		cache.save().map_err(|e| format!("Failed to save milestone cache: {e}"))?;
		Ok(link)
	}

	/// Set current to the embedded link whose display string matches `pattern` (case-insensitive).
	/// If exactly one link matches, selects it directly.
	/// If multiple links match (or no pattern given), opens fzf with matching entries pre-filled.
	pub fn set_by_pattern(pattern: Option<&str>) -> Result<IssueLink, String> {
		let mut cache = Self::load().ok_or("No milestone blocker cache. Run `todo milestones edit` first.")?;
		let links = cache.embedded_links();
		if links.is_empty() {
			return Err("No issues in milestone.".into());
		}
		let all_indexed: Vec<(usize, IssueLink)> = links.into_iter().enumerate().collect();
		let (matches, fzf_query): (Vec<(usize, IssueLink)>, &str) = match pattern {
			None => (all_indexed, ""),
			Some(p) => {
				let pattern_lower = p.to_lowercase();
				let filtered: Vec<_> = all_indexed
					.into_iter()
					.filter(|(_, link)| Self::display_for_link(link).to_lowercase().contains(&pattern_lower))
					.collect();
				(filtered, p)
			}
		};
		let (idx, link) = match matches.len() {
			0 => return Err(format!("No issue matching '{}' in milestone.", pattern.unwrap_or(""))),
			1 if pattern.is_some() => matches.into_iter().next().unwrap(),
			_ => {
				let displays: Vec<String> = matches.iter().map(|(_, link)| Self::display_for_link(link)).collect();
				let selected = Local::fzf_select(&displays, fzf_query).map_err(|e| format!("fzf failed: {e}"))?;
				matches
					.into_iter()
					.find(|(_, link)| Self::display_for_link(link) == selected)
					.ok_or_else(|| format!("fzf returned unknown entry: {selected}"))?
			}
		};
		cache.current_index = idx;
		cache.save().map_err(|e| format!("Failed to save milestone cache: {e}"))?;
		Ok(link)
	}

	/// Display string for a link: local relative path if resolvable, otherwise `owner/repo#number`.
	fn display_for_link(link: &IssueLink) -> String {
		Self::resolve_link_to_path(link)
			.and_then(|p| p.strip_prefix(Local::issues_dir()).ok().map(|rel| rel.to_string_lossy().to_string()))
			.unwrap_or_else(|| format!("{}/{}#{}", link.owner(), link.repo(), link.number()))
	}

	/// Write/update the cache from a milestone description.
	/// Preserves `current_index` if the previously-selected issue still exists.
	pub fn update_from_description(description: &str) -> Result<(), std::io::Error> {
		let old = Self::load();
		let old_link = old.as_ref().and_then(|c| c.current_link());
		let old_index = old.as_ref().map(|c| c.current_index).unwrap_or(0);

		let mut cache = MilestoneBlockerCache {
			current_index: 0,
			milestone_description: description.to_string(),
			ref_targets: HashMap::new(),
		};

		// Try to keep pointing at the same issue
		let new_links = cache.embedded_links();
		if let Some(old_link) = old_link {
			if let Some(pos) = new_links
				.iter()
				.position(|l| l.number() == old_link.number() && l.owner() == old_link.owner() && l.repo() == old_link.repo())
			{
				cache.current_index = pos;
			}
			// If old issue gone, index stays at 0
		} else if !new_links.is_empty() {
			cache.current_index = old_index.min(new_links.len() - 1);
		}

		cache.save()
	}

	/// Scan all embedded issues' blockers and populate `ref_targets`.
	/// An issue gets a ref_target entry if its current deepest blocker is an issue ref.
	pub fn refresh_ref_targets(&mut self) {
		self.ref_targets.clear();
		let links = self.embedded_links();
		for link in &links {
			let Some(path) = Self::resolve_link_to_path(link) else { continue };
			let Ok(content) = std::fs::read_to_string(&path) else { continue };
			let (_, blockers) = split_blockers(&content);
			let mut issue_ref = match blockers.deepest_issue_ref() {
				Some(r) => r,
				None => continue,
			};
			// Resolve bare refs using this issue's repo as context
			let ctx = format!("{}/{}", link.owner(), link.repo());
			issue_ref.resolve_with_context(&ctx);
			if let Some(target_link) = issue_ref.to_issue_link() {
				self.ref_targets.insert(link.as_str().to_string(), target_link.as_str().to_string());
			}
		}
	}

	/// Clean up dead refs: when an issue's blockers are empty, find any issues
	/// in `ref_targets` that point to it and pop the dead ref from each of them.
	/// Cascades recursively if popping also empties an issue.
	pub fn cleanup_dead_refs(&mut self, empty_issue_url: &str) -> Vec<String> {
		let mut cleaned = Vec::new();
		self.cleanup_dead_refs_inner(empty_issue_url, &mut cleaned);
		cleaned
	}

	fn cleanup_dead_refs_inner(&mut self, empty_issue_url: &str, cleaned: &mut Vec<String>) {
		// Find all issues whose ref_target points to the empty issue
		let pointing_at_empty: Vec<String> = self
			.ref_targets
			.iter()
			.filter(|(_, target)| target.as_str() == empty_issue_url)
			.map(|(source, _)| source.clone())
			.collect();

		for source_url in pointing_at_empty {
			// Remove the ref_target entry
			self.ref_targets.remove(&source_url);

			// Find the issue's local path and pop the dead ref
			let Some(source_link) = IssueLink::parse(&source_url) else { continue };
			let Some(path) = Self::resolve_link_to_path(&source_link) else { continue };
			let Ok(content) = std::fs::read_to_string(&path) else { continue };

			// Use split_blockers to isolate the blocker section, pop from it, then reconstruct
			let lines: Vec<&str> = content.lines().collect();
			let marker_idx = lines.iter().position(|line| matches!(Marker::decode(line), Some(Marker::BlockersSection(_))));
			let Some(marker_idx) = marker_idx else { continue };

			let before = &lines[..=marker_idx]; // includes marker line
			let after_marker = lines[marker_idx + 1..].join("\n");
			let mut blockers = Blockers::parse(&after_marker);

			let popped = pop_deepest(&mut blockers.items);
			if popped.is_none() {
				continue;
			}

			// Reconstruct: before (including marker) + blockers
			let mut new_content = before.join("\n");
			let blockers_str: String = String::from(&blockers);
			if !blockers_str.is_empty() {
				new_content.push('\n');
				new_content.push_str(&blockers_str);
			}
			new_content.push('\n');

			if std::fs::write(&path, &new_content).is_err() {
				continue;
			}
			cleaned.push(format!("{}/{}#{}", source_link.owner(), source_link.repo(), source_link.number()));

			// If this issue's blockers are now empty, cascade
			if blockers.is_empty() {
				self.cleanup_dead_refs_inner(&source_url, cleaned);
			}
		}
	}
}

/// Pop the deepest item from the tree (depth-first, rightmost).
fn pop_deepest(items: &mut Vec<BlockerItem>) -> Option<BlockerItem> {
	let last = items.last_mut()?;
	if let Some(popped) = pop_deepest(&mut last.children) {
		return Some(popped);
	}
	items.pop()
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_milestone_cache_serde_roundtrip() {
		let cache = MilestoneBlockerCache {
			current_index: 1,
			milestone_description: "# Sprint\n- [ ] Issue A <!-- @user https://github.com/o/r/issues/1 -->".to_string(),
			ref_targets: HashMap::new(),
		};
		let json = serde_json::to_string_pretty(&cache).unwrap();
		let deserialized: MilestoneBlockerCache = serde_json::from_str(&json).unwrap();
		assert_eq!(deserialized.current_index, cache.current_index);
		assert_eq!(deserialized.milestone_description, cache.milestone_description);
	}

	#[test]
	fn test_milestone_cache_embedded_links() {
		let cache = MilestoneBlockerCache {
			current_index: 0,
			milestone_description:
				"# Sprint\n\n- [ ] Issue A <!-- @user https://github.com/o/r/issues/1 -->\n\t# Blockers\n\t- task\n\n- [ ] Issue B <!-- @user https://github.com/o/r/issues/2 -->".to_string(),
			ref_targets: HashMap::new(),
		};
		let links = cache.embedded_links();
		assert_eq!(links.len(), 2);
		assert_eq!(links[0].number(), 1);
		assert_eq!(links[1].number(), 2);
	}

	#[test]
	fn test_milestone_cache_current_link() {
		let cache = MilestoneBlockerCache {
			current_index: 1,
			milestone_description: "- [ ] A <!-- @u https://github.com/o/r/issues/1 -->\n\n- [ ] B <!-- @u https://github.com/o/r/issues/2 -->".to_string(),
			ref_targets: HashMap::new(),
		};
		let link = cache.current_link().unwrap();
		assert_eq!(link.number(), 2);
	}

	#[test]
	fn test_milestone_cache_empty_description() {
		let cache = MilestoneBlockerCache {
			current_index: 0,
			milestone_description: "# Sprint\nNo issues yet".to_string(),
			ref_targets: HashMap::new(),
		};
		assert!(cache.embedded_links().is_empty());
		assert!(cache.current_link().is_none());
	}
}
