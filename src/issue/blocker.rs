//! Pure blocker types for issue files.
//!
//! This module contains the core data structures for blockers without I/O dependencies.
//! These types can be used in both the library and binary contexts.
//!
//! # Format
//!
//! Blockers use indent-based nesting:
//! - Lines starting with `- ` are blocker items
//! - All other non-empty lines are comments (attached to the preceding item)
//! - Indent level (2 spaces) determines nesting depth
//! - Once a nested blocker appears under an item, no more comments can follow at that level

use std::{
	collections::HashMap,
	path::{Path, PathBuf},
};

use serde::{Deserialize, Serialize};

use super::{Events, Marker, OwnedEvent, OwnedTag, OwnedTagEnd};

/// Split text at the blockers marker, returning (content_before, blockers).
/// If no blockers marker is found, returns the original text and an empty BlockerSequence.
pub fn split_blockers(text: &str) -> (String, BlockerSequence) {
	let lines: Vec<&str> = text.lines().collect();

	// Find the blockers marker
	let marker_idx = lines.iter().position(|line| matches!(Marker::decode(line), Some(Marker::BlockersSection(_))));

	match marker_idx {
		Some(idx) => {
			// Content before the marker
			let before: String = lines[..idx].join("\n");
			// Content after the marker (blocker lines)
			(before, BlockerSequence::parse(&lines[idx + 1..].join("\n")))
		}
		None => (text.to_string(), BlockerSequence::default()),
	}
}

/// Join body events with blockers, appending the blockers section.
/// If blockers is empty, returns the body events unchanged.
pub fn join_with_blockers(body_events: &[OwnedEvent], blockers: &BlockerSequence) -> Events {
	//NB: DO NOT CHANGE Output Type
	let mut events = body_events.to_vec();
	if blockers.is_empty() {
		return events.into();
	}
	// Blockers header
	events.push(OwnedEvent::Start(OwnedTag::Heading {
		level: pulldown_cmark::HeadingLevel::H1,
		id: None,
		classes: Vec::new(),
		attrs: Vec::new(),
	}));
	events.push(OwnedEvent::Text("Blockers".to_string()));
	events.push(OwnedEvent::End(OwnedTagEnd::Heading(pulldown_cmark::HeadingLevel::H1)));
	events.extend(blockers.to_events());
	events.into()
}

/// A single blocker item with optional comments and nested sub-blockers.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BlockerItem {
	pub text: String,
	pub comments: Vec<String>,
	pub children: Vec<BlockerItem>,
}
impl BlockerItem {
	/// Try to parse this item's text as an issue reference.
	pub fn issue_ref(&self) -> Option<super::issue_ref::IssueRef> {
		let trimmed = self.text.trim();
		if trimmed.is_empty() || trimmed.contains(' ') {
			return None;
		}
		super::issue_ref::IssueRef::parse_word(trimmed)
	}
}

/// Whether this blocker block has been marked as the active selection via `!s`.
#[derive(Clone, Copy, Debug)]
pub enum BlockerSetState {
	/// Parsed `!s` but not yet persisted to cache.
	Pending,
	/// Successfully persisted to cache.
	Applied,
}

/// A sequence of blocker items.
#[derive(Clone, Debug, Default)]
pub struct BlockerSequence {
	pub items: Vec<BlockerItem>,
	/// Transient state for `!s` marker. Not serialized, not compared.
	pub set_state: Option<BlockerSetState>,
}
impl BlockerSequence {
	/// Parse raw text content into a BlockerSequence.
	pub fn parse(content: &str) -> Self {
		let lines: Vec<&str> = content.lines().collect();
		Self::build_from_lines(&lines)
	}

	/// Build tree from raw lines, using indentation for nesting.
	fn build_from_lines(lines: &[&str]) -> Self {
		let mut root_items: Vec<BlockerItem> = Vec::new();
		Self::parse_items_at_indent(lines, &mut 0, 0, &mut root_items);
		Self {
			items: root_items,
			..Default::default()
		}
	}

	/// Parse items at a given indent level, advancing `pos` through the lines.
	/// `indent` is the number of 2-space units expected for items at this level.
	fn parse_items_at_indent(lines: &[&str], pos: &mut usize, indent: usize, items: &mut Vec<BlockerItem>) {
		let indent_str = "  ".repeat(indent);

		while *pos < lines.len() {
			let line = lines[*pos];

			// Skip empty lines
			if line.trim().is_empty() {
				*pos += 1;
				continue;
			}

			// Check if this line is at our indent level
			let Some(content) = line.strip_prefix(&indent_str) else {
				// Less indented than us — return to parent
				break;
			};

			// If the content starts with more indentation, it's deeper than us
			if content.starts_with("  ") {
				// This belongs to a deeper level — but we don't have a parent item to attach to
				// at this point, so just break (shouldn't happen with well-formed input)
				break;
			}

			// Check if this is a blocker item (starts with `- `)
			if let Some(item_text) = content.strip_prefix("- ") {
				*pos += 1;
				let mut item = BlockerItem {
					text: item_text.to_string(),
					comments: Vec::new(),
					children: Vec::new(),
				};

				// Collect comments and children at the next indent level
				let child_indent = indent + 1;
				let child_indent_str = "  ".repeat(child_indent);

				while *pos < lines.len() {
					let next_line = lines[*pos];

					// Skip empty lines
					if next_line.trim().is_empty() {
						*pos += 1;
						continue;
					}

					// Check if next line is at child indent level
					let Some(child_content) = next_line.strip_prefix(&child_indent_str) else {
						// Not at child level — done with this item
						break;
					};

					// If deeper than child level, it belongs to deeper nesting
					if child_content.starts_with("  ") {
						// Deeper than immediate child — break, let recursive call handle it
						break;
					}

					if child_content.starts_with("- ") {
						// Nested blocker — parse all children at this indent level
						Self::parse_items_at_indent(lines, pos, child_indent, &mut item.children);
						// After parsing children, we continue to check for more at our level
						// but no more comments are allowed (children already started)
					} else {
						// Comment line
						if !item.children.is_empty() {
							// Comments after nested blockers are not allowed at this level;
							// treat as comment on the last child? No — the spec says
							// "once we get a first nested blocker, there can be no comments at the same level"
							// So we panic/warn. For robustness, let's just skip with a warning.
							eprintln!("Warning: comment after nested blockers at indent level {child_indent}: {child_content:?}");
						}
						item.comments.push(child_content.to_string());
						*pos += 1;
					}
				}

				items.push(item);
			} else {
				// Not a blocker item and not deeper — this is a comment line without a parent item.
				// At root level this could be a bare comment. Just skip it with a warning.
				eprintln!("Warning: orphan comment (no preceding blocker item): {content:?}");
				*pos += 1;
			}
		}
	}

	/// Check if the sequence has no blocker items
	pub fn is_empty(&self) -> bool {
		self.items.is_empty()
	}

	/// Walk the path to the current (deepest, rightmost) blocker item,
	/// returning the deepest issue ref found along that path.
	pub fn deepest_issue_ref(&self) -> Option<super::issue_ref::IssueRef> {
		fn walk(items: &[BlockerItem]) -> Option<super::issue_ref::IssueRef> {
			let last = items.last()?;
			let mine = last.issue_ref();
			if last.children.is_empty() {
				return mine;
			}
			walk(&last.children).or(mine)
		}
		walk(&self.items)
	}

	/// If `set_state` is `Pending`, find the issue in the milestone cache and set `current_index`.
	/// Transitions to `Applied` on success.
	pub fn ensure_set(&mut self, issue_path: &Path) {
		if matches!(self.set_state, Some(BlockerSetState::Pending)) {
			if let Err(e) = MilestoneBlockerCache::set_by_path(issue_path) {
				tracing::warn!("failed to persist blocker selection: {e}");
				return;
			}
			tracing::info!(path = %issue_path.display(), "blocker selection persisted via !s");
			self.set_state = Some(BlockerSetState::Applied);
		}
	}

	/// Build cmark event stream representing this blocker sequence as a list.
	pub fn to_events(&self) -> Events {
		if self.items.is_empty() {
			return Events::default();
		}
		let mut events = Vec::new();
		events.push(OwnedEvent::Start(OwnedTag::List(None)));
		for item in &self.items {
			item_to_events(item, &mut events);
		}
		events.push(OwnedEvent::End(OwnedTagEnd::List(false)));
		events.into()
	}
}

impl From<&BlockerSequence> for String {
	fn from(seq: &BlockerSequence) -> Self {
		let mut out = String::new();
		for item in &seq.items {
			render_item_text(&mut out, item, 0);
		}
		// render_item_text appends \n after each item; trim the trailing one
		// so the output doesn't include a spurious trailing newline
		if out.ends_with('\n') {
			out.pop();
		}
		out
	}
}

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
		v_utils::xdg_cache_file!("milestone_blockers.json")
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
	pub fn embedded_links(&self) -> Vec<super::IssueLink> {
		let mut doc = super::MilestoneDoc::parse(&self.milestone_description);
		doc.resolve_bare_refs();
		doc.issue_links()
	}

	/// Get the currently selected issue link, if any.
	pub fn current_link(&self) -> Option<super::IssueLink> {
		let links = self.embedded_links();
		links.into_iter().nth(self.current_index)
	}

	/// Resolve an IssueLink to a local filesystem path.
	/// Uses synchronous filesystem operations (no network).
	pub fn resolve_link_to_path(link: &super::IssueLink) -> Option<PathBuf> {
		use crate::local::{FsReader, Local};
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
	pub fn move_by(delta: isize) -> Result<super::IssueLink, String> {
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
	pub fn set_by_pattern(pattern: Option<&str>) -> Result<super::IssueLink, String> {
		let mut cache = Self::load().ok_or("No milestone blocker cache. Run `todo milestones edit` first.")?;
		let links = cache.embedded_links();
		if links.is_empty() {
			return Err("No issues in milestone.".into());
		}
		let all_indexed: Vec<(usize, super::IssueLink)> = links.into_iter().enumerate().collect();
		let (matches, fzf_query): (Vec<(usize, super::IssueLink)>, &str) = match pattern {
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
				let selected = crate::local::Local::fzf_select(&displays, fzf_query).map_err(|e| format!("fzf failed: {e}"))?;
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
	fn display_for_link(link: &super::IssueLink) -> String {
		Self::resolve_link_to_path(link)
			.and_then(|p| p.strip_prefix(crate::local::Local::issues_dir()).ok().map(|rel| rel.to_string_lossy().to_string()))
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
			let Some(source_link) = super::IssueLink::parse(&source_url) else { continue };
			let Some(path) = Self::resolve_link_to_path(&source_link) else { continue };
			let Ok(content) = std::fs::read_to_string(&path) else { continue };

			// Use split_blockers to isolate the blocker section, pop from it, then reconstruct
			let lines: Vec<&str> = content.lines().collect();
			let marker_idx = lines.iter().position(|line| matches!(Marker::decode(line), Some(Marker::BlockersSection(_))));
			let Some(marker_idx) = marker_idx else { continue };

			let before = &lines[..=marker_idx]; // includes marker line
			let after_marker = lines[marker_idx + 1..].join("\n");
			let mut blockers = BlockerSequence::parse(&after_marker);

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

/// Render a blocker item as raw text lines with indentation.
/// Avoids pulldown_cmark_to_cmark which escapes `#` at line start.
fn render_item_text(out: &mut String, item: &BlockerItem, indent: usize) {
	let prefix = "  ".repeat(indent);
	out.push_str(&prefix);
	out.push_str("- ");
	out.push_str(&item.text);
	out.push('\n');
	for comment in &item.comments {
		out.push_str(&prefix);
		out.push_str("  ");
		out.push_str(comment);
		out.push('\n');
	}
	for child in &item.children {
		render_item_text(out, child, indent + 1);
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

impl PartialEq for BlockerSequence {
	fn eq(&self, other: &Self) -> bool {
		self.items == other.items
	}
}

/// Parse inline markdown text into events, stripping the paragraph wrapper.
///
/// pulldown_cmark wraps bare text in Start(Paragraph)..End(Paragraph).
/// We strip that wrapper since the caller provides block-level context.
fn inline_text_to_events(text: &str, events: &mut Vec<OwnedEvent>) {
	let parsed = super::Events::parse(text);
	let slice: &[OwnedEvent] = &parsed;
	// Strip leading Start(Paragraph) and trailing End(Paragraph)
	let inner = match slice {
		[OwnedEvent::Start(OwnedTag::Paragraph), inner @ .., OwnedEvent::End(OwnedTagEnd::Paragraph)] => inner,
		other => other,
	};
	events.extend(inner.iter().cloned());
}

/// Build events for a single blocker item (including comments and children).
fn item_to_events(item: &BlockerItem, events: &mut Vec<OwnedEvent>) {
	events.push(OwnedEvent::Start(OwnedTag::Item));
	inline_text_to_events(&item.text, events);
	for comment in &item.comments {
		events.push(OwnedEvent::SoftBreak);
		inline_text_to_events(comment, events);
	}
	if !item.children.is_empty() {
		events.push(OwnedEvent::Start(OwnedTag::List(None)));
		for child in &item.children {
			item_to_events(child, events);
		}
		events.push(OwnedEvent::End(OwnedTagEnd::List(false)));
	}
	events.push(OwnedEvent::End(OwnedTagEnd::Item));
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_simple_items() {
		let seq = BlockerSequence::parse("- task 1\n- task 2");
		assert_eq!(seq.items.len(), 2);
		assert_eq!(seq.items[0].text, "task 1");
		assert_eq!(seq.items[1].text, "task 2");
	}

	#[test]
	fn test_parse_items_with_comments() {
		let content = "- task 1\n  comment on task 1\n  another comment\n- task 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 2);
		assert_eq!(seq.items[0].text, "task 1");
		assert_eq!(seq.items[0].comments, vec!["comment on task 1", "another comment"]);
		assert_eq!(seq.items[1].text, "task 2");
		assert!(seq.items[1].comments.is_empty());
	}

	#[test]
	fn test_parse_nested_items() {
		let content = "- parent\n  - child 1\n  - child 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 1);
		assert_eq!(seq.items[0].text, "parent");
		assert_eq!(seq.items[0].children.len(), 2);
		assert_eq!(seq.items[0].children[0].text, "child 1");
		assert_eq!(seq.items[0].children[1].text, "child 2");
	}

	#[test]
	fn test_parse_comments_then_children() {
		let content = "- parent\n  comment\n  - child";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 1);
		assert_eq!(seq.items[0].comments, vec!["comment"]);
		assert_eq!(seq.items[0].children.len(), 1);
		assert_eq!(seq.items[0].children[0].text, "child");
	}

	#[test]
	fn test_parse_deeply_nested() {
		let content = "- level 0\n  - level 1\n    - level 2\n      - level 3";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items[0].text, "level 0");
		assert_eq!(seq.items[0].children[0].text, "level 1");
		assert_eq!(seq.items[0].children[0].children[0].text, "level 2");
		assert_eq!(seq.items[0].children[0].children[0].children[0].text, "level 3");
	}

	#[test]
	fn test_serialize_roundtrip() {
		let content = "- task 1\n  comment\n  - child\n- task 2";
		let seq = BlockerSequence::parse(content);
		let serialized = String::from(&seq);
		// Roundtrip: parse back and compare structure
		let reparsed = BlockerSequence::parse(&serialized);
		assert_eq!(seq, reparsed);
	}

	#[test]
	fn test_serialize_nested_roundtrip() {
		let content = "- parent\n  comment on parent\n  - child 1\n    comment on child\n  - child 2\n- sibling";
		let seq = BlockerSequence::parse(content);
		let serialized = String::from(&seq);
		// Roundtrip: parse back and compare structure
		let reparsed = BlockerSequence::parse(&serialized);
		assert_eq!(seq, reparsed);
	}

	#[test]
	fn test_split_blockers_no_marker() {
		let text = "Some content\nwithout blockers";
		let (content, blockers) = split_blockers(text);
		assert_eq!(content, text);
		assert!(blockers.is_empty());
	}

	#[test]
	fn test_split_blockers_with_marker() {
		let text = "Description here\n\n# Blockers\n- task 1\n- task 2";
		let (content, blockers) = split_blockers(text);
		insta::assert_snapshot!(format!("content: {content:?}\nblockers: {}", String::from(&blockers)), @r#"
		content: "Description here\n"
		blockers: - task 1
		- task 2
		"#);
	}

	#[test]
	fn test_join_with_blockers_empty() {
		let body = Events::parse("Some content");
		let blockers = BlockerSequence::default();
		let result: String = join_with_blockers(&body, &blockers).into();
		insta::assert_snapshot!(result, @"Some content");
	}

	#[test]
	fn test_join_with_blockers_non_empty() {
		let body = Events::parse("Description");
		let blockers = BlockerSequence::parse("- task 1\n- task 2");
		let result: String = join_with_blockers(&body, &blockers).into();
		insta::assert_snapshot!(result, @"
		Description
		# Blockers
		- task 1
		- task 2
		");
	}

	#[test]
	fn test_split_join_roundtrip() {
		let original = "Body text\n\n# Blockers\n- task 1\n- task 2";
		let (content, blockers) = split_blockers(original);
		let body_events = Events::parse(&content);
		let rejoined: String = join_with_blockers(&body_events, &blockers).into();
		let (content2, blockers2) = split_blockers(&rejoined);
		insta::assert_snapshot!(
			format!("content: {content2:?}\nblockers: {}", String::from(&blockers2)),
			@r#"
		content: "Body text"
		blockers: - task 1
		- task 2
		"#
		);
	}

	#[test]
	fn test_blocker_sequence_strips_empty_lines() {
		let content = "\n\n- task 1\n- task 2\n\n\n";
		let seq = BlockerSequence::parse(content);
		let serialized = String::from(&seq);
		insta::assert_snapshot!(serialized, @r"
  - task 1
  - task 2
  ");
	}

	#[test]
	fn test_is_empty() {
		let empty = BlockerSequence::default();
		let with_content = BlockerSequence::parse("- task");
		insta::assert_snapshot!(
			format!("default: {}\nwith_content: {}", empty.is_empty(), with_content.is_empty()),
			@"
		default: true
		with_content: false
		"
		);
	}

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

	#[test]
	fn test_deepest_issue_ref_simple() {
		let seq = BlockerSequence::parse("- o/r#42");
		let r = seq.deepest_issue_ref().unwrap();
		assert_eq!(r.to_string(), "o/r#42");
	}

	#[test]
	fn test_deepest_issue_ref_nested() {
		let seq = BlockerSequence::parse("- parent\n  - o/r#99");
		let r = seq.deepest_issue_ref().unwrap();
		assert_eq!(r.to_string(), "o/r#99");
	}

	#[test]
	fn test_deepest_issue_ref_none_for_text() {
		let seq = BlockerSequence::parse("- some plain task");
		assert!(seq.deepest_issue_ref().is_none());
	}

	#[test]
	fn test_deepest_issue_ref_parent_ref_with_text_child() {
		// Parent is a ref, child is text — deepest ref is the parent
		let seq = BlockerSequence::parse("- o/r#10\n  - plain task");
		let r = seq.deepest_issue_ref().unwrap();
		assert_eq!(r.to_string(), "o/r#10");
	}

	#[test]
	fn test_issue_ref_blocker_roundtrip_no_escaping() {
		// Verify that `#` in blocker text does not get escaped on roundtrip
		let content = "- #13\n- #42";
		let seq = BlockerSequence::parse(content);
		let serialized = String::from(&seq);
		let reparsed = BlockerSequence::parse(&serialized);
		assert_eq!(seq, reparsed, "structural roundtrip must preserve blocker items");
		insta::assert_snapshot!(serialized, @"
		- #13
		- #42
		");
	}
}
