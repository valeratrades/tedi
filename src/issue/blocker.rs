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

use std::path::{Path, PathBuf};

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
		seq.to_events().into()
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

	/// Advance to the next embedded issue. Returns the new current link.
	pub fn toggle() -> Result<super::IssueLink, String> {
		let mut cache = Self::load().ok_or("No milestone blocker cache. Run `todo milestones edit` first.")?;
		let links = cache.embedded_links();
		if links.is_empty() {
			return Err("No issues in milestone. Add issues to the milestone first.".into());
		}
		if links.len() == 1 {
			return Err("Only one issue in milestone. Nothing to toggle to.".into());
		}
		cache.current_index = (cache.current_index + 1) % links.len();
		let link = links[cache.current_index].clone();
		cache.save().map_err(|e| format!("Failed to save milestone cache: {e}"))?;
		Ok(link)
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
}

impl PartialEq for BlockerSequence {
	fn eq(&self, other: &Self) -> bool {
		self.items == other.items
	}
}

/// Build events for a single blocker item (including comments and children).
fn item_to_events(item: &BlockerItem, events: &mut Vec<OwnedEvent>) {
	events.push(OwnedEvent::Start(OwnedTag::Item));
	events.push(OwnedEvent::Text(item.text.clone()));
	for comment in &item.comments {
		events.push(OwnedEvent::SoftBreak);
		events.push(OwnedEvent::Text(comment.clone()));
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
		content: "Body text\n"
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
		};
		let link = cache.current_link().unwrap();
		assert_eq!(link.number(), 2);
	}

	#[test]
	fn test_milestone_cache_empty_description() {
		let cache = MilestoneBlockerCache {
			current_index: 0,
			milestone_description: "# Sprint\nNo issues yet".to_string(),
		};
		assert!(cache.embedded_links().is_empty());
		assert!(cache.current_link().is_none());
	}
}
