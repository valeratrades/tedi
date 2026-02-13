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
//! - Indent level (tabs) determines nesting depth
//! - Once a nested blocker appears under an item, no more comments can follow at that level

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::Marker;

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

/// Join text with blockers, appending the blockers section with proper spacing.
/// If blockers is empty, returns the original text unchanged.
pub fn join_with_blockers(text: &str, blockers: &BlockerSequence) -> String {
	if blockers.is_empty() {
		return text.to_string();
	}

	let mut result = text.trim_end().to_string();

	// Add blank line before blockers marker if there's content
	if !result.is_empty() {
		result.push_str("\n\n");
	}

	// Add blockers marker and content
	result.push_str("# Blockers\n");
	result.push_str(&blockers.serialize());

	result
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
	/// `indent` is the number of tabs expected for items at this level.
	fn parse_items_at_indent(lines: &[&str], pos: &mut usize, indent: usize, items: &mut Vec<BlockerItem>) {
		let indent_str = "\t".repeat(indent);

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

			// If the content starts with another tab, it's deeper than us
			if content.starts_with('\t') {
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
				let child_indent_str = "\t".repeat(child_indent);

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
					if child_content.starts_with('\t') {
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

	/// If `set_state` is `Pending`, write `issue_path` to the blocker cache and transition to `Applied`.
	pub fn ensure_set(&mut self, issue_path: &Path) {
		if matches!(self.set_state, Some(BlockerSetState::Pending)) {
			if let Err(e) = Revolver::set(issue_path.to_path_buf()) {
				tracing::warn!("failed to persist blocker selection: {e}");
				return;
			}
			tracing::info!(path = %issue_path.display(), "blocker selection persisted via !s");
			self.set_state = Some(BlockerSetState::Applied);
		}
	}

	/// XDG cache path for the blocker revolver state.
	pub fn cache_path() -> PathBuf {
		v_utils::xdg_cache_file!("blocker_revolver.json")
	}

	/// Serialize to text (indent-based format)
	pub fn serialize(&self) -> String {
		let mut lines = Vec::new();
		for item in &self.items {
			serialize_item(item, 0, &mut lines);
		}
		lines.join("\n")
	}

	/// Get all lines in order (for iteration).
	/// Returns (indent_level, line_content) pairs serialized as raw text.
	pub fn raw_lines(&self) -> Vec<String> {
		let mut result = Vec::new();
		for item in &self.items {
			collect_raw_lines(item, 0, &mut result);
		}
		result
	}
}

/// Rotating list of blocker issue sources with a current-selection pointer.
///
/// Supports cycling between multiple active issues via `toggle`.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Revolver {
	pub entries: Vec<PathBuf>,
	pub current_index: usize,
	pub previous: Option<PathBuf>,
}

impl Revolver {
	fn cache_path() -> PathBuf {
		BlockerSequence::cache_path()
	}

	/// Load revolver from cache. Returns None if no cache file exists.
	pub fn load() -> Option<Self> {
		let cache_path = Self::cache_path();
		let content = std::fs::read_to_string(&cache_path).ok()?;
		let mut revolver: Self = serde_json::from_str(&content).ok()?;
		// Filter out dead paths
		let was_current = revolver.entries.get(revolver.current_index).cloned();
		revolver.entries.retain(|p| p.exists());
		// Adjust index if entries were removed
		if revolver.entries.is_empty() {
			revolver.current_index = 0;
		} else if let Some(was) = was_current {
			// Try to keep pointing at the same entry
			revolver.current_index = revolver.entries.iter().position(|p| p == &was).unwrap_or(0);
		} else {
			revolver.current_index = revolver.current_index.min(revolver.entries.len() - 1);
		}
		// Also clean dead previous
		if let Some(ref prev) = revolver.previous {
			if !prev.exists() {
				revolver.previous = None;
			}
		}
		Some(revolver)
	}

	fn save(&self) -> Result<(), std::io::Error> {
		let cache_path = Self::cache_path();
		if let Some(parent) = cache_path.parent() {
			std::fs::create_dir_all(parent)?;
		}
		let content = serde_json::to_string_pretty(self).expect("Revolver serialization");
		std::fs::write(&cache_path, content)
	}

	/// Current issue path, if any.
	pub fn current_path(&self) -> Option<&Path> {
		self.entries.get(self.current_index).map(|p| p.as_path())
	}

	/// Set a single entry (clears revolver). Preserves old current as `previous`.
	pub fn set(path: PathBuf) -> Result<(), std::io::Error> {
		let previous = Self::load().and_then(|r| r.current_path().map(|p| p.to_path_buf()));
		let revolver = Self {
			entries: vec![path],
			current_index: 0,
			previous,
		};
		revolver.save()
	}

	/// Add an entry to the revolver without changing current selection.
	pub fn add(path: PathBuf) -> Result<(), std::io::Error> {
		let mut revolver = Self::load().unwrap_or(Self {
			entries: Vec::new(),
			current_index: 0,
			previous: None,
		});
		// Don't add duplicates
		if !revolver.entries.contains(&path) {
			revolver.entries.push(path);
		}
		revolver.save()
	}

	/// Advance to the next entry in the revolver.
	/// Returns the new current path, or an error message.
	pub fn toggle() -> Result<PathBuf, String> {
		let mut revolver = Self::load().ok_or("No blocker set. Use `todo blocker set <pattern>` first.")?;
		if revolver.entries.is_empty() {
			return Err("No blocker set. Use `todo blocker set <pattern>` first.".into());
		}
		match revolver.entries.len() {
			0 => unreachable!(),
			1 => {
				// Pull previous into revolver to make a 2-slot rotation
				let prev = revolver.previous.take().ok_or("Nothing to toggle to. Use `set-more` to add issues to the rotation.")?;
				revolver.entries.push(prev);
				revolver.current_index = 1;
			}
			_ => {
				revolver.current_index = (revolver.current_index + 1) % revolver.entries.len();
			}
		}
		let current = revolver.entries[revolver.current_index].clone();
		revolver.save().map_err(|e| format!("Failed to save revolver: {e}"))?;
		Ok(current)
	}
}

impl PartialEq for BlockerSequence {
	fn eq(&self, other: &Self) -> bool {
		self.items == other.items
	}
}

/// Serialize a single item at the given indent depth.
fn serialize_item(item: &BlockerItem, indent: usize, lines: &mut Vec<String>) {
	let indent_str = "\t".repeat(indent);
	lines.push(format!("{indent_str}- {}", item.text));
	for comment in &item.comments {
		lines.push(format!("{indent_str}\t{comment}"));
	}
	for child in &item.children {
		serialize_item(child, indent + 1, lines);
	}
}

/// Collect raw lines for a single item.
fn collect_raw_lines(item: &BlockerItem, indent: usize, result: &mut Vec<String>) {
	let indent_str = "\t".repeat(indent);
	result.push(format!("{indent_str}- {}", item.text));
	for comment in &item.comments {
		result.push(format!("{indent_str}\t{comment}"));
	}
	for child in &item.children {
		collect_raw_lines(child, indent + 1, result);
	}
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
		let content = "- task 1\n\tcomment on task 1\n\tanother comment\n- task 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 2);
		assert_eq!(seq.items[0].text, "task 1");
		assert_eq!(seq.items[0].comments, vec!["comment on task 1", "another comment"]);
		assert_eq!(seq.items[1].text, "task 2");
		assert!(seq.items[1].comments.is_empty());
	}

	#[test]
	fn test_parse_nested_items() {
		let content = "- parent\n\t- child 1\n\t- child 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 1);
		assert_eq!(seq.items[0].text, "parent");
		assert_eq!(seq.items[0].children.len(), 2);
		assert_eq!(seq.items[0].children[0].text, "child 1");
		assert_eq!(seq.items[0].children[1].text, "child 2");
	}

	#[test]
	fn test_parse_comments_then_children() {
		let content = "- parent\n\tcomment\n\t- child";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items.len(), 1);
		assert_eq!(seq.items[0].comments, vec!["comment"]);
		assert_eq!(seq.items[0].children.len(), 1);
		assert_eq!(seq.items[0].children[0].text, "child");
	}

	#[test]
	fn test_parse_deeply_nested() {
		let content = "- level 0\n\t- level 1\n\t\t- level 2\n\t\t\t- level 3";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.items[0].text, "level 0");
		assert_eq!(seq.items[0].children[0].text, "level 1");
		assert_eq!(seq.items[0].children[0].children[0].text, "level 2");
		assert_eq!(seq.items[0].children[0].children[0].children[0].text, "level 3");
	}

	#[test]
	fn test_serialize_roundtrip() {
		let content = "- task 1\n\tcomment\n\t- child\n- task 2";
		let seq = BlockerSequence::parse(content);
		let serialized = seq.serialize();
		assert_eq!(serialized, content);
	}

	#[test]
	fn test_serialize_nested_roundtrip() {
		let content = "- parent\n\tcomment on parent\n\t- child 1\n\t\tcomment on child\n\t- child 2\n- sibling";
		let seq = BlockerSequence::parse(content);
		let serialized = seq.serialize();
		assert_eq!(serialized, content);
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
		insta::assert_snapshot!(format!("content: {content:?}\nblockers: {}", blockers.serialize()), @r#"
		content: "Description here\n"
		blockers: - task 1
		- task 2
		"#);
	}

	#[test]
	fn test_join_with_blockers_empty() {
		let text = "Some content";
		let blockers = BlockerSequence::default();
		let result = join_with_blockers(text, &blockers);
		assert_eq!(result, text);
	}

	#[test]
	fn test_join_with_blockers_non_empty() {
		let text = "Description";
		let blockers = BlockerSequence::parse("- task 1\n- task 2");
		let result = join_with_blockers(text, &blockers);
		assert_eq!(result, "Description\n\n# Blockers\n- task 1\n- task 2");
	}

	#[test]
	fn test_split_join_roundtrip() {
		let original = "Body text\n\n# Blockers\n- task 1\n- task 2";
		let (content, blockers) = split_blockers(original);
		let rejoined = join_with_blockers(&content, &blockers);
		let (content2, blockers2) = split_blockers(&rejoined);
		insta::assert_snapshot!(
			format!("content: {content2:?}\nblockers: {}", blockers2.serialize()),
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
		let serialized = seq.serialize();
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
	fn test_revolver_serde_roundtrip() {
		let revolver = Revolver {
			entries: vec![PathBuf::from("/a/b.md"), PathBuf::from("/c/d.md")],
			current_index: 1,
			previous: Some(PathBuf::from("/old.md")),
		};
		let json = serde_json::to_string_pretty(&revolver).unwrap();
		let deserialized: Revolver = serde_json::from_str(&json).unwrap();
		assert_eq!(deserialized.entries, revolver.entries);
		assert_eq!(deserialized.current_index, revolver.current_index);
		assert_eq!(deserialized.previous, revolver.previous);
	}

	#[test]
	fn test_revolver_current_path() {
		let revolver = Revolver {
			entries: vec![PathBuf::from("/a.md"), PathBuf::from("/b.md")],
			current_index: 0,
			previous: None,
		};
		assert_eq!(revolver.current_path(), Some(Path::new("/a.md")));

		let revolver2 = Revolver {
			entries: vec![PathBuf::from("/a.md"), PathBuf::from("/b.md")],
			current_index: 1,
			previous: None,
		};
		assert_eq!(revolver2.current_path(), Some(Path::new("/b.md")));
	}

	#[test]
	fn test_revolver_current_path_empty() {
		let revolver = Revolver {
			entries: vec![],
			current_index: 0,
			previous: None,
		};
		assert_eq!(revolver.current_path(), None);
	}
}
