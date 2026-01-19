//! Extended stack operations for blocker management.
//!
//! This module provides extension methods on BlockerSequence from the library:
//! - `add`: Push a new blocker onto the stack
//! - `pop`: Remove the last blocker from the stack
//! - `current`: Get the current (last) blocker with its parent context
//!
//! # Hierarchy Model
//!
//! Blockers are organized in a tree structure where headers define sections.
//! Each header level creates a nested scope that "owns" its contents:
//! - H1 headers own everything until the next H1 (or end)
//! - H2 headers own everything until the next H2/H1 (or end)
//! - Items at the root level (before any header) belong to an implicit H1
//!
//! Two rendering modes are available:
//! - `headers`: Traditional flat format with `# Header` lines
//! - `nested`: Indented format showing hierarchy visually

use tedi::{BlockerItem, BlockerSequence};

/// Extension trait for BlockerSequence with additional operations
pub trait BlockerSequenceExt {
	/// Get the current (last) blocker item
	fn current(&self) -> Option<&BlockerItem>;

	/// Get the current blocker with context prepended (joined by ": ").
	fn current_with_context(&self, ownership_hierarchy: &[String]) -> Option<String>;

	/// Add a content line to the blocker sequence (at current position)
	fn add(&mut self, text: &str);

	/// Remove the last content line from the blocker sequence.
	fn pop(&mut self) -> Option<String>;
}

impl BlockerSequenceExt for BlockerSequence {
	fn current(&self) -> Option<&BlockerItem> {
		last_item(self)
	}

	fn current_with_context(&self, ownership_hierarchy: &[String]) -> Option<String> {
		let current = self.current()?;

		// Get path of headers to the current item
		let path = path_to_last(self);

		// Build final output: ownership hierarchy + blocker headers + task
		let mut parts: Vec<&str> = ownership_hierarchy.iter().map(|s| s.as_str()).collect();
		parts.extend(path.iter().map(|s| s.as_str()));

		if parts.is_empty() {
			Some(current.text.clone())
		} else {
			Some(format!("{}: {}", parts.join(": "), current.text))
		}
	}

	fn add(&mut self, text: &str) {
		let item = BlockerItem {
			text: text.to_string(),
			comments: Vec::new(),
		};
		// Add to the deepest current section
		add_item_to_current(self, item);
	}

	fn pop(&mut self) -> Option<String> {
		pop_last(self).map(|item| item.text)
	}
}

/// Get the last item in the tree (depth-first, rightmost)
fn last_item(seq: &BlockerSequence) -> Option<&BlockerItem> {
	// Check children first (rightmost child's last item)
	for child in seq.children.iter().rev() {
		if let Some(item) = last_item(child) {
			return Some(item);
		}
	}
	// Then check our own items
	seq.items.last()
}

/// Get the path of headers leading to the last item
fn path_to_last(seq: &BlockerSequence) -> Vec<String> {
	let mut path = Vec::new();
	path_to_last_inner(seq, &mut path);
	path
}

fn path_to_last_inner(seq: &BlockerSequence, path: &mut Vec<String>) {
	// Check if any child has content
	for child in seq.children.iter().rev() {
		if !child.is_empty() {
			if !seq.title.is_empty() {
				path.push(seq.title.clone());
			}
			path_to_last_inner(child, path);
			return;
		}
	}
	// If we have items, add our title
	if !seq.items.is_empty() && !seq.title.is_empty() {
		path.push(seq.title.clone());
	}
}

/// Pop the last item from the tree
fn pop_last(seq: &mut BlockerSequence) -> Option<BlockerItem> {
	// Try children first (rightmost)
	for child in seq.children.iter_mut().rev() {
		if let Some(item) = pop_last(child) {
			return Some(item);
		}
	}
	// Then our own items
	seq.items.pop()
}

fn add_item_to_current(seq: &mut BlockerSequence, item: BlockerItem) {
	// Find the deepest non-empty section and add there
	// If all empty, add to root
	fn add_to_deepest(seq: &mut BlockerSequence, item: BlockerItem) -> bool {
		// Try children first (rightmost)
		for child in seq.children.iter_mut().rev() {
			if add_to_deepest(child, item.clone()) {
				return true;
			}
		}
		// If this sequence has items or is non-root, add here
		if !seq.items.is_empty() || seq.level > 0 {
			seq.items.push(item);
			return true;
		}
		false
	}

	if !add_to_deepest(seq, item.clone()) {
		// Nothing found, add to root
		seq.items.push(item);
	}
}

#[cfg(test)]
mod tests {
	use tedi::DisplayFormat;

	use super::*;

	#[test]
	fn test_parse_and_serialize() {
		let content = "# Header\n- task 1\n\tcomment\n- task 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.serialize(DisplayFormat::Headers), content);
	}

	#[test]
	fn test_current() {
		let seq = BlockerSequence::parse("- task 1\n- task 2\n- task 3");
		assert_eq!(seq.current().map(|i| i.text.as_str()), Some("task 3"));
	}

	#[test]
	fn test_current_skips_comments() {
		let seq = BlockerSequence::parse("- task 1\n\tcomment\n- task 2\n\tanother comment");
		assert_eq!(seq.current().map(|i| i.text.as_str()), Some("task 2"));
		// Comments should be attached to the item
		assert_eq!(seq.current().map(|i| i.comments.len()), Some(1));
	}

	#[test]
	fn test_current_with_context_no_hierarchy() {
		let seq = BlockerSequence::parse("# Phase 1\n- task 1\n# Phase 2\n- task 2");
		assert_eq!(seq.current_with_context(&[]), Some("Phase 2: task 2".to_string()));
	}

	#[test]
	fn test_current_with_context_with_hierarchy() {
		let seq = BlockerSequence::parse("# Phase 1\n- task 1");
		let hierarchy = vec!["project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("project: Phase 1: task 1".to_string()));
	}

	#[test]
	fn test_current_with_context_multi_level_hierarchy() {
		let seq = BlockerSequence::parse("# Section\n- task");
		let hierarchy = vec!["workspace".to_string(), "project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("workspace: project: Section: task".to_string()));
	}

	#[test]
	fn test_nested_headers() {
		let content = "# H1\n## H2\n- task under H2\n# Another H1\n- task under another H1";
		let seq = BlockerSequence::parse(content);

		// Current should be "task under another H1" with path "Another H1"
		assert_eq!(seq.current_with_context(&[]), Some("Another H1: task under another H1".to_string()));
	}

	#[test]
	fn test_deeply_nested() {
		let content = "# Level 1\n## Level 2\n### Level 3\n- deep task";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("Level 1: Level 2: Level 3: deep task".to_string()));
	}

	#[test]
	fn test_add() {
		let mut seq = BlockerSequence::parse("- task 1");
		seq.add("task 2");
		assert_eq!(seq.serialize(DisplayFormat::Headers), "- task 1\n- task 2");
	}

	#[test]
	fn test_add_to_section() {
		let mut seq = BlockerSequence::parse("# Section\n- task 1");
		seq.add("task 2");
		// Should add under the same section
		assert_eq!(seq.serialize(DisplayFormat::Headers), "# Section\n- task 1\n- task 2");
	}

	#[test]
	fn test_pop() {
		let mut seq = BlockerSequence::parse("- task 1\n- task 2");
		let popped = seq.pop();
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(seq.serialize(DisplayFormat::Headers), "- task 1");
	}

	#[test]
	fn test_pop_from_section() {
		let mut seq = BlockerSequence::parse("# Section\n- task 1\n- task 2");
		let popped = seq.pop();
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(seq.serialize(DisplayFormat::Headers), "# Section\n- task 1");
	}

	#[test]
	fn test_pop_empty() {
		let mut seq = BlockerSequence::default();
		let popped = seq.pop();
		assert!(popped.is_none());
	}

	#[test]
	fn test_serialize_roundtrip() {
		let input = "# Header 1\n- task 1\n# Header 2\n- task 2";
		let seq = BlockerSequence::parse(input);
		insta::assert_snapshot!(seq.serialize(DisplayFormat::Headers), @"
		# Header 1
		- task 1
		# Header 2
		- task 2
		");
	}

	#[test]
	fn test_is_empty() {
		let empty = BlockerSequence::default();
		assert!(empty.is_empty());

		let with_content = BlockerSequence::parse("- task");
		assert!(!with_content.is_empty());

		// Only comments - the tree won't have items because comments need items
		let only_header = BlockerSequence::parse("# Just a header");
		assert!(only_header.is_empty()); // Headers without items are empty
	}

	#[test]
	fn test_serialize_nested() {
		let seq = BlockerSequence::parse("# Section A\n- task 1\n## Subsection\n- task 2\n# Section B\n- task 3");

		let nested = seq.serialize(DisplayFormat::Nested);
		let expected = "Section A\n\t- task 1\n\tSubsection\n\t\t- task 2\nSection B\n\t- task 3";
		assert_eq!(nested, expected);
	}

	#[test]
	fn test_serialize_headers() {
		let content = "# Section A\n- task 1\n## Subsection\n- task 2";
		let seq = BlockerSequence::parse(content);

		let headers = seq.serialize(DisplayFormat::Headers);
		assert_eq!(headers, content);
	}

	#[test]
	fn test_items_before_first_header() {
		let content = "- root task\n# Section\n- section task";
		let seq = BlockerSequence::parse(content);

		// Should serialize back correctly
		assert_eq!(seq.serialize(DisplayFormat::Headers), content);

		// Current should be the section task
		assert_eq!(seq.current_with_context(&[]), Some("Section: section task".to_string()));
	}

	#[test]
	fn test_multiple_h1_sections() {
		let content = "# A\n- task a\n# B\n- task b\n# C\n- task c";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("C: task c".to_string()));

		// Pop should remove from C
		let mut seq = seq;
		seq.pop();
		assert_eq!(seq.current_with_context(&[]), Some("B: task b".to_string()));
	}

	#[test]
	fn test_comments_preserved() {
		let content = "# Section\n- task 1\n\tcomment 1\n\tcomment 2\n- task 2";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.serialize(DisplayFormat::Headers), content);
	}
}
