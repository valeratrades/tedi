//! Extended stack operations for blocker management.
//!
//! This module provides extension methods on BlockerSequence from the library:
//! - `add`: Push a new blocker onto the stack
//! - `pop`: Remove the last blocker from the stack
//! - `current`: Get the current (last) blocker with its parent context

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
		last_item_in_list(&self.items)
	}

	fn current_with_context(&self, ownership_hierarchy: &[String]) -> Option<String> {
		let current = self.current()?;

		// Get path of parent item texts to the current item
		let path = path_to_last(&self.items);

		// Build final output: ownership hierarchy + blocker path + task
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
			children: Vec::new(),
		};
		// Add to the deepest current section
		add_item_to_current(&mut self.items, item);
	}

	fn pop(&mut self) -> Option<String> {
		pop_last(&mut self.items).map(|item| item.text)
	}
}

/// Get the last item in a list of items (depth-first, rightmost)
fn last_item_in_list(items: &[BlockerItem]) -> Option<&BlockerItem> {
	let last = items.last()?;
	// Check if the last item has children — if so, recurse into them
	if let Some(child_last) = last_item_in_list(&last.children) {
		Some(child_last)
	} else {
		Some(last)
	}
}

/// Get the path of parent item texts leading to the last item (not including the last item itself)
fn path_to_last(items: &[BlockerItem]) -> Vec<String> {
	let mut path = Vec::new();
	path_to_last_inner(items, &mut path);
	path
}

fn path_to_last_inner(items: &[BlockerItem], path: &mut Vec<String>) {
	let Some(last) = items.last() else {
		return;
	};

	if !last.children.is_empty() {
		// The last item has children — it's a parent on the path
		path.push(last.text.clone());
		path_to_last_inner(&last.children, path);
	}
	// If no children, this is the leaf (current item) — don't add to path
}

/// Pop the last item from the tree (depth-first, rightmost)
fn pop_last(items: &mut Vec<BlockerItem>) -> Option<BlockerItem> {
	let last = items.last_mut()?;
	// Try children first
	if let Some(popped) = pop_last(&mut last.children) {
		return Some(popped);
	}
	// No children — pop ourselves
	items.pop()
}

/// Add item to the deepest current section
fn add_item_to_current(items: &mut Vec<BlockerItem>, item: BlockerItem) {
	if let Some(last) = items.last_mut() {
		if !last.children.is_empty() {
			// Recurse into children
			add_item_to_current(&mut last.children, item);
			return;
		}
		// Add as sibling (same level as the last item)
	}
	items.push(item);
}

#[cfg(test)]
mod tests {
	use super::*;

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
		let seq = BlockerSequence::parse("- Phase 1\n\t- task 1\n- Phase 2\n\t- task 2");
		assert_eq!(seq.current_with_context(&[]), Some("Phase 2: task 2".to_string()));
	}

	#[test]
	fn test_current_with_context_with_hierarchy() {
		let seq = BlockerSequence::parse("- Phase 1\n\t- task 1");
		let hierarchy = vec!["project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("project: Phase 1: task 1".to_string()));
	}

	#[test]
	fn test_current_with_context_multi_level_hierarchy() {
		let seq = BlockerSequence::parse("- Section\n\t- task");
		let hierarchy = vec!["workspace".to_string(), "project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("workspace: project: Section: task".to_string()));
	}

	#[test]
	fn test_nested_items() {
		let content = "- H1\n\t- H2\n\t\t- task under H2\n- Another H1\n\t- task under another H1";
		let seq = BlockerSequence::parse(content);

		// Current should be "task under another H1" with path "Another H1"
		assert_eq!(seq.current_with_context(&[]), Some("Another H1: task under another H1".to_string()));
	}

	#[test]
	fn test_deeply_nested() {
		let content = "- Level 1\n\t- Level 2\n\t\t- Level 3\n\t\t\t- deep task";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("Level 1: Level 2: Level 3: deep task".to_string()));
	}

	#[test]
	fn test_add() {
		let mut seq = BlockerSequence::parse("- task 1");
		seq.add("task 2");
		assert_eq!(seq.serialize(), "- task 1\n- task 2");
	}

	#[test]
	fn test_add_to_section() {
		let mut seq = BlockerSequence::parse("- Section\n\t- task 1");
		seq.add("task 2");
		// Should add under the same section (as sibling of task 1)
		assert_eq!(seq.serialize(), "- Section\n\t- task 1\n\t- task 2");
	}

	#[test]
	fn test_pop() {
		let mut seq = BlockerSequence::parse("- task 1\n- task 2");
		let popped = seq.pop();
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(seq.serialize(), "- task 1");
	}

	#[test]
	fn test_pop_from_section() {
		let mut seq = BlockerSequence::parse("- Section\n\t- task 1\n\t- task 2");
		let popped = seq.pop();
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(seq.serialize(), "- Section\n\t- task 1");
	}

	#[test]
	fn test_pop_empty() {
		let mut seq = BlockerSequence::default();
		let popped = seq.pop();
		assert!(popped.is_none());
	}

	#[test]
	fn test_serialize_roundtrip() {
		let input = "- Header 1\n\t- task 1\n- Header 2\n\t- task 2";
		let seq = BlockerSequence::parse(input);
		insta::assert_snapshot!(seq.serialize(), @r"
		- Header 1
			- task 1
		- Header 2
			- task 2
		");
	}

	#[test]
	fn test_is_empty() {
		let empty = BlockerSequence::default();
		assert!(empty.is_empty());

		let with_content = BlockerSequence::parse("- task");
		assert!(!with_content.is_empty());
	}

	#[test]
	fn test_items_before_children() {
		let content = "- root task\n- Section\n\t- section task";
		let seq = BlockerSequence::parse(content);

		// Current should be the section task
		assert_eq!(seq.current_with_context(&[]), Some("Section: section task".to_string()));
	}

	#[test]
	fn test_multiple_top_sections() {
		let content = "- A\n\t- task a\n- B\n\t- task b\n- C\n\t- task c";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("C: task c".to_string()));

		// Pop removes task c (last child of C)
		let mut seq = seq;
		seq.pop();
		// C still exists as a leaf item
		assert_eq!(seq.current_with_context(&[]), Some("C".to_string()));

		// Pop again removes C itself
		seq.pop();
		assert_eq!(seq.current_with_context(&[]), Some("B: task b".to_string()));
	}

	#[test]
	fn test_comments_preserved() {
		let content = "- task 1\n\tcomment 1\n\tcomment 2\n- task 2";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.serialize(), content);
	}

	#[test]
	fn test_parse_and_serialize() {
		let content = "- task 1\n\tcomment\n\t- nested\n- task 2";
		let seq = BlockerSequence::parse(content);
		assert_eq!(seq.serialize(), content);
	}
}
