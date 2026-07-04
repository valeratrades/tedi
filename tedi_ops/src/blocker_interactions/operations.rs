//! Extended stack operations for blocker management.
//!
//! This module provides extension methods on Blockers from the library:
//! - `add`: Push a new blocker onto the stack
//! - `pop`: Remove the last blocker from the stack
//! - `current`: Get the current (last) blocker with its parent context

use crate::{BlockerItem, Blockers, IssueRef};

/// Extension trait for Blockers with additional operations
pub trait BlockersExt {
	/// Get the current (last) blocker item
	fn current(&self) -> Option<&BlockerItem>;

	/// Get the current blocker with context prepended (joined by ": ").
	fn current_with_context(&self, ownership_hierarchy: &[String]) -> Option<String>;

	/// Walk the path from root to the current (deepest) blocker item,
	/// returning the deepest ancestor (or leaf) that is an issue ref.
	fn current_issue_ref(&self) -> Option<IssueRef>;

	/// Add a content line to the blocker sequence (at current position)
	fn add(&mut self, text: &str);

	/// Add a content line as a child of the current deepest item
	fn add_child(&mut self, text: &str);

	/// Remove the current (deepest) blocker, plus `parents` ancestors along its path.
	///
	/// With `parents = 0`, pops only the current leaf. With `parents = N`, pops the leaf
	/// plus its N nearest ancestors (i.e. removes the ancestor at depth `leaf_depth - N`,
	/// which takes its descendants with it).
	///
	/// Returns the text of the topmost popped item, or `None` if the sequence is empty
	/// or `parents` exceeds the current chain depth.
	fn pop(&mut self, parents: usize) -> Option<String>;

	/// Replace the text of the current (deepest) blocker in-place.
	/// Preserves the item's position in the tree (unlike pop+add, which can change nesting).
	/// Resets the item's comments (they belonged to the previous task).
	/// Returns the previous text, or None if the sequence is empty.
	fn set(&mut self, text: &str) -> Option<String>;
}

impl BlockersExt for Blockers {
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

	fn current_issue_ref(&self) -> Option<IssueRef> {
		self.deepest_issue_ref()
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

	fn add_child(&mut self, text: &str) {
		let item = BlockerItem {
			text: text.to_string(),
			comments: Vec::new(),
			children: Vec::new(),
		};
		add_child_to_current(&mut self.items, item);
	}

	fn pop(&mut self, parents: usize) -> Option<String> {
		pop_last(&mut self.items, parents).map(|item| item.text)
	}

	fn set(&mut self, text: &str) -> Option<String> {
		replace_last(&mut self.items, text.to_string())
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

/// Replace the text of the deepest leaf (depth-first, rightmost) in-place.
/// Comments on the replaced item are cleared. Returns the old text.
fn replace_last(items: &mut Vec<BlockerItem>, new_text: String) -> Option<String> {
	let last = items.last_mut()?;
	if !last.children.is_empty() {
		return replace_last(&mut last.children, new_text);
	}
	let old = std::mem::replace(&mut last.text, new_text);
	last.comments.clear();
	Some(old)
}

/// Pop the deepest (rightmost) item from the tree, plus `parents` ancestors along its path.
///
/// Walks down the "last child" chain. At each level, the local chain depth equals the
/// number of further descents available. When that depth matches the requested `parents`
/// count, pops the current `last` (which also drops its descendants).
///
/// Returns `None` if the sequence is empty or `parents` exceeds the available chain depth.
fn pop_last(items: &mut Vec<BlockerItem>, parents: usize) -> Option<BlockerItem> {
	let last = items.last()?;
	let depth = chain_depth(last);
	if parents > depth {
		return None;
	}
	if parents == depth {
		return items.pop();
	}
	let last = items.last_mut().expect("just inspected via .last()");
	pop_last(&mut last.children, parents)
}

/// Number of additional levels reachable by descending into the last child chain.
/// A leaf (no children) has depth 0.
fn chain_depth(item: &BlockerItem) -> usize {
	match item.children.last() {
		Some(child) => 1 + chain_depth(child),
		None => 0,
	}
}

/// Add item to the deepest current section
fn add_item_to_current(items: &mut Vec<BlockerItem>, item: BlockerItem) {
	if let Some(last) = items.last_mut()
		&& !last.children.is_empty()
	{
		// Recurse into children
		add_item_to_current(&mut last.children, item);
		return;
	}
	items.push(item);
}

/// Add item as a child of the deepest current item
fn add_child_to_current(items: &mut Vec<BlockerItem>, item: BlockerItem) {
	let Some(last) = items.last_mut() else {
		// No items exist, just push at root
		items.push(item);
		return;
	};
	if !last.children.is_empty() {
		// Recurse into children to find the deepest
		add_child_to_current(&mut last.children, item);
	} else {
		// This is the deepest item — add as its child
		last.children.push(item);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_current() {
		let seq = Blockers::parse("- task 1\n- task 2\n- task 3");
		assert_eq!(seq.current().map(|i| i.text.as_str()), Some("task 3"));
	}

	#[test]
	fn test_current_skips_comments() {
		let seq = Blockers::parse("- task 1\n  comment\n- task 2\n  another comment");
		assert_eq!(seq.current().map(|i| i.text.as_str()), Some("task 2"));
		// Comments should be attached to the item
		assert_eq!(seq.current().map(|i| i.comments.len()), Some(1));
	}

	#[test]
	fn test_current_with_context_no_hierarchy() {
		let seq = Blockers::parse("- Phase 1\n  - task 1\n- Phase 2\n  - task 2");
		assert_eq!(seq.current_with_context(&[]), Some("Phase 2: task 2".to_string()));
	}

	#[test]
	fn test_current_with_context_with_hierarchy() {
		let seq = Blockers::parse("- Phase 1\n  - task 1");
		let hierarchy = vec!["project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("project: Phase 1: task 1".to_string()));
	}

	#[test]
	fn test_current_with_context_multi_level_hierarchy() {
		let seq = Blockers::parse("- Section\n  - task");
		let hierarchy = vec!["workspace".to_string(), "project".to_string()];
		assert_eq!(seq.current_with_context(&hierarchy), Some("workspace: project: Section: task".to_string()));
	}

	#[test]
	fn test_nested_items() {
		let content = "- H1\n  - H2\n    - task under H2\n- Another H1\n  - task under another H1";
		let seq = Blockers::parse(content);

		// Current should be "task under another H1" with path "Another H1"
		assert_eq!(seq.current_with_context(&[]), Some("Another H1: task under another H1".to_string()));
	}

	#[test]
	fn test_deeply_nested() {
		let content = "- Level 1\n  - Level 2\n    - Level 3\n      - deep task";
		let seq = Blockers::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("Level 1: Level 2: Level 3: deep task".to_string()));
	}

	#[test]
	fn test_add() {
		let mut seq = Blockers::parse("- task 1");
		seq.add("task 2");
		assert_eq!(String::from(&seq), "- task 1\n- task 2");
	}

	#[test]
	fn test_add_to_section() {
		let mut seq = Blockers::parse("- Section\n  - task 1");
		seq.add("task 2");
		// Should add under the same section (as sibling of task 1)
		assert_eq!(String::from(&seq), "- Section\n  - task 1\n  - task 2");
	}

	#[test]
	fn test_pop() {
		let mut seq = Blockers::parse("- task 1\n- task 2");
		let popped = seq.pop(0);
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(String::from(&seq), "- task 1");
	}

	#[test]
	fn test_pop_from_section() {
		let mut seq = Blockers::parse("- Section\n  - task 1\n  - task 2");
		let popped = seq.pop(0);
		assert_eq!(popped, Some("task 2".to_string()));
		assert_eq!(String::from(&seq), "- Section\n  - task 1");
	}

	#[test]
	fn test_pop_empty() {
		let mut seq = Blockers::default();
		let popped = seq.pop(0);
		assert!(popped.is_none());
	}

	#[test]
	fn test_serialize_roundtrip() {
		let input = "- Header 1\n  - task 1\n- Header 2\n  - task 2";
		let seq = Blockers::parse(input);
		insta::assert_snapshot!(String::from(&seq), @"
		- Header 1
		  - task 1
		- Header 2
		  - task 2
		");
	}

	#[test]
	fn test_is_empty() {
		let empty = Blockers::default();
		assert!(empty.is_empty());

		let with_content = Blockers::parse("- task");
		assert!(!with_content.is_empty());
	}

	#[test]
	fn test_items_before_children() {
		let content = "- root task\n- Section\n  - section task";
		let seq = Blockers::parse(content);

		// Current should be the section task
		assert_eq!(seq.current_with_context(&[]), Some("Section: section task".to_string()));
	}

	#[test]
	fn test_multiple_top_sections() {
		let content = "- A\n  - task a\n- B\n  - task b\n- C\n  - task c";
		let seq = Blockers::parse(content);

		assert_eq!(seq.current_with_context(&[]), Some("C: task c".to_string()));

		// Pop removes task c (last child of C)
		let mut seq = seq;
		seq.pop(0);
		// C still exists as a leaf item
		assert_eq!(seq.current_with_context(&[]), Some("C".to_string()));

		// Pop again removes C itself
		seq.pop(0);
		assert_eq!(seq.current_with_context(&[]), Some("B: task b".to_string()));
	}

	#[test]
	fn test_comments_preserved() {
		let content = "- task 1\n  comment 1\n  comment 2\n- task 2";
		let seq = Blockers::parse(content);

		assert_eq!(String::from(&seq), content);
	}

	#[test]
	fn test_parse_and_serialize() {
		let content = "- task 1\n  comment\n  - nested\n- task 2";
		let seq = Blockers::parse(content);
		assert_eq!(String::from(&seq), content);
	}

	#[test]
	fn test_add_child_flat() {
		let mut seq = Blockers::parse("- task 1");
		seq.add_child("subtask");
		insta::assert_snapshot!(String::from(&seq), @"
		- task 1
		  - subtask
		");
	}

	#[test]
	fn test_add_child_to_section() {
		let mut seq = Blockers::parse("- Section\n  - task 1");
		seq.add_child("subtask of task 1");
		insta::assert_snapshot!(String::from(&seq), @"
		- Section
		  - task 1
		    - subtask of task 1
		");
	}

	#[test]
	fn test_add_child_empty() {
		let mut seq = Blockers::default();
		seq.add_child("first");
		insta::assert_snapshot!(String::from(&seq), @"- first");
	}

	#[test]
	fn test_set_flat() {
		let mut seq = Blockers::parse("- task 1\n- task 2");
		let old = seq.set("replaced");
		assert_eq!(old, Some("task 2".to_string()));
		assert_eq!(String::from(&seq), "- task 1\n- replaced");
	}

	#[test]
	fn test_set_only_child_preserves_nesting() {
		// The key case: current is the only child of its parent.
		// pop+add would un-nest the replacement; set must keep it nested.
		let mut seq = Blockers::parse("- Section\n  - lonely task");
		seq.set("replacement");
		insta::assert_snapshot!(String::from(&seq), @"
		- Section
		  - replacement
		");
	}

	#[test]
	fn test_set_deeply_nested() {
		let mut seq = Blockers::parse("- L1\n  - L2\n    - L3");
		seq.set("L3-new");
		insta::assert_snapshot!(String::from(&seq), @"
		- L1
		  - L2
		    - L3-new
		");
	}

	#[test]
	fn test_set_clears_comments() {
		let mut seq = Blockers::parse("- task\n  comment 1\n  comment 2");
		seq.set("replaced");
		assert_eq!(String::from(&seq), "- replaced");
	}

	#[test]
	fn test_set_empty() {
		let mut seq = Blockers::default();
		assert_eq!(seq.set("anything"), None);
	}

	#[test]
	fn test_pop_with_one_parent_linear_chain() {
		// farm tasks: shave yak: get a stool: remember where I left it
		let mut seq = Blockers::parse("- farm tasks\n  - shave yak\n    - get a stool\n      - remember where I left it");
		let popped = seq.pop(1);
		// Topmost popped item is "get a stool" (parent of leaf)
		assert_eq!(popped, Some("get a stool".to_string()));
		assert_eq!(seq.current_with_context(&[]), Some("farm tasks: shave yak".to_string()));
	}

	#[test]
	fn test_pop_with_two_parents_linear_chain() {
		let mut seq = Blockers::parse("- farm tasks\n  - shave yak\n    - get a stool\n      - remember where I left it");
		let popped = seq.pop(2);
		assert_eq!(popped, Some("shave yak".to_string()));
		assert_eq!(seq.current_with_context(&[]), Some("farm tasks".to_string()));
	}

	#[test]
	fn test_pop_parents_exceeds_chain() {
		let mut seq = Blockers::parse("- only");
		// chain depth = 0, asking for 1 parent → no-op, return None
		let before: String = (&seq).into();
		let popped = seq.pop(1);
		assert!(popped.is_none());
		let after: String = (&seq).into();
		assert_eq!(before, after, "sequence must be unchanged when pop fails");
	}

	#[test]
	fn test_pop_parents_empty_sequence() {
		let mut seq = Blockers::default();
		assert!(seq.pop(2).is_none());
	}

	#[test]
	fn test_add_child_deeply_nested() {
		let mut seq = Blockers::parse("- L1\n  - L2\n    - L3");
		seq.add_child("L4");
		insta::assert_snapshot!(String::from(&seq), @"
		- L1
		  - L2
		    - L3
		      - L4
		");
	}
}
