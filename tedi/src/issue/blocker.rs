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
	/// Set when parse dropped lines that didn't fit the format. Not serialized, not compared.
	pub had_orphans: bool,
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
		let mut had_orphans = false;
		Self::parse_items_at_indent(lines, &mut 0, 0, &mut root_items, &mut had_orphans);
		Self {
			items: root_items,
			had_orphans,
			..Default::default()
		}
	}

	/// Parse items at a given indent level, advancing `pos` through the lines.
	/// `indent` is the number of 2-space units expected for items at this level.
	fn parse_items_at_indent(lines: &[&str], pos: &mut usize, indent: usize, items: &mut Vec<BlockerItem>, had_orphans: &mut bool) {
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
						Self::parse_items_at_indent(lines, pos, child_indent, &mut item.children, had_orphans);
						// After parsing children, we continue to check for more at our level
						// but no more comments are allowed (children already started)
					} else {
						// Comment line
						if !item.children.is_empty() {
							// Comments after nested blockers are not allowed at this level;
							// treat as comment on the last child? No — the spec says
							// "once we get a first nested blocker, there can be no comments at the same level"
							// So we panic/warn. For robustness, let's just skip with a warning.
							tracing::warn!("comment after nested blockers at indent level {child_indent}: {child_content:?}");
						}
						item.comments.push(child_content.to_string());
						*pos += 1;
					}
				}

				items.push(item);
			} else {
				// Not a blocker item and not deeper — this is a comment line without a parent item.
				tracing::warn!("orphan comment (no preceding blocker item): {content:?}");
				*had_orphans = true;
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
