//! Pure blocker types for issue files.
//!
//! This module contains the core data structures for blockers without I/O dependencies.
//! These types can be used in both the library and binary contexts.

use clap::ValueEnum;

use super::util::is_blockers_marker;

/// Split text at the blockers marker, returning (content_before, blockers).
/// If no blockers marker is found, returns the original text and an empty BlockerSequence.
pub fn split_blockers(text: &str) -> (String, BlockerSequence) {
	let lines: Vec<&str> = text.lines().collect();

	// Find the blockers marker
	let marker_idx = lines.iter().position(|line| is_blockers_marker(line));

	match marker_idx {
		Some(idx) => {
			// Content before the marker
			let before: String = lines[..idx].join("\n");
			// Content after the marker (blocker lines)
			let after: Vec<Line> = lines[idx + 1..].iter().filter_map(|line| classify_line(line)).collect();
			(before, BlockerSequence::from_lines(after))
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
	result.push_str(&blockers.serialize(DisplayFormat::Headers));

	result
}

/// Display format for blocker output
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq, ValueEnum)]
pub enum DisplayFormat {
	/// Traditional flat format with `# Header` lines
	#[default]
	Headers,
	/// Indented format showing hierarchy visually
	Nested,
}

/// Header level (1-5), where 1 is the highest/largest.
#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum HeaderLevel {
	One,
	Two,
	Three,
	Four,
	Five,
}

impl HeaderLevel {
	/// Get the numeric level (1-5)
	pub fn to_usize(self) -> usize {
		match self {
			HeaderLevel::One => 1,
			HeaderLevel::Two => 2,
			HeaderLevel::Three => 3,
			HeaderLevel::Four => 4,
			HeaderLevel::Five => 5,
		}
	}

	/// Create from numeric level (1-5)
	pub fn from_usize(level: usize) -> Option<Self> {
		match level {
			1 => Some(HeaderLevel::One),
			2 => Some(HeaderLevel::Two),
			3 => Some(HeaderLevel::Three),
			4 => Some(HeaderLevel::Four),
			5 => Some(HeaderLevel::Five),
			_ => None,
		}
	}
}

/// A parsed line in a blocker file.
/// This is the boundary format between raw text and structured blocker data.
#[derive(Clone, Debug, PartialEq)]
pub enum Line {
	/// Header with level and text (without # prefix)
	Header { level: HeaderLevel, text: String },
	/// List item content (without - prefix)
	Item(String),
	/// Comment line - tab-indented explanatory text (without leading tab)
	Comment(String),
}

impl Line {
	/// Check if this line is a header
	pub fn is_header(&self) -> bool {
		matches!(self, Line::Header { .. })
	}

	/// Check if this line contributes to the blocker list (headers and items)
	pub fn is_content(&self) -> bool {
		!matches!(self, Line::Comment(_))
	}

	/// Serialize to raw text format
	pub fn to_raw(&self) -> String {
		match self {
			Line::Header { level, text } => format!("{} {text}", "#".repeat(level.to_usize())),
			Line::Item(text) => format!("- {text}"),
			Line::Comment(text) => format!("\t{text}"),
		}
	}
}

/// Parse a line into a structured Line.
/// - Lines starting with tab are Comments (content without leading tab)
/// - Lines starting with 2+ spaces (likely editor-converted tabs) are Comments
/// - Lines starting with # are Headers (levels 1-5, text without # prefix)
/// - Lines starting with - are Items (content without - prefix)
/// - All other non-empty lines are Items (raw content)
/// - Returns None for empty lines or whitespace-only lines
pub fn classify_line(line: &str) -> Option<Line> {
	// Skip empty or whitespace-only lines
	if line.trim().is_empty() {
		return None;
	}

	// Comment: tab-indented
	if let Some(content) = line.strip_prefix('\t') {
		return Some(Line::Comment(content.to_string()));
	}

	// Comment: 2+ spaces (likely editor tab-to-space conversion)
	// But not if it looks like an indented list item
	if line.starts_with("  ") && !line.trim_start().starts_with('-') {
		let content = line.trim_start();
		return Some(Line::Comment(content.to_string()));
	}

	let trimmed = line.trim();

	// Header: # with space after
	if trimmed.starts_with('#') {
		let mut count = 0;
		for ch in trimmed.chars() {
			if ch == '#' {
				count += 1;
			} else {
				break;
			}
		}

		// Valid header must have space after the # characters
		if count > 0 && trimmed.len() > count {
			let next_char = trimmed.chars().nth(count);
			if next_char == Some(' ') {
				let text = trimmed[count + 1..].to_string();

				// Warn if header is nested too deeply (level > 5)
				if count > 5 {
					eprintln!("Warning: Header level {count} is too deep (max 5 supported). Treating as regular item: {trimmed}");
					return Some(Line::Item(trimmed.to_string()));
				}

				if let Some(level) = HeaderLevel::from_usize(count) {
					return Some(Line::Header { level, text });
				}
			}
		}
	}

	// Item: strip - prefix if present
	let content = trimmed.strip_prefix("- ").unwrap_or(trimmed);
	Some(Line::Item(content.to_string()))
}

/// A single blocker item with optional comments.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BlockerItem {
	pub text: String,
	pub comments: Vec<String>,
}

/// A sequence of blocker lines that can be manipulated.
/// Internally represented as a tree, where each sequence can contain child sequences.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct BlockerSequence {
	/// Header text (empty for root)
	pub title: String,
	/// Header level (0 for root, 1-5 for real headers)
	pub level: usize,
	/// Items directly under this sequence (not under any child header)
	pub items: Vec<BlockerItem>,
	/// Child sections (sub-headers)
	pub children: Vec<BlockerSequence>,
}

impl BlockerSequence {
	/// Create a new empty sequence with the given title and level
	pub fn new(title: impl Into<String>, level: usize) -> Self {
		Self {
			title: title.into(),
			level,
			items: Vec::new(),
			children: Vec::new(),
		}
	}

	/// Create an implicit root sequence (level 0)
	pub fn root() -> Self {
		Self::new("", 0)
	}

	/// Create a BlockerSequence from parsed lines
	pub fn from_lines(lines: Vec<Line>) -> Self {
		Self::build_from_lines(&lines)
	}

	/// Parse raw text content into a BlockerSequence
	pub fn parse(content: &str) -> Self {
		let lines: Vec<Line> = content.lines().filter_map(classify_line).collect();
		Self::build_from_lines(&lines)
	}

	/// Build tree from flat line list
	fn build_from_lines(lines: &[Line]) -> Self {
		let mut root = Self::root();
		let mut current_item_comments: Vec<String> = Vec::new();
		let mut pending_item: Option<String> = None;

		// Stack of levels to track where we are
		let mut level_stack: Vec<usize> = vec![0]; // Start at root level

		for line in lines {
			match line {
				Line::Header { level, text } => {
					// Flush pending item
					if let Some(item_text) = pending_item.take() {
						let item = BlockerItem {
							text: item_text,
							comments: std::mem::take(&mut current_item_comments),
						};
						Self::add_item_to_tree(&mut root, &level_stack, item);
					}

					let lvl = level.to_usize();

					// Pop levels that are >= this header's level
					while level_stack.last().is_some_and(|&l| l >= lvl) {
						level_stack.pop();
					}

					// Add new sequence at appropriate position
					let new_seq = Self::new(text.clone(), lvl);
					Self::add_child_to_tree(&mut root, &level_stack, new_seq);

					// Push this level
					level_stack.push(lvl);
				}
				Line::Item(text) => {
					// Flush pending item first
					if let Some(item_text) = pending_item.take() {
						let item = BlockerItem {
							text: item_text,
							comments: std::mem::take(&mut current_item_comments),
						};
						Self::add_item_to_tree(&mut root, &level_stack, item);
					}
					pending_item = Some(text.clone());
				}
				Line::Comment(text) => {
					// Comments belong to the pending item
					current_item_comments.push(text.clone());
				}
			}
		}

		// Flush final pending item
		if let Some(item_text) = pending_item.take() {
			let item = BlockerItem {
				text: item_text,
				comments: std::mem::take(&mut current_item_comments),
			};
			Self::add_item_to_tree(&mut root, &level_stack, item);
		}

		root
	}

	/// Add an item to the correct position in the tree based on level stack
	fn add_item_to_tree(root: &mut BlockerSequence, level_stack: &[usize], item: BlockerItem) {
		let target = Self::get_sequence_for_level(root, level_stack);
		target.items.push(item);
	}

	/// Add a child sequence to the correct position in the tree
	fn add_child_to_tree(root: &mut BlockerSequence, level_stack: &[usize], seq: BlockerSequence) {
		let target = Self::get_sequence_for_level(root, level_stack);
		target.children.push(seq);
	}

	/// Navigate to the correct sequence based on level stack
	fn get_sequence_for_level<'a>(root: &'a mut BlockerSequence, level_stack: &[usize]) -> &'a mut BlockerSequence {
		let mut current = root;

		// Skip the root level (0) in the stack
		for &level in level_stack.iter().skip(1) {
			// Find the child with this level (it should be the last one added at this level)
			let child_idx = current.children.iter().rposition(|c| c.level == level);
			if let Some(idx) = child_idx {
				current = &mut current.children[idx];
			} else {
				// Should not happen if build_from_lines is correct
				break;
			}
		}

		current
	}

	/// Check if the sequence has no blocker items (headers without items count as empty)
	pub fn is_empty(&self) -> bool {
		self.items.is_empty() && self.children.iter().all(|c| c.is_empty())
	}

	/// Serialize to text in the specified format
	pub fn serialize(&self, format: DisplayFormat) -> String {
		let mut lines = Vec::new();
		match format {
			DisplayFormat::Headers => self.serialize_headers(&mut lines),
			DisplayFormat::Nested => self.serialize_nested(&mut lines, 0),
		}
		lines.join("\n")
	}

	/// Internal recursive serialization (headers format)
	fn serialize_headers(&self, lines: &mut Vec<String>) {
		// Items first
		for item in &self.items {
			lines.push(format!("- {}", item.text));
			for comment in &item.comments {
				lines.push(format!("\t{comment}"));
			}
		}

		// Then children
		for child in &self.children {
			// Add header
			if child.level > 0 {
				lines.push(format!("{} {}", "#".repeat(child.level), child.title));
			}
			child.serialize_headers(lines);
		}
	}

	/// Internal recursive serialization (nested/indented format)
	fn serialize_nested(&self, lines: &mut Vec<String>, indent: usize) {
		let indent_str = "\t".repeat(indent);

		// Output title if this is not the root
		if self.level > 0 && !self.title.is_empty() {
			lines.push(format!("{indent_str}{}", self.title));
		}

		// Output items (indented one more level if we have a title)
		let item_indent = if self.level > 0 { indent + 1 } else { indent };
		let item_indent_str = "\t".repeat(item_indent);

		for item in &self.items {
			lines.push(format!("{item_indent_str}- {}", item.text));
			for comment in &item.comments {
				lines.push(format!("{item_indent_str}\t{comment}"));
			}
		}

		// Output children
		let child_indent = if self.level > 0 { indent + 1 } else { indent };
		for child in &self.children {
			child.serialize_nested(lines, child_indent);
		}
	}

	/// Get all lines in order (for iteration)
	pub fn lines(&self) -> Vec<Line> {
		let mut result = Vec::new();
		self.collect_lines(&mut result);
		result
	}

	/// Internal recursive line collection
	fn collect_lines(&self, result: &mut Vec<Line>) {
		// Items first
		for item in &self.items {
			result.push(Line::Item(item.text.clone()));
			for comment in &item.comments {
				result.push(Line::Comment(comment.clone()));
			}
		}

		// Then children
		for child in &self.children {
			if child.level > 0
				&& let Some(level) = HeaderLevel::from_usize(child.level)
			{
				result.push(Line::Header { level, text: child.title.clone() });
			}
			child.collect_lines(result);
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_classify_line() {
		assert_eq!(classify_line(""), None);
		assert_eq!(classify_line("\tComment"), Some(Line::Comment("Comment".to_string())));
		assert_eq!(classify_line("Content"), Some(Line::Item("Content".to_string())));
		assert_eq!(classify_line("  Spaces"), Some(Line::Comment("Spaces".to_string())));
		assert_eq!(
			classify_line("# Header 1"),
			Some(Line::Header {
				level: HeaderLevel::One,
				text: "Header 1".to_string()
			})
		);
		assert_eq!(
			classify_line("## Header 2"),
			Some(Line::Header {
				level: HeaderLevel::Two,
				text: "Header 2".to_string()
			})
		);
	}

	#[test]
	fn test_blocker_sequence_parse() {
		let content = "# Section\n- item 1\n\tcomment\n- item 2";
		let seq = BlockerSequence::parse(content);

		assert_eq!(seq.children.len(), 1);
		assert_eq!(seq.children[0].title, "Section");
		assert_eq!(seq.children[0].items.len(), 2);
		assert_eq!(seq.children[0].items[0].text, "item 1");
		assert_eq!(seq.children[0].items[0].comments, vec!["comment"]);
	}

	#[test]
	fn test_blocker_sequence_serialize() {
		let content = "- item 1\n\tcomment\n# Section\n- item 2";
		let seq = BlockerSequence::parse(content);
		let serialized = seq.serialize(DisplayFormat::Headers);

		assert!(serialized.contains("- item 1"));
		assert!(serialized.contains("\tcomment"));
		assert!(serialized.contains("# Section"));
		assert!(serialized.contains("- item 2"));
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
		assert_eq!(content, "Description here\n");
		assert!(!blockers.is_empty());
		assert_eq!(blockers.items.len(), 2);
		assert_eq!(blockers.items[0].text, "task 1");
		assert_eq!(blockers.items[1].text, "task 2");
	}

	#[test]
	fn test_split_blockers_with_headers() {
		let text = "Body\n\n# Blockers\n# Phase 1\n- task a\n# Phase 2\n- task b";
		let (content, blockers) = split_blockers(text);
		assert_eq!(content, "Body\n");
		assert_eq!(blockers.children.len(), 2);
		assert_eq!(blockers.children[0].title, "Phase 1");
		assert_eq!(blockers.children[1].title, "Phase 2");
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
		// Re-split should give same results
		let (content2, blockers2) = split_blockers(&rejoined);
		assert_eq!(content, content2);
		assert_eq!(blockers.items.len(), blockers2.items.len());
	}

	#[test]
	fn test_blocker_sequence_strips_empty_lines() {
		// 2 empty lines before, 2 after - one pure empty, one with whitespace
		let content = "\n\t \n- task 1\n- task 2\n\n\t \n";
		let seq = BlockerSequence::parse(content);
		let serialized = seq.serialize(DisplayFormat::Headers);
		insta::assert_snapshot!(serialized, @r#"
  - task 1
  - task 2
  "#);
	}
}
