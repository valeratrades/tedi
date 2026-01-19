//! Owned markdown event types for storage.
//!
//! This module provides owned versions of pulldown_cmark events that can be stored
//! in data structures without lifetime concerns. Events are parsed using pulldown_cmark
//! and rendered back to markdown on demand.

use std::fmt;

use pulldown_cmark::{Alignment, BlockQuoteKind, CodeBlockKind, Event, HeadingLevel, LinkType, MetadataBlockKind, Tag, TagEnd};

/// An owned markdown event that can be stored without lifetimes.
/// This is the internal representation - we parse markdown into these and render back on demand.
#[derive(Clone, Debug, PartialEq)]
pub enum OwnedEvent {
	Start(OwnedTag),
	End(OwnedTagEnd),
	Text(String),
	Code(String),
	InlineHtml(String),
	Html(String),
	InlineMath(String),
	DisplayMath(String),
	FootnoteReference(String),
	SoftBreak,
	HardBreak,
	Rule,
	TaskListMarker(bool),
}

impl OwnedEvent {
	/// Convert from a pulldown_cmark Event (borrowing the data).
	pub fn from_event(event: Event<'_>) -> Self {
		match event {
			Event::Start(tag) => OwnedEvent::Start(OwnedTag::from_tag(tag)),
			Event::End(tag_end) => OwnedEvent::End(OwnedTagEnd::from_tag_end(tag_end)),
			Event::Text(text) => OwnedEvent::Text(text.into_string()),
			Event::Code(code) => OwnedEvent::Code(code.into_string()),
			Event::InlineHtml(html) => OwnedEvent::InlineHtml(html.into_string()),
			Event::Html(html) => OwnedEvent::Html(html.into_string()),
			Event::InlineMath(math) => OwnedEvent::InlineMath(math.into_string()),
			Event::DisplayMath(math) => OwnedEvent::DisplayMath(math.into_string()),
			Event::FootnoteReference(name) => OwnedEvent::FootnoteReference(name.into_string()),
			Event::SoftBreak => OwnedEvent::SoftBreak,
			Event::HardBreak => OwnedEvent::HardBreak,
			Event::Rule => OwnedEvent::Rule,
			Event::TaskListMarker(checked) => OwnedEvent::TaskListMarker(checked),
		}
	}

	/// Convert back to a pulldown_cmark Event (borrowing from self).
	pub fn to_event(&self) -> Event<'_> {
		match self {
			OwnedEvent::Start(tag) => Event::Start(tag.to_tag()),
			OwnedEvent::End(tag_end) => Event::End(tag_end.to_tag_end()),
			OwnedEvent::Text(text) => Event::Text(text.as_str().into()),
			OwnedEvent::Code(code) => Event::Code(code.as_str().into()),
			OwnedEvent::InlineHtml(html) => Event::InlineHtml(html.as_str().into()),
			OwnedEvent::Html(html) => Event::Html(html.as_str().into()),
			OwnedEvent::InlineMath(math) => Event::InlineMath(math.as_str().into()),
			OwnedEvent::DisplayMath(math) => Event::DisplayMath(math.as_str().into()),
			OwnedEvent::FootnoteReference(name) => Event::FootnoteReference(name.as_str().into()),
			OwnedEvent::SoftBreak => Event::SoftBreak,
			OwnedEvent::HardBreak => Event::HardBreak,
			OwnedEvent::Rule => Event::Rule,
			OwnedEvent::TaskListMarker(checked) => Event::TaskListMarker(*checked),
		}
	}
}

/// An owned tag for Start events.
#[derive(Clone, Debug, PartialEq)]
pub enum OwnedTag {
	Paragraph,
	Heading {
		level: HeadingLevel,
		id: Option<String>,
		classes: Vec<String>,
		attrs: Vec<(String, Option<String>)>,
	},
	BlockQuote(Option<BlockQuoteKind>),
	CodeBlock(OwnedCodeBlockKind),
	HtmlBlock,
	List(Option<u64>),
	Item,
	FootnoteDefinition(String),
	DefinitionList,
	DefinitionListTitle,
	DefinitionListDefinition,
	Table(Vec<Alignment>),
	TableHead,
	TableRow,
	TableCell,
	Emphasis,
	Strong,
	Strikethrough,
	Link {
		link_type: LinkType,
		dest_url: String,
		title: String,
		id: String,
	},
	Image {
		link_type: LinkType,
		dest_url: String,
		title: String,
		id: String,
	},
	MetadataBlock(MetadataBlockKind),
	Superscript,
	Subscript,
}

impl OwnedTag {
	pub fn from_tag(tag: Tag<'_>) -> Self {
		match tag {
			Tag::Paragraph => OwnedTag::Paragraph,
			Tag::Heading { level, id, classes, attrs } => OwnedTag::Heading {
				level,
				id: id.map(|s| s.into_string()),
				classes: classes.into_iter().map(|s| s.into_string()).collect(),
				attrs: attrs.into_iter().map(|(k, v)| (k.into_string(), v.map(|s| s.into_string()))).collect(),
			},
			Tag::BlockQuote(kind) => OwnedTag::BlockQuote(kind),
			Tag::CodeBlock(kind) => OwnedTag::CodeBlock(OwnedCodeBlockKind::from_kind(kind)),
			Tag::HtmlBlock => OwnedTag::HtmlBlock,
			Tag::List(start) => OwnedTag::List(start),
			Tag::Item => OwnedTag::Item,
			Tag::FootnoteDefinition(name) => OwnedTag::FootnoteDefinition(name.into_string()),
			Tag::DefinitionList => OwnedTag::DefinitionList,
			Tag::DefinitionListTitle => OwnedTag::DefinitionListTitle,
			Tag::DefinitionListDefinition => OwnedTag::DefinitionListDefinition,
			Tag::Table(alignments) => OwnedTag::Table(alignments),
			Tag::TableHead => OwnedTag::TableHead,
			Tag::TableRow => OwnedTag::TableRow,
			Tag::TableCell => OwnedTag::TableCell,
			Tag::Emphasis => OwnedTag::Emphasis,
			Tag::Strong => OwnedTag::Strong,
			Tag::Strikethrough => OwnedTag::Strikethrough,
			Tag::Link { link_type, dest_url, title, id } => OwnedTag::Link {
				link_type,
				dest_url: dest_url.into_string(),
				title: title.into_string(),
				id: id.into_string(),
			},
			Tag::Image { link_type, dest_url, title, id } => OwnedTag::Image {
				link_type,
				dest_url: dest_url.into_string(),
				title: title.into_string(),
				id: id.into_string(),
			},
			Tag::MetadataBlock(kind) => OwnedTag::MetadataBlock(kind),
			Tag::Superscript => OwnedTag::Superscript,
			Tag::Subscript => OwnedTag::Subscript,
		}
	}

	pub fn to_tag(&self) -> Tag<'_> {
		match self {
			OwnedTag::Paragraph => Tag::Paragraph,
			OwnedTag::Heading { level, id, classes, attrs } => Tag::Heading {
				level: *level,
				id: id.as_deref().map(Into::into),
				classes: classes.iter().map(|s| s.as_str().into()).collect(),
				attrs: attrs.iter().map(|(k, v)| (k.as_str().into(), v.as_deref().map(Into::into))).collect(),
			},
			OwnedTag::BlockQuote(kind) => Tag::BlockQuote(*kind),
			OwnedTag::CodeBlock(kind) => Tag::CodeBlock(kind.to_kind()),
			OwnedTag::HtmlBlock => Tag::HtmlBlock,
			OwnedTag::List(start) => Tag::List(*start),
			OwnedTag::Item => Tag::Item,
			OwnedTag::FootnoteDefinition(name) => Tag::FootnoteDefinition(name.as_str().into()),
			OwnedTag::DefinitionList => Tag::DefinitionList,
			OwnedTag::DefinitionListTitle => Tag::DefinitionListTitle,
			OwnedTag::DefinitionListDefinition => Tag::DefinitionListDefinition,
			OwnedTag::Table(alignments) => Tag::Table(alignments.clone()),
			OwnedTag::TableHead => Tag::TableHead,
			OwnedTag::TableRow => Tag::TableRow,
			OwnedTag::TableCell => Tag::TableCell,
			OwnedTag::Emphasis => Tag::Emphasis,
			OwnedTag::Strong => Tag::Strong,
			OwnedTag::Strikethrough => Tag::Strikethrough,
			OwnedTag::Link { link_type, dest_url, title, id } => Tag::Link {
				link_type: *link_type,
				dest_url: dest_url.as_str().into(),
				title: title.as_str().into(),
				id: id.as_str().into(),
			},
			OwnedTag::Image { link_type, dest_url, title, id } => Tag::Image {
				link_type: *link_type,
				dest_url: dest_url.as_str().into(),
				title: title.as_str().into(),
				id: id.as_str().into(),
			},
			OwnedTag::MetadataBlock(kind) => Tag::MetadataBlock(*kind),
			OwnedTag::Superscript => Tag::Superscript,
			OwnedTag::Subscript => Tag::Subscript,
		}
	}
}

/// An owned tag end for End events.
#[derive(Clone, Debug, PartialEq)]
pub enum OwnedTagEnd {
	Paragraph,
	Heading(HeadingLevel),
	BlockQuote(Option<BlockQuoteKind>),
	CodeBlock,
	HtmlBlock,
	List(bool),
	Item,
	FootnoteDefinition,
	DefinitionList,
	DefinitionListTitle,
	DefinitionListDefinition,
	Table,
	TableHead,
	TableRow,
	TableCell,
	Emphasis,
	Strong,
	Strikethrough,
	Link,
	Image,
	MetadataBlock(MetadataBlockKind),
	Superscript,
	Subscript,
}

impl OwnedTagEnd {
	pub fn from_tag_end(tag_end: TagEnd) -> Self {
		match tag_end {
			TagEnd::Paragraph => OwnedTagEnd::Paragraph,
			TagEnd::Heading(level) => OwnedTagEnd::Heading(level),
			TagEnd::BlockQuote(kind) => OwnedTagEnd::BlockQuote(kind),
			TagEnd::CodeBlock => OwnedTagEnd::CodeBlock,
			TagEnd::HtmlBlock => OwnedTagEnd::HtmlBlock,
			TagEnd::List(ordered) => OwnedTagEnd::List(ordered),
			TagEnd::Item => OwnedTagEnd::Item,
			TagEnd::FootnoteDefinition => OwnedTagEnd::FootnoteDefinition,
			TagEnd::DefinitionList => OwnedTagEnd::DefinitionList,
			TagEnd::DefinitionListTitle => OwnedTagEnd::DefinitionListTitle,
			TagEnd::DefinitionListDefinition => OwnedTagEnd::DefinitionListDefinition,
			TagEnd::Table => OwnedTagEnd::Table,
			TagEnd::TableHead => OwnedTagEnd::TableHead,
			TagEnd::TableRow => OwnedTagEnd::TableRow,
			TagEnd::TableCell => OwnedTagEnd::TableCell,
			TagEnd::Emphasis => OwnedTagEnd::Emphasis,
			TagEnd::Strong => OwnedTagEnd::Strong,
			TagEnd::Strikethrough => OwnedTagEnd::Strikethrough,
			TagEnd::Link => OwnedTagEnd::Link,
			TagEnd::Image => OwnedTagEnd::Image,
			TagEnd::MetadataBlock(kind) => OwnedTagEnd::MetadataBlock(kind),
			TagEnd::Superscript => OwnedTagEnd::Superscript,
			TagEnd::Subscript => OwnedTagEnd::Subscript,
		}
	}

	pub fn to_tag_end(&self) -> TagEnd {
		match self {
			OwnedTagEnd::Paragraph => TagEnd::Paragraph,
			OwnedTagEnd::Heading(level) => TagEnd::Heading(*level),
			OwnedTagEnd::BlockQuote(kind) => TagEnd::BlockQuote(*kind),
			OwnedTagEnd::CodeBlock => TagEnd::CodeBlock,
			OwnedTagEnd::HtmlBlock => TagEnd::HtmlBlock,
			OwnedTagEnd::List(ordered) => TagEnd::List(*ordered),
			OwnedTagEnd::Item => TagEnd::Item,
			OwnedTagEnd::FootnoteDefinition => TagEnd::FootnoteDefinition,
			OwnedTagEnd::DefinitionList => TagEnd::DefinitionList,
			OwnedTagEnd::DefinitionListTitle => TagEnd::DefinitionListTitle,
			OwnedTagEnd::DefinitionListDefinition => TagEnd::DefinitionListDefinition,
			OwnedTagEnd::Table => TagEnd::Table,
			OwnedTagEnd::TableHead => TagEnd::TableHead,
			OwnedTagEnd::TableRow => TagEnd::TableRow,
			OwnedTagEnd::TableCell => TagEnd::TableCell,
			OwnedTagEnd::Emphasis => TagEnd::Emphasis,
			OwnedTagEnd::Strong => TagEnd::Strong,
			OwnedTagEnd::Strikethrough => TagEnd::Strikethrough,
			OwnedTagEnd::Link => TagEnd::Link,
			OwnedTagEnd::Image => TagEnd::Image,
			OwnedTagEnd::MetadataBlock(kind) => TagEnd::MetadataBlock(*kind),
			OwnedTagEnd::Superscript => TagEnd::Superscript,
			OwnedTagEnd::Subscript => TagEnd::Subscript,
		}
	}
}

/// An owned code block kind.
#[derive(Clone, Debug, PartialEq)]
pub enum OwnedCodeBlockKind {
	Indented,
	Fenced(String),
}

impl OwnedCodeBlockKind {
	pub fn from_kind(kind: CodeBlockKind<'_>) -> Self {
		match kind {
			CodeBlockKind::Indented => OwnedCodeBlockKind::Indented,
			CodeBlockKind::Fenced(info) => OwnedCodeBlockKind::Fenced(info.into_string()),
		}
	}

	pub fn to_kind(&self) -> CodeBlockKind<'_> {
		match self {
			OwnedCodeBlockKind::Indented => CodeBlockKind::Indented,
			OwnedCodeBlockKind::Fenced(info) => CodeBlockKind::Fenced(info.as_str().into()),
		}
	}
}

/// A sequence of owned markdown events.
/// This is the primary type for storing markdown content.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Events(pub Vec<OwnedEvent>);

impl Events {
	/// Create a new empty events sequence.
	pub fn new() -> Self {
		Self(Vec::new())
	}

	/// Parse markdown content into events.
	pub fn parse(content: &str) -> Self {
		use pulldown_cmark::{Options, Parser};
		let options = Options::ENABLE_TASKLISTS | Options::ENABLE_STRIKETHROUGH;
		let parser = Parser::new_ext(content, options);
		let events: Vec<OwnedEvent> = parser.map(OwnedEvent::from_event).collect();
		Self(events)
	}

	/// Render events back to markdown.
	/// Note: This may not produce identical output to the original due to markdown normalization.
	pub fn render(&self) -> String {
		use pulldown_cmark_to_cmark::cmark;
		let events = self.0.iter().map(|e| e.to_event());
		let mut output = String::new();
		// Use pulldown-cmark-to-cmark for proper markdown output
		cmark(events, &mut output).expect("markdown rendering should not fail");
		output
	}

	/// Check if the events sequence is empty.
	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	/// Get the number of events.
	pub fn len(&self) -> usize {
		self.0.len()
	}

	/// Get a reference to the underlying events.
	pub fn events(&self) -> &[OwnedEvent] {
		&self.0
	}

	/// Get a mutable reference to the underlying events.
	pub fn events_mut(&mut self) -> &mut Vec<OwnedEvent> {
		&mut self.0
	}

	/// Extract plain text from events (for display/comparison purposes).
	pub fn plain_text(&self) -> String {
		let mut text = String::new();
		for event in &self.0 {
			match event {
				OwnedEvent::Text(t) => text.push_str(t),
				OwnedEvent::Code(c) => text.push_str(c),
				OwnedEvent::SoftBreak | OwnedEvent::HardBreak => text.push(' '),
				_ => {}
			}
		}
		text
	}

	/// Create Events from a simple string (wraps in paragraph).
	pub fn from_plain_text(text: &str) -> Self {
		if text.is_empty() {
			return Self::new();
		}
		Self(vec![
			OwnedEvent::Start(OwnedTag::Paragraph),
			OwnedEvent::Text(text.to_string()),
			OwnedEvent::End(OwnedTagEnd::Paragraph),
		])
	}

	/// Create Events from inline content (no paragraph wrapper).
	pub fn from_inline_text(text: &str) -> Self {
		if text.is_empty() {
			return Self::new();
		}
		Self(vec![OwnedEvent::Text(text.to_string())])
	}
}

impl fmt::Display for Events {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.render())
	}
}

impl From<String> for Events {
	fn from(s: String) -> Self {
		Self::parse(&s)
	}
}

impl From<&str> for Events {
	fn from(s: &str) -> Self {
		Self::parse(s)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_simple_text() {
		let events = Events::parse("Hello world");
		assert!(!events.is_empty());
		assert_eq!(events.plain_text(), "Hello world");
	}

	#[test]
	fn test_parse_with_formatting() {
		let events = Events::parse("Hello **bold** and `code`");
		let plain = events.plain_text();
		assert!(plain.contains("Hello"));
		assert!(plain.contains("bold"));
		assert!(plain.contains("code"));
	}

	#[test]
	fn test_roundtrip_simple() {
		let original = "Simple paragraph.";
		let events = Events::parse(original);
		let rendered = events.render();
		// The rendered output should contain the same text
		assert!(rendered.contains("Simple paragraph"));
	}

	#[test]
	fn test_from_plain_text() {
		let events = Events::from_plain_text("Test");
		assert_eq!(events.len(), 3); // Start(Paragraph), Text, End(Paragraph)
		assert_eq!(events.plain_text(), "Test");
	}

	#[test]
	fn test_from_inline_text() {
		let events = Events::from_inline_text("Test");
		assert_eq!(events.len(), 1); // Just Text
		assert_eq!(events.plain_text(), "Test");
	}

	#[test]
	fn test_empty() {
		let events = Events::new();
		assert!(events.is_empty());
		assert_eq!(events.len(), 0);
	}
}
