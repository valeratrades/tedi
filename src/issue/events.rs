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
	/// Checkbox: raw contents between `[` and `]`.
	/// Standard: `" "` (unchecked), `"x"` (checked).
	/// Custom: anything else (e.g. `"."` for partial).
	CheckBox(String),
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
			Event::TaskListMarker(checked) => OwnedEvent::CheckBox(if checked { "x" } else { " " }.into()),
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
			OwnedEvent::CheckBox(inner) => match inner.as_str() {
				" " => Event::TaskListMarker(false),
				"x" => Event::TaskListMarker(true),
				_ => panic!("CheckBox(\"{inner}\") cannot be converted to Event directly; use prepare_for_render()"),
			},
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
#[derive(Clone, Debug, Default, derive_more::Deref, derive_more::IntoIterator)]
pub struct Events(Vec<OwnedEvent>);
impl Events {
	/// Parse markdown content into events.
	///
	/// Handles checkbox coercion inline: pulldown_cmark turns `\[.\]` into
	/// `Text("[.")` + `Text("] rest")` — we detect this pattern inside list items
	/// and replace with `CheckBox(inner)` + optional `Text(rest)`.
	pub fn parse(content: &str) -> Self {
		use pulldown_cmark::Parser;
		let parser = Parser::new_ext(content, parser_options());
		let raw: Vec<OwnedEvent> = parser.map(OwnedEvent::from_event).collect();

		// Single pass: detect escaped-checkbox patterns and coerce them.
		// After Start(Item) [+ optional Start(Paragraph)], detect:
		//   2-event: Text("[<inner>") + Text("] <rest>")
		//   4-event: Text("[") + Text(<inner>) + Text("]") + Text(" <rest>")
		// Replace with CheckBox(inner) + optional Text(rest).
		let mut events = Vec::with_capacity(raw.len());
		let mut i = 0;
		while i < raw.len() {
			if !matches!(&raw[i], OwnedEvent::Start(OwnedTag::Item)) {
				events.push(raw[i].clone());
				i += 1;
				continue;
			}
			// Push Start(Item)
			events.push(raw[i].clone());
			i += 1;

			// Skip optional Start(Paragraph)
			let had_paragraph = i < raw.len() && matches!(&raw[i], OwnedEvent::Start(OwnedTag::Paragraph));
			if had_paragraph {
				events.push(raw[i].clone());
				i += 1;
			}

			// Already a CheckBox (from TaskListMarker) — skip coercion
			if i < raw.len() && matches!(&raw[i], OwnedEvent::CheckBox(_)) {
				continue;
			}

			// Try 4-event pattern: Text("[") + Text(<inner>) + Text("]") + Text(" <rest>")
			if i + 3 < raw.len() {
				if let (OwnedEvent::Text(open), OwnedEvent::Text(inner), OwnedEvent::Text(close), OwnedEvent::Text(rest)) = (&raw[i], &raw[i + 1], &raw[i + 2], &raw[i + 3]) {
					if open == "[" && close == "]" && (rest.is_empty() || rest.starts_with(' ')) {
						events.push(OwnedEvent::CheckBox(inner.clone()));
						let rest = rest.strip_prefix(' ').unwrap_or(rest);
						if !rest.is_empty() {
							events.push(OwnedEvent::Text(rest.to_string()));
						}
						i += 4;
						continue;
					}
				}
			}

			// Try 2-event pattern: Text("[<inner>") + Text("] <rest>")
			if i + 1 < raw.len() {
				if let (OwnedEvent::Text(first), OwnedEvent::Text(second)) = (&raw[i], &raw[i + 1]) {
					if let Some(inner) = first.strip_prefix('[') {
						if let Some(rest) = second.strip_prefix(']') {
							if rest.is_empty() || rest.starts_with(' ') {
								events.push(OwnedEvent::CheckBox(inner.to_string()));
								let rest = rest.strip_prefix(' ').unwrap_or(rest);
								if !rest.is_empty() {
									events.push(OwnedEvent::Text(rest.to_string()));
								}
								i += 2;
								continue;
							}
						}
					}
				}
			}
			// No pattern matched — continue normally
		}

		Self(split_blockers_from_checkboxes(events))
	}
}

/// Compare by rendered markdown, not by event structure.
///
/// Different sources (tight vs loose list items, standalone text) produce structurally
/// different events for the same content (e.g. with/without Paragraph wrappers).
/// Comparing rendered output makes merge/sync ignore these irrelevant structural differences.
impl PartialEq for Events {
	fn eq(&self, other: &Self) -> bool {
		if self.0 == other.0 {
			return true;
		}
		render_events(&self.0) == render_events(&other.0)
	}
}

/// Split a list after a `# Blockers` heading at the point where checkbox items begin.
///
/// pulldown_cmark merges all consecutive `- ` items into a single list, even when
/// some are plain blocker items and some are checkbox items from a semantically separate list.
/// We split them, but ONLY when all conditions are met:
/// 1. A `Marker::BlockersSection` heading immediately precedes the list
/// 2. The list starts with non-checkbox items (the actual blockers)
/// 3. At some point checkbox items begin — that's where we split
///
/// Milestone lists (where checkboxes and plain refs coexist) are left intact.
fn split_blockers_from_checkboxes(events: Vec<OwnedEvent>) -> Vec<OwnedEvent> {
	let mut out = Vec::with_capacity(events.len());
	let mut i = 0;

	while i < events.len() {
		if !matches!(&events[i], OwnedEvent::Start(OwnedTag::List(_))) {
			out.push(events[i].clone());
			i += 1;
			continue;
		}

		let list_start_event = events[i].clone();
		let list_end = find_matching_end_list(&events, i);

		// Check condition 1: is this list immediately preceded by a BlockersSection heading?
		if !preceded_by_blockers_heading(&out) {
			// Not a blockers list — pass through unchanged, but recurse into nested lists
			out.push(events[i].clone());
			i += 1;
			continue;
		}

		// Walk items at depth=1, record (start_idx, end_idx, has_checkbox)
		let mut items: Vec<(usize, usize, bool)> = Vec::new();
		let mut depth = 0;
		let mut j = i;
		while j < list_end {
			match &events[j] {
				OwnedEvent::Start(OwnedTag::List(_)) => depth += 1,
				OwnedEvent::End(OwnedTagEnd::List(_)) => depth -= 1,
				OwnedEvent::Start(OwnedTag::Item) if depth == 1 => {
					let item_start = j;
					let item_end = find_matching_end_item(&events, j);
					let has_checkbox = item_has_checkbox(&events[item_start..item_end]);
					items.push((item_start, item_end, has_checkbox));
					j = item_end;
					continue;
				}
				_ => {}
			}
			j += 1;
		}

		// Condition 2: list must start with non-checkbox items
		// Condition 3: must transition to checkbox at some point
		let first_checkbox_idx = items.iter().position(|(_, _, has)| *has);
		let should_split = match first_checkbox_idx {
			Some(idx) if idx > 0 => true, // starts non-checkbox, then transitions
			_ => false,
		};

		if !should_split {
			out.push(events[i].clone());
			i += 1;
			continue;
		}

		let split_at = first_checkbox_idx.unwrap();

		// Emit blocker items as one list
		out.push(list_start_event.clone());
		for &(item_start, item_end, _) in &items[..split_at] {
			out.extend(events[item_start..item_end].iter().cloned());
		}
		out.push(OwnedEvent::End(OwnedTagEnd::List(false)));

		// Emit checkbox items as a separate list
		out.push(list_start_event.clone());
		for &(item_start, item_end, _) in &items[split_at..] {
			out.extend(events[item_start..item_end].iter().cloned());
		}
		out.push(OwnedEvent::End(OwnedTagEnd::List(false)));

		i = list_end;
	}

	out
}

/// Check whether the events accumulated so far end with a BlockersSection heading.
/// Reconstructs the heading text from events and delegates to `Marker::decode`.
fn preceded_by_blockers_heading(out: &[OwnedEvent]) -> bool {
	// Walk backwards: expect End(Heading), then text events, then Start(Heading)
	let Some(last) = out.last() else { return false };
	let OwnedEvent::End(OwnedTagEnd::Heading(level)) = last else { return false };

	// Collect heading text by walking backwards past the End(Heading)
	let mut heading_text = String::new();
	let mut found_start = false;
	for ev in out[..out.len() - 1].iter().rev() {
		match ev {
			OwnedEvent::Start(OwnedTag::Heading { level: start_level, .. }) if start_level == level => {
				found_start = true;
				break;
			}
			OwnedEvent::Text(t) => {
				// Prepend (since we're walking backwards)
				heading_text.insert_str(0, t);
			}
			_ => break, // unexpected event inside heading
		}
	}
	if !found_start {
		return false;
	}

	// Reconstruct the line as it would appear in markdown
	let hashes: String = "#".repeat(match level {
		HeadingLevel::H1 => 1,
		HeadingLevel::H2 => 2,
		HeadingLevel::H3 => 3,
		HeadingLevel::H4 => 4,
		HeadingLevel::H5 => 5,
		HeadingLevel::H6 => 6,
	});
	let line = format!("{hashes} {heading_text}");

	matches!(super::Marker::decode(&line), Some(super::Marker::BlockersSection(_)))
}

fn find_matching_end_list(events: &[OwnedEvent], start: usize) -> usize {
	let mut depth = 0;
	for (j, ev) in events[start..].iter().enumerate() {
		match ev {
			OwnedEvent::Start(OwnedTag::List(_)) => depth += 1,
			OwnedEvent::End(OwnedTagEnd::List(_)) => {
				depth -= 1;
				if depth == 0 {
					return start + j + 1;
				}
			}
			_ => {}
		}
	}
	events.len()
}

fn find_matching_end_item(events: &[OwnedEvent], start: usize) -> usize {
	let mut depth = 0;
	for (j, ev) in events[start..].iter().enumerate() {
		match ev {
			OwnedEvent::Start(OwnedTag::Item) => depth += 1,
			OwnedEvent::End(OwnedTagEnd::Item) => {
				depth -= 1;
				if depth == 0 {
					return start + j + 1;
				}
			}
			_ => {}
		}
	}
	events.len()
}

fn item_has_checkbox(item_events: &[OwnedEvent]) -> bool {
	let mut i = 0;
	if matches!(item_events.get(i), Some(OwnedEvent::Start(OwnedTag::Item))) {
		i += 1;
	}
	if matches!(item_events.get(i), Some(OwnedEvent::Start(OwnedTag::Paragraph))) {
		i += 1;
	}
	matches!(item_events.get(i), Some(OwnedEvent::CheckBox(_)))
}

fn parser_options() -> pulldown_cmark::Options {
	pulldown_cmark::Options::ENABLE_TASKLISTS | pulldown_cmark::Options::ENABLE_STRIKETHROUGH
}

pub(crate) fn cmark_options() -> pulldown_cmark_to_cmark::Options<'static> {
	pulldown_cmark_to_cmark::Options {
		list_token: '-',
		newlines_after_headline: 1,
		newlines_after_paragraph: 1,
		..Default::default()
	}
}

/// Convert `&[OwnedEvent]` to `Vec<Event>` ready for `pulldown_cmark_to_cmark`.
///
/// Handles:
/// 1. `CheckBox(" ")`/`CheckBox("x")` → `TaskListMarker`
/// 2. Custom `CheckBox` → escaped `Text("[<inner>\] ")`
/// 3. Loose-list spacing: inserts `Html("\n")` between items in lists that contain checkboxes
pub(crate) fn prepare_for_render(events: &[OwnedEvent]) -> Vec<Event<'_>> {
	let mut out = Vec::with_capacity(events.len());

	// Identify which list ranges contain checkboxes (for loose inter-item spacing).
	struct ListInfo {
		start_idx: usize,
		has_checkbox: bool,
	}
	let mut list_stack: Vec<ListInfo> = Vec::new();
	let mut checkbox_lists: Vec<(usize, usize)> = Vec::new();

	for (i, ev) in events.iter().enumerate() {
		match ev {
			OwnedEvent::Start(OwnedTag::List(_)) => {
				list_stack.push(ListInfo { start_idx: i, has_checkbox: false });
			}
			OwnedEvent::CheckBox(_) =>
				if let Some(info) = list_stack.last_mut() {
					info.has_checkbox = true;
				},
			OwnedEvent::End(OwnedTagEnd::List(_)) =>
				if let Some(info) = list_stack.pop() {
					if info.has_checkbox {
						checkbox_lists.push((info.start_idx, i));
					}
				},
			_ => {}
		}
	}

	// Build a set of Item end indices where we should insert Html("\n") for loose spacing.
	let mut loose_item_ends: std::collections::HashSet<usize> = std::collections::HashSet::new();
	for (list_start, list_end) in &checkbox_lists {
		let mut depth = 0;
		let mut item_ends = Vec::new();
		for (i, ev) in events[*list_start..=*list_end].iter().enumerate() {
			let abs_i = list_start + i;
			match ev {
				OwnedEvent::Start(OwnedTag::List(_)) => depth += 1,
				OwnedEvent::End(OwnedTagEnd::List(_)) => depth -= 1,
				OwnedEvent::End(OwnedTagEnd::Item) if depth == 1 => item_ends.push(abs_i),
				_ => {}
			}
		}
		if item_ends.len() > 1 {
			for &idx in &item_ends[..item_ends.len() - 1] {
				loose_item_ends.insert(idx);
			}
		}
	}

	for (i, ev) in events.iter().enumerate() {
		match ev {
			OwnedEvent::CheckBox(inner) => match inner.as_str() {
				" " => out.push(Event::TaskListMarker(false)),
				"x" => out.push(Event::TaskListMarker(true)),
				_ => out.push(Event::Text(format!("[{inner}\\] ").into())),
			},
			_ => out.push(ev.to_event()),
		}
		if loose_item_ends.contains(&i) {
			out.push(Event::Html("\n".into()));
		}
	}

	out
}

/// Append `content` to `out`, prefixing each line with `prefix`.
/// Preserves trailing newline from content.
pub(super) fn indent_into(out: &mut String, content: &str, prefix: &str) {
	for line in content.lines() {
		out.push_str(prefix);
		out.push_str(line);
		out.push('\n');
	}
}

/// Render a slice of `OwnedEvent`s to markdown using the shared cmark options.
fn render_events(events: &[OwnedEvent]) -> String {
	let prepared = prepare_for_render(events);
	let mut output = String::new();
	pulldown_cmark_to_cmark::cmark_with_options(prepared.into_iter(), &mut output, cmark_options()).expect("markdown rendering should not fail");
	output
}

impl From<Events> for String {
	fn from(events: Events) -> Self {
		render_events(&events)
	}
}

impl fmt::Display for Events {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", render_events(self))
	}
}

impl From<Vec<OwnedEvent>> for Events {
	fn from(events: Vec<OwnedEvent>) -> Self {
		Self(events)
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
		let rendered: String = events.into();
		assert!(rendered.contains("Hello world"));
	}

	#[test]
	fn test_parse_with_formatting() {
		let events = Events::parse("Hello **bold** and `code`");
		let rendered: String = events.into();
		assert!(rendered.contains("Hello"));
		assert!(rendered.contains("bold"));
		assert!(rendered.contains("code"));
	}

	#[test]
	fn test_roundtrip_simple() {
		let original = "Simple paragraph.";
		let events = Events::parse(original);
		let rendered: String = events.into();
		assert!(rendered.contains("Simple paragraph"));
	}

	#[test]
	fn test_empty() {
		let events = Events::default();
		assert!(events.is_empty());
		assert_eq!(events.len(), 0);
	}

	#[test]
	fn blockers_list_split_from_checkbox_items() {
		// After # Blockers heading, non-checkbox items then checkbox items → split
		let events = Events::parse("# Blockers\n- a\n- b\n- [ ] item\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 2, "blockers + checkbox tight should split into 2 lists");

		let events = Events::parse("# Blockers\n- a\n- b\n\n- [ ] item\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 2, "blockers + checkbox loose should split into 2 lists");
	}

	#[test]
	fn milestone_mixed_list_stays_unified() {
		// Milestone: checkbox + plain ref in same list → must NOT split
		let events = Events::parse("- [ ] one item\n- ref in the same list\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 1, "milestone mixed tight should stay as 1 list");

		let events = Events::parse("- [ ] one item\n\n- ref in the same list\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 1, "milestone mixed loose should stay as 1 list");
	}

	#[test]
	fn non_blockers_mixed_list_stays_unified() {
		// Mixed list without blockers heading → must NOT split
		let events = Events::parse("- a\n- [ ] item\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 1, "mixed list without heading should stay as 1 list");
	}

	#[test]
	fn blockers_all_checkbox_stays_unified() {
		// # Blockers followed by all-checkbox list → no split needed
		let events = Events::parse("# Blockers\n- [ ] a\n- [ ] b\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 1, "all-checkbox list after blockers heading should stay as 1 list");
	}

	#[test]
	fn loose_list_no_padding_before_body() {
		let input = "- [ ] item\n\n  body text\n";
		let rendered: String = Events::parse(input).into();
		insta::assert_snapshot!(rendered, @"
		- [ ] item
		  body text
		");
	}

	#[test]
	fn blockers_starts_with_checkbox_no_split() {
		// # Blockers where first item is checkbox → don't split (condition: must START non-checkbox)
		let events = Events::parse("# Blockers\n- [ ] a\n- b\n");
		let list_count = events.iter().filter(|e| matches!(e, OwnedEvent::Start(OwnedTag::List(_)))).count();
		assert_eq!(list_count, 1, "checkbox-first blockers list should stay as 1 list");
	}
}
