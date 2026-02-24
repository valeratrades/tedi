//! Parsing and serialization of milestone descriptions as structured documents.
//!
//! Milestones contain a mix of free-form markdown and nested lists with issue refs.
//! This module parses them into a `MilestoneDoc` AST via pulldown_cmark, enabling
//! structured operations like parent-context resolution for bare `#123` refs.

use super::{Events, Issue, IssueLink, IssueMarker, OwnedEvent, OwnedTag, OwnedTagEnd, events::indent_into};

/// A parsed milestone document: a sequence of top-level sections.
pub struct MilestoneDoc {
	sections: Vec<MilestoneSection>,
}
impl MilestoneDoc {
	pub fn parse(content: &str) -> Self {
		let events = Events::parse(content);
		let mut pos = 0;
		let sections = parse_sections(&events, &mut pos, None, 0);
		let mut doc = Self { sections };
		doc.normalize_lists();
		doc
	}

	/// Resolve bare `#N` refs using their parent list item's text as repo context.
	pub fn resolve_bare_refs(&mut self) {
		for section in &mut self.sections {
			resolve_section(section, None);
		}
	}

	/// Collect all issue links from the document.
	pub fn issue_links(&self) -> Vec<IssueLink> {
		let mut links = Vec::new();
		for section in &self.sections {
			collect_links_from_section(section, &mut links);
		}
		links
	}

	/// Iterate over items that are embedded issues (have title + linked marker).
	/// Returns the `IssueLink` and the serialized section text (title + children).
	pub fn embedded_issues(&self) -> Vec<(IssueLink, String)> {
		let mut result = Vec::new();
		for section in &self.sections {
			collect_embedded_from_section(section, &mut result);
		}
		result
	}

	/// Expand issue refs in-place using pre-loaded serialized views.
	/// `expansions` maps issue number → serialized blocker view (from `serialize_blockers_view`).
	/// Items with no matching expansion are left unchanged.
	pub fn expand_with(&mut self, expansions: &std::collections::HashMap<u64, String>) {
		for section in &mut self.sections {
			expand_section(section, expansions);
		}
		self.normalize_lists();
	}

	/// Collapse all embedded issues and shorthand refs to bare URLs for storage.
	pub fn collapse_to_links(&mut self) {
		for section in &mut self.sections {
			collapse_section(section);
		}
		self.normalize_lists();
	}

	fn normalize_lists(&mut self) {
		for section in &mut self.sections {
			if let MilestoneSection::List(list) = section {
				if list.normalize_item_types() {
					let kind = if list.is_checkbox_list() { "checkbox (- [..])" } else { "plain (- )" };
					eprintln!("warning: mixed list item types detected; normalized to {kind} (determined by first element)");
				}
			}
		}
	}

	pub fn serialize(&self) -> String {
		let mut output = String::new();
		for section in &self.sections {
			serialize_section(section, &mut output);
		}
		output
	}
}

/// Serialize an issue as title line + blockers only (for milestone embedding).
pub fn serialize_blockers_view(issue: &Issue) -> String {
	let checkbox_contents = issue.contents.state.to_checkbox_contents();
	let issue_marker = IssueMarker::from(&issue.identity);
	let labels_part = if issue.contents.labels.is_empty() {
		String::new()
	} else {
		format!("({}) ", issue.contents.labels.join(", "))
	};

	// Build events: a single list item with inline title + optional blocker children
	let mut events = Vec::new();
	events.push(OwnedEvent::Start(OwnedTag::List(None)));
	events.push(OwnedEvent::Start(OwnedTag::Item));

	// Checkbox + title + marker as inline content
	events.push(OwnedEvent::CheckBox(checkbox_contents));
	events.push(OwnedEvent::Text(format!("{labels_part}{} ", issue.contents.title)));
	events.push(OwnedEvent::InlineHtml(format!("<!-- {} -->", issue_marker.encode())));

	// Blockers section as children
	if !issue.contents.blockers.is_empty() {
		// Wrap inline in paragraph since we have children
		// (need to rebuild: insert Paragraph start/end around inline content)
		let inline_start = 2; // after Start(List), Start(Item)
		events.insert(inline_start, OwnedEvent::Start(OwnedTag::Paragraph));
		events.push(OwnedEvent::End(OwnedTagEnd::Paragraph));

		let header = crate::Header::new(1, "Blockers");
		events.push(OwnedEvent::Start(OwnedTag::Heading {
			level: pulldown_cmark::HeadingLevel::H1,
			id: None,
			classes: Vec::new(),
			attrs: Vec::new(),
		}));
		events.push(OwnedEvent::Text(header.content.clone()));
		events.push(OwnedEvent::End(OwnedTagEnd::Heading(pulldown_cmark::HeadingLevel::H1)));
		events.extend(issue.contents.blockers.to_events());
	}

	events.push(OwnedEvent::End(OwnedTagEnd::Item));
	events.push(OwnedEvent::End(OwnedTagEnd::List(false)));

	Events::from(events).into()
}
/// Parse blockers from an embedded issue section in milestone content.
/// The section is: title line, then optionally a `# Blockers` header + blocker lines.
pub fn parse_blockers_from_embedded(section: &str) -> super::BlockerSequence {
	let lines: Vec<&str> = section.lines().collect();
	if lines.len() < 2 {
		return super::BlockerSequence::default();
	}

	// Find the blockers header (at one level of indent — tab or spaces)
	let mut blockers_start = None;
	let mut select_blockers = false;
	for (idx, line) in lines.iter().enumerate().skip(1) {
		let content = line.trim_start();
		// Check for !s suffix
		let (effective, has_select) = match content.trim_end().strip_suffix("!s").or_else(|| content.trim_end().strip_suffix("!S")) {
			Some(before) => (before.trim_end(), true),
			None => (content, false),
		};
		if matches!(super::Marker::decode(effective), Some(super::Marker::BlockersSection(_))) {
			blockers_start = Some(idx + 1);
			if has_select {
				select_blockers = true;
			}
			break;
		}
		// Standalone `!s`
		if content.trim().eq_ignore_ascii_case("!s") {
			select_blockers = true;
		}
	}

	let Some(start) = blockers_start else {
		return super::BlockerSequence::default();
	};

	// Collect blocker lines (strip one level of indent — tab or 2 spaces)
	let blocker_lines: Vec<String> = lines[start..]
		.iter()
		.filter(|l| !l.trim().is_empty())
		.map(|l| l.strip_prefix('\t').or_else(|| l.strip_prefix("  ")).unwrap_or(l).to_string())
		.collect();

	let mut seq = super::BlockerSequence::parse(&blocker_lines.join("\n"));
	if select_blockers {
		seq.set_state = Some(super::BlockerSetState::Pending);
	}
	seq
}
/// A section is either free-form markdown content or a list.
enum MilestoneSection {
	/// Non-list content (headings, paragraphs, code blocks, etc.).
	FreeContent(Vec<OwnedEvent>),
	/// A bullet/task list, possibly nested.
	List(MilestoneList),
}

/// A list of items.
struct MilestoneList {
	items: Vec<MilestoneItem>,
}
impl MilestoneList {
	/// Whether this list uses checkboxes, determined by the first item.
	fn is_checkbox_list(&self) -> bool {
		self.items.first().is_some_and(|item| item.checkbox.is_some())
	}

	/// Normalize all items to match the list type of the first item.
	/// Returns `true` if any items were changed.
	fn normalize_item_types(&mut self) -> bool {
		let is_checkbox = self.is_checkbox_list();
		let mut changed = false;
		for item in &mut self.items {
			if is_checkbox && item.checkbox.is_none() {
				item.checkbox = Some(" ".into());
				changed = true;
			} else if !is_checkbox && item.checkbox.is_some() {
				item.checkbox = None;
				changed = true;
			}
		}
		changed
	}
}

/// A single list item.
struct MilestoneItem {
	/// The checkbox contents (between `[` and `]`), or None for plain items.
	checkbox: Option<String>,
	/// Semantic interpretation of this item's inline text.
	content: ItemContent,
	/// Nested content under this item (sub-lists, headings, etc.).
	children: Vec<MilestoneSection>,
}

/// Semantic interpretation of a list item's inline text.
enum ItemContent {
	/// A bare issue reference (URL or shorthand).
	Ref(super::issue_ref::IssueRef),
	/// Embedded issue with title + marker: `Title <!-- @user url -->`
	EmbeddedIssue { prefix_events: Vec<OwnedEvent>, marker: IssueMarker },
	/// Plain text (category headers like `discretionary_engine`, `valeratrades/tedi`, or anything else).
	Text(Vec<OwnedEvent>),
}

// ─── Parsing ─────────────────────────────────────────────────────────────────

/// Parse sections from the event stream until we hit `stop_at` or end of events.
/// `stop_at` is used for nested parsing inside list items — stops at `End(Item)`.
/// `depth` tracks list nesting (0 = not yet inside any list).
fn parse_sections(events: &[OwnedEvent], pos: &mut usize, stop_at: Option<OwnedTagEnd>, depth: usize) -> Vec<MilestoneSection> {
	let mut sections = Vec::new();
	let mut free_events: Vec<OwnedEvent> = Vec::new();

	while *pos < events.len() {
		// Check stop condition
		if let Some(ref stop) = stop_at
			&& matches!(&events[*pos], OwnedEvent::End(tag_end) if tag_end == stop)
		{
			break;
		}

		match &events[*pos] {
			OwnedEvent::Start(OwnedTag::List(_)) => {
				// Flush accumulated free content
				if !free_events.is_empty() {
					sections.push(MilestoneSection::FreeContent(std::mem::take(&mut free_events)));
				}
				sections.push(MilestoneSection::List(parse_list(events, pos, depth)));
			}
			_ => {
				free_events.push(events[*pos].clone());
				*pos += 1;
			}
		}
	}

	if !free_events.is_empty() {
		sections.push(MilestoneSection::FreeContent(free_events));
	}

	sections
}

/// Parse a `List` from the event stream. Expects `pos` to point at `Start(List(_))`.
/// `depth` is the nesting level (1 = top-level list, 2+ = nested).
fn parse_list(events: &[OwnedEvent], pos: &mut usize, depth: usize) -> MilestoneList {
	debug_assert!(matches!(&events[*pos], OwnedEvent::Start(OwnedTag::List(_))));
	let depth = depth + 1;
	*pos += 1; // consume Start(List)

	let mut items = Vec::new();

	while *pos < events.len() {
		match &events[*pos] {
			OwnedEvent::End(OwnedTagEnd::List(_)) => {
				*pos += 1;
				break;
			}
			OwnedEvent::Start(OwnedTag::Item) => {
				items.push(parse_item(events, pos, depth));
			}
			_ => {
				// Unexpected event inside list, skip
				*pos += 1;
			}
		}
	}

	MilestoneList { items }
}

/// Parse a single list `Item`. Expects `pos` to point at `Start(Item)`.
/// `depth` is the list nesting level this item belongs to.
fn parse_item(events: &[OwnedEvent], pos: &mut usize, depth: usize) -> MilestoneItem {
	debug_assert!(matches!(&events[*pos], OwnedEvent::Start(OwnedTag::Item)));
	*pos += 1; // consume Start(Item)

	// Collect CheckBox if present.
	// In loose lists, pulldown_cmark emits: Start(Item), Start(Paragraph), CheckBox
	// In tight lists: Start(Item), CheckBox
	// So we need to peek past an optional Start(Paragraph).
	let starts_with_paragraph = matches!(events.get(*pos), Some(OwnedEvent::Start(OwnedTag::Paragraph)));
	if starts_with_paragraph {
		*pos += 1;
	}
	let checkbox = if let Some(OwnedEvent::CheckBox(inner)) = events.get(*pos) {
		let inner = inner.clone();
		*pos += 1;
		Some(inner)
	} else {
		None
	};

	// Collect inline events (the item's own text content).
	// Strip paragraph wrappers that pulldown_cmark adds for loose lists — we control
	// looseness ourselves based on depth.
	let mut inline_events: Vec<OwnedEvent> = Vec::new();
	let mut in_paragraph = starts_with_paragraph;

	// Consume inline content: everything before child blocks or End(Item)
	while *pos < events.len() {
		match &events[*pos] {
			OwnedEvent::End(OwnedTagEnd::Item) => break,
			OwnedEvent::Start(OwnedTag::List(_)) | OwnedEvent::Start(OwnedTag::Heading { .. }) => break,
			OwnedEvent::Start(OwnedTag::Paragraph) => {
				// Also strip preceding SoftBreak (normalization artifact between title and body)
				if matches!(inline_events.last(), Some(OwnedEvent::SoftBreak)) {
					inline_events.pop();
				}
				in_paragraph = true;
				*pos += 1;
			}
			OwnedEvent::End(OwnedTagEnd::Paragraph) if in_paragraph => {
				in_paragraph = false;
				*pos += 1;
			}
			_ => {
				inline_events.push(events[*pos].clone());
				*pos += 1;
			}
		}
	}

	// Split inline events at SoftBreak if the tail is a bare URL or shorthand ref.
	// This handles the case where a category item has a URL continuation:
	//   `- discretionary_engine\n  https://github.com/.../issues/77`
	// pulldown_cmark emits [Text("discretionary_engine"), SoftBreak, Text("https://...")]
	let (content, softbreak_children) = split_inline_at_ref_softbreak(inline_events);

	// Collect children (sub-lists, headings, etc.) until End(Item)
	let mut children = softbreak_children;
	children.extend(parse_sections(events, pos, Some(OwnedTagEnd::Item), depth));

	// Consume End(Item)
	if matches!(events.get(*pos), Some(OwnedEvent::End(OwnedTagEnd::Item))) {
		*pos += 1;
	}

	MilestoneItem { checkbox, content, children }
}

/// Split inline events at a SoftBreak if the part after it is a bare URL or shorthand ref.
/// Returns (item_content, child_sections) where child_sections contains a single-item List if split occurred.
fn split_inline_at_ref_softbreak(events: Vec<OwnedEvent>) -> (ItemContent, Vec<MilestoneSection>) {
	// Find the last SoftBreak
	if let Some(break_pos) = events.iter().rposition(|e| matches!(e, OwnedEvent::SoftBreak)) {
		let tail = &events[break_pos + 1..];
		let tail_classified = classify_inline_events(tail.to_vec());
		if matches!(tail_classified, ItemContent::Ref(_)) {
			let head = events[..break_pos].to_vec();
			let content = classify_inline_events(head);
			let child_item = MilestoneItem {
				checkbox: None,
				content: tail_classified,
				children: Vec::new(),
			};
			let child_list = MilestoneList { items: vec![child_item] };
			return (content, vec![MilestoneSection::List(child_list)]);
		}
	}
	(classify_inline_events(events), Vec::new())
}

/// Classify the inline events of a list item into an `ItemContent`.
fn classify_inline_events(events: Vec<OwnedEvent>) -> ItemContent {
	// Concatenate all text and look for InlineHtml markers
	let mut full_text = String::new();
	let mut marker_html: Option<(usize, String)> = None; // (index, html_content)

	for (i, event) in events.iter().enumerate() {
		match event {
			OwnedEvent::Text(t) => full_text.push_str(t),
			OwnedEvent::InlineHtml(html) => {
				// Try to decode as an issue marker
				if let Some(inner) = html.strip_prefix("<!--").and_then(|s| s.strip_suffix("-->")) {
					let decoded = IssueMarker::decode(inner.trim());
					if matches!(decoded, IssueMarker::Linked { .. }) {
						marker_html = Some((i, html.clone()));
					}
				}
			}
			_ => {}
		}
	}

	// Case 1: Has a linked issue marker → EmbeddedIssue
	if let Some((marker_idx, html)) = marker_html {
		let inner = html.strip_prefix("<!--").unwrap().strip_suffix("-->").unwrap().trim();
		let marker = IssueMarker::decode(inner);
		let prefix_events: Vec<OwnedEvent> = events[..marker_idx].to_vec();
		return ItemContent::EmbeddedIssue { prefix_events, marker };
	}

	let trimmed = full_text.trim();

	// Case 2: Single word, no spaces → might be shorthand ref or bare URL
	if !trimmed.is_empty() && !trimmed.contains(' ') {
		if let Some(issue_ref) = super::issue_ref::IssueRef::parse_word(trimmed) {
			return ItemContent::Ref(issue_ref);
		}
	}

	// Case 3: plain text
	ItemContent::Text(events)
}

// ─── Operations ──────────────────────────────────────────────────────────────

fn resolve_section(section: &mut MilestoneSection, parent_context: Option<&str>) {
	if let MilestoneSection::List(list) = section {
		for item in list.items.iter_mut() {
			// Determine what context this item provides to its children
			let my_context = match &item.content {
				ItemContent::Text(events) => {
					let text = events_to_plain_text(events);
					let trimmed = text.trim();
					if !trimmed.is_empty() && !trimmed.contains(' ') { Some(trimmed.to_string()) } else { None }
				}
				_ => None,
			};

			// Resolve this item if it's a ref with missing owner/repo
			if let ItemContent::Ref(issue_ref) = &mut item.content {
				if let Some(ctx) = parent_context {
					issue_ref.resolve_with_context(ctx);
				}
			}

			// Recurse into children with this item's context
			let child_ctx = my_context.as_deref().or(parent_context);
			for child in &mut item.children {
				resolve_section(child, child_ctx);
			}
		}
	}
}

fn collect_links_from_section(section: &MilestoneSection, links: &mut Vec<IssueLink>) {
	match section {
		MilestoneSection::FreeContent(_) => {}
		MilestoneSection::List(list) =>
			for item in list.items.iter() {
				if let Some(link) = item_issue_link(item) {
					links.push(link);
				}
				for child in &item.children {
					collect_links_from_section(child, links);
				}
			},
	}
}

fn collect_embedded_from_section(section: &MilestoneSection, result: &mut Vec<(IssueLink, String)>) {
	if let MilestoneSection::List(list) = section {
		for item in list.items.iter() {
			if let ItemContent::EmbeddedIssue {
				marker: IssueMarker::Linked { link, .. },
				..
			} = &item.content
			{
				let section_text = serialize_item_to_section_text(item);
				result.push((link.clone(), section_text));
			}
			for child in &item.children {
				collect_embedded_from_section(child, result);
			}
		}
	}
}

fn expand_section(section: &mut MilestoneSection, expansions: &std::collections::HashMap<u64, String>) {
	if let MilestoneSection::List(list) = section {
		for item in list.items.iter_mut() {
			if let Some(link) = item_issue_link(item)
				&& let Some(view) = expansions.get(&link.number())
			{
				// Re-parse the serialized view as a MilestoneDoc to get the item structure
				let expanded_doc = MilestoneDoc::parse(view);
				// The view should be a single list with one item
				if let Some(MilestoneSection::List(expanded_list)) = expanded_doc.sections.into_iter().next()
					&& let Some(expanded_item) = expanded_list.items.into_iter().next()
				{
					item.checkbox = expanded_item.checkbox;
					item.content = expanded_item.content;
					item.children = expanded_item.children;
				}
				continue;
			}
			for child in &mut item.children {
				expand_section(child, expansions);
			}
		}
	}
}

fn collapse_section(section: &mut MilestoneSection) {
	if let MilestoneSection::List(list) = section {
		for item in list.items.iter_mut() {
			let link = match &item.content {
				ItemContent::EmbeddedIssue {
					marker: IssueMarker::Linked { link, .. },
					..
				} => Some(link.clone()),
				ItemContent::Ref(issue_ref) => issue_ref.to_issue_link(),
				_ => None,
			};
			if let Some(link) = link {
				item.content = ItemContent::Ref(super::issue_ref::IssueRef::Url(link));
				item.checkbox = None;
				item.children.clear();
			}
			for child in &mut item.children {
				collapse_section(child);
			}
		}
	}
}

/// Extract the IssueLink from an item, if it represents an issue ref.
fn content_issue_link(content: &ItemContent) -> Option<IssueLink> {
	match content {
		ItemContent::Ref(issue_ref) => issue_ref.to_issue_link(),
		ItemContent::EmbeddedIssue { marker, .. } => match marker {
			IssueMarker::Linked { link, .. } => Some(link.clone()),
			_ => None,
		},
		ItemContent::Text(_) => None,
	}
}

fn item_issue_link(item: &MilestoneItem) -> Option<IssueLink> {
	content_issue_link(&item.content)
}

// ─── Serialization ───────────────────────────────────────────────────────────

/// Ensure `output` ends with `\n\n` (a blank line separator).
fn ensure_blank_line(output: &mut String) {
	let trailing_newlines = output.bytes().rev().take_while(|&b| b == b'\n').count();
	for _ in trailing_newlines..2 {
		output.push('\n');
	}
}

fn serialize_section(section: &MilestoneSection, output: &mut String) {
	match section {
		MilestoneSection::FreeContent(owned) => {
			if !output.is_empty() {
				ensure_blank_line(output);
			}
			let s: String = Events::from(owned.clone()).into();
			output.push_str(&s);
		}
		MilestoneSection::List(list) => {
			let loose = list.is_checkbox_list();
			for (i, item) in list.items.iter().enumerate() {
				if i > 0 && loose {
					ensure_blank_line(output);
				} else if !output.is_empty() && !output.ends_with('\n') {
					output.push('\n');
				}
				serialize_item(item, output);
			}
		}
	}
}

fn serialize_item(item: &MilestoneItem, output: &mut String) {
	let mut events = Vec::new();
	events.push(OwnedEvent::Start(OwnedTag::List(None)));
	events.push(OwnedEvent::Start(OwnedTag::Item));

	let mut inline_events = item_content_to_events(&item.content);
	if let Some(ref inner) = item.checkbox {
		inline_events.insert(0, OwnedEvent::CheckBox(inner.clone()));
	}
	if !inline_events.is_empty() && !item.children.is_empty() {
		events.push(OwnedEvent::Start(OwnedTag::Paragraph));
		events.extend(inline_events);
		events.push(OwnedEvent::End(OwnedTagEnd::Paragraph));
	} else {
		events.extend(inline_events);
	}

	// Render inline part + item/list wrappers
	events.push(OwnedEvent::End(OwnedTagEnd::Item));
	events.push(OwnedEvent::End(OwnedTagEnd::List(false)));
	let s: String = Events::from(events).into();
	output.push_str(&s);

	// Render children recursively with proper indentation
	if !item.children.is_empty() {
		output.push('\n');
		let mut child_output = String::new();
		for child in &item.children {
			serialize_section(child, &mut child_output);
		}
		indent_into(output, &child_output, "  ");
	}
}

fn item_content_to_events(content: &ItemContent) -> Vec<OwnedEvent> {
	match content {
		ItemContent::Ref(issue_ref) => {
			vec![OwnedEvent::Text(issue_ref.to_string())]
		}
		ItemContent::EmbeddedIssue { prefix_events, marker } => {
			let mut events: Vec<OwnedEvent> = prefix_events.clone();
			events.push(OwnedEvent::InlineHtml(format!("<!-- {} -->", marker.encode())));
			events
		}
		ItemContent::Text(events) => events.clone(),
	}
}

/// Serialize a single item (with children) into its section text.
/// Used for `parse_blockers_from_embedded`.
fn serialize_item_to_section_text(item: &MilestoneItem) -> String {
	let mut output = String::new();
	serialize_item(item, &mut output);
	output
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn events_to_plain_text(events: &[OwnedEvent]) -> String {
	let mut text = String::new();
	for event in events {
		if let OwnedEvent::Text(t) = event {
			text.push_str(t);
		}
	}
	text
}

// ─── Standalone functions (kept for direct use) ──────────────────────────────

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_serialize_basic_structure() {
		let content = "# Sprint Goals\nSome description\n\n- [ ] discretionary_engine\n    - [ ] owner/repo#42\n\nFooter text\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		# Sprint Goals
		Some description
		- [ ] discretionary_engine
		  - [ ] owner/repo#42

		Footer text
		");
	}

	#[test]
	fn test_parse_shorthand_refs() {
		let content = "- [ ] owner/repo#123\n- [ ] tedi#42\n- [ ] #77\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		- [ ] owner/repo#123

		- [ ] tedi#42

		- [ ] \#77
		");
	}

	#[test]
	fn test_parse_embedded_issue() {
		let content = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->
		  # Blockers
		  - task 1
		");
	}

	#[test]
	fn test_parse_bare_url() {
		let content = "- https://github.com/owner/repo/issues/99\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"- https://github.com/owner/repo/issues/99");
	}

	#[test]
	fn test_resolve_bare_refs_single_word_parent() {
		crate::current_user::set("myowner".to_string());
		let content = "- [ ] discretionary_engine\n    - [ ] #77\n";
		let mut doc = MilestoneDoc::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @"
		- [ ] discretionary_engine
		  - [ ] myowner/discretionary_engine#77
		");
	}

	#[test]
	fn test_resolve_bare_refs_owner_repo_parent() {
		let content = "- valeratrades/tedi\n    - #80\n";
		let mut doc = MilestoneDoc::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @"
		- valeratrades/tedi
		  - valeratrades/tedi#80
		");
	}

	#[test]
	fn test_resolve_bare_refs_multilevel_takes_immediate_parent() {
		crate::current_user::set("myowner".to_string());
		let content = "- discretionary_engine\n    - tedi\n        - [ ] #80\n";
		let mut doc = MilestoneDoc::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @"
		- discretionary_engine
		  - tedi
		    - [ ] myowner/tedi#80
		");
	}

	#[test]
	fn test_issue_links() {
		crate::current_user::set("myowner".to_string());
		let content = "- [ ] owner/repo#42\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/99 -->\n";
		let mut doc = MilestoneDoc::parse(content);
		doc.resolve_bare_refs();
		let links: Vec<_> = doc.issue_links().iter().map(|l| l.number()).collect();
		assert_eq!(links, [42, 99]);
	}

	#[test]
	fn test_collapse_to_links() {
		let content = "# Sprint\n\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n\nFooter\n";
		let mut doc = MilestoneDoc::parse(content);
		doc.collapse_to_links();
		insta::assert_snapshot!(doc.serialize(), @"
		# Sprint
		- https://github.com/owner/repo/issues/42

		Footer
		");
	}

	#[test]
	fn test_serialize_roundtrip() {
		let content = "# Sprint Goals\n\nSome description\n\n- [ ] owner/repo#42\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/99 -->\n\nFooter text\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		# Sprint Goals
		Some description
		- [ ] owner/repo#42

		- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/99 -->

		Footer text
		");
	}

	#[test]
	fn test_serialize_blockers_view() {
		use super::super::{BlockerSequence, IssueContents, IssueIdentity, IssueTimestamps};

		let link = IssueLink::parse("https://github.com/owner/repo/issues/42").unwrap();
		let identity = IssueIdentity::new_linked(None, None, link, IssueTimestamps::default());
		let blockers = BlockerSequence::parse("- task 1\n- task 2");
		let issue = Issue {
			identity,
			contents: IssueContents {
				title: "My Issue".to_string(),
				blockers,
				..Default::default()
			},
			children: std::collections::HashMap::new(),
		};

		let serialized = serialize_blockers_view(&issue);
		insta::assert_snapshot!(serialized, @"
		- [ ] My Issue <!-- https://github.com/owner/repo/issues/42 -->
		  # Blockers
		  - task 1
		  - task 2
		");

		// Parse it back
		let parsed_blockers = parse_blockers_from_embedded(&serialized);
		assert_eq!(parsed_blockers.items.len(), 2);
		assert_eq!(parsed_blockers.items[0].text, "task 1");
		assert_eq!(parsed_blockers.items[1].text, "task 2");
	}

	#[test]
	fn test_serialize_blockers_view_no_blockers() {
		use super::super::{IssueContents, IssueIdentity, IssueTimestamps};

		let link = IssueLink::parse("https://github.com/owner/repo/issues/42").unwrap();
		let identity = IssueIdentity::new_linked(None, None, link, IssueTimestamps::default());
		let issue = Issue {
			identity,
			contents: IssueContents {
				title: "No Blockers".to_string(),
				..Default::default()
			},
			children: std::collections::HashMap::new(),
		};
		insta::assert_snapshot!(serialize_blockers_view(&issue), @"- [ ] No Blockers <!-- https://github.com/owner/repo/issues/42 -->");
	}

	#[test]
	fn test_serialize_blockers_view_with_labels() {
		use super::super::{BlockerSequence, IssueContents, IssueIdentity, IssueTimestamps};

		let link = IssueLink::parse("https://github.com/owner/repo/issues/42").unwrap();
		let identity = IssueIdentity::new_linked(None, None, link, IssueTimestamps::default());
		let blockers = BlockerSequence::parse("- do thing");
		let issue = Issue {
			identity,
			contents: IssueContents {
				title: "Labeled".to_string(),
				labels: vec!["bug".to_string(), "urgent".to_string()],
				blockers,
				..Default::default()
			},
			children: std::collections::HashMap::new(),
		};
		insta::assert_snapshot!(serialize_blockers_view(&issue), @"
		- [ ] (bug, urgent) Labeled <!-- https://github.com/owner/repo/issues/42 -->
		  # Blockers
		  - do thing
		");
	}

	#[test]
	fn test_parse_blockers_from_embedded() {
		let section = "\
- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->
\t# Blockers
\t- task 1
\t- task 2";

		let blockers = parse_blockers_from_embedded(section);
		assert_eq!(blockers.items.len(), 2);
		assert_eq!(blockers.items[0].text, "task 1");
		assert_eq!(blockers.items[1].text, "task 2");
		assert!(blockers.set_state.is_none());
	}

	#[test]
	fn test_parse_blockers_from_embedded_with_select() {
		let section = "\
- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->
\t# Blockers !s
\t- task 1";

		let blockers = parse_blockers_from_embedded(section);
		assert_eq!(blockers.items.len(), 1);
		assert!(blockers.set_state.is_some());
	}

	#[test]
	fn test_parse_blockers_from_embedded_no_blockers() {
		let section = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->";
		assert!(parse_blockers_from_embedded(section).is_empty());
	}

	#[test]
	fn test_no_checkbox_items() {
		let content = "- valeratrades/tedi\n    - #80\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		- valeratrades/tedi
		  - \#80
		");
	}

	#[test]
	fn test_mixed_content() {
		let content = "# Header\n\nParagraph text\n\n- item 1\n- item 2\n\nMore text\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		# Header
		Paragraph text
		- item 1
		- item 2

		More text
		");
	}

	#[test]
	fn test_tight_list_roundtrip() {
		let content = "- item 1\n- item 2\n- item 3\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		- item 1
		- item 2
		- item 3
		");
	}

	#[test]
	fn test_loose_list_roundtrip() {
		let content = "- item 1\n\n- item 2\n\n- item 3\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		- item 1
		- item 2
		- item 3
		");
	}

	#[test]
	fn test_top_level_items_with_children_padded() {
		let content = "- [ ] OpenClaw\n\t# Blockers\n\t- wait on Vincent\n- [ ] discretionary_engine\n\t# Blockers\n\t- new protocols\n\t- define interface\n";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		- [ ] OpenClaw
		  # Blockers
		  - wait on Vincent

		- [ ] discretionary_engine
		  # Blockers
		  - new protocols
		  - define interface
		");
	}

	#[test]
	fn test_list_item_count() {
		let content = "- item 1\n\n- item 2\n";
		let doc = MilestoneDoc::parse(content);
		let list = match &doc.sections[0] {
			MilestoneSection::List(l) => l,
			_ => panic!("expected list"),
		};
		assert_eq!(list.items.len(), 2);
	}

	#[test]
	fn test_expand_serialize_roundtrip() {
		let view = "- [ ] Empty Issue <!-- @user https://github.com/o/r/issues/50 -->";
		let mut doc = MilestoneDoc::parse(view);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @"- [ ] Empty Issue <!-- @user https://github.com/o/r/issues/50 -->");
	}

	#[test]
	fn test_embedded_issues_detected() {
		let content = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n";
		let doc = MilestoneDoc::parse(content);
		let embedded = doc.embedded_issues();
		assert_eq!(embedded.len(), 1);
		assert_eq!(embedded[0].0.number(), 42);
		insta::assert_snapshot!(embedded[0].1, @"
		- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->
		  # Blockers
		  - task 1
		");
	}

	#[test]
	fn test_embedded_issues_after_edit_simulation() {
		let expanded = "- [ ] Empty Issue <!-- @mock_user https://github.com/o/r/issues/50 -->";
		let edited = format!("{expanded}\n\t# Blockers\n\t- todo\n");
		let doc = MilestoneDoc::parse(&edited);
		let embedded = doc.embedded_issues();
		assert_eq!(embedded.len(), 1);
		let blockers = parse_blockers_from_embedded(&embedded[0].1);
		assert_eq!(blockers.items.len(), 1);
		assert_eq!(blockers.items[0].text, "todo");
	}

	/// In loose lists (blank lines between items or inside items), pulldown_cmark
	/// emits Start(Paragraph) before CheckBox. We must handle this ordering.
	#[test]
	fn test_stored_form_loose_checkbox_list() {
		let stored = "- [ ] OpenClaw\n  text\n\n  # Blockers\n  - wait\n- [ ] Second\n";
		let doc = MilestoneDoc::parse(stored);
		insta::assert_snapshot!(doc.serialize(), @"
		- [ ] OpenClaw
		  text
		  # Blockers
		  - wait

		- [ ] Second
		");
	}

	#[test]
	fn test_collapsed_list_expanded_gets_spacing() {
		// Simulates what happens after collapse → re-expand:
		// the stored form has bare URLs (plain list), then expand_with replaces them
		// with checkbox items. Serialize should detect the expanded list as checkbox.
		let collapsed = "\
- https://github.com/owner/repo/issues/42
- https://github.com/owner/repo/issues/43
";
		let mut doc = MilestoneDoc::parse(collapsed);

		// Simulate expand_with by manually replacing items
		let mut expansions = std::collections::HashMap::new();
		expansions.insert(42, "- [ ] Issue One <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1".to_string());
		expansions.insert(43, "- [ ] Issue Two <!-- @user https://github.com/owner/repo/issues/43 -->".to_string());
		doc.expand_with(&expansions);

		insta::assert_snapshot!(doc.serialize(), @r#"
		- [ ] Issue One <!-- @user https://github.com/owner/repo/issues/42 -->
		  # Blockers
		  - task 1

		- [ ] Issue Two <!-- @user https://github.com/owner/repo/issues/43 -->
		"#);
	}

	/// Simulates the full edit flow: stored collapsed form → expand → serialize.
	/// Non-issue items (like "OpenClaw" with blockers) should be kept and the
	/// list should get blank lines between items since the list contains checkboxes.
	#[test]
	fn test_expand_with_mixed_issue_and_text_items() {
		// Stored form: first item is checkbox text, rest are bare URLs
		let stored = "\
# important today
- [ ] OpenClaw
  # Blockers
  - wait on Vincent
- https://github.com/valeratrades/discretionary_engine/issues/77
- https://github.com/valeratrades/discretionary_engine/issues/78
";
		let mut doc = MilestoneDoc::parse(stored);

		let mut expansions = std::collections::HashMap::new();
		expansions.insert(
			77,
			"- [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->\n\t# Blockers\n\t- new protocols".to_string(),
		);
		expansions.insert(78, "- [ ] risk <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/78 -->".to_string());
		doc.expand_with(&expansions);

		insta::assert_snapshot!(doc.serialize(), @r#"
		# important today
		- [ ] OpenClaw
		  # Blockers
		  - wait on Vincent

		- [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->
		  # Blockers
		  - new protocols

		- [ ] risk <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/78 -->
		"#);
	}

	/// When ALL stored items are bare URLs, the list starts as "plain" (no checkbox).
	/// After expand_with, items gain checkboxes but is_checkbox_list checks live state.
	#[test]
	fn test_expand_all_bare_urls() {
		let stored = "\
# important today
- https://github.com/owner/repo/issues/1
- https://github.com/owner/repo/issues/2
- https://github.com/owner/repo/issues/3
";
		let mut doc = MilestoneDoc::parse(stored);

		let mut expansions = std::collections::HashMap::new();
		expansions.insert(1, "- [ ] First <!-- @user https://github.com/owner/repo/issues/1 -->\n\t# Blockers\n\t- task A".to_string());
		expansions.insert(2, "- [ ] Second <!-- @user https://github.com/owner/repo/issues/2 -->".to_string());
		expansions.insert(3, "- [x] Third <!-- @user https://github.com/owner/repo/issues/3 -->".to_string());
		doc.expand_with(&expansions);

		insta::assert_snapshot!(doc.serialize(), @"
		# important today
		- [ ] First <!-- @user https://github.com/owner/repo/issues/1 -->
		  # Blockers
		  - task A

		- [ ] Second <!-- @user https://github.com/owner/repo/issues/2 -->

		- [x] Third <!-- @user https://github.com/owner/repo/issues/3 -->
		");
	}

	#[test]
	fn test_checkbox_list_after_heading() {
		let content = "\
# important today
- [ ] OpenClaw
  have relevant stuff in tg/general. Also would include probably buying the VPS, as it needs to be able to work fully autonomously
  # Blockers
  - wait on Vincent to set up the server
- [ ] discretionary_engine
  - [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->
    # Blockers
    - new protocols attach options
    - define the exact new target interface
    - start drawing it on the whiteboard
    - update current `strategy/main.rs` to interpret protocols itself
- [ ] discretionary_engine
  - [ ] strategy
    - \\[.\\] communication layer
      - move clap interface into \\_strategy, alongside `dummy_market.rs` for only protocol
      - take exact string, send over redis, be able to interpret it again
      <!--x-->

      - get dummy market suggesting orders correctly //NB: no execution yet, - don't rush the implementation
    - [ ] allow defining `percent_controlled` for any protocol
    - [ ] update the standard for clear delineation between opening new pos and updating an existing one
      - on \\_strategy:
        adj
        can provide all the same arguments (except for target assets, - changing them requires a new position), but now absolutely all support starting with a minus. So can do `adj \"shorts_basket_trade\" -f \"-dm\"`, and it will remove the `dm` followup protocol.
        Q: that on earth do I do about basket trades then?
        new
      - main entrypoint:
        new (with --quality instead of size)
      - define in terms of target delta change for \\_strategy, and expand on total def space for main entrypoint
  - [ ] risk
    - [ ] update the logic for %controlled on followup algos -> integrate `adjust-pos` into the main loop (require id provision)
    - [ ] transition to distinctly different datatypes for AggrBook and External
    - [ ] fix: limit chase is failing to update consistently as the price moves up
    - \\[.\\] rm_engine (XXX: might be outdated):
      - [ ] make stop_loss_proximity layer reasonable
        plug fix: just nuke certainty
      - [ ] some mechanic to check how much bias our suggestions actually had historically; warn if outside of some certainty\\*val range
      - [ ] integrate diffusion testing
        backtests could also be a thing, but don't like lack of consistency for which layers get backtests run on them.
";
		let doc = MilestoneDoc::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r#"
		# important today
		- [ ] OpenClaw
		  have relevant stuff in tg/general. Also would include probably buying the VPS, as it needs to be able to work fully autonomously
		  # Blockers
		  - wait on Vincent to set up the server

		- [ ] discretionary_engine
		  - [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->
		    # Blockers
		    - new protocols attach options
		    - define the exact new target interface
		    - start drawing it on the whiteboard
		    - update current `strategy/main.rs` to interpret protocols itself

		- [ ] discretionary_engine
		  - [ ] strategy
		    - \[.\] communication layer
		      - move clap interface into \_strategy, alongside `dummy_market.rs` for only protocol
		      - take exact string, send over redis, be able to interpret it again
		      
		      <!--x-->
		      - get dummy market suggesting orders correctly //NB: no execution yet, - don't rush the implementation
		    
		    - [ ] allow defining `percent_controlled` for any protocol
		    
		    - [ ] update the standard for clear delineation between opening new pos and updating an existing one
		      - on \_strategy:
		        adj
		        can provide all the same arguments (except for target assets, - changing them requires a new position), but now absolutely all support starting with a minus. So can do `adj "shorts_basket_trade" -f "-dm"`, and it will remove the `dm` followup protocol.
		        Q: that on earth do I do about basket trades then?
		        new
		      - main entrypoint:
		        new (with --quality instead of size)
		      - define in terms of target delta change for \_strategy, and expand on total def space for main entrypoint
		  
		  - [ ] risk
		    - [ ] update the logic for %controlled on followup algos -> integrate `adjust-pos` into the main loop (require id provision)
		    
		    - [ ] transition to distinctly different datatypes for AggrBook and External
		    
		    - [ ] fix: limit chase is failing to update consistently as the price moves up
		    
		    - \[.\] rm_engine (XXX: might be outdated):
		      - [ ] make stop_loss_proximity layer reasonable
		        plug fix: just nuke certainty
		      
		      - [ ] some mechanic to check how much bias our suggestions actually had historically; warn if outside of some certainty\*val range
		      
		      - [ ] integrate diffusion testing
		        backtests could also be a thing, but don't like lack of consistency for which layers get backtests run on them.
		"#);
	}
}
