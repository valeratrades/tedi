//! Parsing and serialization of milestone descriptions as structured documents.
//!
//! Milestones contain a mix of free-form markdown and nested lists with issue refs.
//! This module parses them into a `MilestoneDoc` AST via pulldown_cmark, enabling
//! structured operations like parent-context resolution for bare `#123` refs.

use pulldown_cmark::{Event, Options, Parser, Tag, TagEnd};

use super::{Issue, IssueLink, IssueMarker, OwnedEvent, OwnedTag, OwnedTagEnd};

/// A parsed milestone document: a sequence of top-level sections.
pub struct MilestoneDoc {
	sections: Vec<MilestoneSection>,
}
impl MilestoneDoc {
	pub fn parse(content: &str) -> Self {
		let parser = Parser::new_ext(content, parser_options());
		let events: Vec<Event<'_>> = parser.collect();
		let mut pos = 0;
		let sections = parse_sections(&events, &mut pos, None, 0);
		Self { sections }
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
	}

	/// Collapse all embedded issues and shorthand refs to bare URLs for storage.
	pub fn collapse_to_links(&mut self) {
		for section in &mut self.sections {
			collapse_section(section);
		}
	}

	pub fn serialize(&self) -> String {
		let events = self.to_events();
		let borrowed: Vec<Event<'_>> = events.iter().map(|e| e.to_event()).collect();
		let mut output = String::new();
		pulldown_cmark_to_cmark::cmark_with_options(borrowed.into_iter(), &mut output, cmark_options()).expect("markdown rendering should not fail");
		output
	}

	fn to_events(&self) -> Vec<OwnedEvent> {
		let mut events = Vec::new();
		for section in &self.sections {
			section_to_events(section, 0, &mut events);
		}
		events
	}
}

/// Serialize an issue as title line + blockers only (for milestone embedding).
pub fn serialize_blockers_view(issue: &Issue) -> String {
	let mut out = String::new();

	// Title line with issue marker
	let checked = issue.contents.state.to_checkbox();
	let issue_marker = IssueMarker::from(&issue.identity);
	let labels_part = if issue.contents.labels.is_empty() {
		String::new()
	} else {
		format!("[{}] ", issue.contents.labels.join(", "))
	};
	out.push_str(&format!("- [{checked}] {labels_part}{} {issue_marker}", issue.contents.title));

	// Blockers section
	if !issue.contents.blockers.is_empty() {
		out.push('\n');
		let header = crate::Header::new(1, "Blockers");
		out.push_str(&format!("\t{}", header.encode()));
		for line in issue.contents.blockers.raw_lines() {
			out.push_str(&format!("\n\t{line}"));
		}
	}

	out
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

	// Collect blocker lines (strip one level of indent — tab or spaces)
	let blocker_lines: Vec<String> = lines[start..]
		.iter()
		.filter(|l| !l.trim().is_empty())
		.map(|l| l.strip_prefix('\t').unwrap_or_else(|| l.trim_start()).to_string())
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

/// A single list item.
struct MilestoneItem {
	/// `None` = no checkbox, `Some(false)` = `[ ]`, `Some(true)` = `[x]`
	checked: Option<bool>,
	/// Semantic interpretation of this item's inline text.
	content: ItemContent,
	/// Nested content under this item (sub-lists, headings, etc.).
	children: Vec<MilestoneSection>,
}

/// Semantic interpretation of a list item's inline text.
enum ItemContent {
	/// Shorthand ref: `owner/repo#N`, `repo#N`, or bare `#N`.
	ShorthandRef { owner: Option<String>, repo: Option<String>, number: u64 },
	/// A bare URL: `https://github.com/owner/repo/issues/N`
	BareUrl(IssueLink),
	/// Embedded issue with title + marker: `Title <!-- @user url -->`
	EmbeddedIssue { prefix_events: Vec<OwnedEvent>, marker: IssueMarker },
	/// Plain text (category headers like `discretionary_engine`, `valeratrades/tedi`, or anything else).
	Text(Vec<OwnedEvent>),
}

// ─── Parsing ─────────────────────────────────────────────────────────────────

fn parser_options() -> Options {
	Options::ENABLE_TASKLISTS | Options::ENABLE_STRIKETHROUGH
}

/// Parse sections from the event stream until we hit `stop_at` or end of events.
/// `stop_at` is used for nested parsing inside list items — stops at `End(Item)`.
/// `depth` tracks list nesting (0 = not yet inside any list).
fn parse_sections(events: &[Event<'_>], pos: &mut usize, stop_at: Option<TagEnd>, depth: usize) -> Vec<MilestoneSection> {
	let mut sections = Vec::new();
	let mut free_events: Vec<OwnedEvent> = Vec::new();

	while *pos < events.len() {
		// Check stop condition
		if let Some(ref stop) = stop_at
			&& matches!(&events[*pos], Event::End(tag_end) if tag_end == stop)
		{
			break;
		}

		match &events[*pos] {
			Event::Start(Tag::List(_)) => {
				// Flush accumulated free content
				if !free_events.is_empty() {
					sections.push(MilestoneSection::FreeContent(std::mem::take(&mut free_events)));
				}
				sections.push(MilestoneSection::List(parse_list(events, pos, depth)));
			}
			_ => {
				free_events.push(OwnedEvent::from_event(events[*pos].clone()));
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
fn parse_list(events: &[Event<'_>], pos: &mut usize, depth: usize) -> MilestoneList {
	debug_assert!(matches!(&events[*pos], Event::Start(Tag::List(_))));
	let depth = depth + 1;
	*pos += 1; // consume Start(List)

	let mut items = Vec::new();

	while *pos < events.len() {
		match &events[*pos] {
			Event::End(TagEnd::List(_)) => {
				*pos += 1;
				break;
			}
			Event::Start(Tag::Item) => {
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
fn parse_item(events: &[Event<'_>], pos: &mut usize, depth: usize) -> MilestoneItem {
	debug_assert!(matches!(&events[*pos], Event::Start(Tag::Item)));
	*pos += 1; // consume Start(Item)

	// Collect TaskListMarker if present
	let checked = if let Some(Event::TaskListMarker(c)) = events.get(*pos) {
		let c = *c;
		*pos += 1;
		Some(c)
	} else {
		None
	};

	// Collect inline events (the item's own text content).
	// Strip paragraph wrappers that pulldown_cmark adds for loose lists — we control
	// looseness ourselves based on depth.
	let mut inline_events: Vec<OwnedEvent> = Vec::new();
	let mut in_paragraph = false;

	// Consume inline content: everything before child blocks or End(Item)
	while *pos < events.len() {
		match &events[*pos] {
			Event::End(TagEnd::Item) => break,
			Event::Start(Tag::List(_)) | Event::Start(Tag::Heading { .. }) => break,
			Event::Start(Tag::Paragraph) => {
				in_paragraph = true;
				*pos += 1;
			}
			Event::End(TagEnd::Paragraph) if in_paragraph => {
				in_paragraph = false;
				*pos += 1;
			}
			_ => {
				inline_events.push(OwnedEvent::from_event(events[*pos].clone()));
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
	children.extend(parse_sections(events, pos, Some(TagEnd::Item), depth));

	// Consume End(Item)
	if matches!(events.get(*pos), Some(Event::End(TagEnd::Item))) {
		*pos += 1;
	}

	MilestoneItem { checked, content, children }
}

/// Split inline events at a SoftBreak if the part after it is a bare URL or shorthand ref.
/// Returns (item_content, child_sections) where child_sections contains a single-item List if split occurred.
fn split_inline_at_ref_softbreak(events: Vec<OwnedEvent>) -> (ItemContent, Vec<MilestoneSection>) {
	// Find the last SoftBreak
	if let Some(break_pos) = events.iter().rposition(|e| matches!(e, OwnedEvent::SoftBreak)) {
		let tail = &events[break_pos + 1..];
		let tail_classified = classify_inline_events(tail.to_vec());
		if matches!(tail_classified, ItemContent::ShorthandRef { .. } | ItemContent::BareUrl(_)) {
			let head = events[..break_pos].to_vec();
			let content = classify_inline_events(head);
			let child_item = MilestoneItem {
				checked: None,
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
		// Try bare URL first
		if let Some(link) = IssueLink::parse(trimmed) {
			return ItemContent::BareUrl(link);
		}

		// Try shorthand: `owner/repo#N`, `repo#N`, or `#N`
		if let Some(shorthand) = parse_shorthand_word(trimmed) {
			return shorthand;
		}
	}

	// Case 3: plain text
	ItemContent::Text(events)
}

/// Try to parse a single word as `owner/repo#N`, `repo#N`, or `#N`.
fn parse_shorthand_word(word: &str) -> Option<ItemContent> {
	let hash_pos = word.find('#')?;
	let before = &word[..hash_pos];
	let after = &word[hash_pos + 1..];
	let number: u64 = after.parse().ok()?;

	match before.find('/') {
		Some(slash_pos) => {
			// `owner/repo#number`
			if before[slash_pos + 1..].contains('/') {
				return None;
			}
			let owner = &before[..slash_pos];
			let repo = &before[slash_pos + 1..];
			if owner.is_empty() || repo.is_empty() {
				return None;
			}
			Some(ItemContent::ShorthandRef {
				owner: Some(owner.to_string()),
				repo: Some(repo.to_string()),
				number,
			})
		}
		None => {
			if before.is_empty() {
				// Bare `#number` — needs parent context to resolve
				Some(ItemContent::ShorthandRef { owner: None, repo: None, number })
			} else {
				// `repo#number`
				Some(ItemContent::ShorthandRef {
					owner: None,
					repo: Some(before.to_string()),
					number,
				})
			}
		}
	}
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

			// Resolve this item if it's a bare `#N`
			if let ItemContent::ShorthandRef { owner, repo, .. } = &mut item.content {
				if repo.is_none()
					&& let Some(ctx) = parent_context
				{
					let (resolved_owner, resolved_repo) = parse_repo_context(ctx);
					*owner = Some(resolved_owner);
					*repo = Some(resolved_repo);
				}
				if owner.is_none()
					&& repo.is_some()
					&& let Some(user) = crate::current_user::get()
				{
					*owner = Some(user);
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

fn parse_repo_context(text: &str) -> (String, String) {
	if let Some(slash) = text.find('/') {
		(text[..slash].to_string(), text[slash + 1..].to_string())
	} else {
		let owner = crate::current_user::get().unwrap_or_else(|| panic!("current_user must be set to resolve bare repo context '{text}'"));
		(owner, text.to_string())
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
					item.checked = expanded_item.checked;
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
			match &item.content {
				ItemContent::EmbeddedIssue {
					marker: IssueMarker::Linked { link, .. },
					..
				} => {
					item.content = ItemContent::BareUrl(link.clone());
					item.checked = None;
					item.children.clear();
				}
				ItemContent::ShorthandRef {
					owner: Some(o),
					repo: Some(r),
					number,
				} => {
					let url = format!("https://github.com/{o}/{r}/issues/{number}");
					if let Some(link) = IssueLink::parse(&url) {
						item.content = ItemContent::BareUrl(link);
						item.checked = None;
						item.children.clear();
					}
				}
				_ => {}
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
		ItemContent::ShorthandRef { owner, repo, number } => {
			let o = owner.as_ref()?;
			let r = repo.as_ref()?;
			let url = format!("https://github.com/{o}/{r}/issues/{number}");
			IssueLink::parse(&url)
		}
		ItemContent::BareUrl(link) => Some(link.clone()),
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

fn cmark_options() -> pulldown_cmark_to_cmark::Options<'static> {
	pulldown_cmark_to_cmark::Options {
		list_token: '-',
		newlines_after_headline: 1,
		..Default::default()
	}
}

/// `depth` tracks list nesting: 0 = not inside a list, 1 = top-level list, 2+ = nested.
fn section_to_events(section: &MilestoneSection, depth: usize, events: &mut Vec<OwnedEvent>) {
	match section {
		MilestoneSection::FreeContent(owned) => {
			events.extend(owned.iter().cloned());
		}
		MilestoneSection::List(list) => {
			list_to_events(list, depth, events);
		}
	}
}

fn list_to_events(list: &MilestoneList, depth: usize, events: &mut Vec<OwnedEvent>) {
	let depth = depth + 1;
	// Top-level lists (depth 1) are always loose; nested lists are always tight.
	let is_loose = depth == 1;

	events.push(OwnedEvent::Start(OwnedTag::List(None)));
	for item in &list.items {
		item_to_events(item, is_loose, depth, events);
	}
	events.push(OwnedEvent::End(OwnedTagEnd::List(false)));
}

fn item_to_events(item: &MilestoneItem, is_loose: bool, depth: usize, events: &mut Vec<OwnedEvent>) {
	events.push(OwnedEvent::Start(OwnedTag::Item));

	if let Some(checked) = item.checked {
		events.push(OwnedEvent::TaskListMarker(checked));
	}

	let inline_events = item_content_to_events(&item.content);
	if !inline_events.is_empty() {
		// Paragraph wrapper needed when list is loose OR item has children
		// (cmark requires it to separate inline content from child blocks).
		if is_loose || !item.children.is_empty() {
			events.push(OwnedEvent::Start(OwnedTag::Paragraph));
			events.extend(inline_events);
			events.push(OwnedEvent::End(OwnedTagEnd::Paragraph));
		} else {
			events.extend(inline_events);
		}
	}

	// Emit children
	for child in &item.children {
		section_to_events(child, depth, events);
	}

	events.push(OwnedEvent::End(OwnedTagEnd::Item));
}

fn item_content_to_events(content: &ItemContent) -> Vec<OwnedEvent> {
	match content {
		ItemContent::ShorthandRef { owner, repo, number } => {
			let text = match (owner, repo) {
				(Some(o), Some(r)) => format!("{o}/{r}#{number}"),
				(None, Some(r)) => format!("{r}#{number}"),
				(None, None) => format!("#{number}"),
				(Some(_), None) => unreachable!("owner set without repo"),
			};
			vec![OwnedEvent::Text(text)]
		}
		ItemContent::BareUrl(link) => {
			vec![OwnedEvent::Text(link.as_str().to_string())]
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
	// Build events for just this item as if it were a standalone document with one list
	let mut events = Vec::new();
	events.push(OwnedEvent::Start(OwnedTag::List(None)));
	// depth=1: this is a top-level embedded item, but we serialize it tight (no paragraph wrapping)
	// so blockers don't get extra spacing
	item_to_events(item, false, 1, &mut events);
	events.push(OwnedEvent::End(OwnedTagEnd::List(false)));

	let borrowed: Vec<Event<'_>> = events.iter().map(|e| e.to_event()).collect();
	let mut output = String::new();
	pulldown_cmark_to_cmark::cmark_with_options(borrowed.into_iter(), &mut output, cmark_options()).expect("markdown rendering should not fail");
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
		- [ ] [bug, urgent] Labeled <!-- https://github.com/owner/repo/issues/42 -->
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
}
