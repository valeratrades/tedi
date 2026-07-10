//! `TaskView`: a structured view into a slice of task space.
//!
//! A task view is a markdown document whose `#`..`######` headers partition it into
//! sections, each holding an ordered list of task components (issue refs, milestone
//! refs, or unlinked virtual text). Components nest arbitrarily via their children.
//! Sprints, Bottlenecks and Searches are all projections built at this level.
//!
//! Parsing/serialization round-trip over `tedi_md::Events`. Expansion (replacing an
//! issue ref with the issue's full `Display`) is a render concern, not stored state;
//! collapse turns any expanded/embedded component back into a bare link.

use std::collections::{BTreeMap, HashMap};

use tedi_md::indent_into;

use crate::{Events, IssueLink, IssueMarker, IssueRef, MilestoneLink, MilestoneRef, OwnedEvent, OwnedTag, OwnedTagEnd};

/// A parsed task view: header-path → ordered components, plus un-itemized prose per section.
pub struct TaskView {
	sections: BTreeMap<Vec<String>, Vec<TaskItem>>,
	prose: BTreeMap<Vec<String>, Vec<OwnedEvent>>,
	/// Header paths in document order — render must not alphabetize.
	order: Vec<Vec<String>>,
}
impl TaskView {
	pub fn parse(content: &str) -> Self {
		let events = Events::parse(content);
		let mut view = Self {
			sections: BTreeMap::new(),
			prose: BTreeMap::new(),
			order: Vec::new(),
		};
		let mut stack: Vec<(usize, String)> = Vec::new();
		let mut prose_buf: Vec<OwnedEvent> = Vec::new();
		let mut pos = 0;

		while pos < events.len() {
			match &events[pos] {
				OwnedEvent::Start(OwnedTag::Heading { .. }) => {
					view.flush_prose(&stack, &mut prose_buf);
					let (level, text, next) = read_heading(&events, pos);
					pos = next;
					while stack.last().is_some_and(|(l, _)| *l >= level) {
						stack.pop();
					}
					stack.push((level, text));
					let key = stack_key(&stack);
					view.touch(&key);
					view.sections.entry(key).or_default();
				}
				OwnedEvent::Start(OwnedTag::List(_)) => {
					view.flush_prose(&stack, &mut prose_buf);
					let items = parse_list(&events, &mut pos, 0);
					let key = stack_key(&stack);
					view.touch(&key);
					view.sections.entry(key).or_default().extend(items);
				}
				_ => {
					prose_buf.push(events[pos].clone());
					pos += 1;
				}
			}
		}
		view.flush_prose(&stack, &mut prose_buf);
		view.normalize();
		view
	}

	fn touch(&mut self, key: &Vec<String>) {
		if !self.order.contains(key) {
			self.order.push(key.clone());
		}
	}

	fn flush_prose(&mut self, stack: &[(usize, String)], buf: &mut Vec<OwnedEvent>) {
		if !buf.is_empty() {
			let key = stack_key(stack);
			self.touch(&key);
			self.prose.entry(key).or_default().extend(buf.drain(..));
		}
	}

	fn normalize(&mut self) {
		for items in self.sections.values_mut() {
			normalize_items(items);
		}
	}

	/// Resolve bare `#N` refs using their parent list item's text as repo context.
	pub fn resolve_bare_refs(&mut self) {
		for items in self.sections.values_mut() {
			resolve_items(items, None);
		}
	}

	/// Collect all issue links, in document order.
	pub fn issue_links(&self) -> Vec<IssueLink> {
		let mut links = Vec::new();
		for items in self.sections.values() {
			collect_issue_links(items, &mut links);
		}
		links
	}

	/// Collect all milestone links, in document order.
	pub fn milestone_links(&self) -> Vec<MilestoneLink> {
		let mut links = Vec::new();
		for items in self.sections.values() {
			collect_milestone_links(items, &mut links);
		}
		links
	}

	/// The expanded (`Title <!-- marker -->` + blockers) issue sections in this view.
	/// Returns each `IssueLink` and the serialized component text (for blocker sync).
	/// Bare links are excluded — only genuinely-expanded issues are synced.
	pub fn embedded_issues(&self) -> Vec<(IssueLink, String)> {
		let mut result = Vec::new();
		for items in self.sections.values() {
			collect_embedded(items, &mut result);
		}
		result
	}

	/// Drop every issue component (with its children) — prunes a fully-closed sprint.
	pub fn remove_issues(&mut self) {
		for items in self.sections.values_mut() {
			remove_issue_items(items);
		}
	}

	/// Collapse every issue/milestone component to a bare link for storage.
	pub fn collapse_to_links(&mut self) {
		for items in self.sections.values_mut() {
			collapse_items(items);
		}
		self.normalize();
	}

	/// Render to markdown, substituting each issue ref (whose link is in `expansions`)
	/// with the pre-rendered issue `Display`. An empty map yields the collapsed form.
	pub fn render(&self, expansions: &HashMap<IssueLink, String>) -> String {
		debug_assert!(
			self.sections.keys().chain(self.prose.keys()).all(|k| self.order.contains(k)),
			"every key is registered in `order` on insertion"
		);

		let mut output = String::new();
		let mut prev: Vec<String> = Vec::new();
		for key in &self.order {
			let common = prev.iter().zip(key).take_while(|(a, b)| a == b).count();
			for (i, seg) in key.iter().enumerate().skip(common) {
				if !output.is_empty() {
					ensure_blank_line(&mut output);
				}
				output.push_str(&"#".repeat(i + 1));
				output.push(' ');
				output.push_str(seg);
				output.push('\n');
			}
			prev = key.clone();

			if let Some(events) = self.prose.get(key) {
				let rendered = String::from(Events::from(events.clone()));
				let rendered = rendered.trim();
				if !rendered.is_empty() {
					if !output.is_empty() {
						ensure_blank_line(&mut output);
					}
					output.push_str(rendered);
					output.push('\n');
				}
			}
			if let Some(items) = self.sections.get(key)
				&& !items.is_empty()
			{
				if !output.is_empty() {
					ensure_blank_line(&mut output);
				}
				serialize_items(items, expansions, &mut output);
			}
		}
		output.trim_matches('\n').to_string()
	}

	pub fn serialize(&self) -> String {
		self.render(&HashMap::new())
	}

	/// Open-checkbox virtual items that read as new tasks, in document order, each with its
	/// serialized block. Skipped: items with an issue/milestone ref among their list
	/// descendants (category headers — we descend into those instead), and single-word
	/// childless items (repo-context headers / half-typed input).
	pub fn homeless_tasks(&self) -> Vec<(TaskItemId, String)> {
		let mut out = Vec::new();
		for key in &self.order {
			if let Some(items) = self.sections.get(key) {
				collect_homeless(items, key, &mut Vec::new(), &mut out);
			}
		}
		out
	}

	/// Replace the item at `id` with a bare link to its materialized issue.
	pub fn assign_link(&mut self, id: &TaskItemId, link: IssueLink) {
		let items = self.sections.get_mut(&id.section).expect("TaskItemId produced by homeless_tasks on this view");
		let item = resolve_item_mut(items, &id.path);
		item.content = TaskContent::Issue {
			r#ref: IssueRef::Url(link),
			embedded: false,
		};
		item.checkbox = None;
		item.children.clear();
	}
}

/// Address of a task item within a `TaskView`: section header-path + positional path
/// (item index, then alternating child-section/item indices for nesting).
pub struct TaskItemId {
	section: Vec<String>,
	path: Vec<usize>,
}

/// A single task-space component: a list item classified as an issue ref, a milestone
/// ref, or unlinked virtual text, plus any nested sub-content.
pub struct TaskItem {
	/// The checkbox contents (between `[` and `]`), or None for plain items.
	checkbox: Option<String>,
	content: TaskContent,
	/// Nested content under this item (sub-lists, headings, prose).
	children: Vec<Section>,
}

/// Parse blockers from an embedded issue section in a task view.
/// The section is the issue's `Display`: title line · body · comments · `# Blockers` + blockers
/// · child links. We read only the blocker items, stopping at the child-link list (`- [`).
pub fn parse_blockers_from_embedded(section: &str) -> crate::Blockers {
	let lines: Vec<&str> = section.lines().collect();
	if lines.len() < 2 {
		return crate::Blockers::default();
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
		if matches!(crate::Marker::decode(effective), Some(crate::Marker::BlockersSection(_))) {
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
		return crate::Blockers::default();
	};

	// Collect blocker lines (strip one level of indent — tab or 2 spaces), stopping at the
	// child-issue link list (a checkbox item `- [`) which follows the blockers in `Display`.
	let mut blocker_lines: Vec<String> = Vec::new();
	for line in &lines[start..] {
		if line.trim().is_empty() {
			continue;
		}
		let stripped = line.strip_prefix('\t').or_else(|| line.strip_prefix("  ")).unwrap_or(line);
		if stripped.trim_start().starts_with("- [") {
			break;
		}
		blocker_lines.push(stripped.to_string());
	}

	let mut seq = crate::Blockers::parse(&blocker_lines.join("\n"));
	if select_blockers {
		seq.set_state = Some(crate::BlockerSetState::Pending);
	}
	seq
}
/// Semantic classification of a component's inline text. Blockers are never a
/// top-level component here — they live inside the individual issues.
enum TaskContent {
	/// A milestone reference (`…/milestone/N`).
	Milestone(MilestoneRef),
	/// An issue reference. `embedded` marks the expanded form (`Title <!-- marker -->`)
	/// as opposed to a bare link; it is a transient parse artifact, cleared on collapse.
	Issue { r#ref: IssueRef, embedded: bool },
	/// Unlinked text (category headers like `discretionary_engine`, or a homeless local issue).
	Virtual(Vec<OwnedEvent>),
}

/// A child block under a component: free-form markdown or a nested list.
enum Section {
	FreeContent(Vec<OwnedEvent>),
	List(Vec<TaskItem>),
}

impl std::fmt::Display for TaskView {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.serialize())
	}
}

// ─── Parsing ─────────────────────────────────────────────────────────────────

fn stack_key(stack: &[(usize, String)]) -> Vec<String> {
	stack.iter().map(|(_, t)| t.clone()).collect()
}

/// Read a heading starting at `pos` (a `Start(Heading)`). Returns (level, text, next_pos).
fn read_heading(events: &[OwnedEvent], pos: usize) -> (usize, String, usize) {
	let mut end = pos + 1;
	while end < events.len() && !matches!(&events[end], OwnedEvent::End(OwnedTagEnd::Heading(_))) {
		end += 1;
	}
	let rendered = String::from(Events::from(events[pos..=end.min(events.len() - 1)].to_vec()));
	let line = rendered.trim();
	let level = line.chars().take_while(|c| *c == '#').count().clamp(1, 6);
	let text = line.trim_start_matches('#').trim().to_string();
	(level, text, end + 1)
}

/// Parse sections (free content + nested lists) until `stop_at` or end of events.
/// Used only for the children of a component; top-level headers are handled by `parse`.
fn parse_sections(events: &[OwnedEvent], pos: &mut usize, stop_at: Option<OwnedTagEnd>, depth: usize) -> Vec<Section> {
	let mut sections = Vec::new();
	let mut free_events: Vec<OwnedEvent> = Vec::new();

	while *pos < events.len() {
		if let Some(ref stop) = stop_at
			&& matches!(&events[*pos], OwnedEvent::End(tag_end) if tag_end == stop)
		{
			break;
		}

		match &events[*pos] {
			OwnedEvent::Start(OwnedTag::List(_)) => {
				if !free_events.is_empty() {
					sections.push(Section::FreeContent(std::mem::take(&mut free_events)));
				}
				sections.push(Section::List(parse_list(events, pos, depth)));
			}
			_ => {
				free_events.push(events[*pos].clone());
				*pos += 1;
			}
		}
	}

	if !free_events.is_empty() {
		sections.push(Section::FreeContent(free_events));
	}
	sections
}

/// Parse a `List`. Expects `pos` at `Start(List)`; advances past `End(List)`.
fn parse_list(events: &[OwnedEvent], pos: &mut usize, depth: usize) -> Vec<TaskItem> {
	debug_assert!(matches!(&events[*pos], OwnedEvent::Start(OwnedTag::List(_))));
	let depth = depth + 1;
	*pos += 1;

	let mut items = Vec::new();
	while *pos < events.len() {
		match &events[*pos] {
			OwnedEvent::End(OwnedTagEnd::List(_)) => {
				*pos += 1;
				break;
			}
			OwnedEvent::Start(OwnedTag::Item) => items.push(parse_item(events, pos, depth)),
			_ => *pos += 1,
		}
	}
	items
}

/// Parse a single `Item`. Expects `pos` at `Start(Item)`.
fn parse_item(events: &[OwnedEvent], pos: &mut usize, depth: usize) -> TaskItem {
	debug_assert!(matches!(&events[*pos], OwnedEvent::Start(OwnedTag::Item)));
	*pos += 1;

	// Loose lists emit Start(Item), Start(Paragraph), CheckBox; tight lists skip the paragraph.
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

	let mut inline_events: Vec<OwnedEvent> = Vec::new();
	let mut in_paragraph = starts_with_paragraph;
	while *pos < events.len() {
		match &events[*pos] {
			OwnedEvent::End(OwnedTagEnd::Item) => break,
			OwnedEvent::Start(OwnedTag::List(_)) | OwnedEvent::Start(OwnedTag::Heading { .. }) => break,
			OwnedEvent::Start(OwnedTag::Paragraph) => {
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

	let (content, softbreak_children) = split_inline_at_ref_softbreak(inline_events);

	let mut children = softbreak_children;
	children.extend(parse_sections(events, pos, Some(OwnedTagEnd::Item), depth));

	if matches!(events.get(*pos), Some(OwnedEvent::End(OwnedTagEnd::Item))) {
		*pos += 1;
	}

	TaskItem { checkbox, content, children }
}

/// Split inline events at a SoftBreak if the part after it is a bare URL or shorthand ref.
/// Handles `- discretionary_engine\n  https://…/issues/77` (category + trailing ref).
fn split_inline_at_ref_softbreak(events: Vec<OwnedEvent>) -> (TaskContent, Vec<Section>) {
	if let Some(break_pos) = events.iter().rposition(|e| matches!(e, OwnedEvent::SoftBreak)) {
		let tail = events[break_pos + 1..].to_vec();
		let tail_classified = classify_inline_events(tail);
		if !matches!(tail_classified, TaskContent::Virtual(_)) {
			let head = events[..break_pos].to_vec();
			let content = classify_inline_events(head);
			let child_item = TaskItem {
				checkbox: None,
				content: tail_classified,
				children: Vec::new(),
			};
			return (content, vec![Section::List(vec![child_item])]);
		}
	}
	(classify_inline_events(events), Vec::new())
}

/// Classify a component's inline events into a `TaskContent`.
fn classify_inline_events(events: Vec<OwnedEvent>) -> TaskContent {
	// A numbered issue marker (linked or numbered-virtual) → embedded (expanded) issue.
	// Title text is a render concern; drop it.
	for event in &events {
		if let OwnedEvent::InlineHtml(html) = event
			&& let Some(inner) = html.strip_prefix("<!--").and_then(|s| s.strip_suffix("-->"))
			&& let IssueMarker::Linked { link, .. } | IssueMarker::Virtual { link: Some(link) } = IssueMarker::decode(inner.trim())
		{
			return TaskContent::Issue {
				r#ref: IssueRef::Url(link),
				embedded: true,
			};
		}
	}

	let full_text: String = events.iter().filter_map(|e| if let OwnedEvent::Text(t) = e { Some(t.as_str()) } else { None }).collect();
	let trimmed = full_text.trim();

	if !trimmed.is_empty() && !trimmed.contains(' ') {
		if let Some(mr) = MilestoneRef::parse_word(trimmed) {
			return TaskContent::Milestone(mr);
		}
		if let Some(ir) = IssueRef::parse_word(trimmed) {
			return TaskContent::Issue { r#ref: ir, embedded: false };
		}
	}

	TaskContent::Virtual(events)
}

// ─── Operations ──────────────────────────────────────────────────────────────

fn normalize_items(items: &mut [TaskItem]) {
	let is_checkbox = items.first().is_some_and(|i| i.checkbox.is_some());
	for item in items.iter_mut() {
		if is_checkbox && item.checkbox.is_none() {
			item.checkbox = Some(" ".into());
		} else if !is_checkbox && item.checkbox.is_some() {
			item.checkbox = None;
		}
	}
}

fn resolve_items(items: &mut [TaskItem], parent_context: Option<&str>) {
	for item in items.iter_mut() {
		let my_context = match &item.content {
			TaskContent::Virtual(events) => {
				let text = events_to_plain_text(events);
				let trimmed = text.trim();
				if !trimmed.is_empty() && !trimmed.contains(' ') { Some(trimmed.to_string()) } else { None }
			}
			_ => None,
		};

		if let TaskContent::Issue { r#ref, .. } = &mut item.content
			&& let Some(ctx) = parent_context
		{
			r#ref.resolve_with_context(ctx);
		}

		let child_ctx = my_context.as_deref().or(parent_context);
		for section in &mut item.children {
			if let Section::List(list) = section {
				resolve_items(list, child_ctx);
			}
		}
	}
}

fn collect_issue_links(items: &[TaskItem], out: &mut Vec<IssueLink>) {
	for item in items {
		if let TaskContent::Issue { r#ref, .. } = &item.content
			&& let Some(link) = r#ref.to_issue_link()
		{
			out.push(link);
		}
		for section in &item.children {
			if let Section::List(list) = section {
				collect_issue_links(list, out);
			}
		}
	}
}

fn collect_milestone_links(items: &[TaskItem], out: &mut Vec<MilestoneLink>) {
	for item in items {
		if let TaskContent::Milestone(mr) = &item.content {
			out.push(mr.to_milestone_link());
		}
		for section in &item.children {
			if let Section::List(list) = section {
				collect_milestone_links(list, out);
			}
		}
	}
}

fn collect_embedded(items: &[TaskItem], out: &mut Vec<(IssueLink, String)>) {
	for item in items {
		if let TaskContent::Issue { r#ref, embedded: true } = &item.content
			&& let Some(link) = r#ref.to_issue_link()
		{
			let mut section_text = String::new();
			serialize_item(item, &HashMap::new(), &mut section_text);
			out.push((link, section_text));
		}
		for section in &item.children {
			if let Section::List(list) = section {
				collect_embedded(list, out);
			}
		}
	}
}

fn remove_issue_items(items: &mut Vec<TaskItem>) {
	items.retain(|item| !matches!(item.content, TaskContent::Issue { .. }));
	for item in items.iter_mut() {
		for section in &mut item.children {
			if let Section::List(list) = section {
				remove_issue_items(list);
			}
		}
	}
}

fn collapse_items(items: &mut [TaskItem]) {
	for item in items.iter_mut() {
		let collapsed = match &item.content {
			TaskContent::Issue { r#ref, .. } => r#ref.to_issue_link().map(|link| TaskContent::Issue {
				r#ref: IssueRef::Url(link),
				embedded: false,
			}),
			TaskContent::Milestone(_) => None, // milestone refs are already bare links
			TaskContent::Virtual(_) => None,
		};
		if let Some(content) = collapsed {
			item.content = content;
			item.checkbox = None;
			item.children.clear();
		} else {
			for section in &mut item.children {
				if let Section::List(list) = section {
					collapse_items(list);
				}
			}
		}
	}
}

fn collect_homeless(items: &[TaskItem], section: &[String], path: &mut Vec<usize>, out: &mut Vec<(TaskItemId, String)>) {
	for (i, item) in items.iter().enumerate() {
		path.push(i);
		if is_new_task(item) {
			// A candidate's subtree is the issue's own content — no candidates inside it.
			let mut block = String::new();
			serialize_item(item, &HashMap::new(), &mut block);
			out.push((
				TaskItemId {
					section: section.to_vec(),
					path: path.clone(),
				},
				block,
			));
		} else {
			for (si, sec) in item.children.iter().enumerate() {
				if let Section::List(list) = sec {
					path.push(si);
					collect_homeless(list, section, path, out);
					path.pop();
				}
			}
		}
		path.pop();
	}
}

fn is_new_task(item: &TaskItem) -> bool {
	let Some(checkbox) = &item.checkbox else { return false };
	if !matches!(crate::CloseState::from_checkbox(checkbox), Ok(crate::CloseState::Open)) {
		return false;
	}
	let TaskContent::Virtual(events) = &item.content else { return false };
	let text = events_to_plain_text(events);
	let trimmed = text.trim();
	if trimmed.is_empty() {
		return false;
	}
	if !trimmed.contains(' ') && item.children.is_empty() {
		return false;
	}
	!has_ref_descendant(item)
}

fn has_ref_descendant(item: &TaskItem) -> bool {
	item.children.iter().any(|sec| match sec {
		Section::List(list) => list
			.iter()
			.any(|child| matches!(child.content, TaskContent::Issue { .. } | TaskContent::Milestone(_)) || has_ref_descendant(child)),
		Section::FreeContent(_) => false,
	})
}

fn resolve_item_mut<'a>(items: &'a mut [TaskItem], path: &[usize]) -> &'a mut TaskItem {
	let item = &mut items[path[0]];
	if path.len() == 1 {
		return item;
	}
	let Section::List(list) = &mut item.children[path[1]] else {
		panic!("TaskItemId path points into a non-list section")
	};
	resolve_item_mut(list, &path[2..])
}

// ─── Serialization ───────────────────────────────────────────────────────────

fn ensure_blank_line(output: &mut String) {
	let trailing_newlines = output.bytes().rev().take_while(|&b| b == b'\n').count();
	for _ in trailing_newlines..2 {
		output.push('\n');
	}
}

/// Whether a list should render loose (blank lines between items): checkbox lists,
/// or any list whose issue refs are being expanded.
fn items_are_loose(items: &[TaskItem], expansions: &HashMap<IssueLink, String>) -> bool {
	items
		.iter()
		.any(|item| item.checkbox.is_some() || matches!(&item.content, TaskContent::Issue { r#ref, .. } if r#ref.to_issue_link().is_some_and(|l| expansions.contains_key(&l))))
}

fn serialize_items(items: &[TaskItem], expansions: &HashMap<IssueLink, String>, output: &mut String) {
	let loose = items_are_loose(items, expansions);
	for (i, item) in items.iter().enumerate() {
		if i > 0 && loose {
			ensure_blank_line(output);
		} else if !output.is_empty() && !output.ends_with('\n') {
			output.push('\n');
		}
		serialize_item(item, expansions, output);
	}
}

fn serialize_section(section: &Section, expansions: &HashMap<IssueLink, String>, output: &mut String) {
	match section {
		Section::FreeContent(owned) => {
			if !output.is_empty() {
				ensure_blank_line(output);
			}
			output.push_str(&String::from(Events::from(owned.clone())));
		}
		Section::List(items) => serialize_items(items, expansions, output),
	}
}

fn serialize_item(item: &TaskItem, expansions: &HashMap<IssueLink, String>, output: &mut String) {
	// Expansion substitution: emit the issue's pre-rendered `Display` block verbatim.
	if let TaskContent::Issue { r#ref, .. } = &item.content
		&& let Some(link) = r#ref.to_issue_link()
		&& let Some(expansion) = expansions.get(&link)
	{
		output.push_str(expansion.trim_matches('\n'));
		return;
	}

	let mut events = vec![OwnedEvent::Start(OwnedTag::List(None)), OwnedEvent::Start(OwnedTag::Item)];
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
	events.push(OwnedEvent::End(OwnedTagEnd::Item));
	events.push(OwnedEvent::End(OwnedTagEnd::List(false)));
	output.push_str(&String::from(Events::from(events)));

	if !item.children.is_empty() {
		output.push('\n');
		let mut child_output = String::new();
		for child in &item.children {
			serialize_section(child, expansions, &mut child_output);
		}
		indent_into(output, &child_output, "  ");
	}
}

fn item_content_to_events(content: &TaskContent) -> Vec<OwnedEvent> {
	match content {
		TaskContent::Milestone(mr) => vec![OwnedEvent::Text(mr.to_string())],
		TaskContent::Issue { r#ref, .. } => vec![OwnedEvent::Text(r#ref.to_string())],
		TaskContent::Virtual(events) => events.clone(),
	}
}

fn events_to_plain_text(events: &[OwnedEvent]) -> String {
	let mut text = String::new();
	for event in events {
		if let OwnedEvent::Text(t) = event {
			text.push_str(t);
		}
	}
	text
}

#[cfg(test)]
mod tests {
	use std::{collections::HashMap, path::PathBuf};

	use super::*;
	use crate::Issue;

	fn expansions(pairs: &[(&str, &str)]) -> HashMap<IssueLink, String> {
		pairs.iter().map(|(url, view)| (IssueLink::parse(url).unwrap(), view.to_string())).collect()
	}

	#[test]
	fn test_parse_serialize_basic_structure() {
		let content = "# Sprint Goals\nSome description\n\n- [ ] discretionary_engine\n    - [ ] owner/repo#42\n\nFooter text\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		# Sprint Goals

		Some description

		Footer text

		- [ ] discretionary_engine
		  - [ ] owner/repo#42
		");
	}

	#[test]
	fn test_parse_shorthand_refs() {
		let content = "- [ ] owner/repo#123\n- [ ] tedi#42\n- [ ] #77\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		- [ ] owner/repo#123

		- [ ] tedi#42

		- [ ] \#77
		");
	}

	#[test]
	fn test_parse_embedded_issue() {
		let content = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		- [ ] https://github.com/owner/repo/issues/42
		  # Blockers
		  - task 1
		");
	}

	#[test]
	fn test_parse_bare_url() {
		let content = "- https://github.com/owner/repo/issues/99\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"- https://github.com/owner/repo/issues/99");
	}

	#[test]
	fn test_resolve_bare_refs_single_word_parent() {
		crate::current_user::set("myowner".to_string());
		let content = "- [ ] discretionary_engine\n    - [ ] #77\n";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @r"
		- [ ] discretionary_engine
		  - [ ] myowner/discretionary_engine#77
		");
	}

	#[test]
	fn test_resolve_bare_refs_owner_repo_parent() {
		let content = "- valeratrades/tedi\n    - #80\n";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @r"
		- valeratrades/tedi
		  - valeratrades/tedi#80
		");
	}

	#[test]
	fn test_resolve_bare_refs_multilevel_takes_immediate_parent() {
		crate::current_user::set("myowner".to_string());
		let content = "- discretionary_engine\n    - tedi\n        - [ ] #80\n";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		insta::assert_snapshot!(doc.serialize(), @r"
		- discretionary_engine
		  - tedi
		    - [ ] myowner/tedi#80
		");
	}

	#[test]
	fn test_issue_links() {
		crate::current_user::set("myowner".to_string());
		let content = "- [ ] owner/repo#42\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/99 -->\n";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		let links: Vec<_> = doc.issue_links().iter().map(|l| l.number()).collect();
		assert_eq!(links, [42, 99]);
	}

	#[test]
	fn test_collapse_to_links() {
		let content = "# Sprint\n\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n\nFooter\n";
		let mut doc = TaskView::parse(content);
		doc.collapse_to_links();
		insta::assert_snapshot!(doc.serialize(), @r"
		# Sprint

		Footer

		- https://github.com/owner/repo/issues/42
		");
	}

	#[test]
	fn test_serialize_roundtrip() {
		let content = "# Sprint Goals\n\nSome description\n\n- [ ] owner/repo#42\n- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/99 -->\n\nFooter text\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		# Sprint Goals

		Some description

		Footer text

		- [ ] owner/repo#42

		- [ ] https://github.com/owner/repo/issues/99
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
	fn test_sections_keep_document_order() {
		let content = "# zebra\n- z item\n\n# alpha\n\n## beta\n- b item\n\n# middle\n- m item\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		# zebra

		- z item

		# alpha

		## beta

		- b item

		# middle

		- m item
		");
	}

	#[test]
	fn test_mixed_content() {
		let content = "# Header\n\nParagraph text\n\n- item 1\n- item 2\n\nMore text\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		# Header

		Paragraph text

		More text

		- item 1
		- item 2
		");
	}

	#[test]
	fn test_tight_list_roundtrip() {
		let content = "- item 1\n- item 2\n- item 3\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		- item 1
		- item 2
		- item 3
		");
	}

	#[test]
	fn test_loose_list_roundtrip() {
		let content = "- item 1\n\n- item 2\n\n- item 3\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
		- item 1
		- item 2
		- item 3
		");
	}

	#[test]
	fn test_top_level_items_with_children_padded() {
		let content = "- [ ] OpenClaw\n\t# Blockers\n\t- wait on Vincent\n- [ ] discretionary_engine\n\t# Blockers\n\t- new protocols\n\t- define interface\n";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @r"
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
	fn test_render_all_bare_urls() {
		let stored = "\
# important today
- https://github.com/owner/repo/issues/1
- https://github.com/owner/repo/issues/2
- https://github.com/owner/repo/issues/3
";
		let doc = TaskView::parse(stored);
		let exp = expansions(&[
			(
				"https://github.com/owner/repo/issues/1",
				"- [ ] First <!-- @user https://github.com/owner/repo/issues/1 -->\n\t# Blockers\n\t- task A",
			),
			("https://github.com/owner/repo/issues/2", "- [ ] Second <!-- @user https://github.com/owner/repo/issues/2 -->"),
			("https://github.com/owner/repo/issues/3", "- [x] Third <!-- @user https://github.com/owner/repo/issues/3 -->"),
		]);
		insta::assert_snapshot!(doc.render(&exp), @"
		# important today

		- [ ] First <!-- @user https://github.com/owner/repo/issues/1 -->
			# Blockers
			- task A

		- [ ] Second <!-- @user https://github.com/owner/repo/issues/2 -->

		- [x] Third <!-- @user https://github.com/owner/repo/issues/3 -->
		");
	}

	#[test]
	fn test_render_mixed_issue_and_text_items() {
		let stored = "\
# important today
- [ ] OpenClaw
  # Blockers
  - wait on Vincent
- https://github.com/valeratrades/discretionary_engine/issues/77
- https://github.com/valeratrades/discretionary_engine/issues/78
";
		let doc = TaskView::parse(stored);
		let exp = expansions(&[
			(
				"https://github.com/valeratrades/discretionary_engine/issues/77",
				"- [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->\n\t# Blockers\n\t- new protocols",
			),
			(
				"https://github.com/valeratrades/discretionary_engine/issues/78",
				"- [ ] risk <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/78 -->",
			),
		]);
		insta::assert_snapshot!(doc.render(&exp), @"
		# important today

		- [ ] OpenClaw
		  # Blockers
		  - wait on Vincent

		- [ ] v2_interface <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/77 -->
			# Blockers
			- new protocols

		- [ ] risk <!-- @valeratrades https://github.com/valeratrades/discretionary_engine/issues/78 -->
		");
	}

	#[test]
	fn test_embedded_issues_detected() {
		let content = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->\n\t# Blockers\n\t- task 1\n";
		let doc = TaskView::parse(content);
		let embedded = doc.embedded_issues();
		assert_eq!(embedded.len(), 1);
		assert_eq!(embedded[0].0.number(), 42);
		insta::assert_snapshot!(embedded[0].1, @"
		- [ ] https://github.com/owner/repo/issues/42
		  # Blockers
		  - task 1
		");
	}

	#[test]
	fn test_embedded_issues_after_edit_simulation() {
		let expanded = "- [ ] Empty Issue <!-- @mock_user https://github.com/o/r/issues/50 -->";
		let edited = format!("{expanded}\n\t# Blockers\n\t- todo\n");
		let doc = TaskView::parse(&edited);
		let embedded = doc.embedded_issues();
		assert_eq!(embedded.len(), 1);
		let blockers = parse_blockers_from_embedded(&embedded[0].1);
		assert_eq!(blockers.items.len(), 1);
		assert_eq!(blockers.items[0].text, "todo");
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
";
		let doc = TaskView::parse(content);
		insta::assert_snapshot!(doc.serialize(), @"
		# important today

		- [ ] OpenClaw
		  have relevant stuff in tg/general. Also would include probably buying the VPS, as it needs to be able to work fully autonomously
		  # Blockers
		  - wait on Vincent to set up the server

		- [ ] discretionary_engine
		  - [ ] https://github.com/valeratrades/discretionary_engine/issues/77
		    # Blockers
		    - new protocols attach options
		    - define the exact new target interface
		");
	}

	#[test]
	fn test_serialize_idempotent_custom_checkboxes() {
		let content = "\
- [ ] OpenClaw
  text
  # Blockers
  - wait
- [-] Not Planned <!-- @user https://github.com/owner/repo/issues/2 -->
  np body
- [42] Duplicate <!-- @user https://github.com/owner/repo/issues/3 -->
  dup body
- \\[.\\] custom marker
  custom body
";
		let doc = TaskView::parse(content);
		let s1 = doc.serialize();
		for cycle in 1..=5 {
			let re = TaskView::parse(&s1);
			let sn = re.serialize();
			assert_eq!(s1, sn, "serialize must be idempotent at cycle {cycle}");
		}
	}

	#[test]
	fn test_homeless_tasks_candidates_and_skips() {
		crate::current_user::set("myowner".to_string());
		let content = "\
# important today
- [ ] OpenClaw
  # Blockers
  - wait on Vincent
- [ ] discretionary_engine
  - [ ] #77
  - [ ] some new task
    # Blockers
    - first step
- [ ] halftyped
- [x] already done task
- [ ] pay for Tokyo server
";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		let found: Vec<String> = doc.homeless_tasks().into_iter().map(|(_, block)| block).collect();
		// OpenClaw (single-word but has blockers) and the two multi-word tasks materialize;
		// the category header (ref descendant), single-word childless, and closed items don't.
		insta::assert_snapshot!(found.join("~~~\n"), @r"
		- [ ] OpenClaw
		  # Blockers
		  - wait on Vincent
		~~~
		- [ ] some new task
		  # Blockers
		  - first step
		~~~
		- [ ] pay for Tokyo server
		");
	}

	#[test]
	fn test_homeless_plain_list_untouched() {
		let content = "- equilibre people research\n  - linkedin token\n- pay for Tokyo server\n";
		let doc = TaskView::parse(content);
		assert!(doc.homeless_tasks().is_empty(), "plain non-checkbox lists must not materialize");
	}

	#[test]
	fn test_assign_link_collapses_to_bare_link() {
		let content = "- [ ] some new task\n  # Blockers\n  - first step\n- [ ] o/r#42\n";
		let mut doc = TaskView::parse(content);
		doc.resolve_bare_refs();
		let homeless = doc.homeless_tasks();
		assert_eq!(homeless.len(), 1);
		let link = IssueLink::parse("https://github.com/local/virtual/issues/1").unwrap();
		doc.assign_link(&homeless[0].0, link);
		doc.collapse_to_links();
		insta::assert_snapshot!(doc.serialize(), @r"
		- https://github.com/local/virtual/issues/1
		- https://github.com/o/r/issues/42
		");
	}

	#[test]
	fn test_cross_session_blocker_escaping() {
		let initial_files = vec![
			"- [ ] Test <!-- https://github.com/o/r/issues/42 -->\n  # Blockers\n  - `insert`semantics on`RoutingHub`\n",
			"- [ ] Test <!-- https://github.com/o/r/issues/42 -->\n  # Blockers\n  - move into \\_strategy\n",
			"- [ ] Test <!-- https://github.com/o/r/issues/42 -->\n  # Blockers\n  - parent\n    - `child` with backtick\n",
		];

		for initial in initial_files {
			let mut file_content = initial.to_string();
			for cycle in 1..=5 {
				let vi = crate::VirtualIssue::parse(&file_content, PathBuf::from("/tmp/test.md")).unwrap();
				let link = crate::IssueLink::parse("https://github.com/o/r/issues/42").unwrap();
				let identity = crate::IssueIdentity::new_linked(None, None, link, crate::IssueTimestamps::default());
				let issue = Issue {
					identity,
					contents: vi.contents.clone(),
					children: HashMap::new(),
				};
				let view = issue.to_string();

				let edited_doc = TaskView::parse(&view);
				let embedded = edited_doc.embedded_issues();
				let (_, section_text) = embedded.first().expect("should have embedded issue");
				let new_blockers = parse_blockers_from_embedded(section_text);

				let mut new_vi = vi;
				new_vi.contents.blockers = new_blockers;
				let link2 = crate::IssueLink::parse("https://github.com/o/r/issues/42").unwrap();
				let identity2 = crate::IssueIdentity::new_linked(None, None, link2, crate::IssueTimestamps::default());
				let new_issue = crate::Issue {
					identity: identity2,
					contents: new_vi.contents,
					children: HashMap::new(),
				};
				let new_file = new_issue.to_string();

				if new_file.trim() == file_content.trim() {
					break;
				}
				file_content = new_file;
				if cycle == 5 {
					panic!("cross-session cycle not stable after 5 cycles for input: {initial:?}\nfinal: {file_content:?}");
				}
			}
		}
	}
}
