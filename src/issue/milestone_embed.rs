//! Parsing and serialization of embedded issue refs in milestone descriptions.
//!
//! Milestones can embed issue references in two forms:
//! - Shorthand: `owner/repo#123`
//! - Full title line with marker: `- [ ] Title <!-- @user url -->`
//!
//! Embedded issues include their blockers section at one tab indent.

use super::{Issue, IssueLink, IssueMarker};

/// An embedded issue reference found in milestone content.
pub struct EmbeddedIssueRef {
	/// The IssueLink parsed from the title line marker.
	pub link: IssueLink,
	/// Line range in the original content (start inclusive, end exclusive).
	pub line_start: usize,
	pub line_end: usize,
}

/// Parse a shorthand ref like `owner/repo#123` from a line.
///
/// Strips list prefixes (`- `, `- [ ] `, etc.) and indentation, then checks
/// that the remaining content is a single word matching `owner/repo#number`.
pub fn parse_shorthand_ref(line: &str) -> Option<IssueLink> {
	let trimmed = line.trim();

	// Strip list prefix: `- [ ] `, `- [x] `, `- `, etc.
	let content = if trimmed.starts_with("- [") {
		let bracket_end = trimmed.find("] ")?;
		trimmed[bracket_end + 2..].trim()
	} else if let Some(rest) = trimmed.strip_prefix("- ") {
		rest.trim()
	} else {
		trimmed
	};

	// Must be a single word (no spaces)
	if content.contains(' ') {
		return None;
	}

	parse_shorthand_word(content)
}

/// Check if a line is an embedded issue title line (has a checkbox + issue marker).
/// Returns the IssueLink if found.
pub fn parse_embedded_title_line(line: &str) -> Option<IssueLink> {
	let trimmed = line.trim();
	if !trimmed.starts_with("- [") {
		return None;
	}
	// Look for an issue marker at the end
	let (marker, _title) = IssueMarker::parse_from_end(trimmed)?;
	match marker {
		IssueMarker::Linked { link, .. } => Some(link),
		_ => None,
	}
}
/// Find all embedded issue sections in the milestone content.
/// An embedded issue is a title line (with checkbox + marker) followed by
/// indented content (blockers section etc) until the next non-indented line.
pub fn find_embedded_issues(content: &str) -> Vec<EmbeddedIssueRef> {
	let lines: Vec<&str> = content.lines().collect();
	let mut refs = Vec::new();
	let mut i = 0;

	while i < lines.len() {
		if let Some(link) = parse_embedded_title_line(lines[i]) {
			let line_start = i;
			i += 1;
			// Consume indented lines that belong to this issue
			while i < lines.len() && (lines[i].starts_with('\t') || lines[i].trim().is_empty()) {
				// Empty lines within the section are included, but stop at two consecutive empty lines
				// or at a non-indented line
				if lines[i].trim().is_empty() {
					// Look ahead: if next line is also empty or not indented, stop
					if i + 1 >= lines.len() || (!lines[i + 1].starts_with('\t') && !lines[i + 1].trim().is_empty()) {
						break;
					}
				}
				i += 1;
			}
			refs.push(EmbeddedIssueRef { link, line_start, line_end: i });
		} else {
			i += 1;
		}
	}

	refs
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

	// Find the blockers header (at indent level 1 = one tab)
	let mut blockers_start = None;
	let mut select_blockers = false;
	for (idx, line) in lines.iter().enumerate().skip(1) {
		let content = line.strip_prefix('\t').unwrap_or(line);
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

	// Collect blocker lines (strip one tab of indent)
	let blocker_lines: Vec<String> = lines[start..]
		.iter()
		.filter(|l| !l.trim().is_empty())
		.map(|l| l.strip_prefix('\t').unwrap_or(l).to_string())
		.collect();

	let mut seq = super::BlockerSequence::parse(&blocker_lines.join("\n"));
	if select_blockers {
		seq.set_state = Some(super::BlockerSetState::Pending);
	}
	seq
}
/// Try to parse a single word as `owner/repo#number`.
fn parse_shorthand_word(word: &str) -> Option<IssueLink> {
	let hash_pos = word.find('#')?;
	let before = &word[..hash_pos];
	let after = &word[hash_pos + 1..];

	// Must have exactly one `/` in the before part
	let slash_pos = before.find('/')?;
	if before[slash_pos + 1..].contains('/') {
		return None;
	}

	let owner = &before[..slash_pos];
	let repo = &before[slash_pos + 1..];
	let number: u64 = after.parse().ok()?;

	if owner.is_empty() || repo.is_empty() {
		return None;
	}

	let url = format!("https://github.com/{owner}/{repo}/issues/{number}");
	IssueLink::parse(&url)
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_parse_shorthand_ref() {
		// Bare ref
		let link = parse_shorthand_ref("owner/repo#123").unwrap();
		assert_eq!(link.owner(), "owner");
		assert_eq!(link.repo(), "repo");
		assert_eq!(link.number(), 123);

		// With whitespace
		let link = parse_shorthand_ref("  owner/repo#456  ").unwrap();
		assert_eq!(link.number(), 456);

		// With list prefix
		let link = parse_shorthand_ref("- owner/repo#789").unwrap();
		assert_eq!(link.number(), 789);

		// With checkbox prefix
		let link = parse_shorthand_ref("- [ ] owner/repo#42").unwrap();
		assert_eq!(link.number(), 42);

		// Indented with checkbox
		let link = parse_shorthand_ref("  - [ ] owner/repo#80").unwrap();
		assert_eq!(link.number(), 80);

		// Invalid: extra words after ref
		assert!(parse_shorthand_ref("- [ ] valeratrades/tedi#80 extra").is_none());
		// Invalid: title + ref (not a shorthand line)
		assert!(parse_shorthand_ref("- [ ] My Title valeratrades/tedi#80").is_none());

		// Invalid basics
		assert!(parse_shorthand_ref("not-a-ref").is_none());
		assert!(parse_shorthand_ref("#123").is_none());
		assert!(parse_shorthand_ref("repo#123").is_none());
		assert!(parse_shorthand_ref("a/b/c#123").is_none());
		assert!(parse_shorthand_ref("owner/repo#abc").is_none());
	}

	#[test]
	fn test_parse_embedded_title_line() {
		let line = "- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->";
		let link = parse_embedded_title_line(line).unwrap();
		assert_eq!(link.owner(), "owner");
		assert_eq!(link.repo(), "repo");
		assert_eq!(link.number(), 42);

		// Closed issue
		let line = "- [x] Done Issue <!-- @user https://github.com/owner/repo/issues/99 -->";
		let link = parse_embedded_title_line(line).unwrap();
		assert_eq!(link.number(), 99);

		// Not an embedded issue
		assert!(parse_embedded_title_line("just text").is_none());
		assert!(parse_embedded_title_line("- [ ] No marker").is_none());
		assert!(parse_embedded_title_line("- [ ] Pending <!-- pending -->").is_none());
	}

	#[test]
	fn test_find_embedded_issues() {
		let content = "\
# Sprint Goals
Some description

- [ ] My Issue <!-- @user https://github.com/owner/repo/issues/42 -->
\t# Blockers
\t- task 1
\t- task 2

More text here

- [x] Done <!-- @user https://github.com/owner/repo/issues/99 -->";

		let refs = find_embedded_issues(content);
		assert_eq!(refs.len(), 2);
		assert_eq!(refs[0].link.number(), 42);
		assert_eq!(refs[0].line_start, 3);
		assert_eq!(refs[0].line_end, 7);
		assert_eq!(refs[1].link.number(), 99);
		assert_eq!(refs[1].line_start, 10);
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
		let blockers = parse_blockers_from_embedded(section);
		assert!(blockers.is_empty());
	}

	#[test]
	fn test_serialize_blockers_view_roundtrip() {
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

		let serialized = serialize_blockers_view(&issue);
		insta::assert_snapshot!(serialized, @"- [ ] No Blockers <!-- https://github.com/owner/repo/issues/42 -->");
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

		let serialized = serialize_blockers_view(&issue);
		insta::assert_snapshot!(serialized, @"
		- [ ] [bug, urgent] Labeled <!-- https://github.com/owner/repo/issues/42 -->
			# Blockers
			- do thing
		");
	}
}
