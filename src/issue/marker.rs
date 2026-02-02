//! Marker types for issue file format.
//!
//! Markers are HTML comments or special syntax that encode metadata in issue files.
//! This module provides decoding and encoding for all marker types, ensuring
//! consistent handling of whitespace and formatting.

use std::fmt;

use crate::{Header, IssueIdentity, IssueLink};

/// Issue identity marker - encodes how an issue relates to GitHub.
///
/// Formats:
/// - Linked: `<!-- @user url -->`
/// - Pending: `<!-- pending -->` or no marker (default)
/// - Virtual: `<!-- virtual -->`
#[derive(Clone, Debug, PartialEq)]
pub enum IssueMarker {
	/// Linked to GitHub: `<!-- @user url -->`
	Linked { user: String, link: IssueLink },
	/// Pending creation on GitHub (will be created on first sync)
	Pending,
	/// Virtual (local-only, never syncs to GitHub)
	Virtual,
}

impl IssueMarker {
	/// Decode an issue marker from HTML comment inner content.
	/// Returns `Pending` for empty/unrecognized content (default).
	pub fn decode(inner: &str) -> Self {
		let s = inner.trim();

		// Empty or explicit pending
		if s.is_empty() || s == "pending" {
			return Self::Pending;
		}

		// Legacy: `local:` also means pending
		if s.starts_with("local:") {
			return Self::Pending;
		}

		// Virtual
		if s == "virtual" || s.starts_with("virtual:") {
			return Self::Virtual;
		}

		// Linked format: `@user url`
		if let Some(rest) = s.strip_prefix('@') {
			if let Some(space_idx) = rest.find(' ') {
				let user = rest[..space_idx].to_string();
				let url = rest[space_idx + 1..].trim();
				if let Some(link) = IssueLink::parse(url) {
					return Self::Linked { user, link };
				}
			}
		}

		// Default to pending for unrecognized
		Self::Pending
	}

	/// Encode the issue marker to HTML comment inner content.
	pub fn encode(&self) -> String {
		match self {
			Self::Linked { user, link } => format!("@{user} {}", link.as_str()),
			Self::Pending => "pending".to_string(),
			Self::Virtual => "virtual".to_string(),
		}
	}
}

impl fmt::Display for IssueMarker {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "<!-- {} -->", self.encode())
	}
}

impl From<&IssueIdentity> for IssueMarker {
	fn from(identity: &IssueIdentity) -> Self {
		if let Some(meta) = identity.as_linked() {
			IssueMarker::Linked {
				user: meta.user.clone(),
				link: meta.link.clone(),
			}
		} else if identity.is_virtual() {
			IssueMarker::Virtual
		} else {
			IssueMarker::Pending
		}
	}
}

/// A marker that can appear in issue files.
/// All markers normalize whitespace on decode and encode with consistent spacing.
#[derive(Clone, Debug, PartialEq)]
pub enum Marker {
	/// Issue identity marker
	Issue(IssueMarker),
	/// Comment marker: `<!-- @user url#issuecomment-123 -->`
	Comment { user: String, url: String, id: u64 },
	/// New comment marker: `<!-- new comment -->`
	NewComment,
	/// Blockers section marker: `# Blockers`
	/// Uses the Header type for encoding/decoding.
	/// Legacy format (`<!-- blockers -->`) is still decoded but encodes to Header format.
	BlockersSection(Header),
	/// Omitted start marker: `<!--omitted {{{always-->` (vim fold start)
	OmittedStart,
	/// Omitted end marker: `<!--,}}}-->` (vim fold end)
	OmittedEnd,
}

impl Marker {
	/// Decode a marker from a string.
	/// For BlockersSection, the entire line (after trimming) must match.
	/// For other markers, decodes the HTML comment content.
	/// Returns None if the string doesn't contain a recognized marker.
	pub fn decode(s: &str) -> Option<Self> {
		let trimmed = s.trim();

		// Shorthand: `!c` or `!C` for new comment
		if trimmed.eq_ignore_ascii_case("!c") {
			return Some(Marker::NewComment);
		}

		// Shorthand: `!b` or `!B` for blockers section
		if trimmed.eq_ignore_ascii_case("!b") {
			return Some(Marker::BlockersSection(Header::new(1, "Blockers")));
		}

		// Check for header-based blockers marker using the Header type: `# Blockers`
		if let Some(header) = Header::decode(trimmed) {
			let content_lower = header.content.to_ascii_lowercase();
			let content_trimmed = content_lower.trim_end_matches(':');
			if content_trimmed == "blockers" || content_trimmed == "blocker" {
				return Some(Marker::BlockersSection(header));
			}
		}

		// Check for HTML comment markers
		if !trimmed.starts_with("<!--") || !trimmed.ends_with("-->") {
			return None;
		}

		// Extract inner content, normalizing whitespace
		let inner = trimmed.strip_prefix("<!--")?.strip_suffix("-->")?.trim();
		let lower = inner.to_ascii_lowercase();

		// Blockers (legacy HTML comment) - entire comment must be just "blockers"
		// Decodes to Header format for consistency
		if lower == "blockers" || lower == "blocker" {
			return Some(Marker::BlockersSection(Header::new(1, "Blockers")));
		}

		// New comment
		if lower == "new comment" {
			return Some(Marker::NewComment);
		}

		// Omitted vim fold markers
		if lower.starts_with("omitted") && lower.contains("{{{") {
			return Some(Marker::OmittedStart);
		}
		if lower.starts_with(",}}}") || lower == ",}}}" {
			return Some(Marker::OmittedEnd);
		}

		// Comment marker (contains #issuecomment-)
		if inner.contains("#issuecomment-") {
			// Must have @user prefix
			if let Some(rest) = inner.strip_prefix('@') {
				if let Some(space_idx) = rest.find(' ') {
					let user = rest[..space_idx].to_string();
					let url = rest[space_idx + 1..].trim();
					let id = url.split("#issuecomment-").nth(1).and_then(|s| {
						let digits: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
						digits.parse().ok()
					})?;
					return Some(Marker::Comment { user, url: url.to_string(), id });
				}
			}
			return None;
		}

		// Issue marker (delegate to IssueMarker::decode)
		Some(Marker::Issue(IssueMarker::decode(inner)))
	}

	/// Encode the marker to a string with consistent formatting.
	pub fn encode(&self) -> String {
		match self {
			Marker::Issue(issue) => issue.to_string(),
			Marker::Comment { user, url, .. } => format!("<!-- @{user} {url} -->"),
			Marker::NewComment => "<!-- new comment -->".to_string(),
			Marker::BlockersSection(header) => header.encode(),
			Marker::OmittedStart => "<!--omitted {{{always-->".to_string(),
			Marker::OmittedEnd => "<!--,}}}-->".to_string(),
		}
	}
}

impl fmt::Display for Marker {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.encode())
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_issue_marker_decode_linked() {
		let marker = IssueMarker::decode("@owner https://github.com/owner/repo/issues/123");
		assert!(matches!(marker, IssueMarker::Linked { user, link } if user == "owner" && link.number() == 123));
	}

	#[test]
	fn test_issue_marker_decode_pending() {
		assert_eq!(IssueMarker::decode(""), IssueMarker::Pending);
		assert_eq!(IssueMarker::decode("pending"), IssueMarker::Pending);
		assert_eq!(IssueMarker::decode("local:"), IssueMarker::Pending);
		assert_eq!(IssueMarker::decode("local:anything"), IssueMarker::Pending);
		assert_eq!(IssueMarker::decode("unrecognized stuff"), IssueMarker::Pending);
	}

	#[test]
	fn test_issue_marker_decode_virtual() {
		assert_eq!(IssueMarker::decode("virtual"), IssueMarker::Virtual);
		assert_eq!(IssueMarker::decode("virtual:"), IssueMarker::Virtual);
	}

	#[test]
	fn test_issue_marker_encode() {
		assert_eq!(IssueMarker::Pending.encode(), "pending");
		assert_eq!(IssueMarker::Virtual.encode(), "virtual");

		let link = IssueLink::parse("https://github.com/owner/repo/issues/123").unwrap();
		let linked = IssueMarker::Linked { user: "owner".to_string(), link };
		assert_eq!(linked.encode(), "@owner https://github.com/owner/repo/issues/123");
	}

	#[test]
	fn test_issue_marker_roundtrip() {
		let link = IssueLink::parse("https://github.com/owner/repo/issues/123").unwrap();
		let markers = vec![IssueMarker::Pending, IssueMarker::Virtual, IssueMarker::Linked { user: "owner".to_string(), link }];

		for marker in markers {
			let encoded = marker.encode();
			let decoded = IssueMarker::decode(&encoded);
			assert_eq!(marker, decoded, "Roundtrip failed for {marker:?}");
		}
	}

	#[test]
	fn test_decode_issue_marker_via_marker() {
		// Linked
		let m = Marker::decode("<!-- @owner https://github.com/owner/repo/issues/123 -->");
		assert!(matches!(m, Some(Marker::Issue(IssueMarker::Linked { .. }))));

		// Pending (explicit)
		let m = Marker::decode("<!-- pending -->");
		assert!(matches!(m, Some(Marker::Issue(IssueMarker::Pending))));

		// Virtual
		let m = Marker::decode("<!-- virtual -->");
		assert!(matches!(m, Some(Marker::Issue(IssueMarker::Virtual))));

		// Legacy local:
		let m = Marker::decode("<!-- local: -->");
		assert!(matches!(m, Some(Marker::Issue(IssueMarker::Pending))));
	}

	#[test]
	fn test_decode_comment() {
		let m = Marker::decode("<!-- @owner https://github.com/owner/repo/issues/123#issuecomment-456 -->");
		assert!(matches!(m, Some(Marker::Comment { user, id, .. }) if user == "owner" && id == 456));
	}

	#[test]
	fn test_decode_blockers_section() {
		fn is_blockers_section(marker: Option<Marker>) -> bool {
			matches!(marker, Some(Marker::BlockersSection(_)))
		}

		// Markdown header (preferred)
		assert!(is_blockers_section(Marker::decode("# Blockers")));
		assert!(is_blockers_section(Marker::decode("## Blockers")));
		assert!(is_blockers_section(Marker::decode("### Blockers:")));
		// With leading/trailing whitespace
		assert!(is_blockers_section(Marker::decode("  # Blockers  ")));
		// Legacy HTML comment (converts to Header)
		assert!(is_blockers_section(Marker::decode("<!--blockers-->")));
		assert!(is_blockers_section(Marker::decode("<!-- blockers -->")));
		assert!(is_blockers_section(Marker::decode("<!--blocker-->")));

		// Shorthand
		assert!(is_blockers_section(Marker::decode("!b")));
		assert!(is_blockers_section(Marker::decode("!B")));
		assert!(is_blockers_section(Marker::decode("  !b  ")));

		// Should NOT match if there's other content on the line
		assert!(!is_blockers_section(Marker::decode("# Blockers and more")));
		assert!(!is_blockers_section(Marker::decode("Some text # Blockers")));

		// Test that Header content and level are preserved
		if let Some(Marker::BlockersSection(header)) = Marker::decode("## Blockers") {
			assert_eq!(header.level, 2);
			assert_eq!(header.content, "Blockers");
		} else {
			panic!("Expected BlockersSection with Header");
		}
	}

	#[test]
	fn test_decode_new_comment() {
		assert_eq!(Marker::decode("<!--new comment-->"), Some(Marker::NewComment));
		assert_eq!(Marker::decode("<!-- new comment -->"), Some(Marker::NewComment));
		// Shorthand
		assert_eq!(Marker::decode("!c"), Some(Marker::NewComment));
		assert_eq!(Marker::decode("!C"), Some(Marker::NewComment));
		assert_eq!(Marker::decode("  !c  "), Some(Marker::NewComment));
	}

	#[test]
	fn test_decode_omitted() {
		assert_eq!(Marker::decode("<!--omitted {{{always-->"), Some(Marker::OmittedStart));
		assert_eq!(Marker::decode("<!-- omitted {{{always -->"), Some(Marker::OmittedStart));
		assert_eq!(Marker::decode("<!--,}}}-->"), Some(Marker::OmittedEnd));
	}

	#[test]
	fn test_encode() {
		assert_eq!(Marker::Issue(IssueMarker::Pending).encode(), "<!-- pending -->");
		assert_eq!(Marker::Issue(IssueMarker::Virtual).encode(), "<!-- virtual -->");
		assert_eq!(Marker::BlockersSection(Header::new(1, "Blockers")).encode(), "# Blockers");
		assert_eq!(Marker::BlockersSection(Header::new(2, "Blockers")).encode(), "## Blockers");
		assert_eq!(Marker::NewComment.encode(), "<!-- new comment -->");
		assert_eq!(Marker::OmittedStart.encode(), "<!--omitted {{{always-->");
		assert_eq!(Marker::OmittedEnd.encode(), "<!--,}}}-->");
	}

	#[test]
	fn test_roundtrip() {
		let link = IssueLink::parse("https://github.com/owner/repo/issues/123").unwrap();
		let markers = vec![
			Marker::Issue(IssueMarker::Pending),
			Marker::Issue(IssueMarker::Virtual),
			Marker::Issue(IssueMarker::Linked {
				user: "owner".to_string(),
				link: link.clone(),
			}),
			Marker::Comment {
				user: "owner".to_string(),
				url: "https://github.com/owner/repo/issues/123#issuecomment-456".to_string(),
				id: 456,
			},
			Marker::NewComment,
			Marker::OmittedStart,
			Marker::OmittedEnd,
		];

		for marker in markers {
			let encoded = marker.encode();
			let decoded = Marker::decode(&encoded).unwrap_or_else(|| panic!("Failed to decode: {encoded}"));
			assert_eq!(marker, decoded, "Roundtrip failed for {marker:?}");
		}
	}
}
