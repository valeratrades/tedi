//! Marker types for issue file format.
//!
//! Markers are HTML comments or special syntax that encode metadata in issue files.
//! This module provides decoding and encoding for all marker types, ensuring
//! consistent handling of whitespace and formatting.

use std::fmt;

use crate::Header;

/// A marker that can appear in issue files.
/// All markers normalize whitespace on decode and encode with consistent spacing.
#[derive(Clone, Debug, PartialEq)]
pub enum Marker {
	/// Issue URL marker: `<!-- url -->` or `<!--immutable url -->`
	IssueUrl { url: String, immutable: bool },
	/// Sub-issue marker: `<!--sub url -->`
	SubIssue { url: String },
	/// Comment marker: `<!-- url#issuecomment-123 -->` or `<!--immutable url#issuecomment-123 -->`
	Comment { url: String, id: u64, immutable: bool },
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

		// Check for immutable prefix
		let (immutable, rest) = if let Some(rest) = inner.strip_prefix("immutable ").or_else(|| inner.strip_prefix("immutable\t")) {
			(true, rest.trim())
		} else {
			(false, inner)
		};

		// Sub-issue marker
		if let Some(url) = rest.strip_prefix("sub ").or_else(|| rest.strip_prefix("sub\t")) {
			return Some(Marker::SubIssue { url: url.trim().to_string() });
		}

		// Comment marker (contains #issuecomment-)
		if rest.contains("#issuecomment-") {
			let id = rest.split("#issuecomment-").nth(1).and_then(|s| {
				// Take only digits
				let digits: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
				digits.parse().ok()
			})?;
			return Some(Marker::Comment {
				url: rest.to_string(),
				id,
				immutable,
			});
		}

		// Issue URL marker (anything else is treated as a URL)
		if !rest.is_empty() {
			return Some(Marker::IssueUrl { url: rest.to_string(), immutable });
		}

		None
	}

	/// Encode the marker to a string with consistent formatting.
	/// Uses `<!-- content -->` HTML comment format.
	pub fn encode(&self) -> String {
		match self {
			Marker::IssueUrl { url, immutable } =>
				if *immutable {
					format!("<!--immutable {url} -->")
				} else {
					format!("<!-- {url} -->")
				},
			Marker::SubIssue { url } => format!("<!--sub {url} -->"),
			Marker::Comment { url, immutable, .. } =>
				if *immutable {
					format!("<!--immutable {url} -->")
				} else {
					format!("<!-- {url} -->")
				},
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
	fn test_decode_issue_url() {
		// With spaces
		assert_eq!(
			Marker::decode("<!-- https://github.com/owner/repo/issues/123 -->"),
			Some(Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: false
			})
		);
		// Without spaces
		assert_eq!(
			Marker::decode("<!--https://github.com/owner/repo/issues/123-->"),
			Some(Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: false
			})
		);
		// Immutable
		assert_eq!(
			Marker::decode("<!--immutable https://github.com/owner/repo/issues/123-->"),
			Some(Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: true
			})
		);
	}

	#[test]
	fn test_decode_sub_issue() {
		assert_eq!(
			Marker::decode("<!--sub https://github.com/owner/repo/issues/124-->"),
			Some(Marker::SubIssue {
				url: "https://github.com/owner/repo/issues/124".to_string()
			})
		);
		assert_eq!(
			Marker::decode("<!--sub https://github.com/owner/repo/issues/124 -->"),
			Some(Marker::SubIssue {
				url: "https://github.com/owner/repo/issues/124".to_string()
			})
		);
	}

	#[test]
	fn test_decode_comment() {
		assert_eq!(
			Marker::decode("<!--https://github.com/owner/repo/issues/123#issuecomment-456-->"),
			Some(Marker::Comment {
				url: "https://github.com/owner/repo/issues/123#issuecomment-456".to_string(),
				id: 456,
				immutable: false
			})
		);
		assert_eq!(
			Marker::decode("<!-- https://github.com/owner/repo/issues/123#issuecomment-456 -->"),
			Some(Marker::Comment {
				url: "https://github.com/owner/repo/issues/123#issuecomment-456".to_string(),
				id: 456,
				immutable: false
			})
		);
		assert_eq!(
			Marker::decode("<!--immutable https://github.com/owner/repo/issues/123#issuecomment-456-->"),
			Some(Marker::Comment {
				url: "https://github.com/owner/repo/issues/123#issuecomment-456".to_string(),
				id: 456,
				immutable: true
			})
		);
	}

	#[test]
	fn test_decode_blockers_section() {
		// Helper to check if a marker is a BlockersSection
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
		assert_eq!(
			Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: false
			}
			.encode(),
			"<!-- https://github.com/owner/repo/issues/123 -->"
		);
		assert_eq!(
			Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: true
			}
			.encode(),
			"<!--immutable https://github.com/owner/repo/issues/123 -->"
		);
		assert_eq!(
			Marker::SubIssue {
				url: "https://github.com/owner/repo/issues/124".to_string()
			}
			.encode(),
			"<!--sub https://github.com/owner/repo/issues/124 -->"
		);
		assert_eq!(Marker::BlockersSection(Header::new(1, "Blockers")).encode(), "# Blockers");
		assert_eq!(Marker::BlockersSection(Header::new(2, "Blockers")).encode(), "## Blockers");
		assert_eq!(Marker::NewComment.encode(), "<!-- new comment -->");
		assert_eq!(Marker::OmittedStart.encode(), "<!--omitted {{{always-->");
		assert_eq!(Marker::OmittedEnd.encode(), "<!--,}}}-->");
	}

	#[test]
	fn test_roundtrip() {
		let markers = vec![
			Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: false,
			},
			Marker::IssueUrl {
				url: "https://github.com/owner/repo/issues/123".to_string(),
				immutable: true,
			},
			Marker::SubIssue {
				url: "https://github.com/owner/repo/issues/124".to_string(),
			},
			Marker::Comment {
				url: "https://github.com/owner/repo/issues/123#issuecomment-456".to_string(),
				id: 456,
				immutable: false,
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
