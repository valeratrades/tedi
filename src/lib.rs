#![feature(vec_peek_mut)]

pub mod issue;
pub mod local;
pub mod sink;

pub mod current_user {
	use std::cell::RefCell;

	thread_local! {
		static CURRENT_USER: RefCell<Option<String>> = const { RefCell::new(None) };
	}

	/// Set the current authenticated user for ownership checks.
	/// Must be called before serializing issues.
	pub fn set(user: String) {
		CURRENT_USER.with(|u| *u.borrow_mut() = Some(user));
	}

	/// Get the current authenticated user.
	/// Returns None if not set.
	pub fn get() -> Option<String> {
		CURRENT_USER.with(|u| u.borrow().clone())
	}

	/// Check if the given user is the current authenticated user.
	pub fn is(user: &str) -> bool {
		CURRENT_USER.with(|u| u.borrow().as_deref() == Some(user))
	}
}

// Re-export all public types from issue module at crate root for convenience
pub use issue::{
	BlockerItem, BlockerSequence, CloseState, Comment, CommentIdentity, DisplayFormat, Events, HeaderLevel, Issue, IssueContents, IssueIdentity, IssueIndex, IssueLink, IssueSelector,
	IssueTimestamps, LazyIssue, Line, LinkedIssueMeta, MAX_INDEX_DEPTH, MAX_LINEAGE_DEPTH, MAX_TITLE_LENGTH, Marker, OwnedCodeBlockKind, OwnedEvent, OwnedTag, OwnedTagEnd, ParseError,
	RepoInfo, classify_line, join_with_blockers, split_blockers,
};

/// A header with a level and content.
///
/// Markdown format: `# Content`, `## Content`, etc.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Header {
	pub level: usize,
	pub content: String,
}

impl Header {
	/// Create a new header with the given level and content.
	/// Level must be >= 1.
	pub fn new(level: usize, content: impl Into<String>) -> Self {
		debug_assert!(level >= 1, "Header level must be >= 1");
		Self {
			level: level.max(1),
			content: content.into(),
		}
	}

	/// Decode a header from a line string.
	/// Returns None if the line is not a valid header.
	pub fn decode(s: &str) -> Option<Self> {
		let trimmed = s.trim();

		// Markdown: # Content, ## Content, etc.
		if !trimmed.starts_with('#') {
			return None;
		}
		let mut level = 0;
		for ch in trimmed.chars() {
			if ch == '#' {
				level += 1;
			} else {
				break;
			}
		}
		// Valid header must have space after the # characters
		if level > 0 && trimmed.len() > level {
			let rest = &trimmed[level..];
			if let Some(stripped) = rest.strip_prefix(' ') {
				return Some(Self {
					level,
					content: stripped.to_string(),
				});
			}
		}
		None
	}

	/// Encode the header to a string.
	pub fn encode(&self) -> String {
		format!("{} {}", "#".repeat(self.level), self.content)
	}

	/// Check if this header's content matches the given text (case-insensitive).
	pub fn content_eq_ignore_case(&self, text: &str) -> bool {
		self.content.eq_ignore_ascii_case(text)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_header_new() {
		let header = Header::new(2, "Test Content");
		assert_eq!(header.level, 2);
		assert_eq!(header.content, "Test Content");
	}

	#[test]
	fn test_header_decode() {
		// Basic markdown headers
		assert_eq!(
			Header::decode("# Heading 1"),
			Some(Header {
				level: 1,
				content: "Heading 1".to_string()
			})
		);
		assert_eq!(
			Header::decode("## Heading 2"),
			Some(Header {
				level: 2,
				content: "Heading 2".to_string()
			})
		);
		assert_eq!(
			Header::decode("### Heading 3"),
			Some(Header {
				level: 3,
				content: "Heading 3".to_string()
			})
		);

		// With leading/trailing whitespace
		assert_eq!(
			Header::decode("  # Trimmed  "),
			Some(Header {
				level: 1,
				content: "Trimmed".to_string()
			})
		);

		// Invalid: no space after #
		assert_eq!(Header::decode("#NoSpace"), None);

		// Invalid: not a header
		assert_eq!(Header::decode("Just text"), None);
		assert_eq!(Header::decode("- List item"), None);
	}

	#[test]
	fn test_header_encode() {
		assert_eq!(Header::new(1, "Test").encode(), "# Test");
		assert_eq!(Header::new(2, "Test").encode(), "## Test");
		assert_eq!(Header::new(3, "Test").encode(), "### Test");
	}

	#[test]
	fn test_header_roundtrip() {
		for level in 1..=6 {
			let original = Header::new(level, "Content");
			let encoded = original.encode();
			let decoded = Header::decode(&encoded).unwrap();
			assert_eq!(original, decoded);
		}
	}

	#[test]
	fn test_header_content_eq_ignore_case() {
		let header = Header::new(1, "Blockers");
		assert!(header.content_eq_ignore_case("blockers"));
		assert!(header.content_eq_ignore_case("BLOCKERS"));
		assert!(header.content_eq_ignore_case("Blockers"));
		assert!(!header.content_eq_ignore_case("Blocker"));
	}
}
