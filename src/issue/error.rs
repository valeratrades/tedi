//! Error types for parsing issue files.
//!
//! Uses miette for rich diagnostics with source code spans.

#![allow(unused_assignments)] // Fields are read by miette's derive macro via attributes

use miette::{Diagnostic, NamedSource, SourceSpan};

/// Error type for issue file parsing.
/// Provides detailed error messages with source locations.
#[derive(Debug, Diagnostic, thiserror::Error)]
pub enum ParseError {
	#[error("file is empty")]
	#[diagnostic(code(tedi::parse::empty_file))]
	EmptyFile,

	#[error("invalid title line")]
	#[diagnostic(code(tedi::parse::invalid_title), help("title must be formatted as: '- [ ] Title <!-- url -->' or '- [x] Title <!-- url -->'"))]
	InvalidTitle {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected checkbox prefix '- [ ] ' or '- [x] '")]
		span: SourceSpan,
		detail: String,
	},

	#[error("missing URL marker in title")]
	#[diagnostic(code(tedi::parse::missing_url_marker), help("title line must contain a URL marker: '<!-- url -->' or '<!--immutable url -->'"))]
	MissingUrlMarker {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected '<!-- url -->' after title")]
		span: SourceSpan,
	},

	#[error("malformed URL marker")]
	#[diagnostic(code(tedi::parse::malformed_url_marker), help("URL marker must be: '<!-- url -->' with closing '-->'"))]
	MalformedUrlMarker {
		#[source_code]
		src: NamedSource<String>,
		#[label("unclosed or malformed comment marker")]
		span: SourceSpan,
	},

	#[error("unexpected indentation")]
	#[diagnostic(code(tedi::parse::bad_indent), help("check that indentation is consistent (use tabs)"))]
	BadIndentation {
		#[source_code]
		src: NamedSource<String>,
		#[label("expected {expected_tabs} tab(s) of indentation")]
		span: SourceSpan,
		expected_tabs: usize,
	},

	#[error("invalid checkbox content: '{content}'")]
	#[diagnostic(
		code(tedi::parse::invalid_checkbox),
		help("valid checkbox values are: ' ' (open), 'x' (closed), '-' (not planned), or a number like '123' (duplicate of issue #123)")
	)]
	InvalidCheckbox {
		#[source_code]
		src: NamedSource<String>,
		#[label("unrecognized checkbox value")]
		span: SourceSpan,
		content: String,
	},

	#[error("duplicate reference to non-existent issue #{issue_number}")]
	#[diagnostic(code(tedi::parse::invalid_duplicate), help("the referenced issue does not exist in this repository"))]
	InvalidDuplicateReference {
		#[source_code]
		src: NamedSource<String>,
		#[label("issue #{issue_number} not found")]
		span: SourceSpan,
		issue_number: u64,
	},
}

/// Holds source content and filename for error reporting.
#[derive(Clone, Debug)]
pub struct ParseContext {
	pub content: String,
	pub filename: String,
}

impl ParseContext {
	pub fn new(content: String, filename: impl Into<String>) -> Self {
		Self { content, filename: filename.into() }
	}

	/// Create a NamedSource for miette diagnostics.
	pub fn named_source(&self) -> NamedSource<String> {
		NamedSource::new(&self.filename, self.content.clone())
	}

	/// Get byte offset for a given line number (1-indexed).
	pub fn line_offset(&self, line_num: usize) -> usize {
		self.content.lines().take(line_num.saturating_sub(1)).map(|l| l.len() + 1).sum()
	}

	/// Get span for an entire line (1-indexed line number).
	pub fn line_span(&self, line_num: usize) -> SourceSpan {
		let offset = self.line_offset(line_num);
		let len = self
			.content
			.lines()
			.nth(line_num.saturating_sub(1))
			.unwrap_or_else(|| panic!("line {line_num} out of bounds for content with {} lines", self.content.lines().count()))
			.len();
		(offset, len).into()
	}
}
