//! Error types for the toasty crate
//!
//! This module defines the centralized error handling for the toasty CLI tool.
//! All fallible operations return `Result<T>` where the error type is `Diagnostic`.

/// Result type alias for all fallible operations
pub type Result<T> = std::result::Result<T, Diagnostic>;

/// # Design
///
/// The diagnostic system uses this container to decouple error detection
/// from error emission, allowing the compiler to collect multiple errors
/// before deciding how to present them to the user. The optional `span_start`
/// field enables precise error highlighting when source text is available.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Primary error message describing the issue
    pub message: String,
    /// Optional file path where the error occurred
    pub file: Option<String>,
    /// Optional additional context or suggestion
    pub note: Option<String>,
    /// Optional byte offset into source text for span-aware highlighting.
    /// When present and source text is provided to `emit_diagnostic`,
    /// the system displays a caret-highlighted diagnostic instead of
    /// basic file header context.
    pub span_start: Option<usize>,
}

impl Diagnostic {
    /// Create a new diagnostic with a message
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            file: None,
            note: None,
            span_start: None,
        }
    }

    /// Create a diagnostic with file context
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// Add a note to the diagnostic
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.note = Some(note.into());
        self
    }

    /// Set the span start position
    pub fn with_span(mut self, span_start: usize) -> Self {
        self.span_start = Some(span_start);
        self
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(file) = &self.file {
            write!(f, "{}: {}", file, self.message)?;
        } else {
            write!(f, "{}", self.message)?;
        }
        if let Some(note) = &self.note {
            write!(f, "\nNote: {}", note)?;
        }
        Ok(())
    }
}

impl std::error::Error for Diagnostic {}

impl From<std::io::Error> for Diagnostic {
    fn from(err: std::io::Error) -> Self {
        Diagnostic::new(format!("IO error: {}", err))
    }
}

impl From<serde_json::Error> for Diagnostic {
    fn from(err: serde_json::Error) -> Self {
        Diagnostic::new(format!("JSON error: {}", err))
    }
}

impl From<ureq::Error> for Diagnostic {
    fn from(err: ureq::Error) -> Self {
        Diagnostic::new(format!("HTTP error: {}", err))
    }
}

impl From<toml::de::Error> for Diagnostic {
    fn from(err: toml::de::Error) -> Self {
        Diagnostic::new(format!("TOML parsing error: {}", err))
    }
}
