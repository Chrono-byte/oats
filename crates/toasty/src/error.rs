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
#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: String,
}

/// Primary error message describing the issue
#[derive(Debug, Clone)]
pub struct Diagnostic {
    /// Optional error code
    pub code: Option<String>,
    /// Primary error message
    pub message: String,
    /// Optional file path where the error occurred
    pub file: Option<String>,
    /// Optional labels for different error spans
    pub labels: Vec<Label>,
    /// Optional additional context
    pub note: Option<String>,
    /// Optional help message with suggestions
    pub help: Option<String>,
}

impl Diagnostic {
    /// Create a new diagnostic with a message
    pub fn new(message: impl Into<String>) -> Self {
        Diagnostic {
            code: None,
            message: message.into(),
            file: None,
            labels: Vec::new(),
            note: None,
            help: None,
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

    /// Set the span for the diagnostic
    pub fn with_label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.labels.push(Label {
            span,
            message: message.into(),
        });
        self
    }

    /// Set the error code
    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set the help message
    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
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
        if let Some(help) = &self.help {
            write!(f, "\nHelp: {}", help)?;
        }
        for label in &self.labels {
            write!(f, "\n{}: {}", label.span.start, label.message)?;
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
