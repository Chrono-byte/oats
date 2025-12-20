//! Diagnostics tests for the Oats compiler.
//!
//! This module tests error message formatting, span-aware reporting,
//! and diagnostic collection.

mod test_utils;

use oatsc::diagnostics::{Diagnostic, Severity, Label, Span};

#[test]
fn test_diagnostic_simple() {
    let diag = Diagnostic::simple(Severity::Error, "Test error");
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "Test error");
}

#[test]
fn test_diagnostic_error() {
    let diag = Diagnostic::error("Error message");
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "Error message");
}

#[test]
fn test_diagnostic_warning() {
    let diag = Diagnostic::warning("Warning message");
    assert_eq!(diag.severity, Severity::Warning);
    assert_eq!(diag.message, "Warning message");
}

#[test]
fn test_diagnostic_note() {
    let diag = Diagnostic::note("Note message");
    assert_eq!(diag.severity, Severity::Note);
    assert_eq!(diag.message, "Note message");
}

#[test]
fn test_diagnostic_with_span() {
    let diag = Diagnostic::simple_with_span(Severity::Error, "Error at position", 10);
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "Error at position");
    assert_eq!(diag.labels.len(), 1);
    assert_eq!(diag.labels[0].span.start, 10);
}

#[test]
fn test_diagnostic_with_code() {
    let diag = Diagnostic::error("Error").with_code("E001");
    assert_eq!(diag.code, Some("E001".to_string()));
}

#[test]
fn test_diagnostic_with_label() {
    let diag = Diagnostic::error("Error").with_label(Label {
        span: Span { start: 0, end: 5 },
        message: "This is the problem".to_string(),
    });
    assert_eq!(diag.labels.len(), 1);
    assert_eq!(diag.labels[0].span.start, 0);
    assert_eq!(diag.labels[0].span.end, 5);
}

#[test]
fn test_diagnostic_with_note() {
    let diag = Diagnostic::error("Error").with_note("Additional context");
    assert_eq!(diag.note, Some("Additional context".to_string()));
}

#[test]
fn test_diagnostic_with_help() {
    let diag = Diagnostic::error("Error").with_help("Try this instead");
    assert_eq!(diag.help, Some("Try this instead".to_string()));
}

#[test]
fn test_diagnostic_with_file() {
    let mut diag = Diagnostic::error("Error");
    diag.file = Some("test.oats".to_string());
    assert_eq!(diag.file, Some("test.oats".to_string()));
}

#[test]
fn test_diagnostic_multiple_labels() {
    let mut diag = Diagnostic::error("Multiple errors");
    diag = diag.with_label(Label {
        span: Span { start: 0, end: 5 },
        message: "First error".to_string(),
    });
    diag = diag.with_label(Label {
        span: Span { start: 10, end: 15 },
        message: "Second error".to_string(),
    });
    assert_eq!(diag.labels.len(), 2);
}

#[test]
fn test_diagnostic_chaining() {
    let diag = Diagnostic::error("Error")
        .with_code("E001")
        .with_note("Note")
        .with_help("Help");
    assert_eq!(diag.code, Some("E001".to_string()));
    assert_eq!(diag.note, Some("Note".to_string()));
    assert_eq!(diag.help, Some("Help".to_string()));
}

#[test]
fn test_diagnostic_simple_boxed() {
    let diag = Diagnostic::simple_boxed(Severity::Error, "Boxed error");
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "Boxed error");
}

#[test]
fn test_diagnostic_simple_with_span_boxed() {
    let diag = Diagnostic::simple_with_span_boxed(Severity::Error, "Span error", 42);
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.message, "Span error");
    assert_eq!(diag.labels.len(), 1);
    assert_eq!(diag.labels[0].span.start, 42);
}

#[test]
fn test_severity_equality() {
    assert_eq!(Severity::Error, Severity::Error);
    assert_eq!(Severity::Warning, Severity::Warning);
    assert_eq!(Severity::Note, Severity::Note);
    assert_ne!(Severity::Error, Severity::Warning);
}

#[test]
fn test_span_creation() {
    let span = Span { start: 10, end: 20 };
    assert_eq!(span.start, 10);
    assert_eq!(span.end, 20);
}

#[test]
fn test_label_creation() {
    let label = Label {
        span: Span { start: 0, end: 5 },
        message: "Error here".to_string(),
    };
    assert_eq!(label.span.start, 0);
    assert_eq!(label.span.end, 5);
    assert_eq!(label.message, "Error here");
}

#[test]
fn test_diagnostic_collection() {
    let diags = [Box::new(Diagnostic::error("Error 1")),
        Box::new(Diagnostic::error("Error 2")),
        Box::new(Diagnostic::warning("Warning 1"))];
    
    let errors: Vec<_> = diags.iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 2);
    
    let warnings: Vec<_> = diags.iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert_eq!(warnings.len(), 1);
}

#[test]
fn test_diagnostic_file_path() {
    let mut diag = Diagnostic::error("Error");
    diag.file = Some("path/to/file.oats".to_string());
    assert_eq!(diag.file.as_ref().unwrap(), "path/to/file.oats");
}

#[test]
fn test_diagnostic_long_message() {
    let long_message = "This is a very long error message that might wrap or be truncated in some display contexts, but should still be stored correctly in the diagnostic structure.".repeat(10);
    let diag = Diagnostic::error(&long_message);
    assert!(diag.message.len() > 100);
}

#[test]
fn test_diagnostic_empty_message() {
    let diag = Diagnostic::error("");
    assert_eq!(diag.message, "");
}

#[test]
fn test_diagnostic_multiple_notes() {
    // Note: The current API only supports one note, but we test what's available
    let diag = Diagnostic::error("Error").with_note("First note");
    assert_eq!(diag.note, Some("First note".to_string()));
}

#[test]
fn test_diagnostic_unicode_in_message() {
    let diag = Diagnostic::error("错误消息");
    assert_eq!(diag.message, "错误消息");
}

#[test]
fn test_diagnostic_span_at_start() {
    let diag = Diagnostic::simple_with_span(Severity::Error, "Error", 0);
    assert_eq!(diag.labels[0].span.start, 0);
}

#[test]
fn test_diagnostic_span_large_offset() {
    let diag = Diagnostic::simple_with_span(Severity::Error, "Error", 1000000);
    assert_eq!(diag.labels[0].span.start, 1000000);
}

#[test]
fn test_diagnostic_label_span_range() {
    let label = Label {
        span: Span { start: 10, end: 20 },
        message: "Range error".to_string(),
    };
    assert_eq!(label.span.end - label.span.start, 10);
}

#[test]
fn test_diagnostic_zero_length_span() {
    let diag = Diagnostic::simple_with_span(Severity::Error, "Error", 5);
    // Zero-length span (start == end)
    assert_eq!(diag.labels[0].span.start, diag.labels[0].span.end);
}

