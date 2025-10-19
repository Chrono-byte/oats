//! Error types for the toasty crate
//!
//! This module defines the centralized error handling for the toasty CLI tool.
//! All fallible operations return `Result<T>` where the error type is `ToastyError`.

use std::path::PathBuf;

/// Result type alias for all fallible operations
pub type Result<T> = std::result::Result<T, ToastyError>;

/// Semantic error type for all build system and configuration errors.
///
/// This enum distinguishes between different failure modes in the build process:
/// - **User code errors**: Compilation failures in user's `.oats` files (handled by oatsc)
/// - **Build system errors**: Environment, configuration, or I/O issues (our responsibility)
#[derive(Debug, thiserror::Error)]
pub enum ToastyError {
    /// Manifest file (`Oats.toml`) was not found
    #[error("Manifest file not found at {path:?}")]
    ManifestNotFound { path: PathBuf },

    /// Failed to parse manifest file
    #[error("Failed to parse manifest '{path:?}': {source}")]
    ManifestParse {
        path: PathBuf,
        #[source]
        source: toml::de::Error,
    },

    /// I/O error for a specific file
    #[error("I/O error for file {path:?}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// Failed to fetch or download a component
    #[error("Failed to fetch component '{name}': {source}")]
    Fetch {
        name: String,
        #[source]
        source: Box<ureq::Error>,
    },

    /// Hash mismatch when verifying downloaded component integrity
    #[error(
        "Component validation failed for '{name}': Hash mismatch (expected {expected}, got {actual})"
    )]
    HashMismatch {
        name: String,
        expected: String,
        actual: String,
    },

    /// Dependency cycle detected in package graph
    #[error("Dependency cycle detected: {cycle_info}")]
    DependencyCycle { cycle_info: String },

    /// Linker command failed with error output
    #[error("Linker failed with exit code {code:?}")]
    LinkerFailed { code: Option<i32>, stderr: String },

    /// oatsc compiler encountered a fatal, non-code-related error (e.g., internal error)
    #[error("Compiler error: {message}")]
    CompilerInternalError { message: String },

    /// User's code failed to compile (oatsc already printed diagnostics)
    ///
    /// This is a signal to the CLI that oatsc has already emitted human-readable
    /// error messages, so we should exit quietly without additional output.
    #[error("Build failed due to previous errors")]
    CompilationFailed,

    /// JSON serialization/deserialization error
    #[error("JSON error: {source}")]
    Json {
        #[source]
        source: serde_json::Error,
    },

    /// TOML serialization/deserialization error
    #[error("TOML error: {source}")]
    Toml {
        #[source]
        source: toml::de::Error,
    },

    /// Generic error for other failures
    #[error("{message}")]
    Other { message: String },
}

impl ToastyError {
    /// Check if this error represents a user code compilation failure
    ///
    /// Returns `true` if the error is `CompilationFailed`, which means oatsc
    /// has already printed the actual error diagnostics to the user.
    pub fn is_user_code_error(&self) -> bool {
        matches!(self, ToastyError::CompilationFailed)
    }

    /// Create an I/O error with context
    pub fn io<P: Into<PathBuf>>(path: P, source: std::io::Error) -> Self {
        ToastyError::Io {
            path: path.into(),
            source,
        }
    }

    /// Create a manifest parsing error with context
    pub fn manifest_parse<P: Into<PathBuf>>(path: P, source: toml::de::Error) -> Self {
        ToastyError::ManifestParse {
            path: path.into(),
            source,
        }
    }

    /// Create a generic error
    pub fn other<S: Into<String>>(message: S) -> Self {
        ToastyError::Other {
            message: message.into(),
        }
    }
}

impl From<serde_json::Error> for ToastyError {
    fn from(err: serde_json::Error) -> Self {
        ToastyError::Json { source: err }
    }
}

impl From<toml::de::Error> for ToastyError {
    fn from(err: toml::de::Error) -> Self {
        ToastyError::Toml { source: err }
    }
}

impl From<ureq::Error> for ToastyError {
    fn from(err: ureq::Error) -> Self {
        ToastyError::Fetch {
            name: "unknown".to_string(),
            source: Box::new(err),
        }
    }
}

impl From<std::io::Error> for ToastyError {
    fn from(err: std::io::Error) -> Self {
        ToastyError::Io {
            path: PathBuf::from("<unknown>"),
            source: err,
        }
    }
}
