//! Diagnostics for the toasty crate
//!
//! This module defines the centralized error and warning handling for the toasty CLI tool.
//! All fallible operations return `Result<T>` where the error type is `ToastyError`.
//! Warnings are collected using `WarningCollector` and emitted as `ToastyWarn`.

use std::fmt;
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

/// A warning that can be emitted by toasty
#[derive(Debug, Clone)]
pub enum ToastyWarn {
    /// A feature is deprecated and will be removed in a future version
    Deprecated {
        /// The deprecated feature name
        feature: String,
        /// Version when it will be removed
        removal_version: Option<String>,
        /// Suggested alternative
        alternative: Option<String>,
    },

    /// An optional tool or dependency is missing
    MissingOptionalTool {
        /// Name of the missing tool
        tool: String,
        /// What functionality is affected
        impact: String,
        /// How to install the tool
        install_hint: Option<String>,
    },

    /// A configuration option has an unusual or potentially problematic value
    ConfigWarning {
        /// The configuration key
        key: String,
        /// The current value
        value: String,
        /// Description of the issue
        issue: String,
        /// Suggested fix
        suggestion: Option<String>,
    },

    /// Performance-related warning
    Performance {
        /// Description of the performance issue
        issue: String,
        /// Suggested improvement
        suggestion: Option<String>,
    },

    /// Build system warning
    BuildWarning {
        /// Description of the build issue
        issue: String,
        /// File or context where it occurred
        context: Option<String>,
    },

    /// The Oats compiler (oatsc) is not available
    MissingCompiler {
        /// Suggested installation method
        install_hint: Option<String>,
    },
}

impl fmt::Display for ToastyWarn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ToastyWarn::Deprecated { feature, removal_version, alternative } => {
                write!(f, "Warning: Feature '{}' is deprecated", feature)?;
                if let Some(version) = removal_version {
                    write!(f, " and will be removed in version {}", version)?;
                }
                if let Some(alt) = alternative {
                    write!(f, ". Consider using '{}' instead", alt)?;
                }
                Ok(())
            }
            ToastyWarn::MissingOptionalTool { tool, impact, install_hint } => {
                write!(f, "Warning: Optional tool '{}' is not available", tool)?;
                write!(f, ". This affects: {}", impact)?;
                if let Some(hint) = install_hint {
                    write!(f, ". To install: {}", hint)?;
                }
                Ok(())
            }
            ToastyWarn::ConfigWarning { key, value, issue, suggestion } => {
                write!(f, "Warning: Configuration '{}' has value '{}' - {}", key, value, issue)?;
                if let Some(sugg) = suggestion {
                    write!(f, ". Suggestion: {}", sugg)?;
                }
                Ok(())
            }
            ToastyWarn::Performance { issue, suggestion } => {
                write!(f, "Performance warning: {}", issue)?;
                if let Some(sugg) = suggestion {
                    write!(f, ". Suggestion: {}", sugg)?;
                }
                Ok(())
            }
            ToastyWarn::BuildWarning { issue, context } => {
                write!(f, "Build warning: {}", issue)?;
                if let Some(ctx) = context {
                    write!(f, " (in {})", ctx)?;
                }
                Ok(())
            }
            ToastyWarn::MissingCompiler { install_hint } => {
                write!(f, "Warning: Oats compiler (oatsc) not found")?;
                if let Some(hint) = install_hint {
                    write!(f, ". To install: {}", hint)?;
                } else {
                    write!(f, ". Please install oatsc or use 'toasty compiler install' to install a pre-built version")?;
                }
                write!(f, ". Use 'toasty compiler list' to see available versions")?;
                Ok(())
            }
        }
    }
}

impl std::error::Error for ToastyWarn {}

/// A collection of warnings that can be accumulated during operations
#[derive(Debug, Clone, Default)]
pub struct WarningCollector {
    warnings: Vec<ToastyWarn>,
}

impl WarningCollector {
    /// Create a new empty warning collector
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a warning to the collection
    pub fn add(&mut self, warning: ToastyWarn) {
        self.warnings.push(warning);
    }

    /// Check if any warnings have been collected
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    /// Get the number of warnings
    pub fn count(&self) -> usize {
        self.warnings.len()
    }

    /// Iterate over all warnings
    pub fn iter(&self) -> impl Iterator<Item = &ToastyWarn> {
        self.warnings.iter()
    }

    /// Clear all warnings
    pub fn clear(&mut self) {
        self.warnings.clear();
    }

    /// Print all warnings to stderr with appropriate formatting
    pub fn print_all(&self) {
        for warning in &self.warnings {
            eprintln!("⚠️  {}", warning);
        }
    }

    /// Print a summary of warnings if any exist
    pub fn print_summary(&self) {
        if self.has_warnings() {
            eprintln!("⚠️  {} warning(s) emitted", self.count());
        }
    }
}