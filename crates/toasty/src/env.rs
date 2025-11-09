//! Environment variable name constants and accessors
//!
//! This module centralizes all environment variable names used throughout
//! the codebase. This improves Locality of Behaviour by making it obvious
//! what environment variables are being read when examining code.
//!
//! All environment variables should be accessed through the functions in
//! this module rather than using `std::env::var()` directly.

/// Environment variable names
pub mod names {
    /// Source file path: `OATS_SRC_FILE`
    /// Used when no source file is provided via CLI arguments
    pub const SRC_FILE: &str = "OATS_SRC_FILE";

    /// Oatsc compiler binary path: `OATS_OATSC_PATH`
    /// Overrides the default "oatsc" command
    pub const OATSC_PATH: &str = "OATS_OATSC_PATH";

    /// Runtime library path: `OATS_RUNTIME_PATH`
    /// Path to the compiled runtime library
    pub const RUNTIME_PATH: &str = "OATS_RUNTIME_PATH";

    /// Standard library path: `OATS_STD_PATH`
    /// Path to the standard library
    pub const STD_PATH: &str = "OATS_STD_PATH";

    /// Output directory: `OATS_OUT_DIR`
    /// Default output directory for compilation artifacts
    pub const OUT_DIR: &str = "OATS_OUT_DIR";

    /// Linker: `OATS_LINKER`
    /// Default linker to use
    pub const LINKER: &str = "OATS_LINKER";

    /// Elide ARC operations: `OATS_ELIDE_ARC`
    /// Set to "1" to enable escape analysis for RC elision
    pub const ELIDE_ARC: &str = "OATS_ELIDE_ARC";
}

/// Get the source file path from environment
/// Returns `None` if not set
pub fn get_src_file() -> Option<String> {
    std::env::var(names::SRC_FILE).ok()
}

/// Get the oatsc compiler path from environment
/// Returns the default "oatsc" if not set
pub fn get_oatsc_path() -> String {
    std::env::var(names::OATSC_PATH)
        .unwrap_or_else(|_| "oatsc".to_string())
}

/// Get the runtime path from environment
/// Returns `None` if not set
pub fn get_runtime_path() -> Option<String> {
    std::env::var(names::RUNTIME_PATH).ok()
}

/// Get the standard library path from environment
/// Returns `None` if not set
pub fn get_std_path() -> Option<String> {
    std::env::var(names::STD_PATH).ok()
}

/// Get the output directory from environment
/// Returns `None` if not set
pub fn get_out_dir() -> Option<String> {
    std::env::var(names::OUT_DIR).ok()
}

/// Get the linker from environment
/// Returns `None` if not set
pub fn get_linker() -> Option<String> {
    std::env::var(names::LINKER).ok()
}

/// Check if ARC elision is enabled
/// Returns `true` if `OATS_ELIDE_ARC=1` is set
pub fn is_elide_arc_enabled() -> bool {
    std::env::var(names::ELIDE_ARC)
        .map(|v| v == "1")
        .unwrap_or(false)
}

