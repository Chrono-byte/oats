//! CLI flag name constants
//!
//! This module centralizes all CLI flag names used when building command-line
//! arguments. This improves Locality of Behaviour by making it obvious what
//! flags are being used when reading argument-building code.

/// CLI flag names for oatsc compiler
pub mod flags {
    /// Output file path: `-o <path>`
    pub const OUTPUT: &str = "-o";

    /// Output name: `--out-name <name>`
    pub const OUT_NAME: &str = "--out-name";

    /// Linker: `--linker <linker>`
    pub const LINKER: &str = "--linker";

    /// External Oats module: `--extern-oats <path>=<meta>`
    pub const EXTERN_OATS: &str = "--extern-oats";

    /// Build profile: `--profile <profile>`
    pub const PROFILE: &str = "--profile";

    /// Optimization level: `--opt-level <level>`
    pub const OPT_LEVEL: &str = "--opt-level";

    /// Link-time optimization: `--lto <mode>`
    pub const LTO: &str = "--lto";

    /// Target triple: `--target-triple <triple>`
    pub const TARGET_TRIPLE: &str = "--target-triple";

    /// Target CPU: `--target-cpu <cpu>`
    pub const TARGET_CPU: &str = "--target-cpu";

    /// Target features: `--target-features <features>`
    pub const TARGET_FEATURES: &str = "--target-features";

    /// Emit object only: `--emit-object-only`
    pub const EMIT_OBJECT_ONLY: &str = "--emit-object-only";
}
