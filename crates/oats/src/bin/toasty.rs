//! Toasty: User-friendly command-line interface for the Oats AOT compiler.
//!
//! This binary provides a more intuitive CLI wrapper around the core Oats
//! compilation functionality, offering structured argument parsing and
//! environment variable management for common compilation workflows.
//!
//! # Design Philosophy
//!
//! Toasty aims to provide a developer-friendly interface that abstracts
//! the lower-level compilation details while maintaining full compatibility
//! with the underlying Oats compiler infrastructure. The tool bridges the
//! gap between power-user workflows and everyday development needs.
//!
//! # Usage Examples
//!
//! ```bash
//! # Compile a single file
//! toasty main.oats
//!
//! # Specify output directory
//! toasty main.oats --out-dir ./build
//!
//! # Use environment variables
//! OATS_SRC_FILE=main.oats toasty
//! ```
//!
//! # Implementation
//!
//! Toasty delegates actual compilation work to the shared builder module,
//! focusing on argument parsing, environment variable setup, and providing
//! a consistent CLI experience across different deployment scenarios.

use anyhow::Result;
use clap::Parser;

/// User-friendly command-line interface for the Oats AOT compiler.
///
/// Toasty provides structured argument parsing and environment variable
/// management for Oats compilation workflows, offering a more accessible
/// interface than the lower-level compiler binaries.
#[derive(Parser)]
#[command(
    name = "toasty",
    about = "Compile Oats programs into native executables"
)]
struct Opts {
    /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
    src: Option<String>,

    /// Output directory for compilation artifacts (defaults to current directory or OATS_OUT_DIR)
    #[arg(short, long)]
    out_dir: Option<String>,
}

fn main() -> Result<()> {
    // ARCHITECTURE: Delegate to the core builder infrastructure while providing
    // a user-friendly CLI interface. This approach maintains compatibility with
    // existing workflows while offering improved usability for common scenarios.

    // Parse command-line arguments using structured argument parsing
    let opts = Opts::parse();

    // Configure environment variables based on CLI input
    if let Some(src) = opts.src {
        unsafe { std::env::set_var("OATS_SRC_FILE", src) };
    }
    if let Some(out) = opts.out_dir {
        unsafe { std::env::set_var("OATS_OUT_DIR", out) };
    }

    // Delegate compilation work to the shared builder implementation
    let argv: Vec<String> = std::env::args().collect();
    // Preserve original argv structure for compatibility with builder expectations.
    // The builder can still access the first argument if needed, while environment
    // variables provide the primary configuration mechanism.
    oats::builder::run_from_args(&argv)
}
