//! Preflight checks for toasty
//!
//! This module contains functions to verify that required tools and dependencies
//! are available before attempting to build or run Oats programs.

use crate::diagnostics::{Result, ToastyWarn, WarningCollector};

/// Check if a command is available in PATH
#[must_use]
pub fn is_command_available(cmd: &str) -> bool {
    match std::process::Command::new(cmd)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
    {
        Ok(status) => status.success(),
        Err(_) => false,
    }
}

/// Perform preflight checks to ensure required tools are available
pub fn check_environment() -> Result<()> {
    // Check if OATS_OATSC_PATH is already set (e.g., by tests)
    if std::env::var("OATS_OATSC_PATH").is_ok() {
        return Ok(());
    }

    // Check for oatsc compiler (try local target first, then system)
    // NOTE: Disabled home directory store check to prefer locally built version
    let oatsc_available = {
        // Try to find local oatsc in target/release or target/debug
        let mut local_oatsc = None;

        // Check target/release/oatsc (relative to workspace root)
        if let Ok(workspace_root) = std::env::var("CARGO_MANIFEST_DIR") {
            let release_path = std::path::Path::new(&workspace_root)
                .parent()
                .map(|p| p.join("target/release/oatsc"))
                .filter(|p| p.exists());
            if release_path.is_some() {
                local_oatsc = release_path;
            }
        }

        // Also check current directory's target/release/oatsc
        if local_oatsc.is_none() {
            let release_path = std::path::Path::new("target/release/oatsc");
            if release_path.exists() {
                local_oatsc = Some(release_path.to_path_buf());
            }
        }

        // Also check target/debug/oatsc
        if local_oatsc.is_none() {
            let debug_path = std::path::Path::new("target/debug/oatsc");
            if debug_path.exists() {
                local_oatsc = Some(debug_path.to_path_buf());
            }
        }

        if let Some(oatsc_path) = local_oatsc {
            // Use local oatsc from target directory
            // SAFETY: set_var is unsafe in Rust 2024 due to thread-safety concerns
            unsafe {
                std::env::set_var("OATS_OATSC_PATH", oatsc_path);
            }
            true
        } else if is_command_available("oatsc") {
            // Use system oatsc
            true
        } else {
            false
        }
    };

    // Collect warnings instead of printing directly so they use the
    // standardized ToastyWarn formatting used throughout the CLI.
    let mut warnings = WarningCollector::new();

    if !oatsc_available {
        warnings.add(ToastyWarn::MissingCompiler {
            install_hint: Some(
                "use 'toasty compiler install' to install a pre-built version".to_string(),
            ),
        });
    }

    // Check for linker (clang preferred, but accept others)
    let linker_available =
        is_command_available("clang") || is_command_available("gcc") || is_command_available("ld");

    if !linker_available {
        warnings.add(ToastyWarn::MissingOptionalTool {
            tool: "linker".to_string(),
            impact: "building and linking compiled binaries".to_string(),
            install_hint: Some("install clang, gcc, or another linker".to_string()),
        });
    }

    // Print any collected warnings in a consistent format
    if warnings.has_warnings() {
        warnings.print_all();
    }

    Ok(())
}
