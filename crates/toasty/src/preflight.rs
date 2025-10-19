//! Preflight checks for toasty
//!
//! This module contains functions to verify that required tools and dependencies
//! are available before attempting to build or run Oats programs.

use crate::diagnostics::{Result, ToastyWarn, WarningCollector};
use crate::fetch;

/// Check if a command is available in PATH
pub fn is_command_available(cmd: &str) -> bool {
    std::process::Command::new(cmd)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Perform preflight checks to ensure required tools are available
pub fn check_environment() -> Result<()> {
    // Check if OATS_OATSC_PATH is already set (e.g., by tests)
    if std::env::var("OATS_OATSC_PATH").is_ok() {
        return Ok(());
    }

    // Check for oatsc compiler (try selected version first, then local target, then system)
    let oatsc_available = if let Some(oatsc_path) = fetch::get_selected_compiler_path() {
        // Use selected oatsc version
        unsafe {
            std::env::set_var("OATS_OATSC_PATH", oatsc_path);
        }
        true
    } else if let Ok(current_exe) = std::env::current_exe() {
        let local_oatsc = current_exe
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("oatsc"))
            .filter(|p| p.exists());

        if let Some(oatsc_path) = local_oatsc {
            // Use local oatsc from target directory
            unsafe {
                std::env::set_var("OATS_OATSC_PATH", oatsc_path);
            }
            true
        } else {
            false
        }
    } else if is_command_available("oatsc") {
        // Use system oatsc
        true
    } else {
        false
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
