//! Linker interface for final executable linking
//!
//! This module handles linking compiled object files with the runtime and std libraries
//! to produce the final executable.

use crate::diagnostics::{Result, ToastyError};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Link compiled modules into a final executable
pub fn link_executable(
    compiled_modules: &HashMap<String, PathBuf>,
    exe_path: &Path,
    linker: Option<String>,
    verbose: bool,
) -> Result<()> {
    // Use specified linker or default to clang
    let linker_cmd = linker.unwrap_or_else(|| "clang".to_string());

    // Ensure runtime is available
    let runtime_lib = if let Ok(runtime_path) = std::env::var("OATS_RUNTIME_PATH") {
        // Use explicitly specified runtime path
        let runtime_lib = PathBuf::from(runtime_path);
        if !runtime_lib.exists() {
            eprintln!(
                "Warning: Runtime library not found at OATS_RUNTIME_PATH: {}",
                runtime_lib.display()
            );
        }
        runtime_lib
    } else {
        // Look for runtime in current directory or standard locations
        let runtime_lib = PathBuf::from("libruntime.a");

        if !runtime_lib.exists() {
            eprintln!(
                "Warning: Runtime library not found. Please either place libruntime.a in the current directory or set OATS_RUNTIME_PATH to the path of libruntime.a"
            );
        }
        runtime_lib
    };

    // Ensure std and primitives libraries are available
    let std_lib = if let Ok(std_path) = std::env::var("OATS_STD_PATH") {
        // Use explicitly specified std path
        let std_lib = PathBuf::from(std_path);
        if !std_lib.exists() {
            eprintln!(
                "Warning: Std library not found at OATS_STD_PATH: {}",
                std_lib.display()
            );
        }
        std_lib
    } else {
        // Look for std in current directory or standard locations
        let std_lib = PathBuf::from("liboats_std.a");

        if !std_lib.exists() {
            eprintln!(
                "Warning: Std library not found. Please either place liboats_std.a in the current directory or set OATS_STD_PATH to the path of liboats_std.a"
            );
        }
        std_lib
    };

    // Primitives library: optional but recommended during migration
    let primitives_lib = if let Ok(p_path) = std::env::var("OATS_PRIMITIVES_PATH") {
        let p = PathBuf::from(p_path);
        if !p.exists() {
            eprintln!(
                "Warning: Primitives library not found at OATS_PRIMITIVES_PATH: {}",
                p.display()
            );
        }
        p
    } else {
        let p = PathBuf::from("liboats_primitives.a");
        if !p.exists() {
            eprintln!(
                "Note: primitives library not found (liboats_primitives.a). If you're migrating std to oats, build and place it in the current directory or set OATS_PRIMITIVES_PATH."
            );
        }
        p
    };

    let mut link_cmd = std::process::Command::new(&linker_cmd);
    link_cmd.arg("-o").arg(exe_path);

    // Add all object files
    for obj_file in compiled_modules.values() {
        link_cmd.arg(obj_file);
    }

    // Add the runtime library
    link_cmd.arg(&runtime_lib);

    // Add the std library
    link_cmd.arg(&std_lib);
    // Add primitives library if present
    link_cmd.arg(&primitives_lib);

    if verbose {
        eprintln!("Link command: {:?}", link_cmd);
    }

    let link_status = link_cmd.status()?;
    if !link_status.success() {
        return Err(ToastyError::other("Linking failed"));
    }

    Ok(())
}
