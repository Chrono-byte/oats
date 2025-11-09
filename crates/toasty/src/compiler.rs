//! Compiler interface for oatsc
//!
//! This module handles invoking the oatsc compiler as an external process
//! and managing compilation options.

use crate::cli::CompileOptions;
use crate::diagnostics::{Result, ToastyError};
use std::path::PathBuf;

/// Invoke oatsc compiler as external command
pub fn invoke_oatsc(options: &CompileOptions) -> Result<Option<PathBuf>> {
    // Get oatsc path from environment (set by preflight check)
    let oatsc_path = crate::env::get_oatsc_path();

    // Build command arguments
    let mut args = vec![];

    // Add source file
    args.push(options.src_file.clone());

    // Note: package_root is handled by toasty, not passed to oatsc.
    // oatsc only compiles individual files; toasty handles package orchestration.

    // Handle output path: if both out_dir and out_name are specified,
    // compute the full path and pass via -o flag
    if let (Some(out_dir), Some(out_name)) = (&options.out_dir, &options.out_name) {
        let output_path = std::path::Path::new(out_dir).join(format!("{}.o", out_name));
        args.push(crate::cli_flags::flags::OUTPUT.to_string());
        args.push(output_path.to_string_lossy().to_string());
    } else if let Some(out_name) = &options.out_name {
        // Only out_name specified, use --out-name
        args.push(crate::cli_flags::flags::OUT_NAME.to_string());
        args.push(out_name.clone());
    }

    // Add linker
    if let Some(linker) = &options.linker {
        args.push(crate::cli_flags::flags::LINKER.to_string());
        args.push(linker.clone());
    }

    // Convert external packages to extern-oats format
    // Package metadata files are passed as extern-oats with the package name as the import path
    for (pkg_name, meta_path) in &options.extern_pkg {
        // Use package name as import path (e.g., "./mylib" -> meta_path)
        let import_path = format!("./{}", pkg_name);
        args.push(crate::cli_flags::flags::EXTERN_OATS.to_string());
        args.push(format!("{}={}", import_path, meta_path));
    }

    // Add external oats modules
    for (path, symbols) in &options.extern_oats {
        args.push(crate::cli_flags::flags::EXTERN_OATS.to_string());
        args.push(format!("{}={}", path, symbols));
    }

    // Add build profile
    if let Some(profile) = &options.build_profile {
        args.push(crate::cli_flags::flags::PROFILE.to_string());
        args.push(profile.clone());
    }

    // Add optimization level
    if let Some(opt_level) = &options.opt_level {
        args.push(crate::cli_flags::flags::OPT_LEVEL.to_string());
        args.push(opt_level.clone());
    }

    // Add LTO
    if let Some(lto) = &options.lto {
        args.push(crate::cli_flags::flags::LTO.to_string());
        args.push(lto.clone());
    }

    // Add target triple
    if let Some(triple) = &options.target_triple {
        args.push(crate::cli_flags::flags::TARGET_TRIPLE.to_string());
        args.push(triple.clone());
    }

    // Add target CPU
    if let Some(cpu) = &options.target_cpu {
        args.push(crate::cli_flags::flags::TARGET_CPU.to_string());
        args.push(cpu.clone());
    }

    // Add target features
    if let Some(features) = &options.target_features {
        args.push(crate::cli_flags::flags::TARGET_FEATURES.to_string());
        args.push(features.clone());
    }

    // Add flags
    if options.emit_object_only {
        args.push(crate::cli_flags::flags::EMIT_OBJECT_ONLY.to_string());
    }

    // Execute command
    let output = std::process::Command::new(&oatsc_path)
        .args(&args)
        .output()
        .map_err(|e| {
            ToastyError::other(format!(
                "Failed to execute oatsc command: {} {:?}: {}",
                oatsc_path, args, e
            ))
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).to_string();

        // Check if this is likely a user code error (oatsc already printed diagnostics)
        if stderr.contains("error:") || stderr.contains("Error") || stdout.contains("error:") {
            return Err(ToastyError::CompilationFailed);
        }

        return Err(ToastyError::CompilerInternalError {
            message: format!(
                "oatsc compilation failed:\nSTDOUT:\n{}\n\nSTDERR:\n{}",
                stdout, stderr
            ),
        });
    }

    // Parse output to get object file path if any
    // oatsc prints the output path on stdout (last line is the path)
    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();

    if stdout.is_empty() {
        Ok(None)
    } else {
        // oatsc prints the output file path on stdout (last line)
        let path_str = stdout.lines().last().unwrap_or(&stdout);
        Ok(Some(PathBuf::from(path_str)))
    }
}
