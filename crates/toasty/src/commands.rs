//! Command handlers for toasty CLI
//!
//! This module contains the implementation of all CLI commands.

use atty::Stream as AtStream;
use colored::Colorize;
use std::path::PathBuf;

use crate::build;
use crate::cli::{Cli, CompileOptions, CompilerCommands, PackageCommands, RuntimeCommands};
use crate::compiler;
use crate::diagnostics::{Result, ToastyError};
use crate::fetch;
use crate::linker;
use crate::project;

/// Handle the build command
pub fn handle_build(
    cli: &Cli,
    src: Option<String>,
    out_dir: Option<String>,
    out_name: Option<String>,
    linker: Option<String>,
    emit_object_only: bool,
    no_link_runtime: bool,
    opt_level: Option<String>,
    lto: Option<String>,
    target_triple: Option<String>,
    target_cpu: Option<String>,
    target_features: Option<String>,
    release: bool,
    quiet: bool,
    color: Option<String>,
) -> Result<()> {
    // determine effective verbosity: CLI flag overrides env var
    let verbose = if cli.verbose {
        true
    } else {
        std::env::var("TOASTY_VERBOSE").is_ok()
    };

    // color handling: support always/never/auto (auto=enable when stderr is a TTY)
    let enable_color = match color.as_deref() {
        Some("always") => true,
        Some("never") => false,
        Some("auto") | None => atty::is(AtStream::Stderr),
        _ => atty::is(AtStream::Stderr),
    };
    colored::control::set_override(enable_color);

    if release {
        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Building in release mode...".green());
        }
    } else if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Building in debug mode...".yellow());
    }

    // Determine source file
    let src_file = determine_source_file(src.clone(), quiet, verbose)?;

    // Check if this is a package-based build
    if let Some(manifest_path_str) = src_file.strip_prefix("package:") {
        let manifest_path = std::path::PathBuf::from(manifest_path_str);
        let build_config = crate::build::BuildConfig {
            verbose,
            quiet,
            release,
            out_dir,
            out_name,
            linker,
            opt_level,
            lto,
            target_triple,
            target_cpu,
            target_features,
            no_link_runtime,
        };

        let exe_path = build::build_package_project(&manifest_path, build_config)?;

        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!(
                "{}",
                format!("Build complete: {}", exe_path.display()).green()
            );
        }

        return Ok(());
    }

    // Read out_dir from environment variable if not provided
    let out_dir = out_dir.or_else(|| std::env::var("OATS_OUT_DIR").ok());

    // Special case: if emit_object_only is requested and this is a single file with no dependencies,
    // compile it directly without the multi-module system
    if emit_object_only {
        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!(
                "{}",
                "Compiling single file with emit-object-only...".blue()
            );
        }

        let mut options = CompileOptions::new(src_file.clone());
        options.out_dir = out_dir.clone();
        options.out_name = out_name.clone();
        options.linker = linker.clone();
        options.emit_object_only = true;
        options.link_runtime = !no_link_runtime;
        options.opt_level = opt_level.clone();
        options.lto = lto.clone();
        options.target_triple = target_triple.clone();
        options.target_cpu = target_cpu.clone();
        options.target_features = target_features.clone();
        options.build_profile = if release {
            Some("release".to_string())
        } else {
            None
        };

        let build_out = compiler::invoke_oatsc(&options)?;
        if let Some(out_path) = build_out
            && !quiet
            && (verbose || cfg!(debug_assertions))
        {
            eprintln!(
                "{}",
                format!("Build finished: {}", out_path.display()).green()
            );
        }
        return Ok(());
    }

    // Build dependency graph starting from entry point
    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Building dependency graph...".blue());
    }
    let (dep_graph, node_indices, _entry_node) =
        project::build_dependency_graph(&src_file, verbose)?;

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Found {} module(s) to compile", node_indices.len()).green()
        );
    }

    // Perform topological sort to get compilation order
    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Computing compilation order...".blue());
    }
    let compilation_order = project::topological_sort(&dep_graph, &node_indices)?;

    if verbose {
        eprintln!("Compilation order:");
        for (i, path) in compilation_order.iter().enumerate() {
            eprintln!("  {}. {}", i + 1, path);
        }
    }

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Orchestrating compilation...".blue());
    }

    // Phase 1: Compile each module separately with extern_oats for dependencies
    // In Phase 2, this will use separate oatsc processes for each package
    let mut compiled_modules = std::collections::HashMap::new();

    for (i, module_path) in compilation_order.iter().enumerate() {
        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!(
                "{}",
                format!(
                    "Compiling module {}/{}: {}",
                    i + 1,
                    compilation_order.len(),
                    module_path
                )
                .blue()
            );
        }

        let mut options = CompileOptions::new(module_path.clone());

        // For Phase 1: if this is not the first module, add extern_oats for previous modules
        // TODO: In Phase 2, this will be based on actual package metadata
        if i > 0 {
            // Hardcode exported symbols for known modules (Phase 1 only)
            for prev_module in &compilation_order[0..i] {
                if prev_module.ends_with("math.oats") {
                    options
                        .extern_oats
                        .insert("./math".to_string(), "add,multiply".to_string());
                }
                // Add more hardcoded mappings as needed
            }
        }

        options.out_dir = out_dir.clone();
        options.out_name = Some(format!("{}_module", i));
        options.linker = linker.clone();
        options.emit_object_only = true; // Emit objects for linking later
        options.link_runtime = !no_link_runtime;
        options.opt_level = opt_level.clone();
        options.lto = lto.clone();
        options.target_triple = target_triple.clone();
        options.target_cpu = target_cpu.clone();
        options.target_features = target_features.clone();
        options.build_profile = if release {
            Some("release".to_string())
        } else {
            None
        };

        // Compile this module
        let build_out = compiler::invoke_oatsc(&options)?;
        if let Some(out_path) = build_out {
            compiled_modules.insert(module_path.clone(), out_path);
        }
    }

    // Phase 1: Link all compiled modules together
    // In Phase 2, this will be done by toasty after all packages are compiled
    if emit_object_only {
        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Skipping linking due to emit-object-only...".blue());
        }
        // For multi-module builds with emit_object_only, just report the object files
        if !quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Compiled object files:".green());
            for (module_path, obj_file) in &compiled_modules {
                eprintln!("  {} -> {}", module_path, obj_file.display());
            }
        }
        return Ok(());
    }

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Linking final executable...".blue());
    }

    // Link all object files together
    let exe_name = out_name.unwrap_or_else(|| {
        std::path::Path::new(&src_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string()
    });
    let exe_path = out_dir
        .as_ref()
        .map(|d| std::path::Path::new(d).join(&exe_name))
        .unwrap_or_else(|| std::path::Path::new(&exe_name).to_path_buf());

    // Link the executable
    linker::link_executable(&compiled_modules, &exe_path, linker, verbose)?;

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Build finished: {}", exe_path.display()).green()
        );
    }
    Ok(())
}

/// Handle the run command
pub fn handle_run(
    cli: &Cli,
    src: Option<String>,
    args: Vec<String>,
    out_name: Option<String>,
    quiet: bool,
    color: Option<String>,
) -> Result<()> {
    let verbose = if cli.verbose {
        true
    } else {
        std::env::var("TOASTY_VERBOSE").is_ok()
    };

    // honor color/quiet: same auto behavior as Build
    let enable_color = match color.as_deref() {
        Some("always") => true,
        Some("never") => false,
        Some("auto") | None => atty::is(AtStream::Stderr),
        _ => atty::is(AtStream::Stderr),
    };
    colored::control::set_override(enable_color);

    // Determine source file
    let src_file = if let Some(s) = src {
        s
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        return Err(ToastyError::other(
            "No source file provided. Pass path as argument or set OATS_SRC_FILE env var.",
        ));
    };

    // Perform module resolution to discover all source files
    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Resolving module dependencies...".blue());
    }
    let modules = project::load_modules(&src_file)?;

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Found {} module(s) to compile", modules.len()).green()
        );
    }

    // Build first
    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Building before run...".blue());
    }

    // Build compile options with all discovered modules
    let mut all_src_files: Vec<String> = modules.keys().cloned().collect();
    // Ensure the entry point is first
    if let Some(pos) = all_src_files.iter().position(|p| p == &src_file) {
        all_src_files.swap(0, pos);
    }
    let entry_point = all_src_files[0].clone();
    let mut options = CompileOptions::with_modules(entry_point);
    options.out_name = out_name.clone();

    let _build_res = compiler::invoke_oatsc(&options)?;

    // Determine output exe path
    let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());
    let src_filename = std::path::Path::new(&src_file)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let exe_name = if let Some(name) = out_name {
        name
    } else {
        src_filename.to_string()
    };
    // Prefer the path returned by the compiler if available
    let _out_exe = if let Some(p) = _build_res {
        p
    } else {
        PathBuf::from(format!("{}/{}", out_dir, exe_name))
    };

    // Allow overriding output name with OATS_OUT_NAME
    let out_exe = if let Ok(name) = std::env::var("OATS_OUT_NAME") {
        format!("{}/{}", out_dir, name)
    } else {
        format!("{}/{}", out_dir, src_filename)
    };

    // Execute the binary with provided args
    let mut cmd = std::process::Command::new(&out_exe);
    if !args.is_empty() {
        cmd.args(&args);
    }

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", format!("Running `{}`", out_exe).green());
    }

    let status = cmd.status()?;
    if !status.success() {
        return Err(ToastyError::other(
            "Executed program returned non-zero exit code",
        ));
    }
    Ok(())
}

/// Handle the new command
pub fn handle_new(name: String, lib: bool, quiet: bool) -> Result<()> {
    // Create a new Oats project
    let project_path = std::path::Path::new(&name);

    if project_path.exists() {
        return Err(ToastyError::other(format!(
            "Directory '{}' already exists",
            name
        )));
    }

    // Create project directory
    std::fs::create_dir_all(project_path)?;

    // Create Oats.toml
    let manifest_content = format!(
        r#"[package]
name = "{}"
version = "0.1.0"
authors = ["Your Name <your.email@example.com>"]
description = "An Oats project"
license = "MIT"

[dependencies]
"#,
        name
    );

    std::fs::write(project_path.join("Oats.toml"), manifest_content)?;

    // Create src directory
    let src_dir = project_path.join("src");
    std::fs::create_dir_all(&src_dir)?;

    // Create main.oats or lib.oats
    let main_content = if lib {
        format!(
            r#"// {}/src/lib.oats
// Library entry point

export function hello(): string {{
    return "Hello from {}!";
}}
"#,
            name, name
        )
    } else {
        format!(
            r#"// {}/src/main.oats
// Executable entry point

export function main(): number {{
    println("Hello, {}!");
    return 0;
}}
"#,
            name, name
        )
    };

    let main_file = if lib { "lib.oats" } else { "main.oats" };
    std::fs::write(src_dir.join(main_file), main_content)?;

    if !quiet {
        eprintln!("{}", format!("Created new Oats project '{}'", name).green());
        eprintln!("  {}", project_path.display());
        eprintln!("Run 'cd {} && toasty build' to build", name);
        if !lib {
            eprintln!("Run 'cd {} && toasty run' to run", name);
        }
    }

    Ok(())
}

/// Handle compiler commands
pub fn handle_compiler(action: CompilerCommands) -> Result<()> {
    match action {
        CompilerCommands::List => {
            // List available compiler versions
            let versions = fetch::list_available_compilers()?;
            if versions.is_empty() {
                println!("No compiler versions found.");
            } else {
                println!("Available compiler versions:");
                for version in versions {
                    println!("  - {}", version);
                }
            }
            Ok(())
        }
        CompilerCommands::Install { version } => {
            // Install a specific compiler version
            fetch::install_compiler_version(&version)?;
            println!("Compiler version {} installed successfully.", version);
            Ok(())
        }
        CompilerCommands::Use { version } => {
            // Switch to a specific compiler version
            fetch::use_compiler_version(&version)?;
            println!("Switched to compiler version {}.", version);
            Ok(())
        }
        CompilerCommands::Current => {
            // Show current compiler version
            let current_version = fetch::current_compiler_version()?;
            println!("Current compiler version: {}", current_version);
            Ok(())
        }
        CompilerCommands::Uninstall { version } => {
            // Uninstall a specific compiler version
            fetch::uninstall_compiler_version(&version)?;
            println!("Compiler version {} uninstalled successfully.", version);
            Ok(())
        }
    }
}

/// Handle runtime commands
pub fn handle_runtime(action: RuntimeCommands) -> Result<()> {
    match action {
        RuntimeCommands::List => {
            // List available runtime versions
            let versions = fetch::list_available_runtimes()?;
            if versions.is_empty() {
                println!("No runtime versions found.");
            } else {
                println!("Available runtime versions:");
                for version in versions {
                    println!("  - {}", version);
                }
            }
            Ok(())
        }
        RuntimeCommands::Install { version } => {
            // Install a specific runtime version
            fetch::install_runtime_version(&version)?;
            println!("Runtime version {} installed successfully.", version);
            Ok(())
        }
        RuntimeCommands::Use { version } => {
            // Switch to a specific runtime version
            fetch::use_runtime_version(&version)?;
            println!("Switched to runtime version {}.", version);
            Ok(())
        }
        RuntimeCommands::Current => {
            // Show current runtime version
            let current_version = fetch::current_runtime_version()?;
            println!("Current runtime version: {}", current_version);
            Ok(())
        }
        RuntimeCommands::Uninstall { version } => {
            // Uninstall a specific runtime version
            fetch::uninstall_runtime_version(&version)?;
            println!("Runtime version {} uninstalled successfully.", version);
            Ok(())
        }
    }
}

/// Handle package commands
pub fn handle_package(action: PackageCommands) -> Result<()> {
    match action {
        PackageCommands::List { quiet } => {
            // List available packages
            let packages = fetch::list_available_packages()?;
            if packages.is_empty() {
                if !quiet {
                    println!("No packages found.");
                }
            } else {
                if !quiet {
                    println!("Available packages:");
                }
                for package in packages {
                    println!("  - {}", package);
                }
            }
            Ok(())
        }
        PackageCommands::Install {
            name,
            version,
            quiet,
        } => {
            // Install a specific package
            let version_to_install = version.unwrap_or_else(|| "latest".to_string());
            fetch::install_package_version(&name, &version_to_install)?;
            if !quiet {
                println!(
                    "Package {}@{} installed successfully.",
                    name, version_to_install
                );
            }
            Ok(())
        }
        PackageCommands::Uninstall { name, quiet } => {
            // Uninstall a specific package
            fetch::uninstall_package_version(&name)?;
            if !quiet {
                println!("Package {} uninstalled successfully.", name);
            }
            Ok(())
        }
        PackageCommands::Update { name, quiet } => {
            // Update a specific package
            fetch::update_package_version(&name)?;
            if !quiet {
                println!("Package {} updated successfully.", name);
            }
            Ok(())
        }
        PackageCommands::Info { name } => {
            // Show information about a specific package
            let info = fetch::get_package_info(&name)?;
            println!("Package: {}", name);
            println!("Version: {}", info.version);
            println!(
                "Description: {}",
                info.description
                    .unwrap_or_else(|| "No description".to_string())
            );
            if let Some(deps) = info.dependencies {
                println!("Dependencies:");
                for dep in deps {
                    println!("  - {}", dep);
                }
            }
            Ok(())
        }
    }
}

/// Determine the effective source file for building
fn determine_source_file(src: Option<String>, quiet: bool, verbose: bool) -> Result<String> {
    // If explicit source file provided, use it
    if let Some(s) = src {
        return Ok(s);
    }

    // Try to discover manifest for package-based build
    match project::Manifest::discover() {
        Ok(Some((_manifest, manifest_path))) => {
            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!(
                    "{}",
                    format!("Found project manifest: {}", manifest_path.display()).blue()
                );
            }
            // Use package-based build - this is the default and expected behavior
            // Return a special marker to indicate package mode should be used
            Ok(format!("package:{}", manifest_path.display()))
        }
        Ok(None) => {
            let cwd = std::env::current_dir().unwrap_or_else(|_| std::path::PathBuf::from("."));
            Err(ToastyError::other(format!(
                "No source file provided and no Oats.toml found.\n\nTo build a project:\n  • Create an Oats.toml manifest in your project root\n  • Or specify a source file: toasty build main.oats\n\nCurrent directory: {}",
                cwd.display()
            )))
        }
        Err(e) => Err(ToastyError::other(format!(
            "Failed to load manifest: {}\n\nTo build a project:\n  • Fix the Oats.toml manifest\n  • Or specify a source file: toasty build main.oats",
            e
        ))),
    }
}
