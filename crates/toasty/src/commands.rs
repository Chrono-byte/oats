//! Command handlers for toasty CLI
//!
//! This module contains the implementation of all CLI commands.

use atty::Stream as AtStream;
use colored::Colorize;
use std::io::Write;
use std::path::PathBuf;

use crate::build;
use crate::cli::{Cli, CompileOptions, VersionCommands};
use crate::compiler;
use crate::diagnostics::{Result, ToastyError};
use crate::fetch;
use crate::linker;
use crate::project;

/// Helper function to determine verbosity and color settings
fn get_verbosity_and_color(cli: &Cli, color: Option<String>) -> bool {
    let verbose = if cli.verbose {
        true
    } else {
        std::env::var("TOASTY_VERBOSE").is_ok()
    };

    let enable_color = match color.as_deref() {
        Some("always") => true,
        Some("never") => false,
        _ => atty::is(AtStream::Stderr),
    };
    colored::control::set_override(enable_color);

    verbose
}

/// CLI build options grouped together
struct CliBuildOptions {
    out_dir: Option<String>,
    out_name: Option<String>,
    linker: Option<String>,
    opt_level: Option<String>,
    lto: Option<String>,
    target_triple: Option<String>,
    target_cpu: Option<String>,
    target_features: Option<String>,
    release: bool,
    emit_object_only: bool,
    link_runtime: bool,
}

/// Helper function to set common `CompileOptions` fields from CLI parameters
fn set_compile_options_from_cli(
    mut options: CompileOptions,
    cli_opts: CliBuildOptions,
) -> CompileOptions {
    options.out_dir = cli_opts.out_dir;
    options.out_name = cli_opts.out_name;
    options.linker = cli_opts.linker;
    options.opt_level = cli_opts.opt_level;
    options.lto = cli_opts.lto;
    options.target_triple = cli_opts.target_triple;
    options.target_cpu = cli_opts.target_cpu;
    options.target_features = cli_opts.target_features;
    options.build_profile = if cli_opts.release {
        Some("release".to_string())
    } else {
        None
    };
    options.emit_object_only = cli_opts.emit_object_only;
    options.link_runtime = cli_opts.link_runtime;
    options
}

/// Build command options grouped together
pub struct BuildCommandOptions {
    pub src: Option<String>,
    pub out_dir: Option<String>,
    pub out_name: Option<String>,
    pub linker: Option<String>,
    pub emit_object_only: bool,
    pub no_link_runtime: bool,
    pub opt_level: Option<String>,
    pub lto: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub release: bool,
    pub quiet: bool,
    pub color: Option<String>,
}

/// Handle package-based build
fn handle_package_build(
    manifest_path_str: &str,
    opts: &BuildCommandOptions,
    verbose: bool,
) -> Result<()> {
    let manifest_path = std::path::PathBuf::from(manifest_path_str);
    let build_config = crate::build::BuildConfig {
        verbose,
        quiet: opts.quiet,
        release: opts.release,
        out_dir: opts.out_dir.clone(),
        out_name: opts.out_name.clone(),
        linker: opts.linker.clone(),
        opt_level: opts.opt_level.clone(),
        lto: opts.lto.clone(),
        target_triple: opts.target_triple.clone(),
        target_cpu: opts.target_cpu.clone(),
        target_features: opts.target_features.clone(),
        no_link_runtime: opts.no_link_runtime,
    };

    let exe_path = build::build_package_project(&manifest_path, build_config)?;

    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Build complete: {}", exe_path.display()).green()
        );
    }

    Ok(())
}

/// Handle single file `emit_object_only` build
fn handle_single_file_emit_object_only(
    src_file: String,
    opts: &BuildCommandOptions,
    out_dir: Option<String>,
    verbose: bool,
) -> Result<()> {
    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            "Compiling single file with emit-object-only...".blue()
        );
    }

    let options = set_compile_options_from_cli(
        CompileOptions::new(src_file),
        CliBuildOptions {
            out_dir,
            out_name: opts.out_name.clone(),
            linker: opts.linker.clone(),
            opt_level: opts.opt_level.clone(),
            lto: opts.lto.clone(),
            target_triple: opts.target_triple.clone(),
            target_cpu: opts.target_cpu.clone(),
            target_features: opts.target_features.clone(),
            release: opts.release,
            emit_object_only: true,
            link_runtime: !opts.no_link_runtime,
        },
    );

    let build_out = compiler::invoke_oatsc(&options)?;
    if let Some(out_path) = build_out
        && !opts.quiet
        && (verbose || cfg!(debug_assertions))
    {
        eprintln!(
            "{}",
            format!("Build finished: {}", out_path.display()).green()
        );
    }
    Ok(())
}

/// Find or generate metadata file for a module
fn find_or_generate_metadata(
    prev_module: &str,
    out_dir: &str,
) -> Option<String> {
    let module_stem = std::path::Path::new(prev_module)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("module");
    let potential_meta =
        std::path::Path::new(out_dir).join(format!("{module_stem}_pkg.oats.meta"));

    if potential_meta.exists() {
        Some(potential_meta.to_string_lossy().to_string())
    } else {
        // Extract exported symbols from source and create metadata file
        if let Ok(exported_symbols) = crate::build::extract_exported_symbols(prev_module) {
            if exported_symbols.is_empty() {
                None
            } else {
                // Generate metadata file with function signatures
                let meta_content = exported_symbols
                    .iter()
                    .map(|name| format!("function {name}(): number;"))
                    .collect::<Vec<_>>()
                    .join("\n");

                let meta_file = std::path::Path::new(out_dir)
                    .join(format!("{module_stem}.oats.meta"));

                if let Err(e) = std::fs::write(&meta_file, meta_content) {
                    eprintln!(
                        "Warning: Failed to write metadata file {}: {}",
                        meta_file.display(),
                        e
                    );
                    None
                } else {
                    Some(meta_file.to_string_lossy().to_string())
                }
            }
        } else {
            None
        }
    }
}

/// Add `extern_oats` dependencies for previous modules
fn add_extern_oats_dependencies(
    options: &mut CompileOptions,
    prev_modules: &[String],
    out_dir: &Option<String>,
) {
    for prev_module in prev_modules {
        // Determine the import path (relative to current module)
        // Uses the module file stem as the import path
        let import_path = match std::path::Path::new(prev_module)
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| format!("./{s}"))
        {
            Some(path) => path,
            None => "./module".to_string(),
        };

        // Try to find or generate metadata file for this module
        let meta_path = if let Some(out_dir) = out_dir {
            find_or_generate_metadata(prev_module, out_dir)
        } else {
            None
        };

        // Add to extern_oats if a metadata file exists
        if let Some(meta) = meta_path {
            options.extern_oats.insert(import_path, meta);
        }
    }
}

/// Compile all modules in the dependency graph
fn compile_modules(
    compilation_order: &[String],
    opts: &BuildCommandOptions,
    out_dir: Option<String>,
    verbose: bool,
) -> Result<std::collections::HashMap<String, PathBuf>> {
    let mut compiled_modules = std::collections::HashMap::new();

    for (i, module_path) in compilation_order.iter().enumerate() {
        if !opts.quiet && (verbose || cfg!(debug_assertions)) {
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

        let mut options = set_compile_options_from_cli(
            CompileOptions::new(module_path.clone()),
            CliBuildOptions {
                out_dir: out_dir.clone(),
                out_name: Some(format!("{i}_module")),
                linker: opts.linker.clone(),
                opt_level: opts.opt_level.clone(),
                lto: opts.lto.clone(),
                target_triple: opts.target_triple.clone(),
                target_cpu: opts.target_cpu.clone(),
                target_features: opts.target_features.clone(),
                release: opts.release,
                emit_object_only: true,
                link_runtime: !opts.no_link_runtime,
            },
        );

        // For Phase 2: if this is not the first module, add extern_oats for previous modules
        if i > 0
            && let Some(prev_modules) = compilation_order.get(0..i)
        {
            add_extern_oats_dependencies(&mut options, prev_modules, &out_dir);
        }

        // Compile this module
        let build_out = compiler::invoke_oatsc(&options)?;
        if let Some(out_path) = build_out {
            compiled_modules.insert(module_path.clone(), out_path);
        }
    }

    Ok(compiled_modules)
}

/// Handle linking phase of the build
fn handle_linking(
    compiled_modules: &std::collections::HashMap<String, PathBuf>,
    src_file: &str,
    opts: &BuildCommandOptions,
    out_dir: Option<String>,
    verbose: bool,
) -> Result<()> {
    if opts.emit_object_only {
        if !opts.quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Skipping linking due to emit-object-only...".blue());
        }
        // For multi-module builds with emit_object_only, report the object files
        if !opts.quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Compiled object files:".green());
            for (module_path, obj_file) in compiled_modules {
                eprintln!("  {} -> {}", module_path, obj_file.display());
            }
        }
        return Ok(());
    }

    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Linking final executable...".blue());
    }

    // Link all object files together
    let exe_name = match opts.out_name.clone() {
        Some(name) => name,
        None => {
            match std::path::Path::new(src_file)
                .file_stem()
                .and_then(|s| s.to_str())
            {
                Some(stem) => stem.to_string(),
                None => "main".to_string(),
            }
        }
    };
    let exe_path = match out_dir.as_ref() {
        Some(d) => std::path::Path::new(d).join(&exe_name),
        None => std::path::Path::new(&exe_name).to_path_buf(),
    };

    // Link the executable
    linker::link_executable(compiled_modules, &exe_path, opts.linker.clone(), verbose)?;

    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Build finished: {}", exe_path.display()).green()
        );
    }
    Ok(())
}

/// Handle the build command
pub fn handle_build(cli: &Cli, opts: BuildCommandOptions) -> Result<()> {
    let verbose = get_verbosity_and_color(cli, opts.color.clone());

    if opts.release {
        if !opts.quiet && (verbose || cfg!(debug_assertions)) {
            eprintln!("{}", "Building in release mode...".green());
        }
    } else if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Building in debug mode...".yellow());
    }

    // Determine source file
    let src_file = determine_source_file(opts.src.clone(), opts.quiet, verbose)?;

    // Check if this is a package-based build
    if let Some(manifest_path_str) = src_file.strip_prefix("package:") {
        return handle_package_build(manifest_path_str, &opts, verbose);
    }

    // Read out_dir from environment variable if not provided
    let out_dir = opts
        .out_dir
        .clone()
        .or_else(|| std::env::var("OATS_OUT_DIR").ok());

    // Special case: if emit_object_only is requested and this is a single file with no dependencies,
    // compile it directly without the multi-module system
    if opts.emit_object_only {
        return handle_single_file_emit_object_only(src_file, &opts, out_dir, verbose);
    }

    // Build dependency graph starting from entry point
    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Building dependency graph...".blue());
    }
    let (dep_graph, node_indices, _entry_node) =
        project::build_dependency_graph(&src_file, verbose)?;

    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!(
            "{}",
            format!("Found {} module(s) to compile", node_indices.len()).green()
        );
    }

    // Perform topological sort to get compilation order
    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Computing compilation order...".blue());
    }
    let compilation_order = project::topological_sort(&dep_graph, &node_indices)?;

    if verbose {
        eprintln!("Compilation order:");
        for (i, path) in compilation_order.iter().enumerate() {
            eprintln!("  {}. {}", i + 1, path);
        }
    }

    if !opts.quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", "Orchestrating compilation...".blue());
    }

    // Compile all modules
    let compiled_modules = compile_modules(&compilation_order, &opts, out_dir.clone(), verbose)?;

    // Link all compiled modules together
    handle_linking(&compiled_modules, &src_file, &opts, out_dir, verbose)
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
    let verbose = get_verbosity_and_color(cli, color);

    // Determine source file
    let src_file = if let Some(s) = src {
        s
    } else if let Some(p) = crate::env::get_src_file() {
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

    // Determine output directory before compiling
    let out_dir = match std::env::var("OATS_OUT_DIR") {
        Ok(dir) => dir,
        Err(_) => ".".to_string(),
    };

    // Ensure output directory exists
    std::fs::create_dir_all(&out_dir).map_err(|e| {
        ToastyError::other(format!("Failed to create output directory {out_dir}: {e}"))
    })?;

    // Build compile options with all discovered modules
    let mut all_src_files: Vec<String> = modules.keys().cloned().collect();
    // Ensure the entry point is first
    if let Some(pos) = all_src_files.iter().position(|p| p == &src_file) {
        all_src_files.swap(0, pos);
    }
    let entry_point = all_src_files
        .first()
        .ok_or_else(|| ToastyError::other("No source files found to compile"))?
        .clone();
    let mut options = CompileOptions::with_modules(entry_point);
    options.out_name = out_name.clone();
    options.out_dir = Some(out_dir.clone());
    let build_res = compiler::invoke_oatsc(&options)?;
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
    let _out_exe = if let Some(p) = build_res {
        p
    } else {
        PathBuf::from(format!("{out_dir}/{exe_name}"))
    };

    // Allow overriding output name with OATS_OUT_NAME
    let out_exe = if let Ok(name) = std::env::var("OATS_OUT_NAME") {
        format!("{out_dir}/{name}")
    } else {
        format!("{out_dir}/{src_filename}")
    };

    // Execute the binary with provided args
    let mut cmd = std::process::Command::new(&out_exe);
    if !args.is_empty() {
        cmd.args(&args);
    }

    if !quiet && (verbose || cfg!(debug_assertions)) {
        eprintln!("{}", format!("Running `{out_exe}`").green());
    }
    let output = cmd.output()?;

    // Forward stdout and stderr from the executed program
    std::io::stdout().write_all(&output.stdout)?;
    std::io::stderr().write_all(&output.stderr)?;

    if !output.status.success() {
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
            "Directory '{name}' already exists"
        )));
    }

    // Create project directory
    std::fs::create_dir_all(project_path)?;

    // Create Oats.toml
    let manifest_content = format!(
        r#"[package]
name = "{name}"
version = "0.1.0"
authors = ["Your Name <your.email@example.com>"]
description = "An Oats project"
license = "MIT"

[dependencies]
"#
    );

    std::fs::write(project_path.join("Oats.toml"), manifest_content)?;

    // Create src directory
    let src_dir = project_path.join("src");
    std::fs::create_dir_all(&src_dir)?;

    // Create main.oats or lib.oats
    let main_content = if lib {
        format!(
            r#"// {name}/src/lib.oats
// Library entry point

export function hello(): string {{
    return "Hello from {name}!";
}}
"#
        )
    } else {
        format!(
            r#"// {name}/src/main.oats
// Executable entry point

export function main(): number {{
    println("Hello, {name}!");
    return 0;
}}
"#
        )
    };

    let main_file = if lib { "lib.oats" } else { "main.oats" };
    std::fs::write(src_dir.join(main_file), main_content)?;

    if !quiet {
        eprintln!("{}", format!("Created new Oats project '{name}'").green());
        eprintln!("  {}", project_path.display());
        eprintln!("Run 'cd {name} && toasty build' to build");
        if !lib {
            eprintln!("Run 'cd {name} && toasty run' to run");
        }
    }

    Ok(())
}

/// Handle compiler commands
pub fn handle_compiler(action: VersionCommands) -> Result<()> {
    handle_version_commands(action, "compiler")
}

/// Handle runtime commands
pub fn handle_runtime(action: VersionCommands) -> Result<()> {
    handle_version_commands(action, "runtime")
}

/// Generic handler for version management commands (compiler/runtime)
fn handle_version_commands(action: VersionCommands, artifact_type: &str) -> Result<()> {
    match action {
        VersionCommands::List => {
            // List available versions
            let versions = match artifact_type {
                "compiler" => fetch::list_available_compilers()?,
                "runtime" => fetch::list_available_runtimes()?,
                _ => {
                    return Err(crate::diagnostics::ToastyError::other(
                        "Invalid artifact type",
                    ));
                }
            };
            if versions.is_empty() {
                println!("No {artifact_type} versions found.");
            } else {
                println!("Available {artifact_type} versions:");
                for version in versions {
                    println!("  - {version}");
                }
            }
            Ok(())
        }
        VersionCommands::Install { version } => {
            // Install a specific version
            match artifact_type {
                "compiler" => fetch::install_compiler_version(&version)?,
                "runtime" => fetch::install_runtime_version(&version)?,
                _ => {
                    return Err(crate::diagnostics::ToastyError::other(
                        "Invalid artifact type",
                    ));
                }
            }
            println!(
                "{} version {} installed successfully.",
                capitalize_first(artifact_type),
                version
            );
            Ok(())
        }
        VersionCommands::Use { version } => {
            // Switch to a specific version
            match artifact_type {
                "compiler" => fetch::use_compiler_version(&version)?,
                "runtime" => fetch::use_runtime_version(&version)?,
                _ => {
                    return Err(crate::diagnostics::ToastyError::other(
                        "Invalid artifact type",
                    ));
                }
            }
            println!("Switched to {artifact_type} version {version}.");
            Ok(())
        }
        VersionCommands::Current => {
            // Show current version
            let current_version = match artifact_type {
                "compiler" => fetch::current_compiler_version()?,
                "runtime" => fetch::current_runtime_version()?,
                _ => {
                    return Err(crate::diagnostics::ToastyError::other(
                        "Invalid artifact type",
                    ));
                }
            };
            println!("Current {artifact_type} version: {current_version}");
            Ok(())
        }
        VersionCommands::Uninstall { version } => {
            // Uninstall a specific version
            match artifact_type {
                "compiler" => fetch::uninstall_compiler_version(&version)?,
                "runtime" => fetch::uninstall_runtime_version(&version)?,
                _ => {
                    return Err(crate::diagnostics::ToastyError::other(
                        "Invalid artifact type",
                    ));
                }
            }
            println!(
                "{} version {} uninstalled successfully.",
                capitalize_first(artifact_type),
                version
            );
            Ok(())
        }
    }
}

/// Helper function to capitalize the first letter of a string
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
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
            let cwd = match std::env::current_dir() {
                Ok(path) => path,
                Err(_) => std::path::PathBuf::from("."),
            };
            Err(ToastyError::other(format!(
                "No source file provided and no Oats.toml found.\n\nTo build a project:\n  • Create an Oats.toml manifest in your project root\n  • Or specify a source file: toasty build main.oats\n\nCurrent directory: {}",
                cwd.display()
            )))
        }
        Err(e) => Err(ToastyError::other(format!(
            "Failed to load manifest: {e}\n\nTo build a project:\n  • Fix the Oats.toml manifest\n  • Or specify a source file: toasty build main.oats"
        ))),
    }
}
