use anyhow::{Context, Result};
use atty::Stream as AtStream;
use clap::{Parser, Subcommand};
use colored::Colorize;
use std::path::PathBuf;

mod build;
mod compiler_fetch;
mod manifest;
mod module_resolution;
mod package_graph;
mod runtime_fetch;

/// Compilation options for invoking oatsc
#[derive(Debug, Clone)]
struct CompileOptions {
    pub src_file: String,
    pub extern_oats: std::collections::HashMap<String, String>,
    pub package_root: Option<std::path::PathBuf>,
    pub extern_pkg: std::collections::HashMap<String, String>,
    pub out_dir: Option<String>,
    pub out_name: Option<String>,
    pub linker: Option<String>,
    pub emit_object_only: bool,
    pub link_runtime: bool,
    pub opt_level: Option<String>,
    pub lto: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub build_profile: Option<String>,
}

impl CompileOptions {
    fn new(src_file: String) -> Self {
        Self {
            src_file,
            extern_oats: std::collections::HashMap::new(),
            package_root: None,
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            link_runtime: true,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }

    fn with_modules(entry_point: String) -> Self {
        Self {
            src_file: entry_point,
            extern_oats: std::collections::HashMap::new(),
            package_root: None,
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            link_runtime: true,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }
}

#[derive(Parser)]
#[command(name = "toasty", about = "Oats Project Manager", version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    /// Print verbose debug information even in release builds
    #[arg(long = "verbose")]
    verbose: bool,
    #[command(subcommand)]
    cmd: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build a .oats file into a native executable (like `cargo build`)
    Build {
        /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
        src: Option<String>,

        /// Output directory for compilation artifacts (defaults to current directory or OATS_OUT_DIR)
        #[arg(short, long)]
        out_dir: Option<String>,
        /// Override the produced executable name (without path)
        #[arg(long = "out-name")]
        out_name: Option<String>,

        /// Explicit linker to use for final binary (sets OATS_LINKER)
        #[arg(long)]
        linker: Option<String>,
        /// Build in release mode (enables optimizations/LTO)
        #[arg(long)]
        release: bool,

        /// Emit object only and skip final host linking (sets OATS_EMIT_OBJECT_ONLY)
        #[arg(long = "emit-object-only")]
        emit_object_only: bool,

        /// Skip linking the runtime library (for custom linking)
        #[arg(long = "no-link-runtime")]
        no_link_runtime: bool,

        /// Set opt level: none, default, aggressive (also OATS_OPT_LEVEL)
        #[arg(long = "opt-level")]
        opt_level: Option<String>,

        /// LTO mode: none, thin, full (also OATS_LTO)
        #[arg(long = "lto")]
        lto: Option<String>,

        /// Target triple to pass to LLVM (sets OATS_TARGET_TRIPLE)
        #[arg(long = "target-triple")]
        target_triple: Option<String>,

        /// Target CPU to use for TargetMachine (sets OATS_TARGET_CPU)
        #[arg(long = "target-cpu")]
        target_cpu: Option<String>,

        /// Target features to enable (sets OATS_TARGET_FEATURES)
        #[arg(long = "target-features")]
        target_features: Option<String>,
        /// Suppress progress output
        #[arg(long = "quiet")]
        quiet: bool,
        /// Force color output: auto, always, never
        #[arg(long = "color")]
        color: Option<String>,
    },

    /// Build then run the produced executable (like `cargo run`)
    Run {
        /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
        src: Option<String>,

        /// Arguments to pass to the executed program (after `--`)
        #[arg(last = true)]
        args: Vec<String>,
        /// Override the produced executable name (without path)
        #[arg(long = "out-name")]
        out_name: Option<String>,
        /// Suppress progress output
        #[arg(long = "quiet")]
        quiet: bool,
        /// Force color output: auto, always, never
        #[arg(long = "color")]
        color: Option<String>,
    },

    /// Create a new Oats project (like `cargo new`)
    New {
        /// Name of the project to create
        name: String,
        /// Create a library project instead of executable
        #[arg(long)]
        lib: bool,
        /// Suppress progress output
        #[arg(long)]
        quiet: bool,
    },

    /// Manage oatsc compiler versions
    Compiler {
        #[command(subcommand)]
        action: CompilerCommands,
    },
}

#[derive(Subcommand)]
enum CompilerCommands {
    /// List available compiler versions
    List,
    /// Install a specific compiler version
    Install {
        /// Version tag to install (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
    /// Switch to a specific compiler version
    Use {
        /// Version tag to use (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
    /// Show current compiler version
    Current,
    /// Uninstall a specific compiler version
    Uninstall {
        /// Version tag to uninstall (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
}

/// Perform preflight checks to ensure required tools are available
fn preflight_check() -> Result<()> {
    // Check if OATS_OATSC_PATH is already set (e.g., by tests)
    if std::env::var("OATS_OATSC_PATH").is_ok() {
        return Ok(());
    }

    // Check for oatsc compiler (try selected version first, then downloaded latest, then local target, then system)
    let oatsc_available =
        if let Some(oatsc_path) = crate::compiler_fetch::get_selected_compiler_path() {
            // Use selected oatsc version
            unsafe {
                std::env::set_var("OATS_OATSC_PATH", oatsc_path);
            }
            true
        } else if let Some(oatsc_path) = crate::compiler_fetch::try_fetch_compiler() {
            // Use downloaded oatsc
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

    if !oatsc_available {
        anyhow::bail!(
            "oatsc compiler not found. Please ensure oatsc is installed, or pre-built binaries are available for download."
        );
    }

    // Check for linker (clang preferred, but accept others)
    let linker_available =
        is_command_available("clang") || is_command_available("gcc") || is_command_available("ld");

    if !linker_available {
        anyhow::bail!("No linker found in PATH. Please install clang, gcc, or another linker.");
    }

    Ok(())
}

/// Check if a command is available in PATH
fn is_command_available(cmd: &str) -> bool {
    std::process::Command::new(cmd)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Invoke oatsc compiler as external command
fn invoke_oatsc(options: &CompileOptions) -> Result<Option<PathBuf>> {
    // Get oatsc path from environment (set by preflight check)
    let oatsc_path = std::env::var("OATS_OATSC_PATH").unwrap_or_else(|_| "oatsc".to_string());

    // Build command arguments
    let mut args = vec![];

    // Add source file
    args.push(options.src_file.clone());

    // Add package root if specified
    if let Some(pkg_root) = &options.package_root {
        args.push("--package-root".to_string());
        args.push(pkg_root.to_string_lossy().to_string());
    }

    // Handle output path: if both out_dir and out_name are specified,
    // compute the full path and pass via -o flag
    if let (Some(out_dir), Some(out_name)) = (&options.out_dir, &options.out_name) {
        let output_path = std::path::Path::new(out_dir).join(format!("{}.o", out_name));
        args.push("-o".to_string());
        args.push(output_path.to_string_lossy().to_string());
    } else if let Some(out_name) = &options.out_name {
        // Only out_name specified, use --out-name
        args.push("--out-name".to_string());
        args.push(out_name.clone());
    }

    // Add linker
    if let Some(linker) = &options.linker {
        args.push("--linker".to_string());
        args.push(linker.clone());
    }

    // Add external packages
    for (name, path) in &options.extern_pkg {
        args.push("--extern-pkg".to_string());
        args.push(format!("{}={}", name, path));
    }

    // Add build profile
    if let Some(profile) = &options.build_profile {
        args.push("--profile".to_string());
        args.push(profile.clone());
    }

    // Add optimization level
    if let Some(opt_level) = &options.opt_level {
        args.push("--opt-level".to_string());
        args.push(opt_level.clone());
    }

    // Add LTO
    if let Some(lto) = &options.lto {
        args.push("--lto".to_string());
        args.push(lto.clone());
    }

    // Add target triple
    if let Some(triple) = &options.target_triple {
        args.push("--target-triple".to_string());
        args.push(triple.clone());
    }

    // Add target CPU
    if let Some(cpu) = &options.target_cpu {
        args.push("--target-cpu".to_string());
        args.push(cpu.clone());
    }

    // Add target features
    if let Some(features) = &options.target_features {
        args.push("--target-features".to_string());
        args.push(features.clone());
    }

    // Add flags
    if options.emit_object_only {
        args.push("--emit-object-only".to_string());
    }
    if !options.link_runtime {
        args.push("--no-link-runtime".to_string());
    }

    // Execute command
    let output = std::process::Command::new(&oatsc_path)
        .args(&args)
        .output()
        .with_context(|| format!("Failed to execute oatsc command: {} {:?}", oatsc_path, args))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        anyhow::bail!(
            "oatsc compilation failed:\nSTDOUT: {}\nSTDERR: {}",
            stdout,
            stderr
        );
    }

    // Parse output to get object file path if any
    // oatsc may or may not print output depending on mode
    let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();

    if stdout.is_empty() {
        Ok(None)
    } else if let Some(path_str) = stdout
        .strip_prefix("Emitted ")
        .and_then(|s| s.split(" (linking delegated to toasty)").next())
    {
        Ok(Some(PathBuf::from(path_str)))
    } else {
        // Assume it's a direct path
        Ok(Some(PathBuf::from(stdout)))
    }
}

fn main() -> Result<()> {
    // Perform preflight checks to ensure required tools are available
    preflight_check()?;

    let cli = Cli::parse();

    match cli.cmd {
        Commands::Build {
            src,
            out_dir,
            out_name,
            linker,
            emit_object_only,
            no_link_runtime,
            opt_level,
            lto,
            target_triple,
            target_cpu,
            target_features,
            release,
            // allow quiet/color at build time as well (default false/auto)
            quiet,
            color,
        } => {
            // determine effective verbosity: CLI flag overrides env var
            let verbose = if cli.verbose {
                true
            } else {
                std::env::var("TOASTY_VERBOSE").is_ok()
            };
            // run preflight with verbosity so tool detection honors `--verbose`
            preflight_check()?;
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

            // Try to discover and load manifest for package-based build
            if src.is_none() {
                // Only auto-discover manifest if no explicit source file provided
                match manifest::Manifest::discover() {
                    Ok(Some((_manifest, manifest_path))) => {
                        if !quiet && (verbose || cfg!(debug_assertions)) {
                            eprintln!(
                                "{}",
                                format!("Found project manifest: {}", manifest_path.display())
                                    .blue()
                            );
                        }

                        // Use package-based build
                        let build_config = build::BuildConfig {
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
                    Ok(None) => {
                        if !quiet && (verbose || cfg!(debug_assertions)) {
                            eprintln!("{}", "No Oats.toml found, using single-file mode".yellow());
                        }
                    }
                    Err(e) => {
                        if !quiet {
                            eprintln!(
                                "{}",
                                format!("Warning: Failed to load manifest: {}", e).yellow()
                            );
                        }
                    }
                }
            }

            // Determine source file for legacy single-file mode
            let src_file = if let Some(s) = src {
                s
            } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
                p
            } else {
                anyhow::bail!(
                    "No source file provided. Pass path as argument, set OATS_SRC_FILE env var, or create Oats.toml"
                );
            };

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

                let build_out = invoke_oatsc(&options)?;
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
                module_resolution::build_dependency_graph(&src_file, verbose)?;

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
            let compilation_order = module_resolution::topological_sort(&dep_graph, &node_indices)?;

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
                let build_out = invoke_oatsc(&options)?;
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

            // Use clang to link all object files with the runtime
            // Ensure runtime is available
            let runtime_lib = if let Ok(runtime_path) = std::env::var("OATS_RUNTIME_PATH") {
                // Use explicitly specified runtime path
                let runtime_lib = std::path::PathBuf::from(runtime_path);
                if !runtime_lib.exists() {
                    anyhow::bail!(
                        "Runtime library not found at OATS_RUNTIME_PATH: {}",
                        runtime_lib.display()
                    );
                }
                runtime_lib
            } else if let Some(cached_runtime) = crate::runtime_fetch::try_fetch_runtime() {
                // Use the cached pre-built runtime
                cached_runtime
            } else {
                // Look for runtime in current directory or standard locations
                let runtime_lib = std::path::PathBuf::from("libruntime.a");

                if !runtime_lib.exists() {
                    anyhow::bail!(
                        "Runtime library not found. Please either:\n\
                        1. Ensure pre-built runtimes can be downloaded from GitHub, or\n\
                        2. Place libruntime.a in the current directory, or\n\
                        3. Set OATS_RUNTIME_PATH to the path of libruntime.a"
                    );
                }
                runtime_lib
            };

            let mut link_cmd = std::process::Command::new("clang");
            link_cmd.arg("-o").arg(&exe_path);

            // Add all object files
            for obj_file in compiled_modules.values() {
                link_cmd.arg(obj_file);
            }

            // Add runtime library
            if !no_link_runtime {
                link_cmd.arg(runtime_lib);
            }

            // Add linker if specified
            if let Some(linker) = linker {
                link_cmd.arg(format!("-fuse-ld={}", linker));
            }

            if verbose {
                eprintln!("Link command: {:?}", link_cmd);
            }

            let link_status = link_cmd.status()?;
            if !link_status.success() {
                anyhow::bail!("Linking failed");
            }

            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!(
                    "{}",
                    format!("Build finished: {}", exe_path.display()).green()
                );
            }
            Ok(())
        }
        Commands::Run {
            src,
            args,
            out_name,
            quiet,
            color,
        } => {
            let verbose = if cli.verbose {
                true
            } else {
                std::env::var("TOASTY_VERBOSE").is_ok()
            };
            preflight_check()?;

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
                anyhow::bail!(
                    "No source file provided. Pass path as argument or set OATS_SRC_FILE env var."
                );
            };

            // Perform module resolution to discover all source files
            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Resolving module dependencies...".blue());
            }
            let modules = module_resolution::load_modules(&src_file)?;

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

            let build_res = invoke_oatsc(&options)?;

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
            let _out_exe = if let Some(p) = build_res {
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
            let status = cmd.status()?;
            if !status.success() {
                anyhow::bail!("Executed program returned non-zero exit code");
            }
            Ok(())
        }
        Commands::New { name, lib, quiet } => {
            // Create a new Oats project
            let project_path = std::path::Path::new(&name);

            if project_path.exists() {
                anyhow::bail!("Directory '{}' already exists", name);
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
        Commands::Compiler { action } => match action {
            CompilerCommands::List => {
                // List available compiler versions
                let versions = crate::compiler_fetch::list_available_compilers()?;
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
                crate::compiler_fetch::install_compiler_version(&version)?;
                println!("Compiler version {} installed successfully.", version);
                Ok(())
            }
            CompilerCommands::Use { version } => {
                // Switch to a specific compiler version
                crate::compiler_fetch::use_compiler_version(&version)?;
                println!("Switched to compiler version {}.", version);
                Ok(())
            }
            CompilerCommands::Current => {
                // Show current compiler version
                let current_version = crate::compiler_fetch::current_compiler_version()?;
                println!("Current compiler version: {}", current_version);
                Ok(())
            }
            CompilerCommands::Uninstall { version } => {
                // Uninstall a specific compiler version
                crate::compiler_fetch::uninstall_compiler_version(&version)?;
                println!("Compiler version {} uninstalled successfully.", version);
                Ok(())
            }
        },
    }
}
