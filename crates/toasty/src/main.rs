use anyhow::Result;
use atty::Stream as AtStream;
use clap::{Parser, Subcommand};
use colored::Colorize;

mod build;
mod manifest;
mod module_resolution;
mod package_graph;

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
}

// Preflight dependency check so CLI users and CI get a clear error early.
#[allow(dead_code)]
fn preflight_check() -> anyhow::Result<()> {
    // Legacy: kept no-arg wrapper for compatibility, forwards to new function
    preflight_check_with_verbosity(false)
}

fn preflight_check_with_verbosity(verbose: bool) -> anyhow::Result<()> {
    use std::process::{Command, Stdio};

    // Check rustc
    // Run `rustc --version` but avoid printing its output by default
    let status = if verbose {
        Command::new("rustc").arg("--version").status()
    } else {
        Command::new("rustc")
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
    };
    match status {
        Ok(s) if s.success() => {}
        Ok(_) => {
            anyhow::bail!("`rustc` present but returned non-zero when invoked with --version")
        }
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::bail!(
                    "`rustc` not found in PATH; please install Rust toolchain (rustup) or ensure `rustc` is available"
                )
            } else {
                return Err(e.into());
            }
        }
    }

    // Try clang candidates (unversioned or common versioned names)
    let clang_candidates = ["clang", "clang-18", "clang-17"];
    let mut any_ok = false;
    for &c in &clang_candidates {
        // Avoid printing clang's version in non-verbose mode by
        // redirecting stdout/stderr to null unless the user requested verbose output.
        let status = if verbose {
            Command::new(c).arg("--version").status()
        } else {
            Command::new(c)
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        };
        match status {
            Ok(s) if s.success() => {
                any_ok = true;
                break;
            }
            Ok(_) => {
                // Found binary but it returned non-zero; continue looking
            }
            Err(_e) => {
                // Not found; try next
            }
        }
    }
    if !any_ok {
        anyhow::bail!(
            "`clang` not found in PATH (tried clang, clang-18, clang-17). Please install clang or add a symlink to a versioned clang binary."
        );
    }

    Ok(())
}

fn main() -> Result<()> {
    // Perform preflight checks to ensure required tools are available
    // preflight_check()?;

    let cli = Cli::parse();

    match cli.cmd {
        Commands::Build {
            src,
            out_dir,
            out_name,
            linker,
            emit_object_only,
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
            preflight_check_with_verbosity(verbose)?;
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

            // Special case: if emit_object_only is requested and this is a single file with no dependencies,
            // compile it directly without the multi-module system
            if emit_object_only {
                if !quiet && (verbose || cfg!(debug_assertions)) {
                    eprintln!(
                        "{}",
                        "Compiling single file with emit-object-only...".blue()
                    );
                }

                let mut options = oatsc::CompileOptions::new(src_file.clone());
                options.out_dir = out_dir.clone();
                options.out_name = out_name.clone();
                options.linker = linker.clone();
                options.emit_object_only = true;
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

                let build_out = oatsc::compile(options)?;
                if let Some(out_path) = build_out
                    && !quiet
                    && (verbose || cfg!(debug_assertions))
                {
                    eprintln!("{}", format!("Build finished: {}", out_path).green());
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

                let mut options = oatsc::CompileOptions::new(module_path.clone());

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
                let build_out = oatsc::compile(options)?;
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
                        eprintln!("  {} -> {}", module_path, obj_file);
                    }
                }
                return Ok(());
            }

            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Linking final executable...".blue());
            }

            // Link all object files together
            let exe_name = out_name.unwrap_or_else(|| "main".to_string());
            let exe_path = out_dir
                .as_ref()
                .map(|d| std::path::Path::new(d).join(&exe_name))
                .unwrap_or_else(|| std::path::Path::new(&exe_name).to_path_buf());

            // Use clang to link all object files with the runtime
            // For now, hardcode the runtime path - TODO: make this more robust
            let runtime_lib = "target/release/libruntime.a";
            if !std::path::Path::new(runtime_lib).exists() {
                // Try to build the runtime
                eprintln!("Building runtime locally...");
                let status = std::process::Command::new("cargo")
                    .arg("build")
                    .arg("-p")
                    .arg("runtime")
                    .arg("--release")
                    .status()?;
                if !status.success() {
                    anyhow::bail!("building rust runtime failed");
                }
            }
            let mut link_cmd = std::process::Command::new("clang");
            link_cmd.arg("-o").arg(&exe_path);

            // Add all object files
            for obj_file in compiled_modules.values() {
                link_cmd.arg(obj_file);
            }

            // Add runtime library
            link_cmd.arg(runtime_lib);

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
            preflight_check_with_verbosity(verbose)?;

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
            let additional_files = if all_src_files.len() > 1 {
                all_src_files[1..].to_vec()
            } else {
                Vec::new()
            };
            let mut options = oatsc::CompileOptions::with_modules(entry_point, additional_files);
            options.out_name = out_name.clone();

            let build_res = oatsc::compile(options)?;

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
            let out_exe = if let Some(p) = build_res {
                p
            } else {
                format!("{}/{}", out_dir, exe_name)
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
    }
}
