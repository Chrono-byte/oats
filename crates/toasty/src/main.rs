use anyhow::Result;
use atty::Stream as AtStream;
use clap::{Parser, Subcommand};
use colored::Colorize;

mod module_resolution;

#[derive(Parser)]
#[command(name = "toasty", about = "Experimental TypeScript Compiler", version = env!("CARGO_PKG_VERSION"))]
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
            match module_resolution::load_modules(&src_file) {
                Ok(modules) => {
                    if !quiet && (verbose || cfg!(debug_assertions)) {
                        eprintln!(
                            "{}",
                            format!("Found {} module(s) to compile", modules.len()).green()
                        );
                        if verbose {
                            for (path, _) in &modules {
                                eprintln!("  - {}", path);
                            }
                        }
                    }
                    // For now, we validate that all modules parse correctly
                    // In future phases, we'll compile them all together
                }
                Err(e) => {
                    if !quiet {
                        eprintln!("{}", format!("Module resolution warning: {}", e).yellow());
                        eprintln!("{}", "Continuing with single-file compilation...".yellow());
                    }
                }
            }

            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Invoking compiler...".blue());
            }

            // Build compile options
            let mut options = oatsc::CompileOptions::new(src_file);
            options.out_dir = out_dir;
            options.out_name = out_name;
            options.linker = linker;
            options.emit_object_only = emit_object_only;
            options.opt_level = opt_level;
            options.lto = lto;
            options.target_triple = target_triple;
            options.target_cpu = target_cpu;
            options.target_features = target_features;
            options.build_profile = if release {
                Some("release".to_string())
            } else {
                None
            };

            // Invoke the compiler
            let _build_out = oatsc::compile(options)?;

            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Build finished.".green());
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

            // Build first
            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Building before run...".blue());
            }

            // Build compile options
            let mut options = oatsc::CompileOptions::new(src_file.clone());
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
    }
}
