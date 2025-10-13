use anyhow::Result;
use atty::Stream as AtStream;
use clap::{Parser, Subcommand};
use colored::Colorize;

#[derive(Parser)]
#[command(name = "toasty", about = "Oats: build and run AOT-compiled programs")]
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
    // legacy: kept no-arg wrapper for compatibility, forwards to new fn
    preflight_check_with_verbosity(false)
}

fn preflight_check_with_verbosity(verbose: bool) -> anyhow::Result<()> {
    use std::process::{Command, Stdio};

    // Check rustc
    // Run `rustc --version` but avoid printing its output by default
    let status = if cfg!(debug_assertions) || verbose {
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
        // Avoid printing clang's version in non-debug (release) builds by
        // redirecting stdout/stderr to null unless we're in debug mode or the
        // user requested verbose output.
        let status = if cfg!(debug_assertions) || verbose {
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
                // not found; try next
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
                unsafe { std::env::set_var("OATS_BUILD_PROFILE", "release") };
            } else {
                if !quiet && (verbose || cfg!(debug_assertions)) {
                    eprintln!("{}", "Building in debug mode...".yellow());
                }
            }
            if let Some(s) = src.clone() {
                unsafe { std::env::set_var("OATS_SRC_FILE", s) };
            }
            if let Some(o) = out_dir {
                unsafe { std::env::set_var("OATS_OUT_DIR", o) };
            }
            if let Some(l) = linker {
                unsafe { std::env::set_var("OATS_LINKER", l) };
            }
            if emit_object_only {
                unsafe { std::env::set_var("OATS_EMIT_OBJECT_ONLY", "1") };
            }
            if let Some(opt) = opt_level {
                unsafe { std::env::set_var("OATS_OPT_LEVEL", opt) };
            }
            if let Some(lm) = lto {
                unsafe { std::env::set_var("OATS_LTO", lm) };
            }
            if let Some(t) = target_triple {
                unsafe { std::env::set_var("OATS_TARGET_TRIPLE", t) };
            }
            if let Some(c) = target_cpu {
                unsafe { std::env::set_var("OATS_TARGET_CPU", c) };
            }
            if let Some(f) = target_features {
                unsafe { std::env::set_var("OATS_TARGET_FEATURES", f) };
            }
            if let Some(name) = out_name {
                unsafe { std::env::set_var("OATS_OUT_NAME", name) };
            }
            // Delegate to builder. Construct argv so builder sees the source path as argv[1]
            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Invoking builder...".blue());
            }
            let prog = std::env::args()
                .next()
                .unwrap_or_else(|| "toasty".to_string());
            let src_opt = std::env::var("OATS_SRC_FILE").ok().or_else(|| src.clone());
            let argv: Vec<String> = if let Some(s) = src_opt {
                vec![prog, s]
            } else {
                vec![prog]
            };
            let _build_out = oatsc::builder::run_from_args(&argv)?;
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
            let src_clone = src.clone();
            // For run, set src if provided then call build and execute produced binary
            if let Some(s) = src {
                unsafe { std::env::set_var("OATS_SRC_FILE", s) };
            }
            // Build first
            if !quiet && (verbose || cfg!(debug_assertions)) {
                eprintln!("{}", "Building before run...".blue());
            }
            let prog = std::env::args()
                .next()
                .unwrap_or_else(|| "toasty".to_string());
            let build_src = std::env::var("OATS_SRC_FILE")
                .ok()
                .or_else(|| src_clone.clone());
            let build_argv: Vec<String> = if let Some(s) = build_src {
                vec![prog.clone(), s]
            } else {
                vec![prog.clone()]
            };
            let build_res = oatsc::builder::run_from_args(&build_argv)?;

            // Determine output exe path (builder uses OATS_OUT_DIR or current dir and input filename)
            let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());
            let src_path = std::env::var("OATS_SRC_FILE").unwrap_or_else(|_| {
                // fallback: try argv[1]
                std::env::args()
                    .nth(1)
                    .unwrap_or_else(|| "out.oats".to_string())
            });
            let src_filename = std::path::Path::new(&src_path)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("out");
            let exe_name = if let Some(name) = out_name {
                name
            } else {
                src_filename.to_string()
            };
            // Prefer the path returned by the builder if available
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
                anyhow::bail!("executed program returned non-zero exit code");
            }
            Ok(())
        }
    }
}
