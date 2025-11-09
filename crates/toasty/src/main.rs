use clap::Parser;
use toasty::cli::{Cli, Commands};
use toasty::diagnostics::{Result, ToastyError};
use toasty::{commands, preflight};

fn run_main() -> Result<()> {
    // Perform preflight checks to ensure required tools are available
    preflight::check_environment()?;

    let cli = Cli::parse();

    match &cli.cmd {
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
            quiet,
            color,
        } => commands::handle_build(
            &cli,
            commands::BuildCommandOptions {
                src: src.clone(),
                out_dir: out_dir.clone(),
                out_name: out_name.clone(),
                linker: linker.clone(),
                emit_object_only: *emit_object_only,
                no_link_runtime: *no_link_runtime,
                opt_level: opt_level.clone(),
                lto: lto.clone(),
                target_triple: target_triple.clone(),
                target_cpu: target_cpu.clone(),
                target_features: target_features.clone(),
                release: *release,
                quiet: *quiet,
                color: color.clone(),
            },
        ),
        Commands::Run {
            src,
            args,
            out_name,
            quiet,
            color,
        } => commands::handle_run(
            &cli,
            src.clone(),
            args.clone(),
            out_name.clone(),
            *quiet,
            color.clone(),
        ),
        Commands::New { name, lib, quiet } => commands::handle_new(name.clone(), *lib, *quiet),
        Commands::Compiler { action } => commands::handle_compiler((*action).clone()),
        Commands::Runtime { action } => commands::handle_runtime((*action).clone()),
        Commands::Package { action } => commands::handle_package((*action).clone()),
    }
}

/// Entry point for the CLI with clean error output handling
fn main_wrapper() -> i32 {
    if let Err(err) = run_toasty() {
        // For user code compilation failures, oatsc already printed diagnostics
        if err.is_user_code_error() {
            return 1;
        }

        // For build system errors, provide a clean cargo-style error message
        let red = "\x1b[31;1m";
        let yellow = "\x1b[33;1m";
        let green = "\x1b[32;1m";
        let reset = "\x1b[0m";

        eprintln!("{}error:{} {}", red, reset, err);

        // Provide context-specific help messages
        match &err {
            ToastyError::ManifestNotFound { path: _ } => {
                eprintln!(
                    "  {}help:{} Ensure you are in the root of an Oats project with Oats.toml",
                    green, reset
                );
                eprintln!("         Or specify a source file directly");
            }
            ToastyError::LinkerFailed { stderr, .. } => {
                eprintln!("  {}note:{} Linker output:", yellow, reset);
                for line in stderr.lines().take(10) {
                    eprintln!("    {}", line);
                }
                if stderr.lines().count() > 10 {
                    eprintln!("    ... (truncated)");
                }
            }
            ToastyError::Io { path: _, source } => {
                if source.kind() == std::io::ErrorKind::NotFound {
                    eprintln!(
                        "  {}help:{} Check that the file exists and the path is correct",
                        green, reset
                    );
                }
            }
            _ => {}
        }

        return 1;
    }

    0
}

/// Main CLI entry point
fn run_toasty() -> Result<()> {
    run_main()
}

fn main() {
    std::process::exit(main_wrapper());
}
