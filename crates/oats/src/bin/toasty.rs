use anyhow::Result;
use clap::Parser;

/// Toasty: A friendly wrapper around the Oats AOT runner.
#[derive(Parser)]
#[command(
    name = "toasty",
    about = "Compile Oats programs into native executables"
)]
struct Opts {
    /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
    src: Option<String>,

    /// Output directory for artifacts (defaults to current directory or OATS_OUT_DIR)
    #[arg(short, long)]
    out_dir: Option<String>,
}

fn main() -> Result<()> {
    // If user invoked via `toasty`, we delegate to the original `aot_run` logic
    // by setting expected environment variables and calling the original main
    // implementation. To minimize duplication we include the aot_run implementation
    // via a module include (to keep parity), but for simplicity we'll call into
    // the existing CLI file by reusing its logic.

    // Parse CLI args
    let opts = Opts::parse();

    if let Some(src) = opts.src {
        unsafe { std::env::set_var("OATS_SRC_FILE", src) };
    }
    if let Some(out) = opts.out_dir {
        unsafe { std::env::set_var("OATS_OUT_DIR", out) };
    }

    // Delegate to the shared builder implementation
    let argv: Vec<String> = std::env::args().collect();
    // If src was provided via CLI we already set OATS_SRC_FILE; keep argv so
    // builder can still read the first arg if needed.
    oats::builder::run_from_args(&argv)
}
