//! Linking and binary generation.
//!
//! This module handles the final stages of compilation: generating object files
//! from LLVM IR and linking them with the runtime library to produce executables.

use anyhow::Result;
use inkwell::OptimizationLevel;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use std::path::Path;
use std::process::Command;

/// Configuration for object file generation and linking.
pub struct LinkingConfig {
    pub out_dir: String,
    pub out_obj: String,
    pub out_exe: String,
    pub opt_level: OptimizationLevel,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub build_profile: String,
    pub lto: Option<String>,
    pub linker: Option<String>,
    pub emit_object_only: bool,
}

/// Compiles LLVM IR to an object file in memory.
///
/// # Arguments
/// * `module` - LLVM module containing the IR
/// * `config` - Linking configuration
///
/// # Returns
/// Bytes of the generated object file
pub fn compile_to_object_bytes(module: &Module, config: &LinkingConfig) -> Result<Vec<u8>> {
    // Initialize native target
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| anyhow::anyhow!("Failed to initialize LLVM target: {e}"))?;

    // Get target triple
    let triple = if let Some(ref triple_str) = config.target_triple {
        inkwell::targets::TargetTriple::create(triple_str)
    } else {
        TargetMachine::get_default_triple()
    };

    // Determine CPU and features
    let cpu_candidates = if let Some(ref c) = config.target_cpu {
        vec![c.clone(), String::new()]
    } else {
        vec![String::new(), "native".to_string()]
    };
    let features = config.target_features.clone().unwrap_or_default();

    // Get the target from the triple
    let target = Target::from_triple(&triple)
        .map_err(|e| anyhow::anyhow!("Failed to get target from triple: {e}"))?;

    // Create TargetMachine, trying CPU candidates in order
    let mut tm = None;
    for cpu in &cpu_candidates {
        if let Some(machine) = target.create_target_machine(
            &triple,
            cpu,
            &features,
            config.opt_level,
            RelocMode::Default,
            CodeModel::Default,
        ) {
            tm = Some(machine);
            break;
        }
    }
    let tm =
        tm.ok_or_else(|| anyhow::anyhow!("Failed to create TargetMachine with any CPU candidate"))?;

    // Note: LLVM optimizations are applied via clang -O3 during linking phase.
    // TargetMachine's write_to_memory_buffer will also apply some optimizations
    // based on the opt_level parameter provided during creation.
    //
    // TODO: For LLVM 20, direct pass manager integration requires either:
    // 1. Using llvm_sys bindings directly for new pass manager API
    // 2. Upgrading to inkwell version with LLVM 20 pass manager support
    //
    // Function attributes like 'nounwind' have been added during code generation
    // to help the optimizer.

    // Emit object file to memory buffer
    let memory_buffer = tm
        .write_to_memory_buffer(module, FileType::Object)
        .map_err(|e| {
            anyhow::anyhow!("TargetMachine failed to emit object file to memory: {e:?}")
        })?;

    // Convert memory buffer to Vec<u8>
    let obj_bytes = memory_buffer.as_slice().to_vec();

    Ok(obj_bytes)
}

/// Compiles LLVM IR to an object file on disk.
///
/// # Arguments
/// * `module` - LLVM module containing the IR
/// * `config` - Linking configuration
///
/// # Returns
/// Path to the generated object file
pub fn compile_to_object(module: &Module, config: &LinkingConfig) -> Result<String> {
    // Compile to bytes first
    let obj_bytes = compile_to_object_bytes(module, config)?;

    // Ensure output directory exists
    std::fs::create_dir_all(&config.out_dir).map_err(|e| {
        anyhow::anyhow!(
            "Failed to create output directory {}: {}",
            config.out_dir,
            e
        )
    })?;

    // Write bytes to file
    std::fs::write(&config.out_obj, obj_bytes)
        .map_err(|e| anyhow::anyhow!("Failed to write object file {}: {}", config.out_obj, e))?;

    Ok(config.out_obj.clone())
}

/// Returns paths to the runtime libraries (runtime and std).
///
/// Looks for libraries in the following order:
/// 1. `OATS_RUNTIME_PATH` / `OATS_STD_PATH` environment variables
/// 2. Current directory (libruntime.so / `liboats_std.so`)
/// 3. Standard library directories (/usr/lib, /usr/local/lib)
pub fn get_runtime_libraries() -> Result<Vec<String>> {
    let mut libs = Vec::new();

    // 1. Runtime Library
    if let Ok(path) = std::env::var("OATS_RUNTIME_PATH") {
        libs.push(path);
    } else {
        // Try current directory first, then standard library directories
        let candidates = [
            "libruntime.so",
            "/usr/lib/libruntime.so",
            "/usr/local/lib/libruntime.so",
        ];
        let mut found = false;
        for candidate in &candidates {
            if std::path::Path::new(candidate).exists() {
                libs.push(candidate.to_string());
                found = true;
                break;
            }
        }
        if !found {
            anyhow::bail!(
                "Could not find libruntime.so. Set OATS_RUNTIME_PATH or install the runtime library."
            );
        }
    }

    // 2. Standard Library
    if let Ok(path) = std::env::var("OATS_STD_PATH") {
        libs.push(path);
    } else {
        // Try current directory first, then standard library directories
        let candidates = [
            "liboats_std.so",
            "/usr/lib/liboats_std.so",
            "/usr/local/lib/liboats_std.so",
        ];
        let mut found = false;
        for candidate in &candidates {
            if std::path::Path::new(candidate).exists() {
                libs.push(candidate.to_string());
                found = true;
                break;
            }
        }
        if !found {
            anyhow::bail!(
                "Could not find liboats_std.so. Set OATS_STD_PATH or install the standard library."
            );
        }
    }

    Ok(libs)
}

/// Locates the runtime main object file.
///
/// # Arguments
/// * `out_dir` - Output directory (unused, kept for API compatibility)
/// * `emitted_host_main` - Whether host main was emitted into the module
///
/// # Returns
/// Path to `rt_main.o`, or empty string if not needed
pub fn locate_rt_main(_out_dir: &str, emitted_host_main: bool) -> Result<String> {
    if emitted_host_main {
        return Ok(String::new());
    }

    // Look for rt_main.o in standard locations
    let candidates = [
        "rt_main.o",
        "/usr/lib/rt_main.o",
        "/usr/local/lib/rt_main.o",
    ];
    for candidate in &candidates {
        if Path::new(candidate).exists() {
            return Ok(candidate.to_string());
        }
    }

    anyhow::bail!("rt_main.o not found. Please install the Oats runtime or set the correct path.");
}

/// Links object files into a final executable.
///
/// # Arguments
/// * `config` - Linking configuration
/// * `out_obj` - Path to the main object file
/// * `rust_libs` - Paths to the runtime libraries
/// * `rt_main_obj` - Path to `rt_main.o` (empty if not needed)
///
/// # Returns
/// Path to the generated executable
pub fn link_executable(
    config: &LinkingConfig,
    out_obj: &str,
    rust_libs: &[String],
    rt_main_obj: &str,
) -> Result<String> {
    // Helper to test whether a program is runnable
    fn is_prog_available(name: &str) -> bool {
        use std::process::Stdio;
        let verbose = std::env::var("TOASTY_VERBOSE").is_ok();
        let status = if verbose {
            Command::new(name).arg("--version").status()
        } else {
            Command::new(name)
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        };
        match status {
            Ok(s) => s.success(),
            Err(_) => false,
        }
    }

    // Detect available tools
    let clang_candidates = ["clang", "clang-18", "clang-17"];
    let mut found_clang: Option<String> = None;
    for &c in &clang_candidates {
        if is_prog_available(c) {
            found_clang = Some(c.to_string());
            break;
        }
    }
    let lld_candidate = if is_prog_available("ld.lld") || is_prog_available("lld") {
        Some("lld".to_string())
    } else {
        None
    };

    let mut linked_ok = false;
    let mut link_run_err: Option<std::io::Error> = None;

    if let Some(ref linker) = config.linker {
        // Try the user-specified linker
        let mut cmd = Command::new(linker);
        if !rt_main_obj.is_empty() {
            cmd.arg(rt_main_obj);
        }
        cmd.arg(out_obj);
        for lib in rust_libs {
            cmd.arg(lib);
        }

        // Add RPATH to find libraries in current directory
        cmd.arg("-rpath");
        cmd.arg(".");

        // Add native system libraries required by oats_std (e.g. OpenSSL)
        cmd.arg("-lssl");
        cmd.arg("-lcrypto");
        cmd.arg("-lpthread");
        cmd.arg("-ldl");
        cmd.arg("-lm");

        cmd.arg("-o").arg(&config.out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{linker} failed to link final binary");
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(ref clang_bin) = found_clang {
        // Use clang
        let mut cmd = Command::new(clang_bin);

        // Add optimization flags
        let host_opt_level = config.opt_level;
        if matches!(host_opt_level, OptimizationLevel::Aggressive)
            || config.build_profile == "release"
        {
            cmd.arg("-O3");
        }

        // Add LTO flags (only if LTO is enabled, skip for "none")
        let lto_mode = config.lto.as_deref().unwrap_or("none");
        if lto_mode != "none" {
            cmd.arg(format!("-flto={lto_mode}"));
        }

        if let Some(ref lld) = lld_candidate {
            cmd.arg(format!("-fuse-ld={lld}"));
        }

        if !rt_main_obj.is_empty() {
            cmd.arg(rt_main_obj);
        }
        cmd.arg(out_obj);
        for lib in rust_libs {
            cmd.arg(lib);
        }

        // Add RPATH to find libraries in current directory
        cmd.arg("-Wl,-rpath,.");

        // Add native system libraries required by oats_std (e.g. OpenSSL)
        cmd.arg("-lssl");
        cmd.arg("-lcrypto");
        cmd.arg("-lpthread");
        cmd.arg("-ldl");
        cmd.arg("-lm");

        cmd.arg("-o").arg(&config.out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{clang_bin} failed to link final binary");
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(ref lld_bin) = lld_candidate {
        // Last resort: call lld directly
        let mut cmd = Command::new(lld_bin);
        if !rt_main_obj.is_empty() {
            cmd.arg(rt_main_obj);
        }
        cmd.arg(out_obj);
        for lib in rust_libs {
            cmd.arg(lib);
        }

        // Add RPATH to find libraries in current directory
        cmd.arg("-rpath");
        cmd.arg(".");

        // Add native system libraries required by oats_std (e.g. OpenSSL)
        cmd.arg("-lssl");
        cmd.arg("-lcrypto");
        cmd.arg("-lpthread");
        cmd.arg("-ldl");
        cmd.arg("-lm");

        cmd.arg("-o").arg(&config.out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{lld_bin} failed to link final binary");
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else {
        anyhow::bail!(
            "No suitable linker found: please install clang or lld, or set OATS_LINKER to a linker path"
        );
    }

    if !linked_ok {
        if let Some(e) = link_run_err {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::bail!(
                    "linker not found in PATH; install clang or lld, or set OATS_LINKER to a path"
                );
            }
            return Err(e.into());
        }
        anyhow::bail!("failed to link final binary");
    }

    Ok(config.out_exe.clone())
}
