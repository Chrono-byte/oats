//! Package-based build orchestration
//!
//! This module implements the build orchestrator for package-based compilation,
//! managing dependency resolution, parallel compilation, and final linking.

use crate::diagnostics::{Result, ToastyError};
use crate::project::{NodeIndex, PackageGraph, build_package_graph, topological_sort_packages};
use petgraph::visit::EdgeRef;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Compilation options for invoking oatsc
#[derive(Debug, Clone)]
pub struct CompileOptions {
    pub src_file: String,
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
    pub fn for_package(package_root: std::path::PathBuf) -> Self {
        Self {
            src_file: String::new(), // Will be set later
            package_root: Some(package_root),
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: true,
            link_runtime: false,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }
}

/// Build result for a single package
#[derive(Debug, Clone)]
pub struct PackageBuildResult {
    /// Name of the package
    pub name: String,
    /// Path to the compiled object file
    pub object_file: PathBuf,
    /// Path to the package metadata file
    pub meta_file: PathBuf,
}

/// Build configuration
pub struct BuildConfig {
    pub verbose: bool,
    pub quiet: bool,
    pub release: bool,
    pub out_dir: Option<String>,
    pub out_name: Option<String>,
    pub linker: Option<String>,
    pub opt_level: Option<String>,
    pub lto: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub no_link_runtime: bool,
}

/// Orchestrate a package-based build starting from a root manifest
pub fn build_package_project(manifest_path: &Path, config: BuildConfig) -> Result<PathBuf> {
    // Build package dependency graph
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Building package dependency graph...");
    }

    let (graph, node_map, root_idx) = build_package_graph(manifest_path, config.verbose)?;

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Package graph has {} package(s)", graph.node_count());
    }

    // Perform topological sort to get build order
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Computing build order...");
    }

    let build_order = topological_sort_packages(&graph, &node_map)?;

    if config.verbose {
        eprintln!("Build order:");
        for (i, (name, _)) in build_order.iter().enumerate() {
            eprintln!("  {}. {}", i + 1, name);
        }
    }

    // Build each package in order
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Compiling packages...");
    }

    let mut build_results = HashMap::new();

    for (i, (pkg_name, pkg_idx)) in build_order.iter().enumerate() {
        if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
            eprintln!(
                "Compiling package {}/{}: {}",
                i + 1,
                build_order.len(),
                pkg_name
            );
        }

        let result = compile_package(&graph, *pkg_idx, &build_results, &config)?;
        build_results.insert(pkg_name.clone(), result);
    }

    // Link all packages together
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Linking final executable...");
    }

    let root_pkg_name = &graph[root_idx].name;
    link_packages(root_pkg_name, &build_results, &config)
}

/// Compile a single package
fn compile_package(
    graph: &PackageGraph,
    pkg_idx: NodeIndex,
    built_deps: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<PackageBuildResult> {
    let pkg_node = &graph[pkg_idx];
    let pkg_name = &pkg_node.name;
    let pkg_root = &pkg_node.root_dir;

    if config.verbose {
        eprintln!("  Package root: {}", pkg_root.display());
        eprintln!("  Entry point: {}", pkg_node.entry_point().display());
    }

    // Build compile options
    let mut options = CompileOptions::for_package(pkg_root.clone());

    // Add external package dependencies
    for edge in graph.edges(pkg_idx) {
        let dep_idx = edge.target();
        let dep_node = &graph[dep_idx];
        let dep_name = &dep_node.name;

        if let Some(dep_result) = built_deps.get(dep_name) {
            options.extern_pkg.insert(
                dep_name.clone(),
                dep_result.meta_file.to_string_lossy().to_string(),
            );
        }
    }

    // Set source file to entry point
    options.src_file = pkg_node.entry_point().to_string_lossy().to_string();

    // Configure output
    let out_dir = config.out_dir.as_deref().unwrap_or("target");
    let obj_filename = format!("{}_pkg.o", pkg_name.replace('-', "_"));
    let obj_path = PathBuf::from(out_dir).join(&obj_filename);
    options.out_dir = Some(out_dir.to_string());
    options.out_name = Some(format!("{}_pkg", pkg_name.replace('-', "_")));

    // Check for incremental build opportunity
    if let Ok(obj_metadata) = std::fs::metadata(&obj_path) {
        let obj_mtime = obj_metadata
            .modified()
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);

        // Check if source files are older than object file
        let mut needs_rebuild = false;

        // Check entry point
        if let Ok(src_metadata) = std::fs::metadata(pkg_node.entry_point())
            && let Ok(src_mtime) = src_metadata.modified()
            && src_mtime > obj_mtime
        {
            needs_rebuild = true;
        }

        // Check manifest if it exists
        let manifest_path = pkg_root.join("Oats.toml");
        if let Ok(manifest_metadata) = std::fs::metadata(&manifest_path)
            && let Ok(manifest_mtime) = manifest_metadata.modified()
            && manifest_mtime > obj_mtime
        {
            needs_rebuild = true;
        }

        // Check dependency metadata files
        for edge in graph.edges(pkg_idx) {
            let dep_idx = edge.target();
            let dep_node = &graph[dep_idx];
            if let Some(dep_result) = built_deps.get(&dep_node.name)
                && let Ok(dep_metadata) = std::fs::metadata(&dep_result.meta_file)
                && let Ok(dep_mtime) = dep_metadata.modified()
                && dep_mtime > obj_mtime
            {
                needs_rebuild = true;
            }
        }

        if !needs_rebuild {
            if config.verbose {
                eprintln!("  Skipping compilation - {} is up to date", obj_filename);
            }

            // Generate metadata file (may still need updating)
            let meta_file = PathBuf::from(format!(
                "{}/{}_pkg.oats.meta",
                out_dir,
                pkg_name.replace('-', "_")
            ));

            // Check if metadata needs updating
            let mut meta_needs_update = true;
            if let Ok(meta_metadata) = std::fs::metadata(&meta_file)
                && let Ok(meta_mtime) = meta_metadata.modified()
                && meta_mtime >= obj_mtime
            {
                meta_needs_update = false;
            }

            if meta_needs_update {
                // Ensure output directory exists
                if let Some(parent) = meta_file.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(&meta_file, e))?;
                }

                // Try to load manifest for package information
                let manifest_info = if let Ok(manifest) =
                    crate::project::Manifest::from_file(&pkg_root.join("Oats.toml"))
                {
                    format!(
                        "name = \"{}\"\nversion = \"{}\"\nentry = \"{}\"\n",
                        manifest.package.name, manifest.package.version, manifest.package.entry
                    )
                } else {
                    format!("name = \"{}\"\n", pkg_name)
                };

                std::fs::write(&meta_file, manifest_info)
                    .map_err(|e| ToastyError::io(&meta_file, e))?;
            }

            return Ok(PackageBuildResult {
                name: pkg_name.clone(),
                object_file: obj_path,
                meta_file,
            });
        }
    }

    // Apply build settings
    options.linker = config.linker.clone();
    options.link_runtime = !config.no_link_runtime;
    options.opt_level = config.opt_level.clone();
    options.lto = config.lto.clone();
    options.target_triple = config.target_triple.clone();
    options.target_cpu = config.target_cpu.clone();
    options.target_features = config.target_features.clone();
    options.build_profile = if config.release {
        Some("release".to_string())
    } else {
        None
    };

    // Compile the package
    let object_path = invoke_oatsc(&options)?;

    // Generate metadata file with package information
    let meta_file = PathBuf::from(format!(
        "{}/{}_pkg.oats.meta",
        out_dir,
        pkg_name.replace('-', "_")
    ));

    // Ensure output directory exists
    if let Some(parent) = meta_file.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(&meta_file, e))?;
    }

    // Try to load manifest for package information
    let manifest_info =
        if let Ok(manifest) = crate::project::Manifest::from_file(&pkg_root.join("Oats.toml")) {
            format!(
                "name = \"{}\"\nversion = \"{}\"\nentry = \"{}\"\n",
                manifest.package.name, manifest.package.version, manifest.package.entry
            )
        } else {
            format!("name = \"{}\"\n", pkg_name)
        };

    std::fs::write(&meta_file, manifest_info).map_err(|e| ToastyError::io(&meta_file, e))?;

    Ok(PackageBuildResult {
        name: pkg_name.clone(),
        object_file: object_path,
        meta_file,
    })
}

/// Invoke oatsc compiler as external command
fn invoke_oatsc(options: &CompileOptions) -> Result<PathBuf> {
    // Get oatsc path from environment (set by preflight check)
    let oatsc_path = std::env::var("OATS_OATSC_PATH").unwrap_or_else(|_| "oatsc".to_string());

    // Build command arguments
    let mut args = vec![];

    // Add source file (only in single-file mode, not package mode)
    if options.package_root.is_none() {
        args.push(options.src_file.clone());
    }

    // Add package root if specified
    if let Some(pkg_root) = &options.package_root {
        args.push("--package-root".to_string());
        args.push(pkg_root.to_string_lossy().to_string());
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
    let output = Command::new(&oatsc_path)
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
        // If stderr contains type errors, parsing errors, etc., treat as compilation failure
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

    // Parse output to get object file path
    // oatsc should print the output path on success
    let stdout = String::from_utf8_lossy(&output.stdout);
    let object_path = stdout.trim().to_string();

    if object_path.is_empty() {
        return Err(ToastyError::other("oatsc did not return output path"));
    }

    Ok(PathBuf::from(object_path))
}

/// Link all compiled packages into final executable
fn link_packages(
    root_pkg_name: &str,
    build_results: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<PathBuf> {
    let out_dir = config.out_dir.as_deref().unwrap_or(".");
    let exe_name = config
        .out_name
        .as_deref()
        .unwrap_or(&build_results[root_pkg_name].name);
    let exe_path = PathBuf::from(out_dir).join(exe_name);

    // Ensure runtime is available
    let runtime_lib = if let Ok(runtime_path) = std::env::var("OATS_RUNTIME_PATH") {
        // Use explicitly specified runtime path
        let runtime_lib = PathBuf::from(runtime_path);
        if !runtime_lib.exists() {
            return Err(ToastyError::io(
                &runtime_lib,
                std::io::Error::new(std::io::ErrorKind::NotFound, "Runtime library not found"),
            ));
        }
        runtime_lib
    } else {
        // Look for runtime in current directory or standard locations
        let runtime_lib = PathBuf::from("libruntime.a");

        if !runtime_lib.exists() {
            return Err(ToastyError::io(
                &runtime_lib,
                std::io::Error::new(std::io::ErrorKind::NotFound, "Runtime library not found"),
            ));
        }
        runtime_lib
    };

    // Build link command
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-o").arg(&exe_path);

    // Add all object files in dependency order
    for result in build_results.values() {
        link_cmd.arg(&result.object_file);
    }

    // Add runtime library if requested
    if !config.no_link_runtime {
        link_cmd.arg(&runtime_lib);
    }

    // Add linker if specified
    if let Some(linker) = &config.linker {
        link_cmd.arg("-fuse-ld").arg(linker);
    }

    if config.verbose {
        eprintln!("Link command: {:?}", link_cmd);
    }

    let output = link_cmd
        .output()
        .map_err(|e| ToastyError::other(format!("Failed to run clang linker: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(ToastyError::LinkerFailed {
            code: output.status.code(),
            stderr,
        });
    }

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Build finished: {}", exe_path.display());
    }

    Ok(exe_path)
}
