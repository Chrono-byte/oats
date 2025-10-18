//! Package-based build orchestration
//!
//! This module implements the build orchestrator for package-based compilation,
//! managing dependency resolution, parallel compilation, and final linking.

use anyhow::{Context, Result};
use petgraph::visit::EdgeRef;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::package_graph::{build_package_graph, topological_sort_packages, PackageGraph, NodeIndex};

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
}

/// Orchestrate a package-based build starting from a root manifest
pub fn build_package_project(
    manifest_path: &Path,
    config: BuildConfig,
) -> Result<PathBuf> {
    // Build package dependency graph
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Building package dependency graph...");
    }

    let (graph, node_map, root_idx) = build_package_graph(manifest_path, config.verbose)?;

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!(
            "Package graph has {} package(s)",
            graph.node_count()
        );
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

        let result = compile_package(
            &graph,
            *pkg_idx,
            &build_results,
            &config,
        )?;

        build_results.insert(pkg_name.clone(), result);
    }

    // Link all packages together
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Linking final executable...");
    }

    let root_pkg_name = &graph[root_idx].name;
    link_packages(
        root_pkg_name,
        &build_results,
        &config,
    )
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
    let mut options = oatsc::CompileOptions::for_package(pkg_root.clone());

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
    options.out_dir = Some(out_dir.to_string());
    options.out_name = Some(format!("{}_pkg", pkg_name.replace('-', "_")));
    options.emit_object_only = true; // Always emit objects in package mode

    // Apply build settings
    options.linker = config.linker.clone();
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
    let object_path = oatsc::compile(options)
        .with_context(|| format!("Failed to compile package '{}'", pkg_name))?
        .ok_or_else(|| anyhow::anyhow!("Compiler did not return output path"))?;

    // Generate metadata file (placeholder for now)
    let meta_file = PathBuf::from(format!("{}/{}_pkg.oats.meta", out_dir, pkg_name.replace('-', "_")));
    std::fs::write(&meta_file, format!("# Package metadata for {}\n", pkg_name))
        .with_context(|| format!("Failed to write metadata file: {}", meta_file.display()))?;

    Ok(PackageBuildResult {
        name: pkg_name.clone(),
        object_file: PathBuf::from(object_path),
        meta_file,
    })
}

/// Link all compiled packages into final executable
fn link_packages(
    root_pkg_name: &str,
    build_results: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<PathBuf> {
    let out_dir = config.out_dir.as_deref().unwrap_or(".");
    let exe_name = config.out_name.as_deref().unwrap_or(root_pkg_name);
    let exe_path = PathBuf::from(out_dir).join(exe_name);

    // Ensure runtime is built
    let runtime_lib = "target/release/libruntime.a";
    if !PathBuf::from(runtime_lib).exists() {
        if !config.quiet {
            eprintln!("Building Oats runtime...");
        }
        let status = Command::new("cargo")
            .arg("build")
            .arg("-p")
            .arg("runtime")
            .arg("--release")
            .status()
            .context("Failed to run cargo to build runtime")?;
        
        if !status.success() {
            anyhow::bail!("Failed to build runtime library");
        }
    }

    // Build link command
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-o").arg(&exe_path);

    // Add all object files in dependency order
    for result in build_results.values() {
        link_cmd.arg(&result.object_file);
    }

    // Add runtime library
    link_cmd.arg(runtime_lib);

    // Add linker if specified
    if let Some(linker) = &config.linker {
        link_cmd.arg(format!("-fuse-ld={}", linker));
    }

    if config.verbose {
        eprintln!("Link command: {:?}", link_cmd);
    }

    let link_status = link_cmd.status()
        .context("Failed to run clang linker")?;

    if !link_status.success() {
        anyhow::bail!("Linking failed");
    }

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Build finished: {}", exe_path.display());
    }

    Ok(exe_path)
}
