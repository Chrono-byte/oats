//! Package dependency graph construction and resolution
//!
//! This module implements package-level dependency resolution, creating a directed
//! graph of package dependencies (not individual files). Each node represents an
//! Oats package (defined by Oats.toml), and edges represent dependency relationships.

use anyhow::{Context, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::manifest::{Dependency, Manifest};

/// Package dependency graph
pub type PackageGraph = petgraph::Graph<PackageNode, (), petgraph::Directed>;

/// Node index in the package graph
pub type NodeIndex = petgraph::graph::NodeIndex;

/// Represents a package node in the dependency graph
#[derive(Debug, Clone)]
pub struct PackageNode {
    /// Package name
    pub name: String,
    /// Absolute path to the package root directory (where Oats.toml is located)
    pub root_dir: PathBuf,
    /// Parsed manifest
    pub manifest: Manifest,
}

impl PackageNode {
    /// Get the absolute path to the package entry point
    pub fn entry_point(&self) -> PathBuf {
        self.manifest.entry_point(&self.root_dir)
    }
}

/// Build a package dependency graph starting from a root package
///
/// # Arguments
/// * `root_manifest_path` - Path to the root Oats.toml file
/// * `verbose` - If true, print debug information
///
/// # Returns
/// A tuple of (graph, node map by package name, root node index)
pub fn build_package_graph(
    root_manifest_path: &Path,
    verbose: bool,
) -> Result<(PackageGraph, HashMap<String, NodeIndex>, NodeIndex)> {
    let mut graph = PackageGraph::new();
    let mut node_map: HashMap<String, NodeIndex> = HashMap::new();
    let mut path_to_node: HashMap<PathBuf, NodeIndex> = HashMap::new();

    // Load root manifest
    let root_manifest = Manifest::from_file(root_manifest_path).with_context(|| {
        format!(
            "Failed to load root manifest: {}",
            root_manifest_path.display()
        )
    })?;

    root_manifest
        .validate()
        .with_context(|| "Root manifest validation failed")?;

    let root_dir = root_manifest_path
        .parent()
        .ok_or_else(|| anyhow::anyhow!("Manifest path has no parent directory"))?
        .to_path_buf();

    if verbose {
        eprintln!("Building package graph from: {}", root_dir.display());
        eprintln!("  Root package: {}", root_manifest.package.name);
    }

    // Create root node
    let root_node = PackageNode {
        name: root_manifest.package.name.clone(),
        root_dir: root_dir.clone(),
        manifest: root_manifest.clone(),
    };

    let root_idx = graph.add_node(root_node);
    node_map.insert(root_manifest.package.name.clone(), root_idx);
    path_to_node.insert(root_dir.clone(), root_idx);

    // Queue for BFS traversal: (node_index, manifest_dir)
    let mut queue = std::collections::VecDeque::new();
    queue.push_back((root_idx, root_dir));

    // Build dependency graph
    while let Some((current_idx, current_dir)) = queue.pop_front() {
        // Clone the manifest to avoid borrowing issues
        let current_manifest = graph[current_idx].manifest.clone();
        let current_name = current_manifest.package.name.clone();

        if verbose {
            eprintln!("Processing package: {}", current_name);
        }

        // Process each dependency
        for (dep_name, dep_spec) in &current_manifest.dependencies {
            if verbose {
                eprintln!("  Checking dependency: {}", dep_name);
            }

            // Resolve dependency path
            let dep_path = match dep_spec {
                Dependency::Detailed(spec) => {
                    if let Some(ref path) = spec.path {
                        current_dir.join(path)
                    } else {
                        anyhow::bail!(
                            "Dependency '{}' in package '{}' has no path specified (external dependencies not yet supported)",
                            dep_name,
                            current_name
                        );
                    }
                }
                Dependency::Version(_) => {
                    anyhow::bail!(
                        "External dependencies not yet supported (dependency '{}' in package '{}')",
                        dep_name,
                        current_name
                    );
                }
            };

            // Canonicalize the dependency path
            let dep_dir = if dep_path.exists() {
                std::fs::canonicalize(&dep_path).with_context(|| {
                    format!("Failed to canonicalize path: {}", dep_path.display())
                })?
            } else {
                anyhow::bail!(
                    "Dependency path does not exist: {} (from package '{}')",
                    dep_path.display(),
                    current_name
                );
            };

            // Look for Oats.toml in dependency directory
            let dep_manifest_path = dep_dir.join("Oats.toml");
            if !dep_manifest_path.exists() {
                anyhow::bail!(
                    "No Oats.toml found in dependency '{}' at: {}",
                    dep_name,
                    dep_dir.display()
                );
            }

            // Check if we've already processed this package
            let dep_idx = if let Some(&existing_idx) = path_to_node.get(&dep_dir) {
                // Verify the package name matches
                let existing_node = &graph[existing_idx];
                if existing_node.name != *dep_name {
                    anyhow::bail!(
                        "Dependency name mismatch: package at {} is named '{}' but referenced as '{}'",
                        dep_dir.display(),
                        existing_node.name,
                        dep_name
                    );
                }
                existing_idx
            } else {
                // Load and validate the dependency manifest
                let dep_manifest = Manifest::from_file(&dep_manifest_path).with_context(|| {
                    format!("Failed to load manifest: {}", dep_manifest_path.display())
                })?;

                dep_manifest
                    .validate()
                    .with_context(|| format!("Manifest validation failed for '{}'", dep_name))?;

                // Verify the package name matches
                if dep_manifest.package.name != *dep_name {
                    anyhow::bail!(
                        "Package name mismatch: Oats.toml at {} declares name '{}' but is referenced as '{}'",
                        dep_dir.display(),
                        dep_manifest.package.name,
                        dep_name
                    );
                }

                if verbose {
                    eprintln!(
                        "    Added new package: {} at {}",
                        dep_name,
                        dep_dir.display()
                    );
                }

                // Create new node
                let dep_node = PackageNode {
                    name: dep_manifest.package.name.clone(),
                    root_dir: dep_dir.clone(),
                    manifest: dep_manifest,
                };

                let new_idx = graph.add_node(dep_node);
                node_map.insert(dep_name.clone(), new_idx);
                path_to_node.insert(dep_dir.clone(), new_idx);

                // Queue for processing
                queue.push_back((new_idx, dep_dir));

                new_idx
            };

            // Add edge: current depends on dep
            graph.add_edge(current_idx, dep_idx, ());
        }
    }

    if verbose {
        eprintln!("Package graph built with {} packages", graph.node_count());
    }

    Ok((graph, node_map, root_idx))
}

/// Perform topological sort on the package graph to determine build order
///
/// Returns packages in dependency-first order (dependencies before dependents)
pub fn topological_sort_packages(
    graph: &PackageGraph,
    node_map: &HashMap<String, NodeIndex>,
) -> Result<Vec<(String, NodeIndex)>> {
    match petgraph::algo::toposort(graph, None) {
        Ok(sorted_nodes) => {
            // Create reverse mapping
            let mut index_to_name = HashMap::new();
            for (name, &idx) in node_map {
                index_to_name.insert(idx, name.clone());
            }

            // Convert to (name, index) pairs and reverse (dependencies first)
            let mut sorted: Vec<(String, NodeIndex)> = sorted_nodes
                .into_iter()
                .filter_map(|idx| index_to_name.get(&idx).map(|name| (name.clone(), idx)))
                .collect();

            sorted.reverse(); // Dependencies come first
            Ok(sorted)
        }
        Err(cycle) => {
            anyhow::bail!(
                "Circular package dependency detected involving node {:?}",
                cycle.node_id()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_package(
        dir: &Path,
        name: &str,
        deps: &[(&str, &str)], // (dep_name, relative_path)
    ) -> Result<()> {
        // Create Oats.toml
        let mut manifest = format!(
            r#"[package]
name = "{}"
version = "0.1.0"

[dependencies]
"#,
            name
        );

        for (dep_name, dep_path) in deps {
            manifest.push_str(&format!(r#"{} = {{ path = "{}" }}"#, dep_name, dep_path));
            manifest.push('\n');
        }

        fs::write(dir.join("Oats.toml"), manifest)?;

        // Create src directory and entry point
        fs::create_dir_all(dir.join("src"))?;
        fs::write(
            dir.join("src/main.oats"),
            format!("// Entry point for {}\n", name),
        )?;

        Ok(())
    }

    #[test]
    fn test_simple_package_graph() -> Result<()> {
        let temp = TempDir::new()?;
        let root = temp.path();

        // Create a simple dependency: app -> lib
        let lib_dir = root.join("lib");
        fs::create_dir(&lib_dir)?;
        create_test_package(&lib_dir, "mylib", &[])?;

        let app_dir = root.join("app");
        fs::create_dir(&app_dir)?;
        create_test_package(&app_dir, "myapp", &[("mylib", "../lib")])?;

        // Build graph
        let manifest_path = app_dir.join("Oats.toml");
        let (graph, node_map, _root_idx) = build_package_graph(&manifest_path, false)?;

        assert_eq!(graph.node_count(), 2);
        assert_eq!(node_map.len(), 2);
        assert!(node_map.contains_key("myapp"));
        assert!(node_map.contains_key("mylib"));

        // Check topological sort
        let sorted = topological_sort_packages(&graph, &node_map)?;
        assert_eq!(sorted.len(), 2);
        assert_eq!(sorted[0].0, "mylib"); // Dependency first
        assert_eq!(sorted[1].0, "myapp"); // Dependent second

        Ok(())
    }

    #[test]
    fn test_circular_dependency_detection() -> Result<()> {
        let temp = TempDir::new()?;
        let root = temp.path();

        // Create circular dependency: a -> b -> a
        let a_dir = root.join("a");
        fs::create_dir(&a_dir)?;

        let b_dir = root.join("b");
        fs::create_dir(&b_dir)?;

        // Create b first (depends on nothing initially)
        create_test_package(&b_dir, "b", &[])?;

        // Create a (depends on b)
        create_test_package(&a_dir, "a", &[("b", "../b")])?;

        // Update b to depend on a (creating cycle)
        create_test_package(&b_dir, "b", &[("a", "../a")])?;

        // Try to build graph - should detect cycle
        let manifest_path = a_dir.join("Oats.toml");
        let result = build_package_graph(&manifest_path, false);

        // The graph will build, but topological sort should fail
        if let Ok((graph, node_map, _)) = result {
            let sort_result = topological_sort_packages(&graph, &node_map);
            assert!(sort_result.is_err());
        }

        Ok(())
    }
}
