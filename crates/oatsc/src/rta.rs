//! Rapid Type Analysis (RTA) for the Oats compiler.
//!
//! This module implements David Bacon's Rapid Type Analysis algorithm to enable
//! ahead-of-time optimizations such as devirtualization of method calls and dead
//! code elimination. RTA operates on the complete program AST to determine which
//! classes and methods are reachable at runtime.
//!
//! # Algorithm Overview
//!
//! 1. **Class Hierarchy Analysis**: Build a graph of class inheritance relationships
//! 2. **Instantiation Analysis**: Find all explicit class instantiations to seed the live set
//! 3. **Call Graph Construction**: Build a graph of method calls and their potential targets
//! 4. **Worklist Propagation**: Iteratively discover all reachable methods and classes
//!
//! # Integration
//!
//! RTA is inserted between type checking and code generation in the compiler pipeline.
//! The analysis results are consumed by the code generator to perform optimizations.

use crate::parser::ParsedModule;
use oats_ast::*;
use std::collections::{HashMap, HashSet, VecDeque};

/// Represents the class hierarchy as a directed graph.
/// Key: class name, Value: list of direct subclasses
pub type ClassHierarchy = HashMap<String, Vec<String>>;

/// Reverse lookup: maps class name to its parent class (None if no parent)
pub type ParentMap = HashMap<String, Option<String>>;

/// Set of live (instantiated) classes
pub type LiveClasses = HashSet<String>;

/// Set of live (reachable) methods
/// Key: class name, Value: set of method names
pub type LiveMethods = HashMap<String, HashSet<String>>;

/// Call graph representing method calls
/// Key: caller method (class::method), Value: list of potential callees
pub type CallGraph = HashMap<String, Vec<String>>;

/// Main RTA analysis result
pub struct RTAResults {
    pub hierarchy: ClassHierarchy,
    pub live_classes: LiveClasses,
    pub live_methods: LiveMethods,
    pub call_graph: CallGraph,
}

impl RTAResults {
    /// Check if a class is live (instantiated at runtime).
    pub fn is_class_live(&self, class_name: &str) -> bool {
        self.live_classes.contains(class_name)
    }

    /// Check if a method is live (reachable at runtime).
    ///
    /// # Arguments
    /// * `class_name` - Name of the class
    /// * `method_name` - Name of the method
    ///
    /// # Returns
    /// `true` if the method is live, `false` otherwise
    pub fn is_method_live(&self, class_name: &str, method_name: &str) -> bool {
        self.live_methods
            .get(class_name)
            .map(|methods| methods.contains(method_name))
            .unwrap_or(false)
    }

    /// Check if a function is live (reachable at runtime).
    ///
    /// Uses call graph analysis to determine reachability from entry points.
    /// Entry points include:
    /// - `main` function
    /// - All exported functions
    /// - All class constructors (since classes can be instantiated)
    ///
    /// # Arguments
    /// * `function_name` - Name of the function to check
    ///
    /// # Returns
    /// `true` if the function is live, `false` otherwise
    pub fn is_function_live(&self, function_name: &str) -> bool {
        // Entry points: main and exported functions are always live
        if function_name == "main" {
            return true;
        }

        // Check if function is reachable from any entry point via call graph
        // We do a simple reachability analysis: if any live function calls this one,
        // or if this function is called by any live function, it's live.
        // Start from entry points and propagate reachability.
        let mut visited = HashSet::new();
        let mut worklist = VecDeque::new();

        // Add entry points: main and all functions that are called
        // For now, we consider main as the only entry point
        // In the future, we could track exported functions
        worklist.push_back("main".to_string());
        visited.insert("main".to_string());

        // Also consider all class constructors as entry points
        for class_name in &self.live_classes {
            let ctor_name = format!("{}_ctor", class_name);
            if !visited.contains(&ctor_name) {
                worklist.push_back(ctor_name.clone());
                visited.insert(ctor_name);
            }
        }

        // Propagate reachability through call graph
        while let Some(caller) = worklist.pop_front() {
            if caller == function_name {
                return true;
            }

            // Check all functions this caller calls
            if let Some(callees) = self.call_graph.get(&caller) {
                for callee in callees {
                    if !visited.contains(callee) {
                        visited.insert(callee.clone());
                        worklist.push_back(callee.clone());
                    }
                }
            }
        }

        // If not found through call graph, check if it's a method of a live class
        if let Some((class_name, _)) = self.split_method_name(function_name) {
            return self.live_classes.contains(&class_name);
        }

        // Default: consider top-level functions as live for safety
        // This is conservative - we could make it more aggressive in the future
        true
    }

    /// Split a method name in format "class_method" into (class, method).
    /// Returns None if it's not a method name.
    fn split_method_name(&self, name: &str) -> Option<(String, String)> {
        if let Some(last_underscore) = name.rfind('_') {
            if last_underscore > 0 && last_underscore < name.len() - 1 {
                let class = name[..last_underscore].to_string();
                let method = name[last_underscore + 1..].to_string();
                return Some((class, method));
            }
        }
        None
    }

    /// Check if a method call can be devirtualized (has only one possible target).
    ///
    /// Devirtualization allows direct method calls instead of virtual dispatch,
    /// which improves performance. This uses the call graph to determine if
    /// a method call has a single target.
    ///
    /// # Arguments
    /// * `class_name` - Name of the class
    /// * `method_name` - Name of the method
    ///
    /// # Returns
    /// `Some(target_method_name)` if devirtualization is possible, `None` otherwise
    pub fn can_devirtualize(&self, class_name: &str, method_name: &str) -> Option<String> {
        // Check if this method is only implemented in one class
        // For now, we do a simple check: if the method is only in one live class
        let mut found_classes = Vec::new();

        // Check the class itself
        if self.is_method_live(class_name, method_name) {
            found_classes.push(class_name.to_string());
        }

        // Check subclasses (if any)
        if let Some(subclasses) = self.hierarchy.get(class_name) {
            for subclass in subclasses {
                if self.is_method_live(subclass, method_name) {
                    found_classes.push(subclass.clone());
                }
            }
        }

        // If only one class has this method live, we can devirtualize
        if found_classes.len() == 1 {
            return Some(format!("{}_{}", found_classes[0], method_name));
        }

        None
    }

    /// Get all possible targets for a method call (for virtual dispatch).
    ///
    /// Returns a list of method names that could be called for this method invocation.
    /// This is useful for generating efficient virtual dispatch code.
    ///
    /// # Arguments
    /// * `class_name` - Name of the class
    /// * `method_name` - Name of the method
    ///
    /// # Returns
    /// Vector of possible target method names in format "class_method"
    pub fn get_method_call_targets(&self, class_name: &str, method_name: &str) -> Vec<String> {
        let mut targets = Vec::new();

        // Check if the method exists in the class
        if self.is_method_live(class_name, method_name) {
            targets.push(format!("{}_{}", class_name, method_name));
        }

        // Check subclasses
        if let Some(subclasses) = self.hierarchy.get(class_name) {
            for subclass in subclasses {
                if self.is_method_live(subclass, method_name) {
                    targets.push(format!("{}_{}", subclass, method_name));
                }
            }
        }

        targets
    }

    /// Get all live methods for a class.
    pub fn get_live_methods(&self, class_name: &str) -> Option<&HashSet<String>> {
        self.live_methods.get(class_name)
    }
}

/// Extracts the class name from a method name in format "class_method"
/// Returns None if it's not a method name (doesn't contain underscore)
fn extract_class_from_method_name(method_name: &str) -> Option<String> {
    if let Some(last_underscore) = method_name.rfind('_') {
        if last_underscore > 0 && last_underscore < method_name.len() - 1 {
            Some(method_name[..last_underscore].to_string())
        } else {
            None
        }
    } else {
        None
    }
}

/// Performs Rapid Type Analysis on the given modules
pub fn analyze(modules: &HashMap<String, ParsedModule>) -> RTAResults {
    let (hierarchy, _parent_map) = build_class_hierarchy(modules);
    let live_classes = find_instantiations(modules);
    let methods = collect_methods(modules);

    // Collect all function ASTs for worklist processing
    let functions = collect_function_asts(modules);

    // Use worklist algorithm for more precise analysis
    let live_methods = run_worklist(&live_classes, &methods, &functions);

    // Build call graph by analyzing all functions and methods
    let call_graph = build_call_graph(modules, &methods);

    RTAResults {
        hierarchy,
        live_classes,
        live_methods,
        call_graph,
    }
}

/// Builds the class hierarchy by traversing all class declarations
/// Returns both the hierarchy (parent -> subclasses) and parent map (class -> parent)
fn build_class_hierarchy(modules: &HashMap<String, ParsedModule>) -> (ClassHierarchy, ParentMap) {
    let mut hierarchy: ClassHierarchy = HashMap::new();
    let mut parent_map: ParentMap = HashMap::new();

    for module in modules.values() {
        for stmt in &module.parsed.body {
            if let Stmt::ClassDecl(class_decl) = stmt {
                let class_name = class_decl.ident.sym.clone();

                // Check for extends clause
                if let Some(super_class) = &class_decl.super_class {
                    if let Expr::Ident(super_ident) = super_class {
                        let super_name = super_ident.sym.clone();
                        hierarchy
                            .entry(super_name.clone())
                            .or_default()
                            .push(class_name.clone());
                        parent_map.insert(class_name.clone(), Some(super_name));
                    } else {
                        parent_map.insert(class_name.clone(), None);
                    }
                } else {
                    // Root class, ensure it's in the map
                    hierarchy.entry(class_name.clone()).or_default();
                    parent_map.insert(class_name, None);
                }
            }
        }
    }

    (hierarchy, parent_map)
}

/// Finds all explicit class instantiations to form the initial live set
fn find_instantiations(modules: &HashMap<String, ParsedModule>) -> LiveClasses {
    let mut live_classes = HashSet::new();

    for module in modules.values() {
        for stmt in &module.parsed.body {
            visit_stmt_for_instantiations(stmt, &mut live_classes);
        }
    }

    live_classes
}

fn visit_stmt_for_instantiations(stmt: &Stmt, live_classes: &mut LiveClasses) {
    match stmt {
        Stmt::ExprStmt(expr_stmt) => visit_expr_for_instantiations(&expr_stmt.expr, live_classes),
        Stmt::VarDecl(vd) => {
            for decl in &vd.decls {
                if let Some(init) = &decl.init {
                    visit_expr_for_instantiations(init, live_classes);
                }
            }
        }
        Stmt::FnDecl(fn_decl) => {
            if let Some(body) = &fn_decl.body {
                for stmt in &body.stmts {
                    visit_stmt_for_instantiations(stmt, live_classes);
                }
            }
        }
        Stmt::ClassDecl(class_decl) => {
            // Visit class body for instantiations
            for member in &class_decl.body {
                if let ClassMember::Method(method) = member
                    && let Some(body) = &method.body
                {
                    for stmt in &body.stmts {
                        visit_stmt_for_instantiations(stmt, live_classes);
                    }
                }
            }
        }
        _ => {}
    }
}

fn visit_expr_for_instantiations(expr: &Expr, live_classes: &mut LiveClasses) {
    if let Expr::New(new_expr) = expr
        && let Expr::Ident(ident) = &*new_expr.callee
    {
        live_classes.insert(ident.sym.clone());
    }
    // Recursively visit children
    if let Expr::New(new_expr) = expr {
        visit_expr_for_instantiations(&new_expr.callee, live_classes);
        for arg in &new_expr.args {
            visit_expr_for_instantiations(arg, live_classes);
        }
    }
}

/// Collects all defined methods for each class
fn collect_methods(modules: &HashMap<String, ParsedModule>) -> HashMap<String, HashSet<String>> {
    let mut methods: HashMap<String, HashSet<String>> = HashMap::new();

    for module in modules.values() {
        for stmt in &module.parsed.body {
            if let Stmt::ClassDecl(class_decl) = stmt {
                let class_name = class_decl.ident.sym.clone();
                let class_methods = methods.entry(class_name).or_default();

                for member in &class_decl.body {
                    if let ClassMember::Method(method) = member {
                        // Get method name
                        class_methods.insert(method.ident.sym.clone());
                    }
                }
            }
        }
    }

    methods
}

/// Builds a call graph by analyzing all functions and methods
fn build_call_graph(
    modules: &HashMap<String, ParsedModule>,
    methods: &HashMap<String, HashSet<String>>,
) -> CallGraph {
    let mut call_graph: CallGraph = HashMap::new();
    let functions = collect_function_asts(modules);

    // Analyze each function/method to find calls
    for (caller_name, func) in &functions {
        let mut callees = Vec::new();
        let method_calls = find_method_calls(func);

        // Extract class context from caller_name if it's a method (format: "class_method")
        let _class_context = extract_class_from_method_name(caller_name);

        // Convert method calls to callee names in format "class_method"
        for (class_name, method_name) in method_calls {
            // Check if this class has this method
            if let Some(class_methods) = methods.get(&class_name)
                && class_methods.contains(&method_name)
            {
                let callee_name = format!("{}_{}", class_name, method_name);
                callees.push(callee_name);
            }
        }

        if !callees.is_empty() {
            call_graph.insert(caller_name.clone(), callees);
        }
    }

    call_graph
}

/// Collects all function and method ASTs for worklist processing
fn collect_function_asts(modules: &HashMap<String, ParsedModule>) -> HashMap<String, Function> {
    let mut functions = HashMap::new();

    for module in modules.values() {
        for stmt in &module.parsed.body {
            if let Stmt::FnDecl(fn_decl) = stmt {
                let name = fn_decl.ident.sym.clone();
                let func = Function {
                    params: fn_decl.params.clone(),
                    body: fn_decl.body.clone(),
                    return_type: fn_decl.return_type.clone(),
                    span: fn_decl.span.clone(),
                    is_async: fn_decl.is_async,
                    is_generator: fn_decl.is_generator,
                };
                functions.insert(name, func);
            }
        }
    }

    // Also collect class methods
    for module in modules.values() {
        for stmt in &module.parsed.body {
            if let Stmt::ClassDecl(class_decl) = stmt {
                let class_name = class_decl.ident.sym.clone();
                for member in &class_decl.body {
                    if let ClassMember::Method(method) = member {
                        let method_name = format!("{}_{}", class_name, method.ident.sym);
                        let func = Function {
                            params: method.params.clone(),
                            body: method.body.clone(),
                            return_type: method.return_type.clone(),
                            span: method.span.clone(),
                            is_async: false,
                            is_generator: false,
                        };
                        functions.insert(method_name, func);
                    }
                }
            }
        }
    }

    functions
}

/// Runs the RTA worklist algorithm to find precisely live methods
fn run_worklist(
    live_classes: &LiveClasses,
    methods: &HashMap<String, HashSet<String>>,
    functions: &HashMap<String, Function>,
) -> LiveMethods {
    let mut live_methods: LiveMethods = HashMap::new();
    let mut worklist: VecDeque<String> = VecDeque::new();

    // Initialize worklist with main and constructors of live classes
    worklist.push_back("main".to_string());
    for class in live_classes {
        if let Some(meths) = methods.get(class) {
            for method in meths {
                let fname = format!("{}_{}", class, method);
                let methods = live_methods.entry(class.clone()).or_default();
                if !methods.contains(method.as_str()) {
                    methods.insert(method.clone());
                    worklist.push_back(fname);
                }
            }
        }
    }

    while let Some(func_name) = worklist.pop_front() {
        if let Some(func) = functions.get(&func_name) {
            let called_methods = find_method_calls(func);
            for (class, method) in called_methods {
                let methods = live_methods.entry(class.clone()).or_default();
                if !methods.contains(method.as_str()) {
                    methods.insert(method.clone());
                    let fname = format!("{}_{}", class, method);
                    worklist.push_back(fname);
                }
            }
        }
    }

    live_methods
}

/// Finds method calls in a function AST
fn find_method_calls(func: &Function) -> Vec<(String, String)> {
    let mut calls = Vec::new();
    if let Some(body) = &func.body {
        for stmt in &body.stmts {
            find_method_calls_in_stmt(stmt, &mut calls);
        }
    }
    calls
}

fn find_method_calls_in_stmt(stmt: &Stmt, calls: &mut Vec<(String, String)>) {
    match stmt {
        Stmt::ExprStmt(expr_stmt) => find_method_calls_in_expr(&expr_stmt.expr, calls),
        Stmt::VarDecl(var_decl) => {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init {
                    find_method_calls_in_expr(init, calls);
                }
            }
        }
        Stmt::Return(ret) => {
            if let Some(arg) = &ret.arg {
                find_method_calls_in_expr(arg, calls);
            }
        }
        Stmt::If(if_stmt) => {
            find_method_calls_in_expr(&if_stmt.test, calls);
            find_method_calls_in_stmt(&if_stmt.cons, calls);
            if let Some(alt) = &if_stmt.alt {
                find_method_calls_in_stmt(alt, calls);
            }
        }
        Stmt::While(while_stmt) => {
            find_method_calls_in_expr(&while_stmt.test, calls);
            find_method_calls_in_stmt(&while_stmt.body, calls);
        }
        Stmt::For(for_stmt) => {
            if let Some(init) = &for_stmt.init {
                match init {
                    ForInit::Expr(expr) => find_method_calls_in_expr(expr, calls),
                    ForInit::VarDecl(var_decl) => {
                        for decl in &var_decl.decls {
                            if let Some(init_expr) = &decl.init {
                                find_method_calls_in_expr(init_expr, calls);
                            }
                        }
                    }
                }
            }
            if let Some(test) = &for_stmt.test {
                find_method_calls_in_expr(test, calls);
            }
            if let Some(update) = &for_stmt.update {
                find_method_calls_in_expr(update, calls);
            }
            find_method_calls_in_stmt(&for_stmt.body, calls);
        }
        Stmt::Block(block) => {
            for stmt in &block.stmts {
                find_method_calls_in_stmt(stmt, calls);
            }
        }
        _ => {}
    }
}

fn find_method_calls_in_expr(expr: &Expr, calls: &mut Vec<(String, String)>) {
    match expr {
        Expr::Call(call_expr) => {
            // Handle method calls: obj.method()
            if let Callee::Expr(callee_expr) = &call_expr.callee
                && let Expr::Member(member) = &**callee_expr
                && let MemberProp::Ident(prop_ident) = &member.prop
            {
                // Try to resolve the object to a class name
                // For now, we'll try to extract from 'this' or variable names
                // This is a simplification; full RTA would need type inference
                if let Expr::Ident(obj_ident) = &*member.obj {
                    // If it's 'this', we'd need context to know the class
                    // For now, we'll record the call and let the caller resolve it
                    // For non-'this' identifiers, we can't easily resolve the class
                    // This is a limitation of the current implementation
                    if obj_ident.sym == "this" {
                        // We can't resolve 'this' without context, so skip for now
                        // In a full implementation, we'd track the current class context
                    } else {
                        // Assume the identifier might be a class instance
                        // This is a heuristic and may not always be correct
                        calls.push((obj_ident.sym.clone(), prop_ident.sym.clone()));
                    }
                }
            }
            // Recursively visit arguments
            for arg in &call_expr.args {
                find_method_calls_in_expr(arg, calls);
            }
        }
        Expr::Bin(bin) => {
            find_method_calls_in_expr(&bin.left, calls);
            find_method_calls_in_expr(&bin.right, calls);
        }
        Expr::Unary(unary) => {
            find_method_calls_in_expr(&unary.arg, calls);
        }
        Expr::Member(member) => {
            find_method_calls_in_expr(&member.obj, calls);
            if let MemberProp::Computed(comp) = &member.prop {
                find_method_calls_in_expr(comp, calls);
            }
        }
        Expr::Assign(assign) => {
            find_method_calls_in_expr(&assign.right, calls);
        }
        Expr::New(new_expr) => {
            find_method_calls_in_expr(&new_expr.callee, calls);
            for arg in &new_expr.args {
                find_method_calls_in_expr(arg, calls);
            }
        }
        _ => {}
    }
}
