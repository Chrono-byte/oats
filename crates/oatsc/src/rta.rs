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
use deno_ast::swc::ast;
use std::collections::{HashMap, HashSet, VecDeque};

/// Represents the class hierarchy as a directed graph.
/// Key: class name, Value: list of direct subclasses
pub type ClassHierarchy = HashMap<String, Vec<String>>;

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

/// Performs Rapid Type Analysis on the given modules
pub fn analyze(modules: &HashMap<String, ParsedModule>) -> RTAResults {
    let hierarchy = build_class_hierarchy(modules);
    let live_classes = find_instantiations(modules);
    let methods = collect_methods(modules);

    // Simple RTA: all methods of live classes are live
    let mut live_methods = HashMap::new();
    for (class, meths) in &methods {
        if live_classes.contains(class) {
            live_methods.insert(class.clone(), meths.clone());
        }
    }

    // TODO: Implement call graph
    let call_graph = HashMap::new();

    RTAResults {
        hierarchy,
        live_classes,
        live_methods,
        call_graph,
    }
}

/// Builds the class hierarchy by traversing all class declarations
fn build_class_hierarchy(modules: &HashMap<String, ParsedModule>) -> ClassHierarchy {
    let mut hierarchy: ClassHierarchy = HashMap::new();

    for module in modules.values() {
        for item in module.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(class_decl) = &decl.decl
            {
                let class_name = class_decl.ident.sym.to_string();

                // Check for extends clause
                if let Some(super_class) = &class_decl.class.super_class {
                    if let ast::Expr::Ident(super_ident) = &**super_class {
                        let super_name = super_ident.sym.to_string();
                        hierarchy.entry(super_name).or_insert(Vec::new()).push(class_name.clone());
                    }
                } else {
                    // Root class, ensure it's in the map
                    hierarchy.entry(class_name).or_insert(Vec::new());
                }
            }
        }
    }

    hierarchy
}

/// Finds all explicit class instantiations to form the initial live set
fn find_instantiations(modules: &HashMap<String, ParsedModule>) -> LiveClasses {
    let mut live_classes = HashSet::new();

    for module in modules.values() {
        for item in module.parsed.program_ref().body() {
            visit_module_item_for_instantiations(item, &mut live_classes);
        }
    }

    live_classes
}

fn visit_module_item_for_instantiations(item: deno_ast::ModuleItemRef, live_classes: &mut LiveClasses) {
    match item {
        deno_ast::ModuleItemRef::ModuleDecl(module_decl) => match module_decl {
            deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) => visit_decl_for_instantiations(&decl.decl, live_classes),
            deno_ast::swc::ast::ModuleDecl::Import(_) => {},
            _ => {},
        },
        deno_ast::ModuleItemRef::Stmt(stmt) => visit_stmt_for_instantiations(&stmt, live_classes),
    }
}

fn visit_decl_for_instantiations(decl: &ast::Decl, live_classes: &mut LiveClasses) {
    match decl {
        ast::Decl::Class(class_decl) => {
            // Visit class body for instantiations
            for member in &class_decl.class.body {
                if let ast::ClassMember::Method(method) = member {
                    visit_function_for_instantiations(&method.function, live_classes);
                }
            }
        }
        ast::Decl::Fn(fn_decl) => visit_function_for_instantiations(&fn_decl.function, live_classes),
        ast::Decl::TsEnum(_) => {
            // Enums don't have instantiations to track for RTA
        }
        _ => {},
    }
}

fn visit_stmt_for_instantiations(stmt: &ast::Stmt, live_classes: &mut LiveClasses) {
    match stmt {
        ast::Stmt::Expr(expr_stmt) => visit_expr_for_instantiations(&expr_stmt.expr, live_classes),
        ast::Stmt::Decl(decl) => visit_decl_for_instantiations(decl, live_classes),
        _ => {},
    }
}

fn visit_expr_for_instantiations(expr: &ast::Expr, live_classes: &mut LiveClasses) {
    match expr {
        ast::Expr::New(new_expr) => {
            if let ast::Expr::Ident(ident) = &*new_expr.callee {
                live_classes.insert(ident.sym.to_string());
            }
        }
        _ => {},
    }
    // Recursively visit children
    match expr {
        ast::Expr::New(new_expr) => {
            visit_expr_for_instantiations(&new_expr.callee, live_classes);
            if let Some(args) = &new_expr.args {
                for arg in args {
                    visit_expr_for_instantiations(&*arg.expr, live_classes);
                }
            }
        }
        _ => {},
    }
}

fn visit_function_for_instantiations(func: &ast::Function, live_classes: &mut LiveClasses) {
    if let Some(body) = &func.body {
        for stmt in &body.stmts {
            visit_stmt_for_instantiations(stmt, live_classes);
        }
    }
}

/// Collects all defined methods for each class
fn collect_methods(modules: &HashMap<String, ParsedModule>) -> HashMap<String, HashSet<String>> {
    let mut methods: HashMap<String, HashSet<String>> = HashMap::new();

    for module in modules.values() {
        for item in module.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(class_decl) = &decl.decl
            {
                let class_name = class_decl.ident.sym.to_string();
                let class_methods = methods.entry(class_name).or_insert(HashSet::new());

                for member in &class_decl.class.body {
                    if let ast::ClassMember::Method(method) = member {
                        // Get method name
                        if let ast::PropName::Ident(ident) = &method.key {
                            class_methods.insert(ident.sym.to_string());
                        }
                    }
                }
            }
        }
    }

    methods
}

/// Collects all function and method ASTs for worklist processing
fn collect_function_asts(modules: &HashMap<String, ParsedModule>) -> HashMap<String, ast::Function> {
    let mut functions = HashMap::new();

    for module in modules.values() {
        for item in module.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
            {
                let name = f.ident.sym.to_string();
                functions.insert(name, (*f.function).clone());
            }
        }
    }

    // Also collect class methods
    for module in modules.values() {
        for item in module.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(class_decl) = &decl.decl
            {
                let class_name = class_decl.ident.sym.to_string();
                for member in &class_decl.class.body {
                    if let ast::ClassMember::Method(method) = member {
                        if let ast::PropName::Ident(ident) = &method.key {
                            let method_name = format!("{}_{}", class_name, ident.sym);
                            functions.insert(method_name, (*method.function).clone());
                        }
                    }
                }
            }
        }
    }

    functions
}

/// Runs the RTA worklist algorithm to find precisely live methods
fn run_worklist(live_classes: &LiveClasses, methods: &HashMap<String, HashSet<String>>, functions: &HashMap<String, ast::Function>) -> LiveMethods {
    let mut live_methods: LiveMethods = HashMap::new();
    let mut worklist: VecDeque<String> = VecDeque::new();

    // Initialize worklist with main and constructors of live classes
    worklist.push_back("main".to_string());
    for class in live_classes {
        if let Some(meths) = methods.get(class) {
            for method in meths {
                let fname = format!("{}_{}", class, method);
                if !live_methods.entry(class.clone()).or_insert(HashSet::new()).contains(method) {
                    live_methods.get_mut(class).unwrap().insert(method.clone());
                    worklist.push_back(fname);
                }
            }
        }
    }

    while let Some(func_name) = worklist.pop_front() {
        if let Some(func) = functions.get(&func_name) {
            let called_methods = find_method_calls(func);
            for (class, method) in called_methods {
                if !live_methods.entry(class.clone()).or_insert(HashSet::new()).contains(&method) {
                    live_methods.get_mut(&class).unwrap().insert(method.clone());
                    let fname = format!("{}_{}", class, method);
                    worklist.push_back(fname);
                }
            }
        }
    }

    live_methods
}

/// Finds method calls in a function AST
fn find_method_calls(func: &ast::Function) -> Vec<(String, String)> {
    let mut calls = Vec::new();
    if let Some(body) = &func.body {
        for stmt in &body.stmts {
            find_method_calls_in_stmt(stmt, &mut calls);
        }
    }
    calls
}

fn find_method_calls_in_stmt(stmt: &ast::Stmt, calls: &mut Vec<(String, String)>) {
    match stmt {
        ast::Stmt::Expr(expr_stmt) => find_method_calls_in_expr(&expr_stmt.expr, calls),
        ast::Stmt::Decl(decl) => {
            if let ast::Decl::Var(var_decl) = decl {
                for decl in &var_decl.decls {
                    if let Some(init) = &decl.init {
                        find_method_calls_in_expr(init, calls);
                    }
                }
            }
        }
        _ => {}
    }
}

fn find_method_calls_in_expr(expr: &ast::Expr, calls: &mut Vec<(String, String)>) {
    match expr {
        ast::Expr::Call(call_expr) => {
            if let ast::Callee::Expr(callee_expr) = &call_expr.callee {
                if let ast::Expr::Member(member) = &**callee_expr {
                    if let ast::Expr::Ident(obj_ident) = &*member.obj {
                        if let ast::MemberProp::Ident(prop_ident) = &member.prop {
                            // Assume obj_ident is 'this' or similar, but for simplicity, assume class name is known
                            // This is a simplification; in full RTA, need type analysis
                            // For now, just collect all member calls
                            calls.push((obj_ident.sym.to_string(), prop_ident.sym.to_string()));
                        }
                    }
                }
            }
        }
        _ => {}
    }
    // Recursively visit children
    match expr {
        ast::Expr::Call(call_expr) => {
            if let ast::Callee::Expr(callee_expr) = &call_expr.callee {
                find_method_calls_in_expr(&callee_expr, calls);
            }
            for arg in &call_expr.args {
                find_method_calls_in_expr(&arg.expr, calls);
            }
        }
        _ => {}
    }
}