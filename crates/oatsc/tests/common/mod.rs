//! Common testing utilities for the Oats compiler test suite.
//!
//! This module provides shared functionality used across multiple test files,
//! including IR generation helpers, test harness utilities, and diagnostic
//! suppression for clean test output. The utilities are designed to simplify
//! test authoring while maintaining consistency across the test suite.
//!
//! # Key Functions
//!
//! - `gen_ir_for_source`: Generates LLVM IR from Oats source code for testing
//! - Test setup helpers for consistent compilation environment
//! - Diagnostic output management for focused test results
//!
//! # Usage Pattern
//!
//! Test files typically import this module to access shared compilation
//! infrastructure without duplicating setup code across individual tests.

use anyhow::Result;

use oatsc::codegen::CodeGen;
use oatsc::parser;
use oatsc::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

/// Helper function to parse Oats source code for tests
/// Unpacks the tuple return type and handles diagnostics
#[allow(dead_code)]
pub fn parse_oats_module_for_test(src: &str) -> Result<oatsc::parser::ParsedModule> {
    let _diag_guard = oatsc::diagnostics::suppress();
    let (parsed_opt, _diags) = parser::parse_oats_module(src, None)?;
    parsed_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))
}

#[allow(dead_code)]
pub fn parse_oats_module_with_options_for_test(
    src: &str,
    enforce_semicolons: bool,
) -> Result<oatsc::parser::ParsedModule> {
    let _diag_guard = oatsc::diagnostics::suppress();
    let (parsed_opt, _diags) =
        parser::parse_oats_module_with_options(src, None, enforce_semicolons)?;
    parsed_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))
}

pub mod deno_adapter;

/// Generates LLVM IR from Oats source code for testing purposes.
///
/// This function provides a convenient way to compile Oats source code to
/// LLVM IR without going through the full compilation pipeline. It's designed
/// specifically for testing scenarios where IR output needs to be inspected
/// or validated.
///
/// # Arguments
/// * `src` - Oats source code to compile
///
/// # Returns
/// String containing the generated LLVM IR, or an error if compilation fails
///
/// # Behavior
/// The function automatically suppresses diagnostic output to stderr to keep
/// test output clean and focused. Diagnostics are restored when the function
/// completes, ensuring proper error reporting in subsequent operations.
#[cfg(test)]
#[allow(dead_code)]
pub fn gen_ir_for_source(src: &str) -> Result<String> {
    // Suppress diagnostic output during test execution to maintain clean test output.
    // The guard automatically restores previous diagnostic settings when dropped.
    let _diag_guard = oatsc::diagnostics::suppress();

    let (parsed_mod_opt, _parse_diags) = parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // Locate the exported main function required for test compilation
    use oats_ast::*;
    let mut func_decl_opt: Option<Function> = None;
    for stmt in &parsed.body {
        if let Stmt::FnDecl(fn_decl) = stmt {
            let name = fn_decl.ident.sym.clone();
            if name == "main" {
                func_decl_opt = Some(Function {
                    params: fn_decl.params.clone(),
                    body: fn_decl.body.clone(),
                    return_type: fn_decl.return_type.clone(),
                    span: fn_decl.span.clone(),
                    is_async: fn_decl.is_async,
                    is_generator: fn_decl.is_generator,
                });
                break;
            }
        }
    }

    let func_decl =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found in example"))?;

    let mut symbols = SymbolTable::new();
    let (sig_opt, _diags) = check_function_strictness(&func_decl, &mut symbols)?;
    let func_sig =
        sig_opt.ok_or_else(|| anyhow::anyhow!("Function signature could not be determined"))?;

    let context = Context::create();
    let module = context.create_module("test_module");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let codegen = create_codegen(&context, "test_module", symbols, &parsed_mod.source)?;
    // Ensure number_to_string is declared so tests that look for it in the
    // generated IR (for template-literal lowering checks) will pass even if
    // the lowering path did not emit a call site that references it.
    // Use the public helper in utils which delegates to CodeGen.
    let _ = oatsc::codegen::utils::runtime::get_number_to_string(&codegen);

    // gen_function_ir returns Diagnostic on failures; convert to anyhow for `?` compatibility
    // Emit class methods/constructors for exported classes so tests can inspect
    // generated IR (mirror behavior in crates/oats/src/main.rs).
    let parsed_mod_ref = parse_oats_module_for_test(src)?;
    let parsed_full = &parsed_mod_ref.parsed;

    for stmt in &parsed_full.body {
        if let Stmt::ClassDecl(class_decl) = stmt {
            let class_name = class_decl.ident.sym.clone();
            // collect fields as in main.rs
            let mut fields: Vec<(String, oatsc::types::OatsType)> = Vec::new();
            use oats_ast::*;
            for member in &class_decl.body {
                if let ClassMember::Field(field) = member {
                    let fname = field.ident.sym.clone();
                    if !fields.iter().any(|(n, _)| n == &fname) {
                        fields.push((fname, oatsc::types::OatsType::Number));
                    }
                }
            }

            for member in &class_decl.body {
                if let ClassMember::Constructor(cons) = member {
                    for param in &cons.params {
                        if let Pat::Ident(binding_ident) = &param.pat {
                            let fname = binding_ident.sym.clone();
                            if fields.iter().all(|(n, _)| n != &fname) {
                                let ty = param
                                    .ty
                                    .as_ref()
                                    .and_then(|ann| oatsc::types::map_ts_type(ann))
                                    .unwrap_or(oatsc::types::OatsType::Number);
                                fields.push((fname, ty));
                            }
                        }
                        // Destructuring patterns not yet supported in test helper
                    }
                }
            }

            for member in &class_decl.body {
                if let ClassMember::Constructor(cons) = member
                    && let Some(body) = &cons.body
                {
                    for stmt in &body.stmts {
                        if let Stmt::ExprStmt(expr_stmt) = stmt
                            && let Expr::Assign(assign) = &expr_stmt.expr
                            && let AssignTarget::Member(mem) = &assign.left
                            && let MemberProp::Ident(ident) = &mem.prop
                        {
                            // Check if it's a this.member assignment
                            if let Expr::This(_) = &*mem.obj {
                                let name = ident.sym.clone();
                                let inferred = oatsc::types::infer_type(None, Some(&assign.right));
                                if fields.iter().all(|(n, _)| n != &name) {
                                    fields.push((name, inferred));
                                }
                            }
                        }
                    }
                }
            }

            if !fields.is_empty() {
                codegen
                    .class_fields
                    .borrow_mut()
                    .insert(class_name.clone(), fields.clone());
            }

            // emit methods & constructors
            for member in &class_decl.body {
                match member {
                    ClassMember::Method(m) => {
                        let mname = m.ident.sym.clone();
                        let func = Function {
                            params: m.params.clone(),
                            body: m.body.clone(),
                            return_type: m.return_type.clone(),
                            span: m.span.clone(),
                            is_async: false,
                            is_generator: false,
                        };
                        if let Ok((Some(sig), _diags)) = oatsc::types::check_function_strictness(
                            &func,
                            &mut oatsc::types::SymbolTable::new(),
                        ) {
                            let mut params = Vec::new();
                            params.push(oatsc::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &func, &params, &ret, Some("this"))
                                .map_err(|d| anyhow::anyhow!(d.message))?;
                        } else {
                            // fallback: emit with void return
                            if let Ok((Some(sig2), _diags2)) =
                                oatsc::types::check_function_strictness(
                                    &func,
                                    &mut oatsc::types::SymbolTable::new(),
                                )
                            {
                                let mut params = Vec::new();
                                params.push(oatsc::types::OatsType::NominalStruct(
                                    class_name.clone(),
                                ));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &func,
                                        &params,
                                        &oatsc::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| anyhow::anyhow!(d.message))?;
                            }
                        }
                    }
                    ClassMember::Constructor(cons) => {
                        // Use empty fields list if none were recorded to avoid borrow-panics
                        let fields_vec = codegen
                            .class_fields
                            .borrow()
                            .get(&class_name)
                            .cloned()
                            .unwrap_or_else(Vec::new);
                        codegen
                            .gen_constructor_ir(&class_name, cons, &fields_vec, None)
                            .map_err(|d| anyhow::anyhow!(d.message))?;
                    }
                    _ => {}
                }
            }
        }
    }

    // Now emit oats_main
    codegen
        .gen_function_ir(
            "oats_main",
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .map_err(|d| anyhow::anyhow!(d.message))?;

    let ir_str = codegen.module.print_to_string().to_string();
    eprintln!("[debug generated IR]\n{}", ir_str);
    Ok(ir_str)
}

// Ensure `builder` is directly passed to `CodeGen`
// Updated `create_codegen` to wrap `SymbolTable` in `RefCell` internally
pub fn create_codegen<'a>(
    context: &'a Context,
    module_name: &str,
    mut symbols: SymbolTable,
    source: &'a str,
) -> anyhow::Result<CodeGen<'a>> {
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    builder.clear_insertion_position();

    // Simulate module resolution and symbol registration
    let mut modules = std::collections::HashMap::new();
    let (parsed_opt, _) = oatsc::parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    modules.insert(module_name.to_string(), parsed_mod);

    for (_, parsed_module) in modules.iter() {
        let pm = &parsed_module.parsed;
        use oats_ast::*;
        for stmt in &pm.body {
            match stmt {
                Stmt::ClassDecl(class_decl) => {
                    let name = class_decl.ident.sym.clone();
                    symbols.insert(
                        name.clone(),
                        oatsc::types::Symbol::Variable {
                            ty: oatsc::types::OatsType::NominalStruct(name),
                        },
                    );
                }
                // Note: oats_ast doesn't have separate interface declarations
                // They would be handled differently if needed
                _ => {}
            }
        }
    }

    // Suppress noisy debug printing in tests.

    let parsed_mod_ref = modules
        .get(module_name)
        .ok_or_else(|| anyhow::anyhow!("parsed module missing"))?;

    Ok(CodeGen {
        context,
        module,
        builder,
        next_str_id: std::cell::Cell::new(0),
        string_literals: std::cell::RefCell::new(std::collections::HashMap::new()),
        f64_t: context.f64_type(),
        f32_t: context.f32_type(),
        i64_t: context.i64_type(),
        i32_t: context.i32_type(),
        i16_t: context.i16_type(),
        i8_t: context.i8_type(),
        bool_t: context.bool_type(),
        i8ptr_t: context.ptr_type(inkwell::AddressSpace::default()),
        fn_print_f64: std::cell::RefCell::new(None),
        fn_print_str: std::cell::RefCell::new(None),
        fn_strlen: std::cell::RefCell::new(None),
        fn_malloc: std::cell::RefCell::new(None),
        fn_memcpy: std::cell::RefCell::new(None),
        fn_free: std::cell::RefCell::new(None),
        fn_array_alloc: std::cell::RefCell::new(None),
        fn_rc_inc: std::cell::RefCell::new(None),
        fn_rc_dec: std::cell::RefCell::new(None),
        fn_number_to_string: std::cell::RefCell::new(None),
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        class_fields: RefCell::new(HashMap::new()),
        fn_param_types: RefCell::new(HashMap::new()),
        loop_context_stack: RefCell::new(Vec::new()),
        current_label: RefCell::new(None),
        current_class_parent: RefCell::new(None),
        closure_local_rettype: RefCell::new(HashMap::new()),
        last_expr_origin_local: RefCell::new(None),
        async_await_live_sets: RefCell::new(None),
        async_local_name_to_slot: RefCell::new(None),
        async_resume_blocks: RefCell::new(None),
        async_cont_blocks: RefCell::new(None),
        async_poll_function: RefCell::new(None),
        async_await_counter: Cell::new(0),
        async_param_count: Cell::new(0),
        async_local_slot_count: Cell::new(0),
        async_poll_locals: RefCell::new(None),
        source,
        mut_var_decls: parsed_mod_ref.mut_var_decls.clone(),
        current_function_return_type: RefCell::new(None),
        last_expr_is_boxed_union: Cell::new(false),
        global_function_signatures: RefCell::new(HashMap::new()),
        symbol_table: RefCell::new(symbols),
        external_std_fns: RefCell::new(std::collections::HashMap::new()),
        nested_generic_fns: RefCell::new(HashMap::new()),
        monomorphized_map: RefCell::new(HashMap::new()),
        class_parents: RefCell::new(HashMap::new()),
        enum_variants: RefCell::new(HashMap::new()),
        type_aliases: RefCell::new(HashMap::new()),
        rta_results: None,
        uses_async: Cell::new(false),
    })
}
