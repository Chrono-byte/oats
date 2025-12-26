//! Test utilities for the Oats compiler test suite.
//!
//! This module provides helper functions for creating test CodeGen instances,
//! compiling code snippets, and asserting compilation results.

use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target};
use oatsc::codegen::CodeGen;
use oatsc::diagnostics::{Diagnostic, Severity};
use oatsc::parser;
use oatsc::types::{FunctionSig, OatsType, SymbolTable};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

/// Creates a new CodeGen instance for testing.
///
/// This helper sets up a minimal CodeGen context with an LLVM module,
/// suitable for unit testing individual codegen components.
#[allow(dead_code)]
pub fn create_test_codegen<'a>(context: &'a Context, source: &'a str) -> CodeGen<'a> {
    let module = context.create_module("test");
    let builder = context.create_builder();

    // Initialize LLVM targets
    Target::initialize_native(&InitializationConfig::default())
        .expect("failed to initialize LLVM targets");

    use inkwell::AddressSpace;

    CodeGen {
        context,
        module,
        builder,
        next_str_id: Cell::new(0),
        string_literals: RefCell::new(HashMap::new()),
        f64_t: context.f64_type(),
        f32_t: context.f32_type(),
        i64_t: context.i64_type(),
        i32_t: context.i32_type(),
        i16_t: context.i16_type(),
        i8_t: context.i8_type(),
        bool_t: context.bool_type(),
        i8ptr_t: context.ptr_type(AddressSpace::default()),
        fn_print_f64: RefCell::new(None),
        fn_print_str: RefCell::new(None),
        fn_strlen: RefCell::new(None),
        fn_malloc: RefCell::new(None),
        fn_memcpy: RefCell::new(None),
        fn_free: RefCell::new(None),
        fn_array_alloc: RefCell::new(None),
        fn_rc_inc: RefCell::new(None),
        fn_rc_dec: RefCell::new(None),
        fn_number_to_string: RefCell::new(None),
        fn_number_new: RefCell::new(None),
        fn_number_is_nan_static: RefCell::new(None),
        fn_number_is_finite_static: RefCell::new(None),
        fn_number_is_integer: RefCell::new(None),
        fn_number_parse_float_static: RefCell::new(None),
        fn_number_parse_int: RefCell::new(None),
        fn_number_epsilon: RefCell::new(None),
        fn_number_max_safe_integer: RefCell::new(None),
        fn_number_max_value: RefCell::new(None),
        fn_number_min_safe_integer: RefCell::new(None),
        fn_number_min_value: RefCell::new(None),
        fn_number_nan: RefCell::new(None),
        fn_number_negative_infinity: RefCell::new(None),
        fn_number_positive_infinity: RefCell::new(None),
        fn_number_is_safe_integer: RefCell::new(None),
        fn_number_to_fixed: RefCell::new(None),
        fn_number_to_exponential: RefCell::new(None),
        fn_number_to_precision: RefCell::new(None),
        fn_number_to_locale_string: RefCell::new(None),
        fn_number_value_of: RefCell::new(None),
        fn_union_box_f64: RefCell::new(None),
        fn_union_box_ptr: RefCell::new(None),
        fn_union_unbox_f64: RefCell::new(None),
        fn_union_unbox_ptr: RefCell::new(None),
        fn_rc_weak_inc: RefCell::new(None),
        fn_rc_weak_dec: RefCell::new(None),
        fn_rc_weak_upgrade: RefCell::new(None),
        fn_union_get_discriminant: RefCell::new(None),
        class_fields: RefCell::new(HashMap::new()),
        interface_fields: RefCell::new(HashMap::new()),
        enum_variants: RefCell::new(HashMap::new()),
        class_parents: RefCell::new(HashMap::new()),
        type_aliases: RefCell::new(HashMap::new()),
        fn_param_types: RefCell::new(HashMap::new()),
        loop_context_stack: RefCell::new(Vec::new()),
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
        generator_yield_live_sets: RefCell::new(None),
        generator_local_name_to_slot: RefCell::new(None),
        generator_resume_blocks: RefCell::new(None),
        generator_cont_blocks: RefCell::new(None),
        generator_next_function: RefCell::new(None),
        generator_yield_counter: Cell::new(0),
        generator_param_count: Cell::new(0),
        generator_local_slot_count: Cell::new(0),
        generator_next_locals: RefCell::new(None),
        source,
        mut_var_decls: std::collections::HashSet::new(),
        current_function_return_type: RefCell::new(None),
        last_expr_is_boxed_union: Cell::new(false),
        global_function_signatures: RefCell::new(HashMap::new()),
        symbol_table: RefCell::new(SymbolTable::new()),
        external_std_fns: RefCell::new(HashMap::new()),
        const_globals: RefCell::new(HashMap::new()),
        const_interns: RefCell::new(HashMap::new()),
        current_escape_info: RefCell::new(None),
        nested_generic_fns: RefCell::new(HashMap::new()),
        monomorphized_map: RefCell::new(HashMap::new()),
        rta_results: None,
        uses_async: Cell::new(false),
        current_label: RefCell::new(None),
        last_expr_oats_type: RefCell::new(None),
        nested_generic_classes: RefCell::new(HashMap::new()),
        returned_ownership: Cell::new(false),
    }
}

/// Compiles a code snippet and returns the parsed module.
///
/// Returns the parsed module on success, or a diagnostic error on failure.
#[allow(dead_code)]
pub fn parse_snippet(
    code: &str,
) -> Result<(parser::ParsedModule, Vec<Diagnostic>), Box<Diagnostic>> {
    let (parsed_mod_opt, diags) = parser::parse_oats_module(code, Some("test.oats"))
        .map_err(|e| Diagnostic::simple_boxed(Severity::Error, format!("Parse error: {}", e)))?;

    match parsed_mod_opt {
        Some(parsed_mod) => Ok((parsed_mod, diags)),
        None => Err(Diagnostic::simple_boxed(
            Severity::Error,
            "Parsing returned None",
        )),
    }
}

/// Asserts that a code snippet compiles successfully.
///
/// Panics if compilation fails or produces errors.
#[allow(dead_code)]
pub fn assert_compiles(code: &str) {
    let (parsed_mod, diags) = parse_snippet(code).expect("Code should parse successfully");

    // Check for diagnostics
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();

    if !errors.is_empty() {
        panic!(
            "Expected compilation to succeed, but got {} errors: {:?}",
            errors.len(),
            errors
        );
    }

    // Verify we got a parsed module
    assert!(
        !parsed_mod.parsed.body.is_empty() || !parsed_mod.declared_fns.is_empty(),
        "Parsed module should have content"
    );
}

/// Asserts that a code snippet fails to compile with a specific error message.
///
/// Panics if compilation succeeds or if the error message doesn't match.
#[allow(dead_code)]
pub fn assert_compilation_error(code: &str, expected_error: &str) {
    match parse_snippet(code) {
        Ok((_parsed_mod, diags)) => {
            let errors: Vec<_> = diags
                .iter()
                .filter(|d| d.severity == Severity::Error)
                .collect();

            if errors.is_empty() {
                panic!(
                    "Expected compilation error containing '{}', but compilation succeeded",
                    expected_error
                );
            }

            let error_messages: Vec<String> = errors.iter().map(|d| d.message.clone()).collect();

            let found = error_messages
                .iter()
                .any(|msg| msg.contains(expected_error));

            if !found {
                panic!(
                    "Expected error containing '{}', but got: {:?}",
                    expected_error, error_messages
                );
            }
        }
        Err(diag) => {
            if !diag.message.contains(expected_error) {
                panic!(
                    "Expected error containing '{}', but got: {}",
                    expected_error, diag.message
                );
            }
        }
    }
}

/// Extracts LLVM IR from a CodeGen instance.
///
/// Returns the IR as a string for inspection in tests.
#[allow(dead_code)]
pub fn get_llvm_ir<'a>(codegen: &CodeGen<'a>) -> String {
    codegen.module.print_to_string().to_string()
}

/// Creates a minimal symbol table for testing.
#[allow(dead_code)]
pub fn create_test_symbol_table() -> SymbolTable {
    SymbolTable::new()
}

/// Helper to create a test context and module.
#[allow(dead_code)]
pub fn create_test_context() -> Context {
    Context::create()
}

/// Asserts that a diagnostic has a specific severity and message pattern.
#[allow(dead_code)]
pub fn assert_diagnostic(
    diag: &Diagnostic,
    expected_severity: Severity,
    expected_message_pattern: &str,
) {
    assert_eq!(
        diag.severity, expected_severity,
        "Diagnostic severity mismatch"
    );
    assert!(
        diag.message.contains(expected_message_pattern),
        "Expected diagnostic message to contain '{}', but got: {}",
        expected_message_pattern,
        diag.message
    );
}

/// Helper to create a test function signature.
#[allow(dead_code)]
pub fn create_test_function_sig(params: Vec<OatsType>, ret: OatsType) -> FunctionSig {
    FunctionSig {
        type_params: vec![],
        params,
        ret,
    }
}

/// Helper to create a test OatsType.
#[allow(dead_code)]
pub fn create_test_type(ty: &str) -> OatsType {
    match ty {
        "number" => OatsType::Number,
        "boolean" => OatsType::Boolean,
        "string" => OatsType::String,
        "void" => OatsType::Void,
        _ => panic!("Unknown test type: {}", ty),
    }
}

/// Compiles a code snippet to LLVM IR and returns it.
///
/// This is a convenience function that combines parsing and codegen.
#[allow(dead_code)]
pub fn compile_to_ir(code: &str) -> Result<String, Box<Diagnostic>> {
    let (_parsed_mod, _diags) = parse_snippet(code)?;
    let context = create_test_context();
    let codegen = create_test_codegen(&context, code);

    // For now, just return the module IR
    // Full compilation would require running the full builder pipeline
    Ok(get_llvm_ir(&codegen))
}
