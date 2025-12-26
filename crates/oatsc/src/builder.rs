//! Main compilation driver and orchestration.
//!
//! This module orchestrates the complete compilation pipeline from source files
//! to executables. It coordinates parsing, type checking, RTA analysis, code
//! generation, and linking.
//!
//! The compilation process follows these stages:
//! 1. **Parsing**: Convert source text to AST
//! 2. **Type Checking**: Validate types and function signatures
//! 3. **RTA Analysis**: Determine reachable classes and methods
//! 4. **Code Generation**: Emit LLVM IR
//! 5. **Linking**: Compile IR to object files and link with runtime
//!
//! External dependencies and linking are handled by separate modules:
//! - `extern_resolution`: External dependency handling
//! - `linking`: Object file generation and linking

use anyhow::Result;
use std::fs::File;
use std::io::Write;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::codegen::CodeGen;
use crate::diagnostics;
use crate::parser;
use crate::types::{SymbolTable, check_function_strictness};
use oxc_ast::ast::{
    BindingPatternKind, Declaration, Expression, Function, Program, Statement,
    TSModuleDeclarationName, VariableDeclarationKind,
};

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicType;

/// Compiles a source file with the given options.
///
/// This is the main entry point for compilation when you have options ready.
/// Unlike `run_from_args`, it doesn't mess with environment variables or CLI parsing.
///
/// # Arguments
/// * `options` - The compilation options
///
/// # Returns
/// Path to the output file on success, or None if only object emission
///
/// # Errors
/// Returns an error if the compilation fails.
pub fn compile_with_options(options: crate::CompileOptions) -> Result<Option<String>> {
    // Pass options directly through the call stack
    let args = vec!["oatsc".to_string(), options.src_file.clone()];
    run_from_args(&args, Some(&options))
}

/// Executes the complete AOT compilation pipeline from source to executable.
///
/// This is the main function that handles everything: parsing, type checking, codegen, linking.
/// It takes command-line args or uses options/env vars for config.
///
/// The compilation pipeline consists of:
/// - Source file resolution
/// - Code parsing
/// - LLVM IR generation
/// - Object file compilation
/// - Linking
///
/// # Returns
/// Path to the output file on success, or None if only object emission
///
/// # Errors
/// Returns an error if the compilation fails or if the source file is not provided.
pub fn run_from_args(
    args: &[String],
    options: Option<&crate::CompileOptions>,
) -> Result<Option<String>> {
    // Resolve source file location from arguments, options, or environment
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Some(opts) = options {
        opts.src_file.clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    let source = std::fs::read_to_string(&src_path)?;

    // Parse source into AST representation (single pass, no transformations)
    let (parsed_mod_opt, parse_diags) = parser::parse_oats_module(&source, Some(&src_path))?;

    // Emit any parsing diagnostics
    for diag in &parse_diags {
        diagnostics::emit_diagnostic(diag, Some(&source));
    }

    // Unpack the parsed module or bail if parsing failed
    let parsed_mod = match parsed_mod_opt {
        Some(pm) => pm,
        None => {
            anyhow::bail!("Parsing failed: module could not be parsed");
        }
    };

    // Use original parsed AST for all subsequent processing phases.
    // Arrow function extraction happens during codegen traversal.
    let parsed = &parsed_mod.parsed;

    // Read extern_oats from options or environment if provided
    let extern_oats: std::collections::HashMap<String, String> = if let Some(opts) = options {
        // Validate that all metadata file paths exist
        for (import_path, meta_path) in &opts.extern_oats {
            let path = std::path::Path::new(meta_path);
            if !path.exists() {
                anyhow::bail!(
                    "extern_oats entry '{import_path}' points to non-existent metadata file: {meta_path}"
                );
            }
            if !path.is_file() {
                anyhow::bail!(
                    "extern_oats entry '{import_path}' points to a non-file path: {meta_path}"
                );
            }
        }
        opts.extern_oats.clone()
    } else if let Ok(extern_oats_json) = std::env::var("OATS_EXTERN_OATS") {
        let map: std::collections::HashMap<String, String> =
            serde_json::from_str(&extern_oats_json)
                .map_err(|e| anyhow::anyhow!("Failed to parse OATS_EXTERN_OATS JSON: {e}"))?;

        // Validate that all metadata file paths exist
        for (import_path, meta_path) in &map {
            let path = std::path::Path::new(meta_path);
            if !path.exists() {
                anyhow::bail!(
                    "extern_oats entry '{import_path}' points to non-existent metadata file: {meta_path}"
                );
            }
            if !path.is_file() {
                anyhow::bail!(
                    "extern_oats entry '{import_path}' points to a non-file path: {meta_path}"
                );
            }
        }

        map
    } else {
        std::collections::HashMap::new()
    };

    /// Extracts top-level arrow function declarations from variable statements.
    ///
    /// This looks for arrow functions assigned to const/let vars at the top level,
    /// since they need special treatment in codegen. It separates exported vs non-exported ones.
    ///
    /// Returns a list of (name, `arrow_expr`, `is_exported`) tuples.
    fn extract_arrow_functions<'a>(
        parsed: &'a Program<'a>,
    ) -> Vec<(String, &'a oxc_ast::ast::ArrowFunctionExpression<'a>, bool)> {
        let mut arrows = Vec::new();

        for stmt in &parsed.body {
            // Process non-exported variable declarations: const/let foo = () => {}
            if let Statement::VariableDeclaration(vdecl) = stmt
                && vdecl.declarations.len() == 1
            {
                let decl = &vdecl.declarations[0];
                if let BindingPatternKind::BindingIdentifier(binding_ident) = &decl.id.kind
                    && let Some(init_expr) = &decl.init
                    && let Expression::ArrowFunctionExpression(arrow) = init_expr
                {
                    let name = binding_ident.name.to_string();
                    arrows.push((name, arrow, false));
                }
                // Destructuring patterns not yet supported for arrow function bindings
            }
            // oxc_ast lacks separate export declarations in the AST
            // Exports are handled differently - currently only handles non-exported
        }
        arrows
            .into_iter()
            .map(|(name, arrow, is_exported)| (name, arrow.as_ref(), is_exported))
            .collect()
    }

    let arrow_functions = extract_arrow_functions(parsed);

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    // NOTE: This function is now consolidated in crate::types::infer_type_from_expr

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
    let mut func_decl_opt: Option<&Function<'_>> = None;
    for stmt in &parsed.body {
        // Check for exported function: export function main() {}
        if let Statement::ExportNamedDeclaration(export_decl) = stmt
            && let Some(export_declaration) = &export_decl.declaration
            && let Declaration::FunctionDeclaration(fn_decl) = export_declaration
            && let Some(id) = &fn_decl.id
        {
            let name = id.name.to_string();
            if name == "main" {
                func_decl_opt = Some(fn_decl);
                break;
            }
        }
        // Check for non-exported function: function main() {}
        if let Statement::FunctionDeclaration(fn_decl) = stmt
            && let Some(id) = &fn_decl.id
        {
            let name = id.name.to_string();
            if name == "main" {
                func_decl_opt = Some(fn_decl);
                break;
            }
        }
    }

    let func_decl: Option<&Function<'_>> = if let Some(f) = func_decl_opt {
        Some(f)
    } else {
        // Check if in library mode (emitting object only)
        let emit_object_only = match options.as_ref().map(|o| o.emit_object_only) {
            Some(val) => val,
            None => std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok(),
        };
        if emit_object_only {
            // For library modules, compilation still occurs but main function requirement is skipped
            // Continues with compilation but does not require a main function
            // The actual return will happen after object file emission
            None
        } else {
            return diagnostics::report_error_and_bail(
                Some(&src_path),
                Some(&source),
                "No exported `main` function found in script. Please export `function main(...)`.",
                Some("Scripts must export a `main` function to serve as the program entrypoint."),
            );
        }
    };

    let mut symbols = SymbolTable::new();
    let func_sig = if let Some(func) = func_decl {
        let (sig_opt, type_diags) = check_function_strictness(func, &mut symbols)?;
        for diag in &type_diags {
            diagnostics::emit_diagnostic(diag, Some(&source));
        }
        sig_opt
    } else {
        None
    };

    let context = Context::create();
    let module = context.create_module(crate::runtime_functions::modules::OATS_AOT);
    // Set the module target triple to the host default so clang doesn't warn
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    // Declare std functions first (console.log functions)
    let void_t = context.void_type();
    let i8ptr_t = context.ptr_type(inkwell::AddressSpace::default());
    let f64_t = context.f64_type();
    let i32_t = context.i32_type();

    // Declare all console.log variants
    module.add_function(
        crate::runtime_functions::names::STD_CONSOLE_LOG,
        void_t.fn_type(&[i8ptr_t.into()], false),
        None,
    );
    module.add_function(
        "oats_std_console_log_number",
        void_t.fn_type(&[f64_t.into()], false),
        None,
    );
    module.add_function(
        "oats_std_console_log_int",
        void_t.fn_type(&[i32_t.into()], false),
        None,
    );
    module.add_function(
        "oats_std_console_log_newline",
        void_t.fn_type(&[], false),
        None,
    );

    // Resolve external dependencies and declare them in the LLVM module
    // Inlined to avoid lifetime issues with Module's invariant lifetime
    let mut external_function_signatures: std::collections::HashMap<
        String,
        (Vec<crate::types::OatsType>, crate::types::OatsType),
    > = std::collections::HashMap::new();

    // First pass: read metadata files and extract function signatures
    for meta_path in extern_oats.values() {
        let meta_content = match std::fs::read_to_string(meta_path) {
            Ok(meta_content) => meta_content,
            Err(e) => {
                eprintln!("Warning: Failed to read metadata file {meta_path}: {e}");
                continue;
            }
        };

        // Parse TypeScript-style function declarations from metadata
        for line in meta_content.lines() {
            let line = line.trim();
            if line.starts_with("function ")
                && line.ends_with(';')
                && let Some((name, func_sig)) =
                    crate::extern_resolution::parse_function_signature_from_metadata(line)
            {
                external_function_signatures.insert(name, (func_sig.params, func_sig.ret));
            }
        }
    }

    // Second pass: declare external functions in LLVM module
    for meta_path in extern_oats.values() {
        let symbols: Vec<String> = if std::path::Path::new(meta_path).exists() {
            external_function_signatures.keys().cloned().collect()
        } else {
            meta_path.split(',').map(|s| s.trim().to_string()).collect()
        };

        for symbol in symbols {
            let (param_types, ret_type) = match external_function_signatures.get(&symbol).cloned() {
                Some(types) => types,
                None => (vec![], crate::types::OatsType::Number),
            };

            // Convert OatsType to LLVM types inline
            let llvm_param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = param_types
                .iter()
                .map(|ty| match ty {
                    crate::types::OatsType::Number | crate::types::OatsType::F64 => {
                        context.f64_type().into()
                    }
                    crate::types::OatsType::F32 => context.f32_type().into(),
                    crate::types::OatsType::I64 => context.i64_type().into(),
                    crate::types::OatsType::I32 => context.i32_type().into(),
                    crate::types::OatsType::I8 => context.i8_type().into(),
                    crate::types::OatsType::Boolean => context.bool_type().into(),
                    crate::types::OatsType::String => {
                        context.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    _ => context.f64_type().into(),
                })
                .collect();

            let fn_type = if ret_type == crate::types::OatsType::Void {
                let void_t = context.void_type();
                void_t.fn_type(&llvm_param_types, false)
            } else {
                let ret_llvm_type = match ret_type {
                    crate::types::OatsType::Number | crate::types::OatsType::F64 => {
                        context.f64_type().into()
                    }
                    crate::types::OatsType::F32 => context.f32_type().into(),
                    crate::types::OatsType::I64 => context.i64_type().into(),
                    crate::types::OatsType::I32 => context.i32_type().into(),
                    crate::types::OatsType::I8 => context.i8_type().into(),
                    crate::types::OatsType::Boolean => context.bool_type().into(),
                    crate::types::OatsType::String => {
                        context.ptr_type(inkwell::AddressSpace::default()).into()
                    }
                    _ => context.f64_type().into(),
                };
                // Convert BasicMetadataTypeEnum to BasicTypeEnum for fn_type
                let ret_basic_type: inkwell::types::BasicTypeEnum = match ret_llvm_type {
                    inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => ft.into(),
                    inkwell::types::BasicMetadataTypeEnum::IntType(it) => it.into(),
                    inkwell::types::BasicMetadataTypeEnum::PointerType(pt) => pt.into(),
                    inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => at.into(),
                    inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => vt.into(),
                    inkwell::types::BasicMetadataTypeEnum::StructType(st) => st.into(),
                    inkwell::types::BasicMetadataTypeEnum::ScalableVectorType(svt) => svt.into(),
                    inkwell::types::BasicMetadataTypeEnum::MetadataType(_) => {
                        context.f64_type().into()
                    }
                };
                ret_basic_type.fn_type(&llvm_param_types, false)
            };
            module.add_function(&symbol, fn_type, None);
        }
    }

    // Perform Rapid Type Analysis on all modules
    // RTA now works with references instead of owned values
    let mut modules_map = std::collections::HashMap::new();
    modules_map.insert(src_path.clone(), &parsed_mod);

    // Run RTA analysis between parsing and code generation
    let rta_results = crate::rta::analyze(&modules_map);

    let builder = context.create_builder();
    let codegen = CodeGen {
        context: &context,
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
        fn_number_new: std::cell::RefCell::new(None),
        fn_number_is_nan_static: std::cell::RefCell::new(None),
        fn_number_is_finite_static: std::cell::RefCell::new(None),
        fn_number_is_integer: std::cell::RefCell::new(None),
        fn_number_parse_float_static: std::cell::RefCell::new(None),
        fn_number_parse_int: std::cell::RefCell::new(None),
        fn_number_epsilon: std::cell::RefCell::new(None),
        fn_number_max_safe_integer: std::cell::RefCell::new(None),
        fn_number_max_value: std::cell::RefCell::new(None),
        fn_number_min_safe_integer: std::cell::RefCell::new(None),
        fn_number_min_value: std::cell::RefCell::new(None),
        fn_number_nan: std::cell::RefCell::new(None),
        fn_number_negative_infinity: std::cell::RefCell::new(None),
        fn_number_positive_infinity: std::cell::RefCell::new(None),
        fn_number_is_safe_integer: std::cell::RefCell::new(None),
        fn_number_to_fixed: std::cell::RefCell::new(None),
        fn_number_to_exponential: std::cell::RefCell::new(None),
        fn_number_to_precision: std::cell::RefCell::new(None),
        fn_number_to_locale_string: std::cell::RefCell::new(None),
        fn_number_value_of: std::cell::RefCell::new(None),
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
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
        last_expr_oats_type: RefCell::new(None),
        async_await_live_sets: RefCell::new(None),
        generator_yield_live_sets: RefCell::new(None),
        generator_local_name_to_slot: RefCell::new(None),
        generator_resume_blocks: RefCell::new(None),
        generator_cont_blocks: RefCell::new(None),
        generator_next_function: RefCell::new(None),
        generator_yield_counter: std::cell::Cell::new(0),
        generator_param_count: std::cell::Cell::new(0),
        generator_local_slot_count: std::cell::Cell::new(0),
        generator_next_locals: RefCell::new(None),
        async_local_name_to_slot: RefCell::new(None),
        async_resume_blocks: RefCell::new(None),
        async_cont_blocks: RefCell::new(None),
        async_poll_function: RefCell::new(None),
        async_await_counter: Cell::new(0),
        async_param_count: Cell::new(0),
        async_local_slot_count: Cell::new(0),
        async_poll_locals: RefCell::new(None),
        source: &parsed_mod.source,
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: RefCell::new(None),
        nested_generic_fns: RefCell::new(HashMap::new()),
        nested_generic_classes: RefCell::new(HashMap::new()),
        monomorphized_map: RefCell::new(HashMap::new()),
        mut_var_decls: parsed_mod.mut_var_decls.clone(),
        current_function_return_type: RefCell::new(None),
        last_expr_is_boxed_union: Cell::new(false),
        global_function_signatures: RefCell::new(HashMap::new()),
        symbol_table: RefCell::new(symbols),
        external_std_fns: RefCell::new(HashMap::new()),
        current_label: RefCell::new(None),
        rta_results: Some(rta_results),
        uses_async: Cell::new(false),
        returned_ownership: Cell::new(false),
    };

    // Note: class field metadata is computed per-class when emitting
    // constructors below. A global pre-scan of the module is avoided so
    // member-field inference is based on declared/annotated types only.

    // Emit IR for class methods/constructors. Emit for both exported and
    // non-exported class declarations so constructors are available for
    // `new` expressions regardless of export status.
    for stmt in &parsed.body {
        // Handle class declarations: `class Foo {}`
        if let oxc_ast::ast::Statement::ClassDeclaration(c) = stmt {
            codegen.gen_class_ir(c, "").map_err(|d| {
                crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                anyhow::anyhow!("{}", d.message)
            })?;
        }

        // Handle enum declarations: `enum Name { Variant1, Variant2(Type1, Type2), ... }`
        if let Statement::TSEnumDeclaration(e) = stmt {
            let enum_name = e.id.name.to_string();
            if let Err(d) = codegen.gen_enum_ir(&enum_name, e) {
                diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                return Err(anyhow::anyhow!(d.message));
            }
        }

        // Handle type alias declarations: `type Name<T?> = Type;`
        if let oxc_ast::ast::Statement::TSTypeAliasDeclaration(ta) = stmt {
            // Store the alias: name -> (type_params, aliased_type)
            // Stores a reference to the type annotation since TSType doesn't implement Clone/Copy
            // The AST nodes live in the allocator for the lifetime of codegen, so this is safe
            let alias_name = ta.id.name.to_string();
            let type_params = ta.type_parameters.as_ref().map(|params| {
                params
                    .params
                    .iter()
                    .map(|p| p.name.name.to_string())
                    .collect::<Vec<String>>()
            });
            codegen
                .type_aliases
                .borrow_mut()
                .insert(alias_name, (type_params, &ta.type_annotation));
        }

        // Handle interface declarations: `interface Name { ... }`
        // Interfaces are structural types in TypeScript/Oats - they don't generate runtime code
        // but are used for type checking. Interface metadata (property names and types) is stored
        // for potential future type checking passes.
        if let Statement::TSInterfaceDeclaration(interface) = stmt {
            let interface_name = interface.id.name.to_string();
            // For top-level interfaces, use the interface name as both key and mangled name
            // (no prefix needed for top-level declarations)
            let mangled_name = interface_name.clone();

            // Register in symbol table so parameter types can resolve to this interface
            codegen
                .symbol_table
                .borrow_mut()
                .insert_type(interface_name.clone(), mangled_name.clone());

            // Extract interface properties and their types
            let mut interface_props: Vec<(String, crate::types::OatsType)> = Vec::new();

            for member in &interface.body.body {
                if let oxc_ast::ast::TSSignature::TSPropertySignature(prop) = member {
                    // Extract property name
                    let prop_name = match &prop.key {
                        oxc_ast::ast::PropertyKey::StaticIdentifier(ident) => {
                            ident.name.to_string()
                        }
                        oxc_ast::ast::PropertyKey::Identifier(ident) => ident.name.to_string(),
                        oxc_ast::ast::PropertyKey::StringLiteral(s) => s.value.to_string(),
                        _ => {
                            eprintln!(
                                "Skipping property key: {:?}",
                                std::mem::discriminant(&prop.key)
                            );
                            continue; // Skip computed keys
                        }
                    };

                    // Extract property type
                    if let Some(type_ann) = &prop.type_annotation {
                        // Convert TSType to OatsType - use map_ts_type_with_aliases to support
                        // type aliases and symbol table lookups (needed for interface types)
                        let oats_type_opt = crate::types::map_ts_type_with_aliases(
                            &type_ann.type_annotation,
                            Some(&*codegen.type_aliases.borrow()),
                            Some(&*codegen.symbol_table.borrow()),
                        );
                        if let Some(oats_type) = oats_type_opt {
                            interface_props.push((prop_name.clone(), oats_type.clone()));
                        }
                        // Skip properties with unsupported types
                    }
                } else {
                    // Methods, index signatures, etc. are not yet stored
                    // They can be added in the future if needed for type checking
                }
            }

            // Store interface metadata with mangled name (same as interface_name for top-level)
            codegen
                .interface_fields
                .borrow_mut()
                .insert(mangled_name, interface_props);
        }

        // Handle namespace declarations: `namespace Name { ... }`
        // Namespaces provide scoping and name mangling. Currently handles them
        // by processing their body statements in the same scope (simplified approach).
        // TODO: Implement proper namespace scoping and name mangling
        if let Statement::TSModuleDeclaration(namespace) = stmt {
            let namespace_name = match &namespace.id {
                TSModuleDeclarationName::Identifier(id) => id.name.to_string(),
                _ => continue,
            };
            // Processes namespace body statements as if they were at module level
            // This is a simplified approach - proper implementation would need:
            // 1. Namespace scoping (qualified names like Namespace::member)
            // 2. Name mangling for codegen (e.g., Namespace_member)
            // 3. Support for nested namespaces
            // TODO: Implement proper namespace handling
            let _ = namespace_name; // Suppress unused variable warning
            // Note: Namespace body statements are already processed in the main loop
            // since they're part of the module's body. This is a placeholder for future work.
        }
    }

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which is handled
    // separately.
    // Before emitting functions, evaluate and emit top-level `const`
    // declarations. This pass collects all top-level consts, builds a
    // dependency graph between them, topologically sorts the graph, and
    // then evaluates each const in order using the codegen const evaluator.
    // Emitted heap-style consts (strings/arrays/objects) are added as LLVM
    // globals via `CodeGen::emit_const_global` and cached in
    // `codegen.const_globals` so expression lowering can reuse them.
    {
        use std::collections::{HashMap, HashSet, VecDeque};

        // Collect top-level const declarations: name -> (init_expr, span_start)
        let mut top_level_consts: Vec<(String, &Expression<'_>, usize)> = Vec::new();
        for stmt in &parsed.body {
            if let Statement::VariableDeclaration(vd) = stmt
                && matches!(vd.kind, VariableDeclarationKind::Const)
            {
                for decl in &vd.declarations {
                    if let BindingPatternKind::BindingIdentifier(binding) = &decl.id.kind {
                        if let Some(init) = &decl.init {
                            let name = binding.name.to_string();
                            let span_start = vd.span.start as usize;
                            top_level_consts.push((name, init, span_start));
                        } else {
                            return diagnostics::report_error_and_bail(
                                Some(&src_path),
                                Some(&source),
                                "top-level `const` must have an initializer",
                                Some("Rust-like `const` requires compile-time initializer."),
                            );
                        }
                    }
                    // Destructuring patterns not yet supported for top-level consts
                }
            }
        }

        if !top_level_consts.is_empty() {
            // Build a name -> index map
            let mut name_to_idx: HashMap<String, usize> = HashMap::new();
            for (i, (n, _, _)) in top_level_consts.iter().enumerate() {
                name_to_idx.insert(n.clone(), i);
            }

            // Helper: collect identifier names referenced by an expression
            fn collect_idents(e: &oxc_ast::ast::Expression<'_>, out: &mut HashSet<String>) {
                use oxc_ast::ast::Expression;
                match e {
                    Expression::Identifier(id) => {
                        out.insert(id.name.to_string());
                    }
                    Expression::ArrayExpression(arr) => {
                        for el in &arr.elements {
                            if let Some(expr) = el.as_expression() {
                                collect_idents(expr, out);
                            } else if let oxc_ast::ast::ArrayExpressionElement::SpreadElement(
                                spread,
                            ) = el
                            {
                                collect_idents(&spread.argument, out);
                            }
                            // Elision (empty slot) - nothing to collect
                        }
                    }
                    Expression::ObjectExpression(obj) => {
                        for prop in &obj.properties {
                            match prop {
                                oxc_ast::ast::ObjectPropertyKind::ObjectProperty(kv) => {
                                    collect_idents(&kv.value, out);
                                }
                                oxc_ast::ast::ObjectPropertyKind::SpreadProperty(spread) => {
                                    collect_idents(&spread.argument, out);
                                }
                            }
                        }
                    }
                    Expression::UnaryExpression(u) => collect_idents(&u.argument, out),
                    Expression::BinaryExpression(b) => {
                        collect_idents(&b.left, out);
                        collect_idents(&b.right, out);
                    }
                    Expression::CallExpression(c) => {
                        // Callee is an Expression
                        collect_idents(&c.callee, out);
                        for a in &c.arguments {
                            if let Some(expr) = a.as_expression() {
                                collect_idents(expr, out);
                            } else if let oxc_ast::ast::Argument::SpreadElement(spread) = a {
                                collect_idents(&spread.argument, out);
                            }
                        }
                    }
                    Expression::StaticMemberExpression(m) => {
                        collect_idents(&m.object, out);
                    }
                    Expression::ComputedMemberExpression(m) => {
                        collect_idents(&m.object, out);
                        collect_idents(&m.expression, out);
                    }
                    Expression::NewExpression(n) => {
                        collect_idents(&n.callee, out);
                        for a in &n.arguments {
                            if let Some(expr) = a.as_expression() {
                                collect_idents(expr, out);
                            } else if let oxc_ast::ast::Argument::SpreadElement(spread) = a {
                                collect_idents(&spread.argument, out);
                            }
                        }
                    }
                    Expression::ParenthesizedExpression(p) => collect_idents(&p.expression, out),
                    Expression::ConditionalExpression(c) => {
                        collect_idents(&c.test, out);
                        collect_idents(&c.consequent, out);
                        collect_idents(&c.alternate, out);
                    }
                    _ => {}
                }
            }

            // Build adjacency and indegree maps only for dependencies that are
            // other top-level const names (ignore other idents).
            let mut adj: Vec<Vec<usize>> = vec![Vec::new(); top_level_consts.len()];
            let mut indeg: Vec<usize> = vec![0; top_level_consts.len()];

            for (i, (_n, init, _)) in top_level_consts.iter().enumerate() {
                let mut ids = HashSet::new();
                collect_idents(init, &mut ids);
                for id in ids {
                    if let Some(&j) = name_to_idx.get(&id) {
                        // edge j -> i (j must be evaluated before i)
                        adj[j].push(i);
                        indeg[i] += 1;
                    }
                }
            }

            // Kahn's algorithm
            let mut q: VecDeque<usize> = VecDeque::new();
            for (i, &d) in indeg.iter().enumerate() {
                if d == 0 {
                    q.push_back(i);
                }
            }
            let mut order: Vec<usize> = Vec::new();
            while let Some(u) = q.pop_front() {
                order.push(u);
                for &v in &adj[u] {
                    indeg[v] -= 1;
                    if indeg[v] == 0 {
                        q.push_back(v);
                    }
                }
            }
            if order.len() != top_level_consts.len() {
                // Cycle detected
                return diagnostics::report_error_and_bail(
                    Some(&src_path),
                    Some(&source),
                    "cyclic dependency among top-level const declarations",
                    Some("Top-level `const` declarations form a cycle."),
                );
            }
        }
    }

    // Handle non-exported function declarations: `function foo() {}`
    for stmt in &parsed.body {
        if let Statement::FunctionDeclaration(fn_decl) = stmt {
            let fname = if let Some(id) = &fn_decl.id {
                id.name.to_string()
            } else {
                continue;
            };
            // Use Function directly from oxc_ast
            let inner_func = fn_decl;
            let mut inner_symbols = SymbolTable::new();
            let (sig_opt, type_diags) = check_function_strictness(inner_func, &mut inner_symbols)?;

            for diag in &type_diags {
                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
            }

            if let Some(fsig) = sig_opt {
                // skip exported `main` (exported main is handled separately)
                if fname != "main" {
                    // Register in symbol table
                    codegen.symbol_table.borrow_mut().insert_std_function(
                        fname.clone(),
                        fname.clone(),
                        fsig.clone(),
                    );

                    codegen
                        .gen_function_ir(&fname, inner_func, &fsig.params, &fsig.ret, None)
                        .map_err(|d| {
                            crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                            anyhow::anyhow!("{}", d.message)
                        })?;
                }
            }
        }
    }

    // Emit arrow functions declared as top-level const/let bindings
    // Example: const foo = (x: number): number => x + 1;
    for (fname, arrow, is_exported) in &arrow_functions {
        // Skip exported main - it will be handled separately
        if fname == "main" && *is_exported {
            continue;
        }
        // Skip non-exported main as well
        if fname == "main" {
            continue;
        }

        // For arrows, infer the signature if missing
        let mut param_types = Vec::new();
        for p in &arrow.params.items {
            if let Some(ann) = &p.pattern.type_annotation
                && let Some(ty) = crate::types::map_ts_type(&ann.type_annotation)
            {
                param_types.push(ty);
                continue;
            }
            param_types.push(crate::types::OatsType::Number);
        }

        let ret_type = if let Some(ann) = &arrow.return_type {
            crate::types::map_ts_type(&ann.type_annotation)
                .unwrap_or(crate::types::OatsType::Number)
        } else {
            crate::types::OatsType::Number
        };

        let sig = crate::types::FunctionSig {
            type_params: Vec::new(),
            params: param_types.clone(),
            ret: ret_type.clone(),
        };

        // Register in symbol table
        codegen.symbol_table.borrow_mut().insert_std_function(
            fname.clone(),
            fname.clone(), // top-level name is not mangled
            sig,
        );

        // Generate IR
        codegen
            .gen_arrow_function_ir(fname, arrow, &param_types, &ret_type)
            .map_err(|d| {
                crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                anyhow::anyhow!("{}", d.message)
            })?;
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but `oats_main` is generated as the emitted symbol the host
    // runtime will call.
    if let (Some(func), Some(sig)) = (func_decl, func_sig.as_ref()) {
        codegen
            .gen_function_ir(
                crate::runtime_functions::names::OATS_MAIN,
                func,
                &sig.params,
                &sig.ret,
                None,
            )
            .map_err(|d| {
                crate::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                anyhow::anyhow!("{}", d.message)
            })?;
    }

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    // Even when emit_object_only is true, if we found a main function,
    // we should emit the host main (this is needed for entry point modules
    // in multi-module builds).
    let emitted_host_main = if let Some(sig) = func_sig {
        codegen.emit_host_main(&sig.params, &sig.ret)
    } else {
        false
    };

    // Note: LLVM optimizations are now applied directly in the compiler before object emission
    // Check if we should dump IR for debugging before optimization
    let dump_ir_before =
        options.as_ref().is_some_and(|o| o.dump_ir) || std::env::var("OATS_DUMP_IR").is_ok();

    if dump_ir_before {
        let out_dir = match options
            .as_ref()
            .and_then(|o| o.out_dir.as_ref())
            .cloned()
            .or_else(|| std::env::var("OATS_OUT_DIR").ok())
        {
            Some(dir) => dir,
            None => ".".to_string(),
        };
        let src_filename = std::path::Path::new(&src_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("out");
        let out_ll_before = format!("{out_dir}/{src_filename}.before.ll");

        if let Err(e) = std::fs::create_dir_all(&out_dir) {
            eprintln!("Warning: failed to create output directory for IR dump: {e}");
        } else {
            let ir_before = codegen.module.print_to_string().to_string();
            if let Err(e) = std::fs::write(&out_ll_before, ir_before) {
                eprintln!("Warning: failed to write pre-optimization IR to {out_ll_before}: {e}");
            } else {
                eprintln!("Dumped pre-optimization IR to {out_ll_before}");
            }
        }
    }

    // Note: LLVM optimizations (inlining, loop opts) are applied in linking.rs before object emission
    let ir = codegen.module.print_to_string().to_string();

    // Determine output directory and file names
    let out_dir = match options
        .as_ref()
        .and_then(|o| o.out_dir.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_OUT_DIR").ok())
    {
        Some(dir) => dir,
        None => ".".to_string(),
    };
    let src_filename = std::path::Path::new(&src_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");

    // Determine if IR and object files should be emitted
    let emit_ir = options.as_ref().is_some_and(|o| o.emit_ir);
    // When emit_object_only is true, we should also emit the object file to disk
    // so it can be used by the linker
    let emit_obj = options.as_ref().is_some_and(|o| o.emit_obj)
        || options.as_ref().is_some_and(|o| o.emit_object_only);

    let out_ll = format!("{out_dir}/{src_filename}.ll");
    let out_exe = if let Some(opts) = options {
        if let Some(ref name) = opts.out_name {
            format!("{out_dir}/{name}")
        } else {
            format!("{out_dir}/{src_filename}")
        }
    } else if let Ok(name) = std::env::var("OATS_OUT_NAME") {
        format!("{out_dir}/{name}")
    } else {
        format!("{out_dir}/{src_filename}")
    };

    // Determine object file path (only needed if emit_obj is true or for emit_object_only)
    let out_obj = format!("{out_dir}/{src_filename}.o");

    // Ensure output directory exists (only needed if files are being emitted there)
    if (emit_ir || emit_obj)
        && let Err(e) = std::fs::create_dir_all(&out_dir)
    {
        anyhow::bail!("failed to create output directory {out_dir}: {e}");
    }

    // Write LLVM IR to file only if emit_ir is enabled
    if emit_ir {
        let mut f = File::create(&out_ll)?;
        f.write_all(ir.as_bytes())?;
        f.sync_all()?;
    }

    // Determine optimization level and build profile
    let build_profile = options
        .as_ref()
        .and_then(|o| o.build_profile.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_BUILD_PROFILE").ok())
        .unwrap_or_default();
    let opt_level = if let Some(opts) = options {
        if let Some(ref opt_str) = opts.opt_level {
            match opt_str.as_str() {
                "none" | "0" => OptimizationLevel::None,
                "2" | "3" | "aggressive" => OptimizationLevel::Aggressive,
                _ => OptimizationLevel::Default,
            }
        } else if build_profile == "release" {
            OptimizationLevel::Aggressive
        } else {
            OptimizationLevel::None
        }
    } else if let Ok(opt_str) = std::env::var("OATS_OPT_LEVEL") {
        match opt_str.as_str() {
            "none" | "0" => OptimizationLevel::None,
            "2" | "3" | "aggressive" => OptimizationLevel::Aggressive,
            _ => OptimizationLevel::Default,
        }
    } else if build_profile == "release" {
        OptimizationLevel::Aggressive
    } else {
        OptimizationLevel::None
    };

    // Create linking configuration
    let linking_config = crate::linking::LinkingConfig {
        out_dir: out_dir.clone(),
        out_obj: out_obj.clone(),
        out_exe,
        opt_level,
        target_triple: options
            .as_ref()
            .and_then(|o| o.target_triple.as_ref())
            .cloned(),
        target_cpu: options
            .as_ref()
            .and_then(|o| o.target_cpu.as_ref())
            .cloned()
            .or_else(|| std::env::var("OATS_TARGET_CPU").ok()),
        target_features: options
            .as_ref()
            .and_then(|o| o.target_features.as_ref())
            .cloned()
            .or_else(|| std::env::var("OATS_TARGET_FEATURES").ok()),
        build_profile,
        lto: options
            .as_ref()
            .and_then(|o| o.lto.as_ref())
            .cloned()
            .or_else(|| std::env::var("OATS_LTO").ok()),
        linker: options
            .as_ref()
            .and_then(|o| o.linker.as_ref())
            .cloned()
            .or_else(|| std::env::var("OATS_LINKER").ok()),
        emit_object_only: match options.as_ref().map(|o| o.emit_object_only) {
            Some(val) => val,
            None => std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok(),
        },
    };

    // Compile LLVM IR to object file bytes (in memory)
    let obj_bytes = crate::linking::compile_to_object_bytes(&codegen.module, &linking_config)?;

    // If emit_obj is true, write object file to disk
    if emit_obj {
        std::fs::write(&out_obj, &obj_bytes)
            .map_err(|e| anyhow::anyhow!("Failed to write object file {out_obj}: {e}"))?;
    }

    // If requested, only emit the object and skip the final host linking step.
    if linking_config.emit_object_only {
        if emit_obj {
            eprintln!("OATS_EMIT_OBJECT_ONLY set; emitted {out_obj} and skipping link");
            println!("{out_obj}");
        } else {
            eprintln!(
                "OATS_EMIT_OBJECT_ONLY set; object file compiled in memory (use --emit-obj to write to disk)"
            );
        }
        return Ok(if emit_obj { Some(out_obj) } else { None });
    }

    // Get runtime libraries (runtime + std)
    let rust_libs = crate::linking::get_runtime_libraries()?;

    // Locate or compile rt_main object
    let rt_main_obj = crate::linking::locate_rt_main(&out_dir, emitted_host_main)?;

    // For linking, the object file must be on disk. Write to temp file if emit_obj is false.
    let (link_obj_path, is_temp) = if emit_obj {
        (out_obj, false)
    } else {
        // Write to temporary file for linking
        let temp_dir = std::env::temp_dir();
        let temp_filename = format!("oatsc_{}_{}.o", src_filename, std::process::id());
        let temp_path = temp_dir.join(temp_filename).to_string_lossy().to_string();
        std::fs::write(&temp_path, &obj_bytes).map_err(|e| {
            anyhow::anyhow!("Failed to write temporary object file {temp_path}: {e}")
        })?;
        (temp_path, true)
    };

    // Link final executable
    let final_exe =
        crate::linking::link_executable(&linking_config, &link_obj_path, &rust_libs, &rt_main_obj)?;

    // Clean up temporary object file if one was created
    if is_temp && let Err(e) = std::fs::remove_file(&link_obj_path) {
        // Log but don't fail - temp files will be cleaned up eventually
        eprintln!("Warning: Failed to remove temporary object file {link_obj_path}: {e}");
    }

    println!("{final_exe}");
    Ok(Some(final_exe))
}
