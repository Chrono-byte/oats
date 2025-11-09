use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::process::Command;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::codegen::CodeGen;
use crate::diagnostics;
use crate::parser;
use crate::types::{SymbolTable, check_function_strictness};

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

/// Compiles a source file with the given options.
///
/// This is the main entry point for compilation when you have options ready.
/// Unlike run_from_args, it doesn't mess with environment variables or CLI parsing.
///
/// # Arguments
/// * `options` - The compilation options
///
/// # Returns
/// Path to the output file on success, or None if just object emission
pub fn compile_with_options(options: crate::CompileOptions) -> Result<Option<String>> {
    // Pass options directly through the call stack
    let args = vec!["oatsc".to_string(), options.src_file.clone()];
    run_from_args(&args, Some(options))
}

/// Executes the complete AOT compilation pipeline from source to executable.
///
/// This is the main function that handles everything: parsing, type checking, codegen, linking.
/// It takes command-line args or uses options/env vars for config.
///
/// Basically, it goes through:
/// - Figuring out the source file
/// - Parsing the code
/// - Generating LLVM IR
/// - Compiling to object files
/// - Linking everything together
///
/// Returns Ok(()) on success, or an error if something goes wrong.
pub fn run_from_args(
    args: &[String],
    options: Option<crate::CompileOptions>,
) -> Result<Option<String>> {
    // Resolve source file location from arguments, options, or environment
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Some(ref opts) = options {
        opts.src_file.clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    let source = std::fs::read_to_string(&src_path)?;

    // OPTIMIZATION: Arrow functions declared as `let foo = (...) => ...` are now
    // processed directly during codegen without intermediate source transformation.
    // This eliminates redundant parsing passes and improves compilation performance.

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
    let extern_oats: std::collections::HashMap<String, String> = if let Some(ref opts) = options {
        // Validate that all metadata file paths exist
        for (import_path, meta_path) in &opts.extern_oats {
            let path = std::path::Path::new(meta_path);
            if !path.exists() {
                anyhow::bail!(
                    "extern_oats entry '{}' points to non-existent metadata file: {}",
                    import_path,
                    meta_path
                );
            }
            if !path.is_file() {
                anyhow::bail!(
                    "extern_oats entry '{}' points to a non-file path: {}",
                    import_path,
                    meta_path
                );
            }
        }
        opts.extern_oats.clone()
    } else if let Ok(extern_oats_json) = std::env::var("OATS_EXTERN_OATS") {
        let map: std::collections::HashMap<String, String> =
            serde_json::from_str(&extern_oats_json)
                .map_err(|e| anyhow::anyhow!("Failed to parse OATS_EXTERN_OATS JSON: {}", e))?;

        // Validate that all metadata file paths exist
        for (import_path, meta_path) in &map {
            let path = std::path::Path::new(meta_path);
            if !path.exists() {
                anyhow::bail!(
                    "extern_oats entry '{}' points to non-existent metadata file: {}",
                    import_path,
                    meta_path
                );
            }
            if !path.is_file() {
                anyhow::bail!(
                    "extern_oats entry '{}' points to a non-file path: {}",
                    import_path,
                    meta_path
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
    /// Returns a list of (name, arrow_expr, is_exported) tuples.
    fn extract_arrow_functions(
        parsed: &oats_ast::Module,
    ) -> Vec<(String, oats_ast::ArrowExpr, bool)> {
        use oats_ast::*;
        let mut arrows = Vec::new();

        for stmt in &parsed.body {
            // Process non-exported variable declarations: const/let foo = () => {}
            if let Stmt::VarDecl(vdecl) = stmt
                && vdecl.decls.len() == 1
            {
                let decl = &vdecl.decls[0];
                if let Pat::Ident(binding_ident) = &decl.name
                    && let Some(init_expr) = &decl.init
                    && let Expr::Arrow(arrow) = init_expr
                {
                    let name = binding_ident.sym.clone();
                    arrows.push((name, arrow.clone(), false));
                }
                // Destructuring patterns not yet supported for arrow function bindings
            }
            // Note: oats_ast doesn't have separate export declarations in the AST
            // Exports are handled differently - for now we only handle non-exported
        }
        arrows
    }

    let arrow_functions = extract_arrow_functions(parsed);

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    // NOTE: This function is now consolidated in crate::types::infer_type_from_expr

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
    let mut func_decl_opt: Option<oats_ast::Function> = None;
    for stmt in &parsed.body {
        use oats_ast::*;
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

    let func_decl: Option<oats_ast::Function> = if let Some(f) = func_decl_opt {
        Some(f)
    } else {
        // Check if we're in library mode (emitting object only)
        let emit_object_only = options
            .as_ref()
            .map(|o| o.emit_object_only)
            .unwrap_or_else(|| std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok());
        if emit_object_only {
            // For library modules, we still need to compile but skip main function requirement
            // We'll continue with compilation but won't require a main function
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
    let func_sig = if let Some(ref func) = func_decl {
        let (sig_opt, type_diags) = check_function_strictness(func, &mut symbols)?;
        for diag in &type_diags {
            diagnostics::emit_diagnostic(diag, Some(&source));
        }
        sig_opt
    } else {
        None
    };

    let context = Context::create();
    let module = context.create_module("oats_aot");
    // Set the module target triple to the host default so clang doesn't warn
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    // Declare external functions for imported symbols
    // Read metadata files to extract function signatures
    let mut external_function_signatures: std::collections::HashMap<
        String,
        (Vec<crate::types::OatsType>, crate::types::OatsType),
    > = std::collections::HashMap::new();

    for meta_path in extern_oats.values() {
        // Read metadata file
        let meta_content = match std::fs::read_to_string(meta_path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Warning: Failed to read metadata file {}: {}", meta_path, e);
                continue;
            }
        };

        // Parse metadata file - currently it contains package info (name, version, entry)
        // TODO: Extend metadata format to include exported function signatures
        // For now, we'll try to extract symbols from the file or use a fallback

        // Check if the metadata file contains symbol information
        // If not, we'll need to fall back to the old behavior or use default signatures
        // For now, we'll parse any function declarations if they exist in the metadata

        // Try to parse TypeScript-style function declarations from metadata
        // Format: function name(param1: type1, param2: type2): returnType;
        for line in meta_content.lines() {
            let line = line.trim();
            if line.starts_with("function ") && line.ends_with(";") {
                // Parse function signature
                // Example: function foo(x: number, y: number): number;
                if let Some((name, func_sig)) = parse_function_signature_from_metadata(line) {
                    external_function_signatures.insert(name, (func_sig.params, func_sig.ret));
                }
            }
        }
    }

    // Declare external functions with their signatures (or defaults)
    for meta_path in extern_oats.values() {
        // For now, if we don't have signature info, we need to know what symbols to declare
        // The old code treated values as comma-separated symbol strings
        // For backward compatibility, check if the value looks like a file path or symbol list
        let symbols: Vec<String> = if std::path::Path::new(meta_path).exists() {
            // It's a file path - we've already read it above
            // Extract symbols from external_function_signatures or use a default list
            external_function_signatures.keys().cloned().collect()
        } else {
            // Backward compatibility: treat as comma-separated symbol list
            meta_path.split(',').map(|s| s.trim().to_string()).collect()
        };

        for symbol in symbols {
            // Get function signature or use default
            let (param_types, ret_type) = external_function_signatures
                .get(&symbol)
                .cloned()
                .unwrap_or_else(|| (vec![], crate::types::OatsType::Number));

            // Convert OatsType to LLVM types
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
                    _ => context.f64_type().into(), // Default to f64 for complex types
                })
                .collect();

            let fn_type = match ret_type {
                crate::types::OatsType::Void => {
                    let void_t = context.void_type();
                    void_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::Number | crate::types::OatsType::F64 => {
                    let f64_t = context.f64_type();
                    f64_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::F32 => {
                    let f32_t = context.f32_type();
                    f32_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::I64 => {
                    let i64_t = context.i64_type();
                    i64_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::I32 => {
                    let i32_t = context.i32_type();
                    i32_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::I8 => {
                    let i8_t = context.i8_type();
                    i8_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::Boolean => {
                    let bool_t = context.bool_type();
                    bool_t.fn_type(&llvm_param_types, false)
                }
                crate::types::OatsType::String => {
                    let ptr_t = context.ptr_type(inkwell::AddressSpace::default());
                    ptr_t.fn_type(&llvm_param_types, false)
                }
                _ => {
                    // Default to f64 for complex types
                    let f64_t = context.f64_type();
                    f64_t.fn_type(&llvm_param_types, false)
                }
            };
            module.add_function(&symbol, fn_type, None);
        }
    }

    // Helper function to parse function signatures from metadata
    fn parse_function_signature_from_metadata(
        line: &str,
    ) -> Option<(String, crate::types::FunctionSig)> {
        // Parse: function name(param1: type1, param2: type2): returnType;
        let line = line.trim().strip_suffix(';')?;
        if !line.starts_with("function ") {
            return None;
        }
        let after_fn = &line[9..]; // Skip "function "
        let paren_pos = after_fn.find('(')?;
        let name = after_fn[..paren_pos].trim().to_string();

        let after_paren = &after_fn[paren_pos + 1..];
        let close_paren_pos = after_paren.find(')')?;
        let params_str = &after_paren[..close_paren_pos];

        // Parse return type
        let ret_str = after_paren[close_paren_pos + 1..].trim();
        let ret_type = if let Some(stripped) = ret_str.strip_prefix(": ") {
            parse_oats_type(stripped)
        } else {
            crate::types::OatsType::Void
        };

        // Parse parameters
        let mut param_types = Vec::new();
        if !params_str.trim().is_empty() {
            for param in params_str.split(',') {
                let param = param.trim();
                if let Some(colon_pos) = param.find(':') {
                    let type_str = param[colon_pos + 1..].trim();
                    param_types.push(parse_oats_type(type_str));
                }
            }
        }

        Some((
            name,
            crate::types::FunctionSig {
                type_params: vec![],
                params: param_types,
                ret: ret_type,
            },
        ))
    }

    fn parse_oats_type(type_str: &str) -> crate::types::OatsType {
        match type_str.trim() {
            "number" => crate::types::OatsType::Number,
            "f64" => crate::types::OatsType::F64,
            "f32" => crate::types::OatsType::F32,
            "i64" => crate::types::OatsType::I64,
            "i32" => crate::types::OatsType::I32,
            "i8" => crate::types::OatsType::I8,
            "boolean" => crate::types::OatsType::Boolean,
            "string" => crate::types::OatsType::String,
            "void" => crate::types::OatsType::Void,
            _ => crate::types::OatsType::Number, // Default
        }
    }

    // Declare std functions
    // console.log functions
    let void_t = context.void_type();
    let i8ptr_t = context.ptr_type(inkwell::AddressSpace::default());
    module.add_function(
        "oats_std_console_log",
        void_t.fn_type(&[i8ptr_t.into()], false),
        None,
    );

    // Perform Rapid Type Analysis on all modules
    // For now, we only have a single module, but RTA is designed to work with multiple modules
    // We create a HashMap with the single module for RTA analysis
    let mut modules_map = std::collections::HashMap::new();
    // Clone parsed_mod since we need it later and RTA only reads from it
    modules_map.insert(src_path.clone(), parsed_mod.clone());

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
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        class_fields: RefCell::new(HashMap::new()),
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
        source: &parsed_mod.source,
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: RefCell::new(None),
        nested_generic_fns: RefCell::new(HashMap::new()),
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
    };

    // Note: class field metadata is computed per-class when emitting
    // constructors below. We avoid a global pre-scan of the module so
    // member-field inference is based on declared/annotated types only.

    // Emit IR for class methods/constructors. Emit for both exported and
    // non-exported class declarations so constructors are available for
    // `new` expressions regardless of export status.
    for stmt in &parsed.body {
        use oats_ast::*;
        // Handle class declarations: `class Foo {}`
        if let Stmt::ClassDecl(c) = stmt {
            let class_name = c.ident.sym.clone();
            // If this class extends a parent, record the parent name so constructors
            // and `super(...)` lowering can find the parent's initializer.
            let parent_name_opt = if let Some(Expr::Ident(id)) = &c.super_class {
                Some(id.sym.clone())
            } else {
                None
            };
            // Store class hierarchy for persistent access
            codegen.class_parents.borrow_mut().insert(class_name.clone(), parent_name_opt.clone());
            // Also set current_class_parent for constructor codegen
            *codegen.current_class_parent.borrow_mut() = parent_name_opt.clone();

            // Emit members for this class
            for member in &c.body {
                match member {
                    ClassMember::Method(m) => {
                        // method name
                        let mname = m.ident.sym.clone();
                        // Convert MethodDecl to Function for type checking
                        let method_func = Function {
                            params: m.params.clone(),
                            body: m.body.clone(),
                            return_type: m.return_type.clone(),
                            span: m.span.clone(),
                            is_async: false,
                            is_generator: false,
                        };
                        // Try to type-check the method function
                        let mut method_symbols = SymbolTable::new();
                        let (sig_opt, type_diags) =
                            check_function_strictness(&method_func, &mut method_symbols)?;

                        // Emit type checking diagnostics
                        for diag in &type_diags {
                            diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                        }

                        if let Some(sig) = sig_opt {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(crate::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &method_func, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            let (sig2_opt, type_diags2) =
                                check_function_strictness(&method_func, &mut method_symbols)?;

                            for diag in &type_diags2 {
                                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                            }

                            if let Some(sig2) = sig2_opt {
                                let mut params = Vec::new();
                                params.push(crate::types::OatsType::NominalStruct(
                                    class_name.clone(),
                                ));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &method_func,
                                        &params,
                                        &crate::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        crate::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for this class from explicit props, constructor
                        // param properties, and `this.x = ...` assignments inside the ctor.
                        let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
                        use oats_ast::*;
                        // explicit class properties
                        for m in &c.body {
                            if let ClassMember::Field(field) = m {
                                let fname = field.ident.sym.clone();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    // If the class property has an Oats type annotation, map it
                                    // to an OatsType; otherwise default to Number.
                                    let ftype = if let Some(type_ann) = &field.ty {
                                        if let Some(mt) = crate::types::map_ts_type_with_aliases(type_ann, Some(&*codegen.type_aliases.borrow())) {
                                            mt
                                        } else {
                                            crate::types::OatsType::Number
                                        }
                                    } else {
                                        crate::types::OatsType::Number
                                    };
                                    fields.push((fname, ftype));
                                }
                            }
                        }
                        // constructor param properties - oats_ast doesn't have param properties
                        // so we skip this for now
                        // scan ctor body for `this.x = ...` assignments
                        // Note: oats_ast::AssignTarget only supports Pat, not member assignments
                        // So we can't directly detect `this.x = ...` patterns here
                        // This functionality may need to be added to oats_ast or handled differently
                        // Register computed fields so lowering can reference them
                        codegen
                            .class_fields
                            .borrow_mut()
                            .insert(class_name.clone(), fields.clone());
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields, None)
                        {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
            // Done emitting this class; clear current parent
            codegen.current_class_parent.borrow_mut().take();
        }

        // Handle enum declarations: `enum Name { Variant1, Variant2(Type1, Type2), ... }`
        if let Stmt::EnumDecl(e) = stmt {
            let enum_name = e.ident.sym.clone();
            if let Err(d) = codegen.gen_enum_ir(&enum_name, e) {
                diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                return Err(anyhow::anyhow!(d.message));
            }
        }

        // Handle type alias declarations: `type Name<T?> = Type;`
        if let Stmt::TypeAlias(ta) = stmt {
            let alias_name = ta.ident.sym.clone();
            // Extract type parameter names if present
            let type_params = ta.type_params.as_ref().map(|params| {
                params.iter().map(|p| p.ident.sym.clone()).collect::<Vec<String>>()
            });
            // Store the alias: name -> (type_params, aliased_type)
            codegen.type_aliases.borrow_mut().insert(alias_name, (type_params, ta.ty.clone()));
        }
    }

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    // Before emitting functions, evaluate and emit top-level `const`
    // declarations. This pass collects all top-level consts, builds a
    // dependency graph between them, topologically sorts the graph, and
    // then evaluates each const in order using the codegen const evaluator.
    // Emitted heap-style consts (strings/arrays/objects) are added as LLVM
    // globals via `CodeGen::emit_const_global` and cached in
    // `codegen.const_globals` so expression lowering can reuse them.
    {
        use oats_ast::*;
        use std::collections::{HashMap, HashSet, VecDeque};

        // Collect top-level const declarations: name -> (init_expr, span_start)
        let mut top_level_consts: Vec<(String, Expr, usize)> = Vec::new();
        for stmt in &parsed.body {
            if let Stmt::VarDecl(vd) = stmt
                && matches!(vd.kind, VarDeclKind::Const)
            {
                for decl in &vd.decls {
                    if let Pat::Ident(binding) = &decl.name {
                        if let Some(init) = &decl.init {
                            let name = binding.sym.clone();
                            let span_start = vd.span.start;
                            top_level_consts.push((name, init.clone(), span_start));
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
            fn collect_idents(e: &oats_ast::Expr, out: &mut HashSet<String>) {
                use oats_ast::*;
                match e {
                    Expr::Ident(id) => {
                        out.insert(id.sym.clone());
                    }
                    Expr::Array(arr) => {
                        for el in arr.elems.iter().flatten() {
                            collect_idents(el, out);
                        }
                    }
                    Expr::Object(obj) => {
                        for prop in &obj.props {
                            if let PropOrSpread::Prop(Prop::KeyValue(kv)) = prop {
                                collect_idents(&kv.value, out);
                            }
                        }
                    }
                    Expr::Unary(u) => collect_idents(&u.arg, out),
                    Expr::Bin(b) => {
                        collect_idents(&b.left, out);
                        collect_idents(&b.right, out);
                    }
                    Expr::Call(c) => {
                        if let Callee::Expr(ec) = &c.callee {
                            collect_idents(ec, out);
                        }
                        for a in &c.args {
                            collect_idents(a, out);
                        }
                    }
                    Expr::Member(m) => {
                        collect_idents(&m.obj, out);
                        if let MemberProp::Computed(cmp) = &m.prop {
                            collect_idents(cmp, out);
                        }
                    }
                    Expr::New(n) => {
                        collect_idents(&n.callee, out);
                        for a in &n.args {
                            collect_idents(a, out);
                        }
                    }
                    Expr::Paren(p) => collect_idents(&p.expr, out),
                    Expr::Cond(c) => {
                        collect_idents(&c.test, out);
                        collect_idents(&c.cons, out);
                        collect_idents(&c.alt, out);
                    }
                    Expr::Tpl(_) | Expr::Lit(_) => {}
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
                    Some(
                        "Top-level `const` declarations form a cycle; Rust-style `const` requires acyclic compile-time dependencies.",
                    ),
                );
            }

            // Evaluate consts in topo order and emit globals where needed
            for idx in order {
                let (name, init_expr, span_start) = &top_level_consts[idx];
                // Evaluate with the current const_items map
                match crate::codegen::const_eval::eval_const_expr(
                    init_expr,
                    *span_start,
                    &codegen.const_items.borrow(),
                ) {
                    Ok(cv) => {
                        // Insert into const_items
                        codegen
                            .const_items
                            .borrow_mut()
                            .insert(name.clone(), cv.clone());
                        // If heap-shaped (string/array/objects) emit a const global
                        match cv {
                            crate::codegen::const_eval::ConstValue::Str(_)
                            | crate::codegen::const_eval::ConstValue::Array(_)
                            | crate::codegen::const_eval::ConstValue::Object(_) => {
                                // Emit a global named `const.<name>` and cache pointer under the bare name
                                let gname = format!("const.{}", name);
                                match codegen.emit_const_global(&gname, &cv) {
                                    Ok(ptr) => {
                                        codegen
                                            .const_globals
                                            .borrow_mut()
                                            .insert(name.clone(), ptr);
                                    }
                                    Err(d) => {
                                        diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                        return Err(anyhow::anyhow!(d.message));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Err(d) => {
                        diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        return Err(anyhow::anyhow!(d.message));
                    }
                }
            }
        }
    }

    // Handle non-exported function declarations: `function foo() {}`
    // Note: oats_ast doesn't have separate export syntax - exports are handled
    // separately via the arrow_functions collection and main function detection
    for stmt in &parsed.body {
        use oats_ast::*;
        if let Stmt::FnDecl(fn_decl) = stmt {
            let fname = fn_decl.ident.sym.clone();
            // Convert FnDecl to Function for codegen
            let inner_func = Function {
                params: fn_decl.params.clone(),
                body: fn_decl.body.clone(),
                return_type: fn_decl.return_type.clone(),
                span: fn_decl.span.clone(),
                is_async: fn_decl.is_async,
                is_generator: fn_decl.is_generator,
            };
            let mut inner_symbols = SymbolTable::new();
            let (sig_opt, type_diags) = check_function_strictness(&inner_func, &mut inner_symbols)?;

            for diag in &type_diags {
                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
            }

            if let Some(fsig) = sig_opt {
                // skip exported `main` (we handle exported main separately)
                if fname != "main" {
                    codegen
                        .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
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

        // Convert arrow function to regular function format that gen_function_ir expects
        // Arrow functions need special handling because their structure differs slightly
        use oats_ast::*;

        // Convert arrow params (Vec<Pat>) to function params (Vec<Param>)
        // For now, we need to extract type annotations from the patterns
        // This is a simplified version - in a full implementation, we'd need to
        // handle type annotations properly from the arrow function parameters
        let func_params: Vec<Param> = arrow
            .params
            .iter()
            .map(|pat| {
                // Extract type from pattern if available
                // For now, create a param with no type annotation
                Param {
                    pat: pat.clone(),
                    ty: None, // TODO: Extract type annotation from arrow param
                    span: arrow.span.clone(),
                }
            })
            .collect();

        // Convert arrow body to function body (BlockStmt)
        let func_body = match &arrow.body {
            ArrowBody::Block(block) => Some(block.clone()),
            ArrowBody::Expr(expr) => {
                // Wrap expression in a return statement
                Some(BlockStmt {
                    span: arrow.span.clone(),
                    stmts: vec![Stmt::Return(ReturnStmt {
                        span: arrow.span.clone(),
                        arg: Some(expr.as_ref().clone()),
                    })],
                })
            }
        };

        let func = Function {
            params: func_params,
            body: func_body,
            return_type: arrow.return_type.clone(),
            span: arrow.span.clone(),
            is_async: false,
            is_generator: false,
        };

        let mut inner_symbols = SymbolTable::new();
        let (sig_opt, type_diags) = check_function_strictness(&func, &mut inner_symbols)?;

        for diag in &type_diags {
            diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
        }

        if let Some(fsig) = sig_opt {
            codegen
                .gen_function_ir(fname, &func, &fsig.params, &fsig.ret, None)
                .map_err(|d| {
                    crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                    anyhow::anyhow!("{}", d.message)
                })?;
        }
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    if let Some(ref func) = func_decl
        && let Some(ref sig) = func_sig
    {
        codegen
            .gen_function_ir("oats_main", func, &sig.params, &sig.ret, None)
            .map_err(|d| {
                crate::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                anyhow::anyhow!("{}", d.message)
            })?;
    }

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    let emitted_host_main = if let Some(ref sig) = func_sig {
        codegen.emit_host_main(&sig.params, &sig.ret)
    } else {
        false
    };

    // Note: LLVM optimizations (inlining, loop opts) are applied via clang -O3 during compilation
    let ir = codegen.module.print_to_string().to_string();

    // determine output directory (optional)
    let out_dir = options
        .as_ref()
        .and_then(|o| o.out_dir.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_OUT_DIR").ok())
        .unwrap_or_else(|| ".".to_string());
    let src_filename = std::path::Path::new(&src_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let out_ll = format!("{}/{}.ll", out_dir, src_filename);
    // Allow overriding output name with options or OATS_OUT_NAME
    let out_exe = if let Some(ref opts) = options {
        if let Some(ref name) = opts.out_name {
            format!("{}/{}", out_dir, name)
        } else {
            format!("{}/{}", out_dir, src_filename)
        }
    } else if let Ok(name) = std::env::var("OATS_OUT_NAME") {
        format!("{}/{}", out_dir, name)
    } else {
        format!("{}/{}", out_dir, src_filename)
    };
    let out_obj = format!("{}/{}.o", out_dir, src_filename);

    // Ensure output directory exists so File::create doesn't fail with ENOENT.
    if let Err(e) = std::fs::create_dir_all(&out_dir) {
        anyhow::bail!("failed to create output directory {}: {}", out_dir, e);
    }

    let mut f = File::create(&out_ll)?;
    f.write_all(ir.as_bytes())?;
    f.sync_all()?;

    // Compile IR to object file using LLVM's in-process TargetMachine. This
    // avoids shelling out to clang for the IR -> object step and is more
    // robust when used programmatically.

    // Initialize native target and get the default target triple for LLVM
    Target::initialize_native(&InitializationConfig::default()).map_err(|e| anyhow::anyhow!(e))?;
    let triple = if let Some(ref opts) = options {
        if let Some(ref triple_str) = opts.target_triple {
            inkwell::targets::TargetTriple::create(triple_str)
        } else {
            TargetMachine::get_default_triple()
        }
    } else {
        TargetMachine::get_default_triple()
    };

    // Read overrides (optional) for CPU/features
    let env_cpu = options
        .as_ref()
        .and_then(|o| o.target_cpu.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_TARGET_CPU").ok());
    let env_features = options
        .as_ref()
        .and_then(|o| o.target_features.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_TARGET_FEATURES").ok());
    let cpu_candidates = if let Some(c) = env_cpu.clone() {
        vec![c, "".to_string()]
    } else {
        // Prefer a generic CPU (empty) before trying 'native' which can be
        // misinterpreted for some LLVM targets (causing subtarget errors).
        vec!["".to_string(), "native".to_string()]
    };
    // Determine optimization level. Priority:
    // 1. Options opt_level (explicit)
    // 2. OATS_OPT_LEVEL env var (explicit)
    // 3. OATS_BUILD_PROFILE=release -> Aggressive
    // 4. Default -> None (fastest)
    let build_profile = options
        .as_ref()
        .and_then(|o| o.build_profile.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_BUILD_PROFILE").ok())
        .unwrap_or_default();
    let opt_level = if let Some(ref opts) = options {
        if let Some(ref opt_str) = opts.opt_level {
            match opt_str.as_str() {
                "none" | "0" => OptimizationLevel::None,
                "1" | "default" => OptimizationLevel::Default,
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
            "1" | "default" => OptimizationLevel::Default,
            "2" | "3" | "aggressive" => OptimizationLevel::Aggressive,
            _ => OptimizationLevel::Default,
        }
    } else if build_profile == "release" {
        OptimizationLevel::Aggressive
    } else {
        OptimizationLevel::None
    };
    let features = env_features.unwrap_or_default();

    // Get the target from the triple
    let target = Target::from_triple(&triple).map_err(|e| anyhow::anyhow!("{}", e))?;

    // Create TargetMachine, trying CPU candidates in order
    let mut tm = None;
    for cpu in &cpu_candidates {
        if let Some(machine) = target.create_target_machine(
            &triple,
            cpu,
            &features,
            opt_level,
            RelocMode::Default,
            CodeModel::Default,
        ) {
            tm = Some(machine);
            break;
        }
    }
    let tm =
        tm.ok_or_else(|| anyhow::anyhow!("Failed to create TargetMachine with any CPU candidate"))?;

    // Emit object file directly from the in-memory module.
    let out_obj_path = std::path::Path::new(&out_obj);
    tm.write_to_file(&codegen.module, FileType::Object, out_obj_path)
        .map_err(|e| {
            anyhow::anyhow!(
                "TargetMachine failed to emit object file {}: {:?}",
                out_obj,
                e
            )
        })?;

    // If requested, only emit the object and skip the final host linking step.
    let emit_object_only = options
        .as_ref()
        .map(|o| o.emit_object_only)
        .unwrap_or_else(|| std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok());
    if emit_object_only {
        eprintln!(
            "OATS_EMIT_OBJECT_ONLY set; emitted {} and skipping link",
            out_obj
        );
        println!("{}", out_obj);
        return Ok(Some(out_obj));
    }

    // Build the runtime library
    let rust_lib = {
        let mut cargo_cmd = Command::new("cargo");
        cargo_cmd
            .arg("build")
            .arg("--release")
            .arg("--package")
            .arg("runtime");
        match cargo_cmd.status() {
            Ok(status) => {
                if !status.success() {
                    anyhow::bail!("cargo failed to build runtime");
                }
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::NotFound {
                    anyhow::bail!("cargo not found in PATH");
                } else {
                    return Err(e.into());
                }
            }
        }
        "target/release/libruntime.a".to_string()
    };

    // Locate or produce rt_main object. Prefer an existing top-level `rt_main.o` so
    // the repo can ship a prebuilt small host object. Otherwise try to compile
    // `runtime/rt_main/src/main.rs` if it exists.
    let rt_main_obj = if emitted_host_main {
        // host main emitted into the module; no external rt_main.o requi
        String::new()
    } else if Path::new("rt_main.o").exists() {
        // Use the repo-provided object file
        String::from("rt_main.o")
    } else if Path::new("crates/runtime/rt_main/src/main.rs").exists() {
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        // Compile rt_main with rustc; check for missing rustc binary explicitly
        let mut rustc_cmd = Command::new("rustc");
        rustc_cmd
            .arg("--crate-type")
            .arg("bin")
            .arg("--emit=obj")
            .arg("crates/runtime/rt_main/src/main.rs")
            .arg("-O")
            .arg("-o")
            .arg(&rt_main_obj);
        match rustc_cmd.status() {
            Ok(status) => {
                if !status.success() {
                    anyhow::bail!("rustc failed to compile rt_main to object");
                }
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::NotFound {
                    anyhow::bail!(
                        "`rustc` not found in PATH; please install Rust toolchain or ensure `rustc` is available"
                    );
                } else {
                    return Err(e.into());
                }
            }
        }
        rt_main_obj
    } else {
        anyhow::bail!(
            "No rt_main.o found and no runtime/rt_main/src/main.rs available; please provide a runtime main (rt_main.o) or add a runtime/rt_main/src/main.rs"
        );
    };

    // Link final binary. Prefer explicit linker via options or OATS_LINKER, otherwise
    // use clang (with -fuse-ld=lld when available) or fall back to ld.lld/lld.
    let oats_linker = options
        .as_ref()
        .and_then(|o| o.linker.as_ref())
        .cloned()
        .or_else(|| std::env::var("OATS_LINKER").ok());

    // helper to test whether a program is runnable
    fn is_prog_available(name: &str) -> bool {
        use std::process::Stdio;
        // Respect TOASTY_VERBOSE to allow printing --version output
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
    // detect lld (prefer ld.lld then lld)
    let lld_candidate = if is_prog_available("ld.lld") || is_prog_available("lld") {
        Some("lld".to_string())
    } else {
        None
    };

    let mut linked_ok = false;
    let mut link_run_err: Option<std::io::Error> = None;

    if let Some(linker) = oats_linker {
        // Try the user-specified linker exactly once
        let mut cmd = Command::new(&linker);
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", linker);
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(clang_bin) = found_clang.clone() {
        // Use clang; if lld is present, prefer clang + -fuse-ld=lld so we keep clang's driver behavior
        let mut cmd = Command::new(&clang_bin);
        // Host-side optimization flags / LTO
        let lto_mode = if let Some(ref opts) = options {
            opts.lto.clone().unwrap_or_else(|| {
                if build_profile == "release" {
                    "auto".to_string()
                } else {
                    "none".to_string()
                }
            })
        } else {
            std::env::var("OATS_LTO").unwrap_or_else(|_| {
                if build_profile == "release" {
                    "auto".to_string()
                } else {
                    "none".to_string()
                }
            })
        };
        // opt-level may also instruct host flags
        let host_opt_level = if let Some(ref opts) = options {
            opts.opt_level.clone()
        } else {
            std::env::var("OATS_OPT_LEVEL").ok()
        };
        if let Some(opt_str) = host_opt_level {
            if opt_str == "3" || opt_str == "aggressive" {
                cmd.arg("-O3");
            }
        } else if build_profile == "release" {
            cmd.arg("-O3");
        }
        // LTO handling
        match lto_mode.as_str() {
            "none" => {}
            "thin" => {
                cmd.arg("-flto=thin");
            }
            "fat" | "full" => {
                cmd.arg("-flto");
            }
            "auto" => {
                if build_profile == "release" {
                    cmd.arg("-flto");
                }
            }
            _ => {}
        }
        if let Some(ref lld) = lld_candidate {
            // use clang driver with lld
            cmd.arg(format!("-fuse-ld={}", lld));
        }
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", clang_bin);
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(lld_bin) = lld_candidate.clone() {
        // Last resort: call lld directly (may not support LTO flags)
        let mut cmd = Command::new(&lld_bin);
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", lld_bin);
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
            } else {
                return Err(e.into());
            }
        } else {
            anyhow::bail!("failed to link final binary");
        }
    }

    println!("{}", out_exe);
    Ok(Some(out_exe))
}
