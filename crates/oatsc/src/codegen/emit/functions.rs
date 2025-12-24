//! Function generation for code generation.
//!
//! This module contains code generation logic for regular (non-async) functions.

use crate::diagnostics::Severity;
use inkwell::values::{BasicValue, FunctionValue};

impl<'a> crate::codegen::CodeGen<'a> {
    /// Generates LLVM IR for a function declaration.
    ///
    /// This is the main entry point for compiling a user function into LLVM
    /// IR. Responsibilities include:
    /// - Mapping Oats parameter and return types to LLVM ABI types.
    /// - Creating the LLVM function and entry basic block.
    /// - Registering any anonymous struct-typed parameters as generated
    ///   nominal structs so member lowering can resolve fields.
    /// - Allocating stack slots for parameters and wiring the initial
    ///   `locals_stack` used by statement lowering.
    /// - Lowering the function body via `lower_stmts` and emitting an
    ///   implicit `return` and `rc_dec` cleanup if the body doesn't
    ///   explicitly terminate.
    ///
    /// # Arguments
    /// * `func_name` - exported name to give the generated LLVM function.
    /// * `func_decl` - the AST `Function` node describing parameters and body.
    /// * `param_types` - the list of resolved `OatsType` for parameters.
    /// * `ret_type` - the declared return `OatsType` for ABI mapping.
    /// * `receiver_name` - optional `this` receiver name for methods.
    ///
    /// # Returns
    /// Returns the created `FunctionValue` on success or a `Diagnostic` on
    /// failure. The function emits calls to runtime helpers (for example
    /// for union boxing) and expects the runtime to provide those symbols
    /// during linking.
    #[allow(clippy::result_large_err)]
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &'a oxc_ast::ast::Function<'a>,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> crate::diagnostics::DiagnosticResult<FunctionValue<'a>> {
        // 0. Scan for nested functions and arrow functions in the body
        if let Some(body) = &func_decl.body {
            self.gen_nested_decls(&body.statements, func_name)?;
        }

        // 1. Build the LLVM function type.
        let llvm_param_types: Vec<_> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();
        // Convert to BasicMetadataTypeEnum for function type signature
        let llvm_param_types_meta: Vec<inkwell::types::BasicMetadataTypeEnum> = llvm_param_types
            .iter()
            .map(|t| match *t {
                inkwell::types::BasicTypeEnum::FloatType(ft) => ft.into(),
                inkwell::types::BasicTypeEnum::IntType(it) => it.into(),
                inkwell::types::BasicTypeEnum::PointerType(pt) => pt.into(),
                inkwell::types::BasicTypeEnum::ArrayType(at) => at.into(),
                inkwell::types::BasicTypeEnum::VectorType(vt) => vt.into(),
                inkwell::types::BasicTypeEnum::StructType(st) => st.into(),
                inkwell::types::BasicTypeEnum::ScalableVectorType(svt) => svt.into(),
            })
            .collect();
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, ret_type);

        // 2. Add the function to the module and set up the entry block.
        self.gen_str_concat(); // Ensure runtime helpers are available.
        let function = self.module.add_function(func_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Run conservative, intra-procedural escape analysis for this function
        // and store the result on the CodeGen so lowering sites can consult it.
        let escape_info = self.analyze_fn(func_decl);
        self.current_escape_info.borrow_mut().replace(escape_info);

        // If this is an `async` function, delegate to async handling
        if func_decl.r#async {
            return self.gen_async_function_ir(
                func_name,
                func_decl,
                &llvm_param_types_meta,
                ret_type,
                function,
                entry,
            );
        }

        // If this is a `generator` function, delegate to generator handling
        if func_decl.generator {
            return self.gen_generator_function_ir(
                func_name,
                func_decl,
                &llvm_param_types_meta,
                ret_type,
                function,
                entry,
            );
        }

        // 3. If any parameter types are anonymous struct literals, register
        // them as nominal structs in `class_fields` under a generated name so
        // downstream lowering (which expects nominal names) can resolve
        // member accesses. Then store param types in `fn_param_types`.
        let mut param_types_owned = param_types.to_vec();
        for (i, p) in param_types_owned.iter_mut().enumerate() {
            if let crate::types::OatsType::StructLiteral(fields) = p {
                // Create a generated nominal name like <func_name>_param_struct_<i>
                let gen_name = format!("{func_name}_param_struct_{i}");
                // Insert into class_fields for lowering
                self.class_fields
                    .borrow_mut()
                    .insert(gen_name.clone(), fields.clone());
                // Replace the param type with NominalStruct so existing lowering uses it
                *p = crate::types::OatsType::NominalStruct(gen_name);
            }
        }
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types_owned.clone());
        let (param_map, mut locals_stack) =
            self.create_param_allocas(function, func_decl, &llvm_param_types, receiver_name)?;

        // Store the function signature in the global map.
        self.global_function_signatures.borrow_mut().insert(
            func_name.to_string(),
            (param_types.to_vec(), ret_type.clone()),
        );

        // Store the function's return type so return statements can box values appropriately.
        *self.current_function_return_type.borrow_mut() = Some(ret_type.clone());

        // 4. Lower the function body statements into IR.
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.statements, function, &param_map, &mut locals_stack)?;
        }

        // Debug: if the function had a body but no terminator was emitted,
        // print a message to help diagnose cases where returns weren't lowered.
        if func_decl.body.is_some() && !emitted_terminator {
            // Function '{}' had a body but no terminator emitted
        }

        // 5. Add an implicit `return void` if the function hasn't already returned.
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            self.emit_rc_dec_for_locals(&locals_stack);
            // If the declared return type is Void, emit a bare `ret void`.
            // Otherwise, return a sensible zero/null value matching the ABI
            // (0.0 for numbers, null ptr for pointer-like returns).
            match ret_type {
                crate::types::OatsType::Void => {
                    self.builder.build_return(None).map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "Failed to build implicit return",
                        )
                    })?;
                }
                crate::types::OatsType::Number | crate::types::OatsType::F64 => {
                    let zero = self.f64_t.const_float(0.0);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (number)",
                            )
                        })?;
                }
                crate::types::OatsType::F32 => {
                    let zero = self.f32_t.const_float(0.0);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (f32)",
                            )
                        })?;
                }
                crate::types::OatsType::I64
                | crate::types::OatsType::U64
                | crate::types::OatsType::Isize
                | crate::types::OatsType::Usize => {
                    let zero = self.i64_t.const_int(0, false);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (i64)",
                            )
                        })?;
                }
                crate::types::OatsType::I32 | crate::types::OatsType::U32 => {
                    let zero = self.i32_t.const_int(0, false);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (i32)",
                            )
                        })?;
                }
                crate::types::OatsType::I16 | crate::types::OatsType::U16 => {
                    let zero = self.i16_t.const_int(0, false);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (i16)",
                            )
                        })?;
                }
                crate::types::OatsType::I8
                | crate::types::OatsType::U8
                | crate::types::OatsType::Char => {
                    let zero = self.i8_t.const_int(0, false);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (i8)",
                            )
                        })?;
                }
                crate::types::OatsType::Boolean => {
                    let zero = self.bool_t.const_int(0, false);
                    self.builder
                        .build_return(Some(&zero.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (bool)",
                            )
                        })?;
                }
                _ => {
                    // Pointer-like or other -> return null i8*.
                    let nullp = self.i8ptr_t.const_null();
                    self.builder
                        .build_return(Some(&nullp.as_basic_value_enum()))
                        .map_err(|_| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "Failed to build implicit return (ptr)",
                            )
                        })?;
                }
            }
        }

        // Clear escape info for this function now that lowering is complete
        let _ = self.current_escape_info.borrow_mut().take();
        Ok(function)
    }

    /// Scan for and generate IR for nested functions and arrow functions.
    pub fn gen_nested_decls(
        &self,
        stmts: &'a [oxc_ast::ast::Statement<'a>],
        prefix: &str,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        use oxc_ast::ast::{BindingPatternKind, Expression, Statement};

        for stmt in stmts {
            match stmt {
                Statement::FunctionDeclaration(f) => {
                    if let Some(id) = &f.id {
                        let name = id.name.to_string();
                        let mangled_name = format!("{prefix}_{name}");

                        let mut nested_symbols = crate::types::SymbolTable::new();
                        let (sig_opt, type_diags) =
                            crate::types::check_function_strictness(f, &mut nested_symbols)
                                .map_err(|e| {
                                    crate::diagnostics::Diagnostic::simple_boxed(
                                        crate::diagnostics::Severity::Error,
                                        format!("Type checking failed for nested function: {e}"),
                                    )
                                })?;

                        for diag in &type_diags {
                            crate::diagnostics::emit_diagnostic(diag, Some(self.source));
                        }

                        if let Some(sig) = sig_opt {
                            if sig.type_params.is_empty() {
                                // Register in symbol table so it can be looked up by its local name
                                self.symbol_table.borrow_mut().insert_std_function(
                                    name.clone(),
                                    mangled_name.clone(),
                                    sig.clone(),
                                );
                                // Generate IR
                                self.gen_function_ir(
                                    &mangled_name,
                                    f,
                                    &sig.params,
                                    &sig.ret,
                                    None,
                                )?;
                            } else {
                                // Generic function: store for later monomorphization
                                self.nested_generic_fns
                                    .borrow_mut()
                                    .insert(name.clone(), (f, sig));
                            }
                        }
                    }
                }
                Statement::VariableDeclaration(vd) => {
                    for decl in &vd.declarations {
                        if let BindingPatternKind::BindingIdentifier(id) = &decl.id.kind
                            && let Some(init) = &decl.init
                            && let Expression::ArrowFunctionExpression(arrow) = init
                        {
                            let name = id.name.to_string();
                            let mangled_name = format!("{prefix}_{name}");

                            // For arrows, we need to infer the signature if missing
                            // For now, assume it's annotated or use a simple inference
                            let mut param_types = Vec::new();
                            for p in &arrow.params.items {
                                if let Some(ann) = &p.pattern.type_annotation
                                    && let Some(ty) =
                                        crate::types::map_ts_type(&ann.type_annotation)
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
                            self.symbol_table.borrow_mut().insert_std_function(
                                name.clone(),
                                mangled_name.clone(),
                                sig,
                            );

                            // Generate IR
                            self.gen_arrow_function_ir(
                                &mangled_name,
                                arrow,
                                &param_types,
                                &ret_type,
                            )?;
                        }
                    }
                }
                Statement::ClassDeclaration(c) => {
                    // Handle class declarations
                    self.gen_class_ir(c, prefix)?;
                }
                Statement::TSInterfaceDeclaration(_interface) => {
                    // Skip interfaces in gen_nested_decls - they should only be processed
                    // at the top level in builder.rs. Processing them here would create
                    // mangled names (e.g., "printPoint_Point") that don't match lookups
                    // which use the unmangled name ("Point").
                    // Top-level interfaces are handled in builder.rs before function codegen.
                }
                Statement::TSTypeAliasDeclaration(ta) => {
                    let alias_name = ta.id.name.to_string();
                    // For now, type aliases are not mangled because they are resolved during mapping
                    let type_params = ta.type_parameters.as_ref().map(|params| {
                        params
                            .params
                            .iter()
                            .map(|p| p.name.name.to_string())
                            .collect::<Vec<String>>()
                    });
                    self.type_aliases
                        .borrow_mut()
                        .insert(alias_name, (type_params, &ta.type_annotation));
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Generates LLVM IR for an arrow function.
    pub fn gen_arrow_function_ir(
        &self,
        func_name: &str,
        arrow: &oxc_ast::ast::ArrowFunctionExpression,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
    ) -> crate::diagnostics::DiagnosticResult<FunctionValue<'a>> {
        // 1. Build the LLVM function type.
        let llvm_param_types: Vec<_> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, ret_type);

        // 2. Add the function to the module and set up the entry block.
        let function = self.module.add_function(func_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");

        // Save current builder position
        let original_bb = self.builder.get_insert_block();

        self.builder.position_at_end(entry);

        // 3. Register param types
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());

        // Create param allocas (simplified for arrows)
        let mut param_map = std::collections::HashMap::new();
        for (i, p) in arrow.params.items.iter().enumerate() {
            if let oxc_ast::ast::BindingPatternKind::BindingIdentifier(ident) = &p.pattern.kind {
                param_map.insert(ident.name.to_string(), i as u32);
            }
        }

        let mut locals_stack = vec![std::collections::HashMap::new()];

        // 4. Lower body
        let mut emitted_terminator = false;
        if arrow.expression {
            // Expression body: () => expr
            if let Some(expr) = arrow.body.statements.first()
                && let oxc_ast::ast::Statement::ExpressionStatement(estmt) = expr
            {
                let val =
                    self.lower_expr(&estmt.expression, function, &param_map, &mut locals_stack)?;
                self.emit_rc_dec_for_locals(&locals_stack);
                self.builder.build_return(Some(&val)).map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "Failed to build return",
                    )
                })?;
                emitted_terminator = true;
            }
        } else {
            // Block body: () => { ... }
            emitted_terminator = self.lower_stmts(
                &arrow.body.statements,
                function,
                &param_map,
                &mut locals_stack,
            )?;
        }

        // 5. Implicit return
        if !emitted_terminator {
            self.emit_rc_dec_for_locals(&locals_stack);
            if matches!(ret_type, crate::types::OatsType::Void) {
                self.builder.build_return(None).map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "Failed to build implicit return",
                    )
                })?;
            } else {
                let zero = self.f64_t.const_float(0.0);
                self.builder
                    .build_return(Some(&zero.as_basic_value_enum()))
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "Failed to build implicit return",
                        )
                    })?;
            }
        }

        // Restore builder position
        if let Some(bb) = original_bb {
            self.builder.position_at_end(bb);
        }

        Ok(function)
    }
}
