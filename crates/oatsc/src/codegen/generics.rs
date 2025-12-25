//! Generic function monomorphization and type inference.
//!
//! This module handles:
//! - Monomorphizing generic functions with concrete type arguments
//! - Type inference for arrow functions and expressions
//! - Collecting return expressions for type inference

use crate::codegen::CodeGen;
use crate::codegen::helpers::extract_basic_value_from_valuekind;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::SymbolTable;
use inkwell::values::BasicValue;
use oxc_ast::ast::{
    BindingPatternKind, ClassElement, Expression, MethodDefinitionKind, PropertyKey, Statement,
};

impl<'a> CodeGen<'a> {
    /// Specializes a generic type with concrete type arguments.
    ///
    /// Creates a specialized instance of a generic type (e.g., `Array<Number>`).
    /// For now, this handles simple generic types like Array and Option.
    /// The `type_params` represent the generic type parameter names, and the
    /// first element of `type_params` should be the base generic type (e.g., Array).
    /// The `args` are expressions that may contain constructor arguments.
    pub fn specialize_generic(
        &self,
        type_params: &[crate::types::OatsType],
        _args: &[Expression<'_>],
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::BasicValueEnum<'a>> {
        if type_params.is_empty() {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "specialize_generic: at least one type parameter required".to_string(),
            ));
        }

        // For now, handle Array<T> specialization
        // The first type_param should indicate the generic type (Array), and we need
        // to determine the element type from context or args
        // This is a simplified implementation - a full version would need to track
        // generic type definitions and apply substitutions

        // If we have Array<ElementType>, create an empty array
        // For now, create a minimal array allocation
        let array_alloc_fn = self.get_array_alloc();
        let len_const = self.i64_t.const_int(0, false);
        let elem_size_const = self.i32_t.const_int(8, false); // Default to pointer size
        let is_number_const = self.i32_t.const_int(0, false); // Default to pointer elements

        let call_site = self
            .builder
            .build_call(
                array_alloc_fn,
                &[
                    len_const.into(),
                    elem_size_const.into(),
                    is_number_const.into(),
                ],
                "specialized_array_alloc",
            )
            .map_err(|_| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to allocate specialized generic type".to_string(),
                )
            })?;

        let arr_ptr = match extract_basic_value_from_valuekind(call_site.try_as_basic_value()) {
            Some(bv) => bv.into_pointer_value(),
            None => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "array allocation returned no value".to_string(),
                ))?;
            }
        };

        Ok(arr_ptr.as_basic_value_enum())
    }

    /// Monomorphizes a generic function with concrete type arguments.
    ///
    /// Returns the name of the specialized function. Caches results to avoid
    /// regenerating the same specialization.
    ///
    /// # Parameters
    /// - `func_name`: Name of the generic function to specialize
    /// - `type_args`: Concrete type arguments to substitute
    pub fn monomorphize_function(
        &self,
        func_name: &str,
        type_args: &[crate::types::OatsType],
    ) -> crate::diagnostics::DiagnosticResult<String> {
        // Create a unique key for this monomorphization
        // Pre-allocate with known capacity to avoid reallocations
        let mut key_parts = Vec::with_capacity(1 + type_args.len());
        key_parts.push(func_name.to_string());
        for arg in type_args {
            key_parts.push(format!("{arg:?}"));
        }
        let key = key_parts.join("_");

        // Check cache
        if let Some(existing_name) = self.monomorphized_map.borrow().get(&key) {
            return Ok(existing_name.clone());
        }

        // Lookup the generic function AST and its signature
        // Note: Function doesn't implement Clone, so we work with references
        // We need to keep the borrow alive for the entire function
        let nested_fns_borrow = self.nested_generic_fns.borrow();
        let (func_ast, fsig) = match nested_fns_borrow.get(func_name) {
            Some((func_ast, fsig)) => (*func_ast, fsig.clone()),
            None => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("Generic function '{func_name}' not found"),
                ));
            }
        };

        // Obtain type parameter names: prefer fsig.type_params
        let tp_names = fsig.type_params.clone();

        if tp_names.len() != type_args.len() {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                format!(
                    "Expected {} type arguments for generic function '{}', got {}",
                    tp_names.len(),
                    func_name,
                    type_args.len()
                ),
            ));
        }

        // Build substitution map name -> OatsType
        // Pre-allocate with known capacity
        let mut subst = std::collections::HashMap::with_capacity(tp_names.len());
        for (n, t) in tp_names.iter().zip(type_args.iter()) {
            subst.insert(n.clone(), t.clone());
        }

        // Compute concrete parameter types by applying the substitution to the
        // AST parameter annotations. Fall back to the fsig params when needed.
        // Pre-allocate with estimated capacity (most params will be processed)
        let mut concrete_params = Vec::with_capacity(func_ast.params.items.len());
        for param in &func_ast.params.items {
            // Only handle simple identifier parameters for now
            if !matches!(
                &param.pattern.kind,
                BindingPatternKind::BindingIdentifier(_)
            ) {
                continue; // Skip destructuring parameters
            }
            let type_ann_opt = param.pattern.type_annotation.as_ref();
            if let Some(type_ann) = type_ann_opt
                && let Some(mapped) =
                    crate::types::map_ts_type_with_subst(&type_ann.type_annotation, &subst)
            {
                concrete_params.push(mapped);
                continue;
            }
            // fallback: use corresponding entry from fsig.params if present
            if let Some(p) = fsig.params.get(concrete_params.len()) {
                concrete_params.push(p.clone());
            } else {
                // As a last resort, assume Number
                concrete_params.push(crate::types::OatsType::Number);
            }
        }

        // Compute concrete return type
        let concrete_ret = match func_ast
            .return_type
            .as_ref()
            .and_then(|rt| crate::types::map_ts_type_with_subst(&rt.type_annotation, &subst))
        {
            Some(ty) => ty,
            None => fsig.ret.clone(),
        };

        // Build specialized name matching expected `_mono` pattern
        let mut specialized_name = format!("{func_name}_mono");
        for arg in type_args {
            specialized_name.push_str(
                &format!("_{arg:?}")
                    .replace(' ', "")
                    .replace(['(', ')'], "")
                    .replace(',', "_"),
            );
        }

        // Reserve mapping before emission to avoid recursion
        self.monomorphized_map
            .borrow_mut()
            .insert(key.clone(), specialized_name.clone());

        // Emit the specialized function IR. gen_function_ir mutates the builder
        // insert point, so preserve and restore it.
        let prev_block = self.builder.get_insert_block();
        if let Err(diag) = self.gen_function_ir(
            &specialized_name,
            func_ast,
            &concrete_params,
            &concrete_ret,
            None,
        ) {
            // On error, remove the reserved mapping and return the diagnostic
            self.monomorphized_map.borrow_mut().remove(&key);
            return Err(diag);
        }
        if let Some(pb) = prev_block {
            self.builder.position_at_end(pb);
        }

        Ok(specialized_name)
    }

    /// Monomorphizes a generic class with concrete type arguments.
    ///
    /// Returns the name of the specialized class. Caches results to avoid
    /// regenerating the same specialization.
    ///
    /// # Parameters
    /// - `class_name`: Name of the generic class to specialize
    /// - `type_args`: Concrete type arguments to substitute
    pub fn monomorphize_class(
        &self,
        class_name: &str,
        type_args: &[crate::types::OatsType],
    ) -> crate::diagnostics::DiagnosticResult<String> {
        // Create a unique key for this monomorphization
        let mut key_parts = Vec::with_capacity(1 + type_args.len());
        key_parts.push(class_name.to_string());
        for arg in type_args {
            key_parts.push(format!("{arg:?}"));
        }
        let key = key_parts.join("_");

        // Check cache
        if let Some(existing_name) = self.monomorphized_map.borrow().get(&key) {
            return Ok(existing_name.clone());
        }

        // Lookup the generic class AST and its type parameters
        let nested_classes_borrow = self.nested_generic_classes.borrow();
        let (class_ast, tp_names) = match nested_classes_borrow.get(class_name) {
            Some((class_ast, tp_names)) => (*class_ast, tp_names.clone()),
            None => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("Generic class '{class_name}' not found"),
                ));
            }
        };

        if tp_names.len() != type_args.len() {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                format!(
                    "Expected {} type arguments for generic class '{}', got {}",
                    tp_names.len(),
                    class_name,
                    type_args.len()
                ),
            ));
        }

        // Build substitution map name -> OatsType
        let mut subst = std::collections::HashMap::with_capacity(tp_names.len());
        for (n, t) in tp_names.iter().zip(type_args.iter()) {
            subst.insert(n.clone(), t.clone());
        }

        // Build specialized name
        let mut specialized_name = format!("{class_name}_mono");
        for arg in type_args {
            specialized_name.push_str(
                &format!("_{arg:?}")
                    .replace(' ', "")
                    .replace(['(', ')'], "")
                    .replace(',', "_"),
            );
        }

        // Reserve mapping before emission to avoid recursion
        self.monomorphized_map
            .borrow_mut()
            .insert(key.clone(), specialized_name.clone());

        // Generate the specialized class IR
        // We need to create a modified class AST with substituted types
        // For now, we'll generate the class with the specialized name and handle type substitution in field resolution
        self.gen_monomorphized_class_ir(&specialized_name, class_ast, &subst)?;

        Ok(specialized_name)
    }

    /// Generates IR for a monomorphized class with type substitutions.
    fn gen_monomorphized_class_ir(
        &self,
        specialized_name: &str,
        class_ast: &'a oxc_ast::ast::Class<'a>,
        subst: &std::collections::HashMap<String, crate::types::OatsType>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        // Create a temporary class AST with the specialized name
        // This is a simplified implementation - in a full implementation we'd need to
        // deep clone and modify the AST with type substitutions

        let raw_name = specialized_name.to_string();

        // Register in symbol table with original name as key, specialized name as value
        self.symbol_table
            .borrow_mut()
            .insert_type(raw_name.clone(), specialized_name.to_string());

        // 1. Record parent class (none for now)
        let parent_name_opt = None;
        self.class_parents
            .borrow_mut()
            .insert(specialized_name.to_string(), parent_name_opt.clone());

        // 2. Scan members for fields and constructor
        let mut fields = Vec::new();
        let mut ctor_opt = None;

        for member in &class_ast.body.body {
            match member {
                ClassElement::MethodDefinition(m) => {
                    if m.kind == MethodDefinitionKind::Constructor {
                        ctor_opt = Some(m);
                    }
                }
                ClassElement::PropertyDefinition(p) => {
                    // Extract field name and type with substitution
                    let pname = match &p.key {
                        PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                        PropertyKey::Identifier(id) => id.name.to_string(),
                        PropertyKey::StringLiteral(s) => s.value.to_string(),
                        _ => continue,
                    };
                    let ptype = if let Some(ann) = &p.type_annotation {
                        // Apply type substitution
                        if let Some(mapped) = crate::types::map_ts_type_with_subst_and_aliases(
                            &ann.type_annotation,
                            subst,
                            Some(&*self.type_aliases.borrow()),
                            Some(&*self.symbol_table.borrow()),
                        ) {
                            mapped
                        } else {
                            crate::types::OatsType::Number
                        }
                    } else {
                        crate::types::OatsType::Number
                    };
                    fields.push((pname, ptype));
                }
                _ => {}
            }
        }

        // Register fields
        self.class_fields
            .borrow_mut()
            .insert(specialized_name.to_string(), fields.clone());

        // 3. Generate methods and constructor
        for member in &class_ast.body.body {
            if let ClassElement::MethodDefinition(m) = member
                && m.kind != MethodDefinitionKind::Constructor
            {
                let mname = match &m.key {
                    PropertyKey::StaticIdentifier(id) => id.name.to_string(),
                    PropertyKey::Identifier(id) => id.name.to_string(),
                    PropertyKey::StringLiteral(s) => s.value.to_string(),
                    _ => continue,
                };

                let method_func = &m.value;
                let mut method_symbols = SymbolTable::new();
                let (sig_opt, _) =
                    crate::types::check_function_strictness(method_func, &mut method_symbols)
                        .map_err(|e| Box::new(Diagnostic::error(e.to_string())))?;

                if let Some(sig) = sig_opt {
                    let mut params = Vec::new();
                    params.push(crate::types::OatsType::NominalStruct(
                        specialized_name.to_string(),
                    ));
                    params.extend(sig.params.into_iter());
                    let ret = sig.ret;
                    let fname = format!("{specialized_name}_{mname}");

                    // Save current class parent for super calls
                    let old_parent = self.current_class_parent.borrow().clone();
                    *self.current_class_parent.borrow_mut() = parent_name_opt.clone();

                    self.gen_function_ir(&fname, method_func, &params, &ret, Some("this"))?;

                    *self.current_class_parent.borrow_mut() = old_parent;
                }
            }
        }

        if let Some(ctor) = ctor_opt {
            // Save current class parent for constructor
            let old_parent = self.current_class_parent.borrow().clone();
            *self.current_class_parent.borrow_mut() = parent_name_opt;

            self.gen_constructor_ir(specialized_name, ctor, &fields, None)?;

            *self.current_class_parent.borrow_mut() = old_parent;
        } else {
            // Generate default constructor
            self.gen_default_constructor_ir(specialized_name, &fields)?;
        }

        Ok(())
    }

    /// Infers the return type from an arrow function body.
    ///
    /// For expression bodies, infers from the expression type. For block bodies,
    /// collects all return expressions and creates a union if multiple types are found.
    pub fn infer_return_type_from_arrow_body(
        &self,
        arrow: &oxc_ast::ast::ArrowFunctionExpression<'_>,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        if arrow.expression {
            // Expression body: extract the expression from the first statement
            if let Some(first_stmt) = arrow.body.statements.first() {
                if let Statement::ExpressionStatement(expr_stmt) = first_stmt {
                    self.infer_type_from_expr(&expr_stmt.expression)
                } else {
                    Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "Arrow function with expression body must have expression statement"
                            .to_string(),
                    ))
                }
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "Arrow function with expression body must have at least one statement"
                        .to_string(),
                ))
            }
        } else {
            // Block body: collect all return expression types from the block
            // Pre-allocate with estimated capacity based on statement count
            // Most functions have few return statements, so use a conservative estimate
            let estimated_returns = arrow.body.statements.len().min(8);
            let mut return_types = Vec::with_capacity(estimated_returns);
            self.collect_return_types(&arrow.body.statements, &mut return_types)?;

            if return_types.is_empty() {
                // No returns, infer as void
                Ok(crate::types::OatsType::Void)
            } else {
                // Deduplicate types by comparing with existing types
                // Note: OatsType doesn't implement Hash, so we use manual comparison
                let mut unique_types = Vec::with_capacity(return_types.len());
                for ty in return_types {
                    if !unique_types
                        .iter()
                        .any(|t: &crate::types::OatsType| t == &ty)
                    {
                        unique_types.push(ty);
                    }
                }

                // If only one unique type, return it; otherwise create union
                if unique_types.len() == 1 {
                    unique_types.into_iter().next().ok_or_else(|| {
                        Diagnostic::simple_boxed(Severity::Error, "expected one unique type")
                    })
                } else {
                    Ok(crate::types::OatsType::Union(unique_types))
                }
            }
        }
    }

    /// Collects all return expressions from statements into `out`.
    fn collect_return_types(
        &self,
        stmts: &[Statement<'_>],
        out: &mut Vec<crate::types::OatsType>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        for stmt in stmts {
            self.collect_return_types_from_stmt(stmt, out)?;
        }
        Ok(())
    }

    fn collect_return_types_from_stmt(
        &self,
        stmt: &Statement<'_>,
        out: &mut Vec<crate::types::OatsType>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        match stmt {
            Statement::ReturnStatement(ret) => {
                if let Some(expr) = &ret.argument {
                    let ty = self.infer_type_from_expr(expr)?;
                    out.push(ty);
                }
            }
            Statement::BlockStatement(block) => {
                self.collect_return_types(&block.body, out)?;
            }
            Statement::IfStatement(if_stmt) => {
                self.collect_return_types_from_stmt(&if_stmt.consequent, out)?;
                if let Some(alt) = &if_stmt.alternate {
                    self.collect_return_types_from_stmt(alt, out)?;
                }
            }
            Statement::ForStatement(for_stmt) => {
                self.collect_return_types_from_stmt(&for_stmt.body, out)?;
            }
            Statement::WhileStatement(while_stmt) => {
                self.collect_return_types_from_stmt(&while_stmt.body, out)?;
            }
            Statement::DoWhileStatement(do_while) => {
                self.collect_return_types_from_stmt(&do_while.body, out)?;
            }
            Statement::SwitchStatement(switch) => {
                for case in &switch.cases {
                    self.collect_return_types(&case.consequent, out)?;
                }
            }
            Statement::TryStatement(try_stmt) => {
                self.collect_return_types(&try_stmt.block.body, out)?;
                if let Some(handler) = &try_stmt.handler {
                    self.collect_return_types(&handler.body.body, out)?;
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    self.collect_return_types(&finalizer.body, out)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Infers the type of an expression.
    ///
    /// Currently only handles literals; identifiers and complex expressions
    /// default to `Number`.
    pub(crate) fn infer_type_from_expr(
        &self,
        expr: &Expression<'_>,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        // Check if we already have type information from lowering
        if let Some(ty) = self.last_expr_oats_type.borrow().as_ref() {
            return Ok(ty.clone());
        }

        match expr {
            Expression::NumericLiteral(_) => Ok(crate::types::OatsType::Number),
            Expression::StringLiteral(_) => Ok(crate::types::OatsType::String),
            Expression::BooleanLiteral(_) => Ok(crate::types::OatsType::Boolean),
            Expression::Identifier(id) => {
                let _name = id.name.to_string();
                // Try to infer type from context
                // This is a simplified implementation - should check locals, params, etc.
                Ok(crate::types::OatsType::Number) // default fallback
            }
            _ => Ok(crate::types::OatsType::Number), // default for complex expr
        }
    }
}
