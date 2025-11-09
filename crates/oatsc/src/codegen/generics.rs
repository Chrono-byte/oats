//! Generic function monomorphization and type inference.
//!
//! This module handles:
//! - Monomorphizing generic functions with concrete type arguments
//! - Type inference for arrow functions and expressions
//! - Collecting return expressions for type inference

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValue;

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
        args: &[oats_ast::Expr],
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

        let arr_ptr = call_site.try_as_basic_value().left().ok_or_else(|| {
            Diagnostic::simple_boxed(
                Severity::Error,
                "array allocation returned no value".to_string(),
            )
        })?;

        Ok(arr_ptr.into_pointer_value().as_basic_value_enum())
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
            key_parts.push(format!("{:?}", arg));
        }
        let key = key_parts.join("_");

        // Check cache
        if let Some(existing_name) = self.monomorphized_map.borrow().get(&key) {
            return Ok(existing_name.clone());
        }

        // Lookup the generic function AST and its signature
        let (func_ast, fsig) = self
            .nested_generic_fns
            .borrow()
            .get(func_name)
            .cloned()
            .ok_or_else(|| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("Generic function '{}' not found", func_name),
                )
            })?;

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
        let mut concrete_params = Vec::with_capacity(func_ast.params.len());
        for param in &func_ast.params {
            use oats_ast::*;
            // Only handle simple identifier parameters for now
            if !matches!(&param.pat, Pat::Ident(_)) {
                continue; // Skip destructuring parameters
            }
            let type_ann_opt = param.ty.as_ref();
            if let Some(type_ann) = type_ann_opt
                && let Some(mapped) = crate::types::map_ts_type_with_subst(type_ann, &subst)
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
        let concrete_ret = func_ast
            .return_type
            .as_ref()
            .and_then(|rt| crate::types::map_ts_type_with_subst(rt, &subst))
            .unwrap_or_else(|| fsig.ret.clone());

        // Build specialized name matching expected `_mono` pattern
        let mut specialized_name = format!("{}_mono", func_name);
        for arg in type_args {
            specialized_name.push_str(
                &format!("_{:?}", arg)
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
            &func_ast,
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

    /// Infers the return type from an arrow function body.
    ///
    /// For expression bodies, infers from the expression type. For block bodies,
    /// collects all return expressions and creates a union if multiple types are found.
    pub fn infer_return_type_from_arrow_body(
        &self,
        body: &oats_ast::ArrowBody,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        match body {
            oats_ast::ArrowBody::Expr(expr) => {
                // Simple inference for single expression body
                self.infer_type_from_expr(expr)
            }
            oats_ast::ArrowBody::Block(block) => {
                // Collect all return expressions from the block
                // Pre-allocate with estimated capacity based on statement count
                // Most functions have few return statements, so use a conservative estimate
                let estimated_returns = block.stmts.len().min(8);
                let mut return_exprs = Vec::with_capacity(estimated_returns);
                self.collect_return_exprs(&block.stmts, &mut return_exprs);

                if return_exprs.is_empty() {
                    // No returns, infer as void
                    Ok(crate::types::OatsType::Void)
                } else {
                    // Infer types from all return expressions
                    let return_types: Result<Vec<_>, _> = return_exprs
                        .iter()
                        .map(|expr| self.infer_type_from_expr(expr))
                        .collect();
                    let return_types = return_types?;

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
    }

    /// Collects all return expressions from statements into `out`.
    fn collect_return_exprs(&self, stmts: &[oats_ast::Stmt], out: &mut Vec<oats_ast::Expr>) {
        for stmt in stmts {
            self.collect_return_exprs_from_stmt(stmt, out);
        }
    }

    fn collect_return_exprs_from_stmt(&self, stmt: &oats_ast::Stmt, out: &mut Vec<oats_ast::Expr>) {
        use oats_ast::*;
        match stmt {
            Stmt::Return(ret) => {
                if let Some(expr) = &ret.arg {
                    out.push(expr.clone());
                }
            }
            Stmt::Block(block) => {
                self.collect_return_exprs(&block.stmts, out);
            }
            Stmt::If(if_stmt) => {
                self.collect_return_exprs_from_stmt(&if_stmt.cons, out);
                if let Some(alt) = &if_stmt.alt {
                    self.collect_return_exprs_from_stmt(alt, out);
                }
            }
            Stmt::For(for_stmt) => {
                self.collect_return_exprs_from_stmt(&for_stmt.body, out);
            }
            Stmt::While(while_stmt) => {
                self.collect_return_exprs_from_stmt(&while_stmt.body, out);
            }
            Stmt::DoWhile(do_while) => {
                self.collect_return_exprs_from_stmt(&do_while.body, out);
            }
            Stmt::Switch(switch) => {
                for case in &switch.cases {
                    self.collect_return_exprs(&case.cons, out);
                }
            }
            Stmt::Try(try_stmt) => {
                self.collect_return_exprs(&try_stmt.block.stmts, out);
                if let Some(handler) = &try_stmt.handler {
                    self.collect_return_exprs(&handler.body.stmts, out);
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    self.collect_return_exprs(&finalizer.stmts, out);
                }
            }
            _ => {}
        }
    }

    /// Infers the type of an expression.
    ///
    /// Currently only handles literals; identifiers and complex expressions
    /// default to `Number`.
    fn infer_type_from_expr(
        &self,
        expr: &oats_ast::Expr,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        match expr {
            oats_ast::Expr::Lit(lit) => match lit {
                oats_ast::Lit::F64(_)
                | oats_ast::Lit::F32(_)
                | oats_ast::Lit::I8(_)
                | oats_ast::Lit::I16(_)
                | oats_ast::Lit::I32(_)
                | oats_ast::Lit::I64(_)
                | oats_ast::Lit::I128(_)
                | oats_ast::Lit::ISize(_)
                | oats_ast::Lit::U8(_)
                | oats_ast::Lit::U16(_)
                | oats_ast::Lit::U32(_)
                | oats_ast::Lit::U64(_)
                | oats_ast::Lit::U128(_)
                | oats_ast::Lit::USize(_) => Ok(crate::types::OatsType::Number),
                oats_ast::Lit::Str(_) => Ok(crate::types::OatsType::String),
                oats_ast::Lit::Bool(_) => Ok(crate::types::OatsType::Boolean),
                _ => Ok(crate::types::OatsType::Number), // default
            },
            oats_ast::Expr::Ident(_) => Ok(crate::types::OatsType::Number), // default fallback - should be resolved by type inference
            _ => Ok(crate::types::OatsType::Number), // default for complex expr
        }
    }
}
