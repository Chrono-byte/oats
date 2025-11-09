//! For-in loop statement lowering
//!
//! This module handles lowering of for-in loops (`for (key in obj) { }`).
//! For-in loops iterate over the enumerable properties of an object.

use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValueEnum, FunctionValue};
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_for_in_stmt(
        &self,
        for_in: &oats_ast::ForInStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        use oats_ast::*;

        // For-in loops iterate over object property keys
        // Only handle the case where the left-hand side is a var declaration (e.g., `for (let key in obj)`)
        if let ForHead::VarDecl(var_decl) = &for_in.left
            && var_decl.decls.len() == 1
        {
            let decl = &var_decl.decls[0];
            let loop_var_name = match &decl.name {
                Pat::Ident(ident) => ident.sym.clone(),
                _ => {
                    return Err(Diagnostic::error(
                        "Destructuring patterns not yet supported in for-in loops",
                    )
                    .with_code("E1006")
                    .with_label(crate::diagnostics::Label {
                        span: crate::diagnostics::Span {
                            start: decl.span.start,
                            end: decl.span.end,
                        },
                        message: "Only simple identifier patterns are supported".into(),
                    })
                    .into());
                }
            };

            // Lower RHS (object to iterate over)
            let obj_val = self.lower_expr(&for_in.right, function, param_map, locals_stack)?;
            let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
                pv
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "for-in loop requires an object (pointer type)",
                    for_in.span.start,
                ));
            };

            // Try to infer the class name from the RHS expression
            // All types must be known at compile time - if we can't infer the type, it's an error
            let mut class_name_opt: Option<String> = None;
            if let Expr::Ident(ident) = &for_in.right {
                let ident_name = ident.sym.clone();
                // Check locals for nominal annotation
                if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                    self.find_local(locals_stack, &ident_name)
                    && let Some(nom) = nominal
                {
                    class_name_opt = Some(nom);
                } else if let Some(param_idx) = param_map.get(&ident_name)
                    && let Some(param_types) = self
                        .fn_param_types
                        .borrow()
                        .get(function.get_name().to_str().unwrap_or(""))
                {
                    let idx = *param_idx as usize;
                    if idx < param_types.len()
                        && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                    {
                        class_name_opt = Some(n.clone());
                    }
                }
            } else if let Expr::New(new_expr) = &for_in.right
                && let Expr::Ident(ident) = &*new_expr.callee
            {
                // new ClassName() - get class name from callee
                class_name_opt = Some(ident.sym.clone());
            } else if let Expr::Member(member) = &for_in.right
                && let Expr::Ident(ident) = &*member.obj
            {
                // obj.field - try to infer from obj
                let ident_name = ident.sym.clone();
                if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                    self.find_local(locals_stack, &ident_name)
                    && let Some(nom) = nominal
                {
                    class_name_opt = Some(nom);
                }
            }

            // Get field names from class metadata
            let field_names: Vec<String> = if let Some(class_name) = class_name_opt {
                if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                    fields.iter().map(|(name, _)| name.clone()).collect()
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        format!("class '{}' has no registered fields", class_name),
                        for_in.span.start,
                    ));
                }
            } else {
                // Cannot determine class type at compile time
                // All types must be known at compile time - this is a type inference failure
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "cannot determine object type for for-in loop: all types must be known at compile time",
                    for_in.span.start,
                ));
            };

            if field_names.is_empty() {
                // Empty object - loop body never executes
                return Ok(false);
            }

            // Create an array of string keys at compile time
            // We'll create a static array of string pointers
            let keys_array = self.create_string_keys_array(&field_names)?;

            // Create loop variable (string pointer)
            let loop_var_alloca = self
                .builder
                .build_alloca(self.i8ptr_t, &format!("for_in_{}", loop_var_name))
                .map_err(|_| Diagnostic::error("alloca failed for for-in loop variable"))?;
            let null_ptr = self.i8ptr_t.const_null();
            let _ = self.builder.build_store(loop_var_alloca, null_ptr);

            // Create index variable
            let idx_alloca = self
                .builder
                .build_alloca(self.i64_t, "for_in_idx")
                .map_err(|_| Diagnostic::error("alloca failed for for-in index"))?;
            let zero = self.i64_t.const_int(0, false);
            let _ = self.builder.build_store(idx_alloca, zero);

            // Create basic blocks
            let loop_cond_bb = self.context.append_basic_block(function, "for_in.cond");
            let loop_body_bb = self.context.append_basic_block(function, "for_in.body");
            let loop_after_bb = self.context.append_basic_block(function, "for_in.after");

            // Push loop context
            self.loop_context_stack
                .borrow_mut()
                .push(crate::codegen::LoopContext {
                    continue_block: loop_cond_bb,
                    break_block: loop_after_bb,
                    locals_start: locals_stack.len(),
                    label: self.current_label.borrow().clone(),
                });

            // Branch to condition
            let _ = self.builder.build_unconditional_branch(loop_cond_bb);

            // Condition block: check if index < field_names.len()
            self.builder.position_at_end(loop_cond_bb);
            let cur_idx = self
                .builder
                .build_load(self.i64_t, idx_alloca, "idx_load")
                .map_err(|_| Diagnostic::error("failed to load index"))?
                .into_int_value();
            let len_const = self.i64_t.const_int(field_names.len() as u64, false);
            let cmp = self
                .builder
                .build_int_compare(
                    inkwell::IntPredicate::ULT,
                    cur_idx,
                    len_const,
                    "cmp_idx",
                )
                .map_err(|_| Diagnostic::error("failed to compare index"))?;
            let _ = self
                .builder
                .build_conditional_branch(cmp, loop_body_bb, loop_after_bb);

            // Body block: load key from array and assign to loop variable
            self.builder.position_at_end(loop_body_bb);

            // Get key at index: array_get_ptr(keys_array, cur_idx)
            let array_get_ptr_fn = self.get_array_get_ptr();
            let key_val = self
                .builder
                .build_call(
                    array_get_ptr_fn,
                    &[keys_array.into(), cur_idx.into()],
                    "get_key",
                )
                .map_err(|_| Diagnostic::error("failed to get key"))?;
            let key_ptr = if let inkwell::Either::Left(bv) = key_val.try_as_basic_value() {
                if let BasicValueEnum::PointerValue(pv) = bv {
                    pv
                } else {
                    return Err(Diagnostic::simple_boxed(
                        crate::diagnostics::Severity::Error,
                        "array_get_ptr returned non-pointer",
                    ));
                }
            } else {
                return Err(Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "array_get_ptr returned void",
                ));
            };

            // Store key in loop variable
            let _ = self.builder.build_store(loop_var_alloca, key_ptr);

            // Register loop variable in locals
            let loop_var_entry: LocalEntry = (
                loop_var_alloca,
                self.i8ptr_t.into(),
                true,  // initialized
                false, // not const
                false, // not weak
                None,  // no nominal type (it's a string)
                Some(crate::types::OatsType::String),
            );
            locals_stack
                .last_mut()
                .ok_or_else(|| Diagnostic::error("locals stack is empty"))?
                .insert(loop_var_name.clone(), loop_var_entry);

            // Lower loop body
            let body_terminated = match &*for_in.body {
                Stmt::Block(block) => {
                    self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
                }
                _ => self.lower_stmt(&for_in.body, function, param_map, locals_stack)?,
            };

            // Remove loop variable from locals after body
            locals_stack
                .last_mut()
                .and_then(|locals| locals.remove(&loop_var_name));

            // Increment index and branch back to condition
            if !body_terminated {
                let one = self.i64_t.const_int(1, false);
                let new_idx = self
                    .builder
                    .build_int_add(cur_idx, one, "inc_idx")
                    .map_err(|_| Diagnostic::error("failed to increment index"))?;
                let _ = self.builder.build_store(idx_alloca, new_idx);
                let _ = self.builder.build_unconditional_branch(loop_cond_bb);
            }

            // After block
            self.builder.position_at_end(loop_after_bb);

            // Pop loop context
            self.loop_context_stack.borrow_mut().pop();

            Ok(false)
        } else {
            Err(Diagnostic::error(
                "for-in loop left-hand side must be a variable declaration",
            )
            .with_code("E1008")
            .with_label(crate::diagnostics::Label {
                span: crate::diagnostics::Span {
                    start: for_in.span.start,
                    end: for_in.span.end,
                },
                message: "Expected: for (let key in obj)".into(),
            })
            .into())
        }
    }

    /// Create a static array of string keys for for-in iteration
    fn create_string_keys_array(
        &self,
        field_names: &[String],
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::PointerValue<'a>> {
        // Create string constants for each field name
        let mut key_strings: Vec<inkwell::values::PointerValue<'a>> = Vec::new();
        for field_name in field_names {
            let str_ptr = self
                .builder
                .build_global_string_ptr(field_name, "field_name")
                .map_err(|_| Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "failed to create global string",
                ))?
                .as_pointer_value();
            key_strings.push(str_ptr);
        }

        // Create array using array_alloc
        // array_alloc(i64 len, i32 elem_size, i32 elem_is_number)
        let array_alloc_fn = self.get_array_alloc();
        let len_val = self.i64_t.const_int(field_names.len() as u64, false);
        let elem_size = self.i32_t.const_int(8, false); // Pointer size
        let is_number = self.i32_t.const_int(0, false); // Not numbers, pointers
        let array_ptr = self
            .builder
            .build_call(
                array_alloc_fn,
                &[len_val.into(), elem_size.into(), is_number.into()],
                "keys_array",
            )
            .map_err(|_| Diagnostic::error("failed to allocate keys array"))?;

        let array_ptr_val = if let inkwell::Either::Left(bv) = array_ptr.try_as_basic_value() {
            if let BasicValueEnum::PointerValue(pv) = bv {
                pv
            } else {
                return Err(Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "array_alloc returned non-pointer",
                ));
                }
            } else {
                return Err(Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "array_alloc returned void",
                ));
        };

        // Create alloca to store array pointer (array_set_ptr needs i8** for potential reallocation)
        let array_ptr_alloca = self
            .builder
            .build_alloca(self.i8ptr_t, "keys_array_alloca")
            .map_err(|_| Diagnostic::error("alloca failed for keys array"))?;
        let _ = self.builder.build_store(array_ptr_alloca, array_ptr_val);

        // Store each string pointer in the array
        let array_set_ptr_fn = self.get_array_set_ptr();
        for (idx, key_str) in key_strings.iter().enumerate() {
            let idx_val = self.i64_t.const_int(idx as u64, false);
            let _ = self.builder.build_call(
                array_set_ptr_fn,
                &[array_ptr_alloca.into(), idx_val.into(), (*key_str).into()],
                "set_key",
            );
        }

        // Load final array pointer (in case reallocation occurred)
        let final_array_ptr = self
            .builder
            .build_load(self.i8ptr_t, array_ptr_alloca, "final_keys_array")
            .map_err(|_| Diagnostic::error("failed to load final array pointer"))?
            .into_pointer_value();

        Ok(final_array_ptr)
    }
}

