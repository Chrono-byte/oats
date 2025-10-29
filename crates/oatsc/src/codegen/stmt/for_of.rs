use crate::diagnostics::Severity;
use inkwell::types::BasicType;
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
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), an optional nominal type
// name used to guide member access lowering, and an optional OatsType
// for union tracking.
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_for_of_stmt(
        &self,
        forof: &deno_ast::swc::ast::ForOfStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Only handle the case where the left-hand side is a var
        // declaration (e.g., `for (let v of rhs)`).
        // forof.left can be either a VarDecl or a Pat; we match on VarDecl
        if let deno_ast::swc::ast::ForHead::VarDecl(var_decl) = &forof.left
            && var_decl.decls.len() == 1
        {
            let decl = &var_decl.decls[0];
            if let deno_ast::swc::ast::Pat::Ident(ident) = &decl.name {
                let loop_var_name = ident.id.sym.to_string();
                // capture nominal type for the loop variable if annotated
                let mut declared_nominal: Option<String> = None;
                if let Some(type_ann) = &ident.type_ann
                    && let Some(mapped) = crate::types::map_ts_type(&type_ann.type_ann)
                    && let crate::types::OatsType::NominalStruct(n) = &mapped
                {
                    declared_nominal = Some(n.clone());
                }
                // Lower RHS (iterable)
                if let Ok(iter_val) =
                    self.lower_expr(&forof.right, function, param_map, locals_stack)
                    && let BasicValueEnum::PointerValue(arr_ptr) = iter_val
                {
                    // create index
                    let idx_alloca = match self.builder.build_alloca(self.i64_t, "for_idx") {
                        Ok(a) => a,
                        Err(_) => {
                            crate::diagnostics::emit_diagnostic(
                                &crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "alloca failed for for-loop index",
                                ),
                                Some(self.source),
                            );
                            return Ok(false);
                        }
                    };
                    let zero = self.i64_t.const_int(0, false);
                    let _ = self.builder.build_store(idx_alloca, zero);

                    // create blocks
                    let loop_cond_bb = self.context.append_basic_block(function, "for.cond");
                    let loop_body_bb = self.context.append_basic_block(function, "for.body");
                    let loop_after_bb = self.context.append_basic_block(function, "for.after");

                    // Push loop context (for-of: continue jumps to condition, break to after)
                    self.loop_context_stack
                        .borrow_mut()
                        .push(crate::codegen::LoopContext {
                            continue_block: loop_cond_bb,
                            break_block: loop_after_bb,
                            locals_start: locals_stack.len(),
                            label: self.current_label.borrow().clone(),
                        });

                    let _ = self.builder.build_unconditional_branch(loop_cond_bb);

                    self.builder.position_at_end(loop_cond_bb);
                    // call strlen to get length
                    if let Some(strlen_fn) = self.module.get_function("strlen") {
                        let cs = match self.builder.build_call(
                            strlen_fn,
                            &[arr_ptr.into()],
                            "strlen_call",
                        ) {
                            Ok(cs) => cs,
                            Err(_) => return Ok(false),
                        };
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let len = bv.into_int_value();
                            let cur_idx =
                                match self.builder.build_load(self.i64_t, idx_alloca, "idx_load") {
                                    Ok(v) => v.into_int_value(),
                                    Err(_) => return Ok(false),
                                };
                            let cmp = match self.builder.build_int_compare(
                                inkwell::IntPredicate::ULT,
                                cur_idx,
                                len,
                                "cmp_idx",
                            ) {
                                Ok(v) => v,
                                Err(_) => return Ok(false),
                            };
                            if self
                                .builder
                                .build_conditional_branch(cmp, loop_body_bb, loop_after_bb)
                                .is_err()
                            {
                                return Ok(false);
                            }
                        } else if self
                            .builder
                            .build_unconditional_branch(loop_after_bb)
                            .is_err()
                        {
                            return Ok(false);
                        }
                    } else if self
                        .builder
                        .build_unconditional_branch(loop_after_bb)
                        .is_err()
                    {
                        return Ok(false);
                    }

                    // body
                    self.builder.position_at_end(loop_body_bb);
                    let cur_idx = match self.builder.build_load(self.i64_t, idx_alloca, "idx_load2")
                    {
                        Ok(v) => v.into_int_value(),
                        Err(_) => return Ok(false),
                    };
                    if let Some(array_get_f64_fn) = self.module.get_function("array_get_f64") {
                        let cs = match self.builder.build_call(
                            array_get_f64_fn,
                            &[arr_ptr.into(), cur_idx.into()],
                            "array_get_f64_call",
                        ) {
                            Ok(cs) => cs,
                            Err(_) => return Ok(false),
                        };
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let ty = bv.get_type().as_basic_type_enum();
                            let alloca = match self.builder.build_alloca(ty, &loop_var_name) {
                                Ok(a) => a,
                                Err(_) => {
                                    crate::diagnostics::emit_diagnostic(
                                        &crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "alloca failed for loop variable",
                                        ),
                                        Some(self.source),
                                    );
                                    return Ok(false);
                                }
                            };
                            let _ = self.builder.build_store(alloca, bv);
                            self.insert_local_current_scope(
                                locals_stack,
                                crate::codegen::helpers::LocalVarInfo {
                                    name: loop_var_name.clone(),
                                    ptr: alloca,
                                    ty,
                                    initialized: true,
                                    is_const: false,
                                    is_weak: false,
                                    nominal: declared_nominal.clone(),
                                    oats_type: None,
                                },
                            );
                        }
                    } else if let Some(array_get_ptr_fn) = self.module.get_function("array_get_ptr")
                    {
                        let cs = match self.builder.build_call(
                            array_get_ptr_fn,
                            &[arr_ptr.into(), cur_idx.into()],
                            "array_get_ptr_call",
                        ) {
                            Ok(cs) => cs,
                            Err(_) => return Ok(false),
                        };
                        if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                            let pv = bv.into_pointer_value();
                            let ty = pv.get_type().as_basic_type_enum();
                            let alloca = match self.builder.build_alloca(ty, &loop_var_name) {
                                Ok(a) => a,
                                Err(_) => {
                                    crate::diagnostics::emit_diagnostic(
                                        &crate::diagnostics::Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "alloca failed for loop variable (ptr)",
                                        ),
                                        Some(self.source),
                                    );
                                    return Ok(false);
                                }
                            };
                            if !self.should_elide_rc_for_local(&loop_var_name) {
                                let rc_inc = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc,
                                    &[pv.into()],
                                    "rc_inc_loop_var",
                                );
                            }
                            let _ = self.builder.build_store(alloca, bv);
                        }
                    }

                    // Lower the loop body: handle a Block or single statement
                    let terminated = match &*forof.body {
                        deno_ast::swc::ast::Stmt::Block(block) => {
                            self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
                        }
                        other => self.lower_stmt(other, function, param_map, locals_stack)?,
                    };
                    let cur_idx2 =
                        match self.builder.build_load(self.i64_t, idx_alloca, "idx_load3") {
                            Ok(v) => v.into_int_value(),
                            Err(_) => return Ok(false),
                        };
                    let one = self.i64_t.const_int(1, false);
                    let next_idx = match self.builder.build_int_add(cur_idx2, one, "idx_next") {
                        Ok(v) => v,
                        Err(_) => return Ok(false),
                    };
                    let _ = self.builder.build_store(idx_alloca, next_idx);
                    if !terminated
                        && self
                            .builder
                            .build_unconditional_branch(loop_cond_bb)
                            .is_err()
                    {
                        self.loop_context_stack.borrow_mut().pop();
                        return Ok(false);
                    }
                    self.builder.position_at_end(loop_after_bb);

                    // Pop loop context
                    self.loop_context_stack.borrow_mut().pop();

                    return Ok(terminated);
                }
            }
        }
        Ok(false)
    }
}
