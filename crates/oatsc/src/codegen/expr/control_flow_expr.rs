use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{BasicValue, PointerValue};

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_cond_expr(
        &self,
        cond: &oats_ast::CondExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Ternary expression: test ? cons : alt
        // Lower test to an i1
        let test_val = self.lower_expr(&cond.test, function, param_map, locals)?;
        // Test expression lowered successfully
        let cond_i1 = match test_val {
            BasicValueEnum::IntValue(iv) => iv.as_basic_value_enum(),
            BasicValueEnum::FloatValue(fv) => {
                // JS-like truthiness for numbers: false for +0, -0, and NaN
                let zero = self.f64_t.const_float(0.0);
                let is_not_zero = match self.builder.build_float_compare(
                    inkwell::FloatPredicate::ONE,
                    fv,
                    zero,
                    "neq0",
                ) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                // check not NaN: fv == fv
                let is_not_nan = match self.builder.build_float_compare(
                    inkwell::FloatPredicate::OEQ,
                    fv,
                    fv,
                    "not_nan",
                ) {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                let cond = match self.builder.build_and(is_not_zero, is_not_nan, "num_truth") {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                cond.as_basic_value_enum()
            }
            BasicValueEnum::PointerValue(pv) => {
                // pointer truthiness: non-null and non-empty string are truthy
                let is_null = match self.builder.build_is_null(pv, "is_null") {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                let is_not_null = match self.builder.build_not(is_null, "not_null") {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                // call strlen(ptr) and check != 0
                if let Some(strlen_fn) = self.module.get_function("strlen") {
                    let cs = match self
                        .builder
                        .build_call(strlen_fn, &[pv.into()], "strlen_call")
                    {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    };
                    let either = cs.try_as_basic_value();
                    if let inkwell::Either::Left(bv) = either {
                        let len = bv.into_int_value();
                        let zero64 = self.i64_t.const_int(0, false);
                        let len_nonzero = match self.builder.build_int_compare(
                            inkwell::IntPredicate::NE,
                            len,
                            zero64,
                            "len_nonzero",
                        ) {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "operation failed",
                                ));
                            }
                        };
                        let cond =
                            match self
                                .builder
                                .build_and(is_not_null, len_nonzero, "ptr_truth")
                            {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "operation failed",
                                    ));
                                }
                            };
                        return Ok(cond.as_basic_value_enum());
                    }
                }
                // fallback: non-null
                is_not_null.as_basic_value_enum()
            }
            _ => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "operation failed",
                ));
            }
        };

        // Create basic blocks
        let then_bb = self.context.append_basic_block(function, "then");
        let else_bb = self.context.append_basic_block(function, "else");
        let merge_bb = self.context.append_basic_block(function, "merge");

        // branch based on cond_i1
        let cond_val = self
            .to_condition_i1(cond_i1)
            .ok_or_else(|| Diagnostic::error("failed to convert to boolean condition"))?;
        self.builder
            .build_conditional_branch(cond_val, then_bb, else_bb)
            .map_err(|_| Diagnostic::error("LLVM builder error"))?;

        // then
        self.builder.position_at_end(then_bb);
        let then_val = self.lower_expr(&cond.cons, function, param_map, locals);

        // else
        self.builder.position_at_end(else_bb);
        let else_val = self.lower_expr(&cond.alt, function, param_map, locals);

        // Fallback: if else arm failed to lower but is the identifier `undefined`,
        // treat it as a null pointer so ternaries like `x ? y : undefined` work.
        let else_val = match else_val {
            Ok(v) => Ok(v),
            Err(_) => {
                if let oats_ast::Expr::Ident(id) = &*cond.alt {
                    if id.sym == "undefined" {
                        let null_ptr = self.i8ptr_t.const_null();
                        // else arm is `undefined` -> using null pointer fallback
                        Ok(null_ptr.as_basic_value_enum())
                    } else {
                        Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "expression lowering failed",
                        ))
                    }
                } else {
                    Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "expression lowering failed",
                    ))
                }
            }
        };

        // At this point we have lowered both arms (or substituted fallback).
        // We will now, if needed, perform boxing inside the predecessor
        // blocks so that incoming phi values are produced in the correct
        // blocks. Only after that we emit unconditional branches to merge.

        // Ensure both predecessor blocks branch to the merge block.
        // Some lowering paths may not have emitted an explicit branch
        // (they just returned a value), so add unconditional branches
        // from `then_bb`/`else_bb` to `merge_bb` when missing.
        if then_bb.get_terminator().is_none() {
            self.builder.position_at_end(then_bb);
            self.ensure_unconditional_branch(merge_bb);
        }
        if else_bb.get_terminator().is_none() {
            self.builder.position_at_end(else_bb);
            self.ensure_unconditional_branch(merge_bb);
        }
        // Now position at merge and build phi nodes
        self.builder.position_at_end(merge_bb);
        // If both sides produced values, try to create a phi. Some common
        // forms mix numeric (f64) with pointer-like (i8*) values where the
        // pointer arm may be `undefined`/`null`. Handle that case by boxing
        // numeric arms into a union pointer and building a pointer phi.
        if let (Ok(tv), Ok(ev)) = (then_val, else_val) {
            use inkwell::types::BasicTypeEnum;
            let tv_ty = tv.get_type();
            let ev_ty = ev.get_type();

            // Helper to box numeric/int values into an i8* union pointer
            let box_to_ptr =
                |codegen: &crate::codegen::CodeGen<'a>,
                 bv: BasicValueEnum<'a>|
                 -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
                    match bv {
                        BasicValueEnum::PointerValue(pv) => Ok(pv.as_basic_value_enum()),
                        BasicValueEnum::FloatValue(fv) => {
                            let box_fn = codegen.get_union_box_f64();
                            let cs = codegen.builder.build_call(box_fn, &[fv.into()], "box_phi");
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv2) = cs.try_as_basic_value()
                            {
                                return Ok(bv2);
                            }
                            Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "boxing failed for phi",
                            ))
                        }
                        BasicValueEnum::IntValue(iv) => {
                            // convert to f64 then box
                            let as_f64 = codegen
                                .builder
                                .build_signed_int_to_float(iv, codegen.f64_t, "i2f_phi")
                                .map_err(|_| Diagnostic::error("int->float cast failed"))?;
                            let box_fn = codegen.get_union_box_f64();
                            let cs =
                                codegen
                                    .builder
                                    .build_call(box_fn, &[as_f64.into()], "box_phi");
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv2) = cs.try_as_basic_value()
                            {
                                return Ok(bv2);
                            }
                            Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "boxing failed for phi",
                            ))
                        }
                        _ => Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "unsupported phi arm type",
                        )),
                    }
                };

            // If either side is a pointer and the other is numeric, prefer a
            // simple fast-path: if the `then` arm is numeric (f64/int) and the
            // `else` arm is pointer-like (commonly `undefined` -> null), box
            // the numeric `then` into a union, unbox both to f64,
            // then build an f64 phi. This is less efficient but preserves
            // the numeric ABI when the function expects it.
            //
            // tv = then value, ev = else value
            // fast-path condition: then is float/int and else is pointer
            if (matches!(tv_ty, BasicTypeEnum::FloatType(_))
                || matches!(tv_ty, BasicTypeEnum::IntType(_)))
                && matches!(ev_ty, BasicTypeEnum::PointerType(_))
            {
                // Decide whether the overall desired ABI is numeric (f64) or pointer.
                let mut func_ret_is_float = false;
                if let Some(rt) = self.current_function_return_type.borrow().clone() {
                    let llvm_ret = self.map_type_to_llvm(&rt);
                    func_ret_is_float = matches!(llvm_ret, BasicTypeEnum::FloatType(_));
                }

                if func_ret_is_float {
                    // The function expects a numeric return (f64). Prefer a numeric phi:
                    // box the pointer `else` arm into a union, unbox both to f64,
                    // then build an f64 phi. This is less efficient but preserves
                    // the numeric ABI when the function expects it.
                    self.builder.position_at_end(then_bb);
                    let then_f64 = match tv {
                        BasicValueEnum::FloatValue(f) => f.as_basic_value_enum(),
                        BasicValueEnum::IntValue(i) => {
                            let as_f64 = self
                                .builder
                                .build_signed_int_to_float(i, self.f64_t, "i2f_then")
                                .map_err(|_| Diagnostic::error("int->float cast failed"))?;
                            as_f64.as_basic_value_enum()
                        }
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "expected numeric in then arm",
                            ));
                        }
                    };
                    self.ensure_unconditional_branch(merge_bb);

                    self.builder.position_at_end(else_bb);
                    let boxed_else =
                        box_to_ptr(self, ev).map_err(|_| Diagnostic::error("phi boxing failed"))?;
                    let unbox_fn = self.get_union_unbox_f64();
                    let cs = self
                        .builder
                        .build_call(unbox_fn, &[boxed_else.into()], "unbox_else");
                    if let Ok(cs) = cs
                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                    {
                        let else_f64 = bv;
                        self.ensure_unconditional_branch(merge_bb);

                        // merge: build phi of f64
                        self.builder.position_at_end(merge_bb);
                        let f64_ty = self.f64_t.as_basic_type_enum();
                        let phi_node = self
                            .builder
                            .build_phi(f64_ty, "phi_tmp")
                            .map_err(|_| Diagnostic::error("phi creation failed"))?;
                        phi_node.add_incoming(&[(&then_f64, then_bb), (&else_f64, else_bb)]);
                        return Ok(phi_node.as_basic_value());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "unboxing failed for phi",
                        ));
                    }
                } else {
                    // The function expects a pointer return (i8*). Prefer a pointer phi:
                    // box the numeric `then` arm into a union pointer, keep the
                    // pointer `else` arm as-is, then build an i8* phi.
                    self.builder.position_at_end(then_bb);
                    let boxed_then =
                        box_to_ptr(self, tv).map_err(|_| Diagnostic::error("phi boxing failed"))?;
                    self.ensure_unconditional_branch(merge_bb);

                    self.builder.position_at_end(else_bb);
                    let else_ptr = match ev {
                        BasicValueEnum::PointerValue(p) => p.as_basic_value_enum(),
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "expected pointer in else arm",
                            ));
                        }
                    };
                    self.ensure_unconditional_branch(merge_bb);

                    // merge: build phi of i8*
                    self.builder.position_at_end(merge_bb);
                    let ptr_ty = self.i8ptr_t.as_basic_type_enum();
                    let phi_node = self
                        .builder
                        .build_phi(ptr_ty, "phi_tmp")
                        .map_err(|_| Diagnostic::error("phi creation failed"))?;
                    phi_node.add_incoming(&[(&boxed_then, then_bb), (&else_ptr, else_bb)]);
                    return Ok(phi_node.as_basic_value());
                }
            }

            // Otherwise, perform general boxing in predecessors (below)...
            if matches!(tv_ty, BasicTypeEnum::PointerType(_))
                && !matches!(ev_ty, BasicTypeEnum::PointerType(_))
                || matches!(ev_ty, BasicTypeEnum::PointerType(_))
                    && !matches!(tv_ty, BasicTypeEnum::PointerType(_))
            {
                let ptr_ty = self.i8ptr_t.as_basic_type_enum();

                // Prepare placeholders for incoming values produced in preds
                #[allow(unused_assignments)]
                let mut then_incoming: Option<BasicValueEnum<'a>> = None;
                #[allow(unused_assignments)]
                let mut else_incoming: Option<BasicValueEnum<'a>> = None;

                if !matches!(tv_ty, BasicTypeEnum::PointerType(_))
                    && matches!(ev_ty, BasicTypeEnum::PointerType(_))
                {
                    // then needs boxing
                    self.builder.position_at_end(then_bb);
                    let boxed_then =
                        box_to_ptr(self, tv).map_err(|_| Diagnostic::error("phi boxing failed"))?;
                    // ensure then block branches to merge
                    self.ensure_unconditional_branch(merge_bb);
                    then_incoming = Some(boxed_then);

                    // else block: keep as pointer value
                    self.builder.position_at_end(else_bb);
                    let else_ptr = match ev {
                        BasicValueEnum::PointerValue(p) => p.as_basic_value_enum(),
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "expected pointer in else arm",
                            ));
                        }
                    };
                    self.ensure_unconditional_branch(merge_bb);
                    else_incoming = Some(else_ptr);
                } else {
                    // else needs boxing
                    self.builder.position_at_end(then_bb);
                    let then_ptr = match tv {
                        BasicValueEnum::PointerValue(p) => p.as_basic_value_enum(),
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "expected pointer in then arm",
                            ));
                        }
                    };
                    self.ensure_unconditional_branch(merge_bb);
                    then_incoming = Some(then_ptr);

                    self.builder.position_at_end(else_bb);
                    let boxed_else =
                        box_to_ptr(self, ev).map_err(|_| Diagnostic::error("phi boxing failed"))?;
                    self.ensure_unconditional_branch(merge_bb);
                    else_incoming = Some(boxed_else);
                }

                // merge: create phi from incoming values (builder at merge_bb)
                self.builder.position_at_end(merge_bb);
                let phi_node = self
                    .builder
                    .build_phi(ptr_ty, "phi_tmp")
                    .map_err(|_| Diagnostic::error("phi creation failed"))?;
                if let (Some(then_val), Some(else_val)) = (then_incoming, else_incoming) {
                    phi_node.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);
                    return Ok(phi_node.as_basic_value());
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "missing incoming values for phi",
                    ));
                }
            }

            // Otherwise try the general helper
            if let Some(phi) = self.build_phi_merge(then_bb, else_bb, tv, ev) {
                return Ok(phi);
            }
        }

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "unsupported conditional expression (ternary) form",
            cond.span.start,
        ))
    }
}
