use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use deno_ast::swc::ast;
use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
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
    pub(super) fn lower_binary_expr(
        &self,
        bin: &deno_ast::swc::ast::BinExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use deno_ast::swc::ast::BinaryOp;
        use inkwell::FloatPredicate;

        // Special-case: typeof <expr> === "string"/"number" patterns
        //
        // This pattern is common in guard code. When the left side is
        // a `typeof` unary we can lower the inner expression and, for
        // pointer-like (boxed union) values, consult the runtime
        // discriminant instead of performing a heavier dynamic check.
        // This keeps the emitted IR small for the common guarded
        // branches used in tests and examples.
        if let ast::Expr::Unary(unary) = &*bin.left
            && let deno_ast::swc::ast::UnaryOp::TypeOf = unary.op
            && let ast::Expr::Lit(deno_ast::swc::ast::Lit::Str(s)) = &*bin.right
        {
            // Lower inner expression and then check union discriminant if it's a pointer
            let inner = self.lower_expr(&unary.arg, function, param_map, locals)?;
            if let BasicValueEnum::PointerValue(pv) = inner {
                // For pointer-like values, consult the union discriminant helper.
                // Runtime layout: union-boxed objects expose a small tag (discriminant)
                // stored in the allocated container. The helper reads that tag and
                // returns an i64 so the compiler can cheaply implement `typeof`
                // without re-running generic dynamic checks. This fast-path is
                // intentionally optimistic: if the inner value is unboxed or a
                // different representation, we fall back to the general path
                // below.
                let get_disc = self.get_union_get_discriminant();
                let cs = self.builder.build_call(get_disc, &[pv.into()], "get_disc");
                if let Ok(cs) = cs
                    && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                {
                    let disc = bv.into_int_value();
                    let expected = match s.value.as_ref() {
                        "number" => self.i64_t.const_int(0, false),
                        "string" => self.i64_t.const_int(1, false),
                        _ => self.i64_t.const_int(2, false),
                    };
                    if let Ok(cmp_iv) = self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ,
                        disc,
                        expected,
                        "typeof_eq",
                    ) {
                        return Ok(cmp_iv.as_basic_value_enum());
                    }
                }
            }
        }

        // Lower left operand and capture its origin (whether it came from a local)
        let l = self.lower_expr(&bin.left, function, param_map, locals)?;
        let left_origin = self.last_expr_origin_local.borrow().clone();
        // Lower right operand and capture its origin
        let r = self.lower_expr(&bin.right, function, param_map, locals)?;
        let right_origin = self.last_expr_origin_local.borrow().clone();

        // Handle string concatenation BEFORE numeric coercion
        // If both operands are pointers and op is Add, treat as string concat
        if let BinaryOp::Add = bin.op
            && let (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) = (l, r)
        {
            if let Some(strcat) = self.module.get_function("str_concat") {
                let call_site =
                    match self
                        .builder
                        .build_call(strcat, &[lp.into(), rp.into()], "concat")
                    {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    };
                let either = call_site.try_as_basic_value();
                match either {
                    inkwell::Either::Left(bv) => {
                        // After creating the concatenated string, decrement
                        // temporary operands that didn't originate from locals.
                        if left_origin.is_none()
                            && let Some(rc_dec_fn) = self.module.get_function("rc_dec")
                        {
                            let _ = self
                                .builder
                                .build_call(rc_dec_fn, &[lp.into()], "rc_dec_tmp_left")
                                .ok();
                        }
                        if right_origin.is_none()
                            && let Some(rc_dec_fn) = self.module.get_function("rc_dec")
                        {
                            let _ = self
                                .builder
                                .build_call(rc_dec_fn, &[rp.into()], "rc_dec_tmp_right")
                                .ok();
                        }
                        return Ok(bv);
                    }
                    _ => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation not supported (bin strcat result)",
                        ));
                    }
                }
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "string concatenation helper `str_concat` not found",
                    bin.span.lo.0 as usize,
                ));
            }
        }

        // Coercion and unboxing notes:
        // - `coerce_to_f64` will convert ints/bools to f64 where
        //   appropriate and will also attempt to unbox numeric payloads
        //   from union-boxed pointers by calling the runtime `union_unbox_f64`.
        // - We attempt the float-path first because numeric arithmetic is
        //   the most common case and produces simpler IR.
        // - If coercion fails we fall back to pointer/other handling.
        // coercion now handled by self.coerce_to_f64

        // Helper to handle float arithmetic
        let float_bin = |builder: &Builder<'a>,
                         lf: inkwell::values::FloatValue<'a>,
                         rf: inkwell::values::FloatValue<'a>,
                         op: &BinaryOp|
         -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
            match op {
                BinaryOp::Add => {
                    let v = builder
                        .build_float_add(lf, rf, "sum")
                        .map_err(|_| Diagnostic::error("float add failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOp::Sub => {
                    let v = builder
                        .build_float_sub(lf, rf, "sub")
                        .map_err(|_| Diagnostic::error("float sub failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOp::Mul => {
                    let v = builder
                        .build_float_mul(lf, rf, "mul")
                        .map_err(|_| Diagnostic::error("float mul failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOp::Div => {
                    let v = builder
                        .build_float_div(lf, rf, "div")
                        .map_err(|_| Diagnostic::error("float div failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOp::Mod => {
                    let v = builder
                        .build_float_rem(lf, rf, "rem")
                        .map_err(|_| Diagnostic::error("float rem failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                _ => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported binary operation",
                )),
            }
        };

        // Helper to handle float comparisons -> i1
        let float_cmp = |builder: &Builder<'a>,
                         lf: inkwell::values::FloatValue<'a>,
                         rf: inkwell::values::FloatValue<'a>,
                         op: &BinaryOp|
         -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
            let pred = match op {
                BinaryOp::Lt => FloatPredicate::OLT,
                BinaryOp::LtEq => FloatPredicate::OLE,
                BinaryOp::Gt => FloatPredicate::OGT,
                BinaryOp::GtEq => FloatPredicate::OGE,
                BinaryOp::EqEq => FloatPredicate::OEQ,
                BinaryOp::EqEqEq => FloatPredicate::OEQ,
                BinaryOp::NotEq => FloatPredicate::ONE,
                _ => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported comparison operation",
                    ));
                }
            };
            let iv = builder
                .build_float_compare(pred, lf, rf, "cmp")
                .map_err(|_| Diagnostic::error("float compare failed"))?;
            Ok(iv.as_basic_value_enum())
        };

        // Try float path by coercing ints/bools to float as needed
        if let (Some(lf), Some(rf)) = (self.coerce_to_f64(l), self.coerce_to_f64(r)) {
            if let Ok(v) = float_bin(&self.builder, lf, rf, &bin.op) {
                return Ok(v);
            }
            if let Ok(v) = float_cmp(&self.builder, lf, rf, &bin.op) {
                return Ok(v);
            }
        }

        // Logical operators: short-circuiting
        //
        // For `&&` and `||` we emit explicit conditional branches and
        // create a merge block with a phi node. The phi node is built
        // by `build_phi_merge` which performs basic coercions when the
        // two sides have differing ABI types (for example f64 vs i64
        // or pointer). This keeps the lowering correct for expressions
        // that return one of the operand values (JS-like semantics).
        //
        // Implementation detail: to preserve JS semantics where `a && b`
        // returns either `a` or `b` we evaluate `a` first, check its
        // truthiness via `to_condition_i1`, and only evaluate `b` when
        // needed. The merge uses a phi node which may need to coerce the
        // two incoming values to a common ABI representation; `build_phi_merge`
        // performs those coercions conservatively (boxing numeric values
        // if the other arm expects a pointer-like value, etc.).
        if let deno_ast::swc::ast::BinaryOp::LogicalAnd = bin.op {
            // a && b -> if a truthy then b else a
            let left_val = l;
            let cond = self
                .to_condition_i1(left_val)
                .ok_or_else(|| Diagnostic::error("failed to convert to boolean condition"))?;
            let then_bb = self.context.append_basic_block(function, "and.then");
            let else_bb = self.context.append_basic_block(function, "and.else");
            let merge_bb = self.context.append_basic_block(function, "and.merge");
            if self
                .builder
                .build_conditional_branch(cond, then_bb, else_bb)
                .is_err()
            {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "expression lowering failed",
                ))?;
            }
            // then: evaluate right. Note: we don't eagerly `rc_inc` here;
            // any pointer ownership changes are performed by the
            // callee/array_get/union_box helpers when producing values.
            self.builder.position_at_end(then_bb);
            let rv = self.lower_expr(&bin.right, function, param_map, locals);
            // Ensure we are positioned at the end of the `then` block before
            // emitting the unconditional branch to the merge block. This
            // guards against lowering paths that may have switched the
            // insertion point while lowering nested expressions.
            if self.builder.get_insert_block().is_some() {
                self.builder.position_at_end(then_bb);
                if self.builder.build_unconditional_branch(merge_bb).is_err() {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "expression lowering failed",
                    ))?;
                }
            }
            // else: keep left (the left expression's value is the
            // result of the `&&` expression when it's falsy). We branch
            // to `merge_bb` to join both control-flow paths and use a
            // phi node to select the correct value.
            self.builder.position_at_end(else_bb);
            if self.builder.get_insert_block().is_some() {
                self.builder.position_at_end(else_bb);
                let _ = self
                    .builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
            }
            self.builder.position_at_end(merge_bb);
            if let Ok(rval) = rv
                && let Some(phi) = self.build_phi_merge(then_bb, else_bb, rval, left_val)
            {
                return Ok(phi);
            }
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "expression lowering failed",
            ))?;
        }
        if let deno_ast::swc::ast::BinaryOp::LogicalOr = bin.op {
            // `||` mirrors `&&` but keeps the left value when truthy.
            // a || b -> if a truthy then a else b
            let left_val = l;
            let cond = self
                .to_condition_i1(left_val)
                .ok_or_else(|| Diagnostic::error("failed to convert to boolean condition"))?;
            let then_bb = self.context.append_basic_block(function, "or.then");
            let else_bb = self.context.append_basic_block(function, "or.else");
            let merge_bb = self.context.append_basic_block(function, "or.merge");
            if self
                .builder
                .build_conditional_branch(cond, then_bb, else_bb)
                .is_err()
            {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "expression lowering failed",
                ))?;
            }
            // then: keep left
            self.builder.position_at_end(then_bb);
            if self.builder.get_insert_block().is_some()
                && self.builder.build_unconditional_branch(merge_bb).is_err()
            {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "expression lowering failed",
                ))?;
            }
            // else: evaluate right
            self.builder.position_at_end(else_bb);
            let rv = self.lower_expr(&bin.right, function, param_map, locals);
            if self.builder.get_insert_block().is_some() {
                let _ = self
                    .builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
            }
            self.builder.position_at_end(merge_bb);
            if let Ok(rval) = rv
                && let Some(phi) = self.build_phi_merge(then_bb, else_bb, left_val, rval)
            {
                return Ok(phi);
            }
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "expression lowering failed",
            ))?;
        }

        // Otherwise, handle pointer concat for Add and pointer equality
        match (l, r) {
            (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                // Keep string concat behavior for Add
                if let BinaryOp::Add = bin.op {
                    if let Some(strcat) = self.module.get_function("str_concat") {
                        let call_site =
                            match self
                                .builder
                                .build_call(strcat, &[lp.into(), rp.into()], "concat")
                            {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "operation failed",
                                    ));
                                }
                            };
                        let either = call_site.try_as_basic_value();
                        match either {
                            inkwell::Either::Left(bv) => Ok(bv),
                            _ => Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation not supported (bin strcat result)",
                            )),
                        }
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "string concatenation helper `str_concat` not found",
                            bin.span.lo.0 as usize,
                        ))
                    }
                } else {
                    Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "pointer binary operation not supported",
                        bin.span.lo.0 as usize,
                    ))
                }
            }
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation not supported (bin fallback)",
            )),
        }
    }
}
