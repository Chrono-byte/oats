use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
use oxc_ast::ast::{BinaryOperator, Expression, UnaryOperator};

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
#[allow(dead_code)]
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
        bin: &oxc_ast::ast::BinaryExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use inkwell::FloatPredicate;

        // Special-case: typeof <expr> === "string"/"number" patterns
        //
        // This pattern is common in guard code. When the left side is
        // a `typeof` unary we can lower the inner expression and, for
        // pointer-like (boxed union) values, consult the runtime
        // discriminant instead of performing a heavier dynamic check.
        // This keeps the emitted IR small for the common guarded
        // branches used in tests and examples.
        if let Expression::UnaryExpression(unary) = &bin.left
            && let UnaryOperator::Typeof = unary.operator
            && let Expression::StringLiteral(s_lit) = &bin.right
        {
            // Lower inner expression and then check union discriminant if it's a pointer
            let inner = self.lower_expr(&unary.argument, function, param_map, locals)?;
            let s = s_lit.value.to_string();
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
                    && let inkwell::values::ValueKind::Basic(bv) = cs.try_as_basic_value()
                {
                    let disc = bv.into_int_value();
                    let expected = match s.as_str() {
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
        let left_type = self.last_expr_oats_type.borrow().clone();
        // Lower right operand and capture its origin
        let r = self.lower_expr(&bin.right, function, param_map, locals)?;
        let right_origin = self.last_expr_origin_local.borrow().clone();
        let right_type = self.last_expr_oats_type.borrow().clone();

        // Handle string concatenation BEFORE numeric coercion
        // If op is Add and at least one operand is a pointer (string), treat as string concat
        if let BinaryOperator::Addition = bin.operator {
            // Convert numbers to strings if needed
            // Helper to convert a numeric value (IntValue or FloatValue) to string
            let num_to_string = |num_val: BasicValueEnum<'a>| -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
                let f64_val = if let Some(fv) = self.coerce_to_f64(num_val) {
                    fv
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "cannot convert value to number for string conversion",
                    ));
                };
                let num_to_str = self.get_number_to_string();
                let str_call = self
                    .builder
                    .build_call(num_to_str, &[f64_val.into()], "num_to_str")
                    .map_err(|_| {
                        Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to convert number to string",
                        )
                    })?;
                match str_call.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(BasicValueEnum::PointerValue(p)) => Ok(p),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "number_to_string returned non-pointer",
                    )),
                }
            };

            // Helper to get a string pointer from a value, handling unions
            let get_string_ptr =
                |val: BasicValueEnum<'a>,
                 val_type: Option<crate::types::OatsType>|
                 -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
                    match val {
                        BasicValueEnum::PointerValue(pv) => {
                            // Check if this is a union that needs unboxing
                            if matches!(val_type, Some(crate::types::OatsType::Union(_))) {
                                // This is a union - we need to unbox it properly
                                // Check discriminant: 0 = number, 1 = string
                                let get_disc = self.get_union_get_discriminant();
                                let disc = self
                                    .builder
                                    .build_call(get_disc, &[pv.into()], "get_union_disc_concat")
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to get union discriminant for string concat",
                                        )
                                    })?;

                                let disc_val = match disc.try_as_basic_value() {
                                    inkwell::values::ValueKind::Basic(bv) => bv.into_int_value(),
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union discriminant must be basic value",
                                        ));
                                    }
                                };

                                // Check if it's a string (discriminant == 1 for pointer/string)
                                let is_string = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::EQ,
                                        disc_val,
                                        self.i64_t.const_int(1, false),
                                        "is_union_string",
                                    )
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to compare union discriminant",
                                        )
                                    })?;

                                // Unbox as string
                                let unbox_str = self.get_union_unbox_ptr();
                                let str_result = self
                                    .builder
                                    .build_call(unbox_str, &[pv.into()], "union_unbox_str_concat")
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to unbox union string",
                                        )
                                    })?;
                                let str_ptr = match str_result.try_as_basic_value() {
                                    inkwell::values::ValueKind::Basic(
                                        BasicValueEnum::PointerValue(p),
                                    ) => p,
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union_unbox_ptr returned non-pointer",
                                        ));
                                    }
                                };

                                // Unbox as number and convert to string
                                let unbox_num = self.get_union_unbox_f64();
                                let num_result = self
                                    .builder
                                    .build_call(unbox_num, &[pv.into()], "union_unbox_num_concat")
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to unbox union number",
                                        )
                                    })?;
                                let num_val = match num_result.try_as_basic_value() {
                                    inkwell::values::ValueKind::Basic(
                                        BasicValueEnum::FloatValue(f),
                                    ) => f,
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union_unbox_f64 returned non-float",
                                        ));
                                    }
                                };
                                let num_str = num_to_string(BasicValueEnum::FloatValue(num_val))
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to convert number to string",
                                        )
                                    })?;

                                // Use select to choose between string and number->string
                                let result_ptr = self
                                    .builder
                                    .build_select(
                                        is_string,
                                        str_ptr,
                                        num_str,
                                        "union_string_select",
                                    )
                                    .map_err(|_| {
                                        Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "failed to select union string",
                                        )
                                    })?;

                                match result_ptr {
                                    BasicValueEnum::PointerValue(p) => Ok(p),
                                    _ => Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "select returned non-pointer",
                                    )),
                                }
                            } else {
                                // Not a union, use directly as string pointer
                                Ok(pv)
                            }
                        }
                        _ => {
                            // Not a pointer, must be a number - convert to string
                            num_to_string(val)
                        }
                    }
                };

            if let Some((left_str, right_str)) = match (l, r) {
                (BasicValueEnum::PointerValue(_), BasicValueEnum::PointerValue(_)) => {
                    // Both could be strings or unions - use helper to handle both
                    match (get_string_ptr(l, left_type), get_string_ptr(r, right_type)) {
                        (Ok(lp), Ok(rp)) => Some((lp, rp)),
                        (Err(e), _) | (_, Err(e)) => return Err(e),
                    }
                }
                (BasicValueEnum::PointerValue(_), _) => {
                    // Left is string/union, right is number - convert right to string
                    match (get_string_ptr(l, left_type), num_to_string(r)) {
                        (Ok(lp), Ok(rp)) => Some((lp, rp)),
                        (Err(e), _) | (_, Err(e)) => return Err(e),
                    }
                }
                (_, BasicValueEnum::PointerValue(_)) => {
                    // Left is number, right is string/union - convert left to string
                    match (num_to_string(l), get_string_ptr(r, right_type)) {
                        (Ok(lp), Ok(rp)) => Some((lp, rp)),
                        (Err(e), _) | (_, Err(e)) => return Err(e),
                    }
                }
                _ => {
                    // Not a string concatenation case, fall through to numeric operations below
                    None
                }
            } {
                if let Some(strcat) = self.module.get_function("str_concat") {
                    let call_site = match self.builder.build_call(
                        strcat,
                        &[left_str.into(), right_str.into()],
                        "concat",
                    ) {
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
                        inkwell::values::ValueKind::Basic(bv) => {
                            // After creating the concatenated string, decrement
                            // temporary operands that didn't originate from locals.
                            // Also decrement any number-to-string conversions we created.
                            if left_origin.is_none()
                                && let Some(rc_dec_fn) = self.module.get_function("rc_dec")
                            {
                                let _ = self
                                    .builder
                                    .build_call(rc_dec_fn, &[left_str.into()], "rc_dec_tmp_left")
                                    .ok();
                            }
                            if right_origin.is_none()
                                && let Some(rc_dec_fn) = self.module.get_function("rc_dec")
                            {
                                let _ = self
                                    .builder
                                    .build_call(rc_dec_fn, &[right_str.into()], "rc_dec_tmp_right")
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
                }
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "string concatenation helper `str_concat` not found",
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
                         op: &BinaryOperator|
         -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
            match op {
                BinaryOperator::Addition => {
                    let v = builder
                        .build_float_add(lf, rf, "sum")
                        .map_err(|_| Diagnostic::error("float add failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Subtraction => {
                    let v = builder
                        .build_float_sub(lf, rf, "sub")
                        .map_err(|_| Diagnostic::error("float sub failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Multiplication => {
                    let v = builder
                        .build_float_mul(lf, rf, "mul")
                        .map_err(|_| Diagnostic::error("float mul failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Division => {
                    let v = builder
                        .build_float_div(lf, rf, "div")
                        .map_err(|_| Diagnostic::error("float div failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Remainder => {
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
                         op: &BinaryOperator|
         -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
            let pred = match op {
                BinaryOperator::LessThan => FloatPredicate::OLT,
                BinaryOperator::LessEqualThan => FloatPredicate::OLE,
                BinaryOperator::GreaterThan => FloatPredicate::OGT,
                BinaryOperator::GreaterEqualThan => FloatPredicate::OGE,
                BinaryOperator::Equality => FloatPredicate::OEQ,
                BinaryOperator::Inequality => FloatPredicate::ONE,
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
            if let Ok(v) = float_bin(&self.builder, lf, rf, &bin.operator) {
                return Ok(v);
            }
            if let Ok(v) = float_cmp(&self.builder, lf, rf, &bin.operator) {
                return Ok(v);
            }
        }

        // Note: Logical operators (&&, ||) are handled as LogicalExpression, not BinaryExpression
        // These cases are removed since LogicalAnd and LogicalOr variants don't exist in BinaryOperator

        // Otherwise, handle pointer concat for Add and pointer equality
        match (l, r) {
            (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                // Keep string concat behavior for Add
                if let BinaryOperator::Addition = bin.operator {
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
                            inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                            _ => Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation not supported (bin strcat result)",
                            )),
                        }
                    } else {
                        Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "string concatenation helper `str_concat` not found",
                            bin.span.start as usize,
                        ))
                    }
                } else {
                    Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "pointer binary operation not supported",
                        bin.span.start as usize,
                    ))
                }
            }
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation not supported (bin fallback)",
            )),
        }
    }

    /// Helper method to perform binary operations on already-lowered values.
    /// This is used for compound assignments where we need to operate on loaded values.
    pub(super) fn lower_binary_expr_with_values(
        &self,
        op: &BinaryOperator,
        left: BasicValueEnum<'a>,
        right: BasicValueEnum<'a>,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Helper to handle float arithmetic
        let float_bin = |builder: &Builder<'a>,
                         lf: inkwell::values::FloatValue<'a>,
                         rf: inkwell::values::FloatValue<'a>,
                         op: &BinaryOperator|
         -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
            match op {
                BinaryOperator::Addition => {
                    let v = builder
                        .build_float_add(lf, rf, "sum")
                        .map_err(|_| Diagnostic::error("float add failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Subtraction => {
                    let v = builder
                        .build_float_sub(lf, rf, "sub")
                        .map_err(|_| Diagnostic::error("float sub failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Multiplication => {
                    let v = builder
                        .build_float_mul(lf, rf, "mul")
                        .map_err(|_| Diagnostic::error("float mul failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Division => {
                    let v = builder
                        .build_float_div(lf, rf, "div")
                        .map_err(|_| Diagnostic::error("float div failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Remainder => {
                    let v = builder
                        .build_float_rem(lf, rf, "rem")
                        .map_err(|_| Diagnostic::error("float rem failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::Exponential => {
                    // Exponentiation: use standard library pow function
                    // Declare pow(double, double) -> double if not already declared
                    let pow_fn = if let Some(f) = self.module.get_function("pow") {
                        f
                    } else {
                        let fn_type = self
                            .f64_t
                            .fn_type(&[self.f64_t.into(), self.f64_t.into()], false);
                        self.module.add_function("pow", fn_type, None)
                    };

                    // Call pow(base, exponent)
                    let call_site = builder
                        .build_call(pow_fn, &[lf.into(), rf.into()], "pow_call")
                        .map_err(|_| Diagnostic::error("pow call failed"))?;

                    if let inkwell::values::ValueKind::Basic(bv) = call_site.try_as_basic_value() {
                        Ok(bv)
                    } else {
                        Err(Diagnostic::simple_boxed(
                            crate::diagnostics::Severity::Error,
                            "pow returned void",
                        ))
                    }
                }
                _ => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported binary operation for compound assignment",
                )),
            }
        };

        // Try float path by coercing to float as needed
        if let (Some(lf), Some(rf)) = (self.coerce_to_f64(left), self.coerce_to_f64(right))
            && let Ok(v) = float_bin(&self.builder, lf, rf, op)
        {
            return Ok(v);
        }

        // Handle integer bitwise operations
        if let (BasicValueEnum::IntValue(li), BasicValueEnum::IntValue(ri)) = (left, right) {
            match op {
                BinaryOperator::ShiftLeft => {
                    // Ensure both are i64 for shift operations
                    let li64 = if li.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(li, self.i64_t, "lshift_left")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        li
                    };
                    let ri64 = if ri.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(ri, self.i64_t, "lshift_right")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        ri
                    };
                    let v = self
                        .builder
                        .build_left_shift(li64, ri64, "lshift")
                        .map_err(|_| Diagnostic::error("left shift failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::ShiftRight => {
                    let li64 = if li.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(li, self.i64_t, "rshift_left")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        li
                    };
                    let ri64 = if ri.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(ri, self.i64_t, "rshift_right")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        ri
                    };
                    let v = self
                        .builder
                        .build_right_shift(li64, ri64, false, "rshift")
                        .map_err(|_| Diagnostic::error("right shift failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                // Note: UnsignedShiftRight (>>>) variant doesn't exist in oxc_ast::ast::BinaryOperator.
                // If oxc_ast adds support for it, we should handle it here using build_right_shift
                // with is_arithmetic=false (logical shift). For now, unsigned right shift is
                // properly implemented in compound assignments (>>>=) in assignments.rs.
                BinaryOperator::BitwiseAnd => {
                    let li64 = if li.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(li, self.i64_t, "and_left")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        li
                    };
                    let ri64 = if ri.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(ri, self.i64_t, "and_right")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        ri
                    };
                    let v = self
                        .builder
                        .build_and(li64, ri64, "bitwise_and")
                        .map_err(|_| Diagnostic::error("bitwise and failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::BitwiseOR => {
                    let li64 = if li.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(li, self.i64_t, "or_left")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        li
                    };
                    let ri64 = if ri.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(ri, self.i64_t, "or_right")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        ri
                    };
                    let v = self
                        .builder
                        .build_or(li64, ri64, "bitwise_or")
                        .map_err(|_| Diagnostic::error("bitwise or failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                BinaryOperator::BitwiseXOR => {
                    let li64 = if li.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(li, self.i64_t, "xor_left")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        li
                    };
                    let ri64 = if ri.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_s_extend(ri, self.i64_t, "xor_right")
                            .map_err(|_| Diagnostic::error("int extend failed"))?
                    } else {
                        ri
                    };
                    let v = self
                        .builder
                        .build_xor(li64, ri64, "bitwise_xor")
                        .map_err(|_| Diagnostic::error("bitwise xor failed"))?;
                    Ok(v.as_basic_value_enum())
                }
                _ => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported integer binary operation for compound assignment",
                )),
            }
        } else {
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "compound assignment only supported for numeric types",
            ))
        }
    }
}
