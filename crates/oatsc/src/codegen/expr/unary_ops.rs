use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::types::OatsType;
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::types::BasicType;
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
    pub(super) fn lower_unary_expr(
        &self,
        unary: &deno_ast::swc::ast::UnaryExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        // Handle unary operators: -, +, !, ~, typeof
        use deno_ast::swc::ast::UnaryOp;

        // Short-circuit typeof since it returns a string literal based on runtime kind
        if matches!(unary.op, UnaryOp::TypeOf) {
            let inner = self.lower_expr(&unary.arg, function, param_map, locals)?;
            // If we can coerce to f64 (including union-unboxed numbers), it's a number
            if self.coerce_to_f64(inner).is_some() {
                let ptr = self.intern_string_literal("number");
                return Ok(ptr.as_basic_value_enum());
            }

            // If it's a pointer, try to consult the union discriminant helper to
            // determine the runtime kind (number/string/boolean/object). This
            // enables typeof guards to work on boxed union values.
            if let BasicValueEnum::PointerValue(pv) = inner {
                // If this pointer may be a boxed union, call the discriminant
                // helper to distinguish number/string/boolean/object at runtime.
                // This keeps `typeof` semantics consistent for boxed union values.
                let disc_fn = self.get_union_get_discriminant();
                let call_site =
                    self.builder
                        .build_call(disc_fn, &[pv.into()], "union_get_disc");
                let cs = call_site.map_err(|_| {
                    Diagnostic::simple("failed to call union_get_discriminant")
                })?;
                if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                    let disc = bv.into_int_value();
                    // Compare disc to constants: 0 -> number, 1 -> string, 2 -> boolean
                    let zero = self.i64_t.const_int(0, false);
                    let one = self.i64_t.const_int(1, false);
                    let two = self.i64_t.const_int(2, false);

                    let cmp_num = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            disc,
                            zero,
                            "disc_eq_num",
                        )
                        .map_err(|_| {
                            Diagnostic::simple("failed to build int compare for typeof")
                        })?;
                    let cmp_str = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            disc,
                            one,
                            "disc_eq_str",
                        )
                        .map_err(|_| {
                            Diagnostic::simple("failed to build int compare for typeof")
                        })?;
                    let cmp_bool = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            disc,
                            two,
                            "disc_eq_bool",
                        )
                        .map_err(|_| {
                            Diagnostic::simple("failed to build int compare for typeof")
                        })?;

                    let s_num = self.intern_string_literal("number");
                    let s_str = self.intern_string_literal("string");
                    let s_bool = self.intern_string_literal("boolean");
                    let s_obj = self.intern_string_literal("object");

                    // Build nested selects: if num -> s_num else if str -> s_str else if bool -> s_bool else s_obj
                    let sel1 = self
                        .builder
                        .build_select(
                            cmp_num,
                            s_num.as_basic_value_enum(),
                            s_str.as_basic_value_enum(),
                            "sel_num_str",
                        )
                        .map_err(|_| {
                            Diagnostic::simple("failed to build select for typeof")
                        })?;
                    let sel2 = self
                        .builder
                        .build_select(
                            cmp_bool,
                            s_bool.as_basic_value_enum(),
                            s_obj.as_basic_value_enum(),
                            "sel_bool_obj",
                        )
                        .map_err(|_| {
                            Diagnostic::simple("failed to build select for typeof")
                        })?;
                    let final_sel = self
                        .builder
                        .build_select(cmp_str, sel1, sel2, "sel_final")
                        .map_err(|_| {
                            Diagnostic::simple("failed to build select for typeof")
                        })?;
                    return Ok(final_sel);
                }
                // If the discriminant call failed, fall back to "string"
                let ptr = self.intern_string_literal("string");
                return Ok(ptr.as_basic_value_enum());
            }

            // integer/boolean-like values -> "boolean"
            if let BasicValueEnum::IntValue(_iv) = inner {
                let ptr = self.intern_string_literal("boolean");
                return Ok(ptr.as_basic_value_enum());
            }

            let ptr = self.intern_string_literal("object");
            return Ok(ptr.as_basic_value_enum());
        }

        let arg_val = self.lower_expr(&unary.arg, function, param_map, locals)?;

        match unary.op {
            UnaryOp::Minus => {
                // Unary minus: negate the value
                if let Some(fv) = self.coerce_to_f64(arg_val) {
                    let neg = self
                        .builder
                        .build_float_neg(fv, "neg")
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    Ok(neg.as_basic_value_enum())
                } else {
                    Err(Diagnostic::simple("unary minus requires numeric operand"))
                }
            }
            UnaryOp::Plus => {
                // Unary plus: coerce to number (no-op for numbers)
                if let Some(fv) = self.coerce_to_f64(arg_val) {
                    Ok(fv.as_basic_value_enum())
                } else {
                    Err(Diagnostic::simple("unary plus requires numeric operand"))
                }
            }
            UnaryOp::Bang => {
                // Logical NOT: convert to boolean and negate using XOR
                let cond = self
                    .to_condition_i1(arg_val)
                    .ok_or_else(|| Diagnostic::simple("failed to convert to boolean"))?;
                let xor_val = self.context.bool_type().const_int(1, false);
                let not = self
                    .builder
                    .build_xor(cond, xor_val, "logical_not")
                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                Ok(not.as_basic_value_enum())
            }
            UnaryOp::Tilde => {
                // Bitwise NOT: convert to integer, apply NOT, convert back
                if let Some(fv) = self.coerce_to_f64(arg_val) {
                    // Convert to i32
                    let iv = self
                        .builder
                        .build_float_to_signed_int(fv, self.context.i32_type(), "f2i")
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    // Apply bitwise NOT
                    let not_iv = self
                        .builder
                        .build_not(iv, "bnot")
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    // Convert back to f64
                    let result_fv = self
                        .builder
                        .build_signed_int_to_float(not_iv, self.f64_t, "i2f")
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                    Ok(result_fv.as_basic_value_enum())
                } else {
                    Err(Diagnostic::simple("bitwise NOT requires numeric operand"))
                }
            }
            _ => Err(Diagnostic::simple("unsupported unary operator")),
        }
    }

    pub(super) fn lower_update_expr(
        &self,
        update: &deno_ast::swc::ast::UpdateExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        // Handle update operators: ++, --
        // These modify the variable and return the old (postfix) or new (prefix) value
        use deno_ast::swc::ast::UpdateOp;

        // Only simple identifier updates are supported at present.
        if let deno_ast::swc::ast::Expr::Ident(ident) = &*update.arg {
            let name = ident.sym.to_string();

            // Find the variable (parameter or local)
            let var_entry = if param_map.contains_key(&name) {
                // It's a parameter - we can't update parameters directly
                // Need to create a local shadow
                return Err(Diagnostic::simple(
                    "cannot update function parameter directly",
                ));
            } else if let Some((
                ptr,
                ty,
                initialized,
                is_const,
                _extra,
                _nominal,
                _oats_type,
            )) = self.find_local(locals, &name)
            {
                if is_const {
                    return Err(Diagnostic::simple("cannot update immutable variable"));
                }
                if !initialized {
                    return Err(Diagnostic::simple("cannot update uninitialized variable"));
                }
                Some((ptr, ty))
            } else {
                None
            };

            if let Some((ptr, ty)) = var_entry {
                // Load current value
                let old_val = self
                    .builder
                    .build_load(ty, ptr, &name)
                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

                // Only support numeric updates
                if let Some(old_fv) = self.coerce_to_f64(old_val) {
                    let one = self.f64_t.const_float(1.0);

                    // Compute new value based on operator
                    let new_fv = match update.op {
                        UpdateOp::PlusPlus => self
                            .builder
                            .build_float_add(old_fv, one, "inc")
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                        UpdateOp::MinusMinus => self
                            .builder
                            .build_float_sub(old_fv, one, "dec")
                            .map_err(|_| Diagnostic::simple("LLVM builder error"))?,
                    };

                    // Store new value
                    self.builder
                        .build_store(ptr, new_fv)
                        .map_err(|_| Diagnostic::simple("LLVM builder error"))?;

                    // Return old value for postfix, new value for prefix
                    if update.prefix {
                        Ok(new_fv.as_basic_value_enum())
                    } else {
                        Ok(old_fv.as_basic_value_enum())
                    }
                } else {
                    Err(Diagnostic::simple(
                        "update operators require numeric operand",
                    ))
                }
            } else {
                Err(Diagnostic::simple("variable not found"))
            }
        } else {
            Err(Diagnostic::simple(
                "update operator only supports simple identifiers",
            ))
        }
    }
}