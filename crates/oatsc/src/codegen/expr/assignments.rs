use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::BasicValue;
use inkwell::values::PointerValue;

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
    pub(super) fn lower_assign_expr(
        &self,
        assign: &oats_ast::AssignExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use oats_ast::*;

        // Handle compound assignment operators (e.g., +=, -=, *=, etc.)
        // For compound assignments, we need to:
        // 1. Load the current value of the left-hand side
        // 2. Perform the binary operation with the right-hand side
        // 3. Store the result back
        if !matches!(assign.op, AssignOp::Eq) {
            // This is a compound assignment (e.g., +=, -=, *=)
            if let AssignTarget::Pat(Pat::Ident(ident)) = &assign.left {
                let name = ident.sym.clone();
                if let Some((ptr, _ty, _init, is_const, _extra, _nominal, _oats_type)) =
                    self.find_local(locals, &name)
                {
                    if is_const {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "assignment to immutable variable",
                        ));
                    }

                    // Load current value
                    let current_val = if _ty == self.i8ptr_t.as_basic_type_enum() {
                        self.builder
                            .build_load(self.i8ptr_t, ptr, "current_val")
                            .map_err(|_| Diagnostic::error("failed to load current value"))?
                    } else if _ty == self.f64_t.as_basic_type_enum() {
                        self.builder
                            .build_load(self.f64_t, ptr, "current_val")
                            .map_err(|_| Diagnostic::error("failed to load current value"))?
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "compound assignment only supported for numbers and pointers",
                        ));
                    };

                    // Lower right-hand side
                    let rhs_val = self.lower_expr(&assign.right, function, param_map, locals)?;

                    // Convert compound assignment operator to binary operator
                    let bin_op = match assign.op {
                        AssignOp::PlusEq => BinaryOp::Plus,
                        AssignOp::MinusEq => BinaryOp::Minus,
                        AssignOp::MulEq => BinaryOp::Mul,
                        AssignOp::DivEq => BinaryOp::Div,
                        AssignOp::ModEq => BinaryOp::Mod,
                        AssignOp::LShiftEq => BinaryOp::LShift,
                        AssignOp::RShiftEq => BinaryOp::RShift,
                        AssignOp::URShiftEq => BinaryOp::URShift,
                        AssignOp::BitwiseAndEq => BinaryOp::BitwiseAnd,
                        AssignOp::BitwiseOrEq => BinaryOp::BitwiseOr,
                        AssignOp::BitwiseXorEq => BinaryOp::BitwiseXor,
                        AssignOp::ExpEq => BinaryOp::Exp,
                        AssignOp::Eq => unreachable!(), // Already handled above
                    };

                    // Lower the binary expression using the current value as left operand
                    let result = self.lower_binary_expr_with_values(
                        &bin_op,
                        current_val,
                        rhs_val,
                        function,
                        param_map,
                        locals,
                    )?;

                    // Store result directly
                    if _ty == self.i8ptr_t.as_basic_type_enum() {
                        if let BasicValueEnum::PointerValue(newpv) = result {
                            // RC protocol: increment new value before storing
                            if !self.should_elide_rc_for_local(&name)
                                && !self.is_unowned_local(locals, &name)
                            {
                                let rc_inc = self.get_rc_inc();
                                let _ = self
                                    .builder
                                    .build_call(rc_inc, &[newpv.into()], "rc_inc_new")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                            }

                            // Decrement old value after storing
                            if _init
                                && !self.should_elide_rc_for_local(&name)
                                && let BasicValueEnum::PointerValue(oldpv) = current_val
                            {
                                let rc_dec = self.get_rc_dec();
                                let _ = self
                                    .builder
                                    .build_call(rc_dec, &[oldpv.into()], "rc_dec_old")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                            }

                            let _ = self.builder.build_store(ptr, result);
                        } else {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "compound assignment result type mismatch",
                            ));
                        }
                    } else {
                        let _ = self.builder.build_store(ptr, result);
                    }

                    self.set_local_initialized(locals, &name, true);
                    return Ok(result);
                }
            } else if let AssignTarget::Member(member) = &assign.left {
                // Compound assignment for member access (e.g., obj.field += 5)
                // Load the field, perform the operation, and store back

                // Only support dot-member (obj.prop), not computed (obj[expr])
                use oats_ast::*;
                if let MemberProp::Ident(prop_ident) = &member.prop {
                    let field_name = prop_ident.sym.clone();

                    // Lower the object to get its pointer
                    let obj_val = self.lower_expr(&member.obj, function, param_map, locals)?;
                    let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
                        pv
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "member assignment requires object pointer",
                            assign.span.start,
                        ));
                    };

                    // Determine class name to get field information
                    // All types must be known at compile time - if we can't infer the type, it's an error
                    let mut class_name_opt: Option<String> = None;
                    if let Expr::Ident(ident) = &*member.obj {
                        let ident_name = ident.sym.clone();
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                            self.find_local(locals, &ident_name)
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
                    } else if matches!(&*member.obj, Expr::This(_)) {
                        let fname = function.get_name().to_str().unwrap_or("");
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                            && !param_types.is_empty()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    }

                    // Get field information
                    if let Some(class_name) = class_name_opt {
                        if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                            if let Some((field_idx, (_fname, field_ty))) = fields
                                .iter()
                                .enumerate()
                                .find(|(_, (n, _))| n == &field_name)
                            {
                                // Compute field offset
                                let hdr_size = self.i64_t.const_int(8u64, false);
                                let meta_slot = self.i64_t.const_int(8u64, false);
                                let ptr_sz = self.i64_t.const_int(8u64, false);
                                let idx_const = self.i64_t.const_int(field_idx as u64, false);
                                let mul = self
                                    .builder
                                    .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let tmp = self
                                    .builder
                                    .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let offset = self
                                    .builder
                                    .build_int_add(tmp, mul, "fld_off")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;

                                let field_i8ptr = self
                                    .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;

                                // Load current field value
                                let current_val = match field_ty {
                                    crate::types::OatsType::Number => {
                                        let f64_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                field_i8ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "f64_ptr_cast",
                                            )
                                            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                        let loaded = self
                                            .builder
                                            .build_load(self.f64_t, f64_ptr, "field_f64_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        loaded.as_basic_value_enum()
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_) => {
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                field_i8ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            )
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        
                                        self
                                            .builder
                                            .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            format!(
                                                "compound assignment not supported for field type"
                                            ),
                                            assign.span.start,
                                        ));
                                    }
                                };

                                // Lower right-hand side
                                let rhs_val =
                                    self.lower_expr(&assign.right, function, param_map, locals)?;

                                // Convert compound assignment operator to binary operator
                                let bin_op = match assign.op {
                                    AssignOp::PlusEq => BinaryOp::Plus,
                                    AssignOp::MinusEq => BinaryOp::Minus,
                                    AssignOp::MulEq => BinaryOp::Mul,
                                    AssignOp::DivEq => BinaryOp::Div,
                                    AssignOp::ModEq => BinaryOp::Mod,
                                    AssignOp::LShiftEq => BinaryOp::LShift,
                                    AssignOp::RShiftEq => BinaryOp::RShift,
                                    AssignOp::URShiftEq => BinaryOp::URShift,
                                    AssignOp::BitwiseAndEq => BinaryOp::BitwiseAnd,
                                    AssignOp::BitwiseOrEq => BinaryOp::BitwiseOr,
                                    AssignOp::BitwiseXorEq => BinaryOp::BitwiseXor,
                                    AssignOp::ExpEq => BinaryOp::Exp,
                                    AssignOp::Eq => unreachable!(),
                                };

                                // Perform binary operation
                                let result = self.lower_binary_expr_with_values(
                                    &bin_op,
                                    current_val,
                                    rhs_val,
                                    function,
                                    param_map,
                                    locals,
                                )?;

                                // Store result back to field
                                match field_ty {
                                    crate::types::OatsType::Number => {
                                        if let BasicValueEnum::FloatValue(fv) = result {
                                            let f64_ptr = self
                                                .builder
                                                .build_pointer_cast(
                                                    field_i8ptr,
                                                    self.context.ptr_type(AddressSpace::default()),
                                                    "f64_ptr_cast",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("LLVM builder error")
                                                })?;
                                            let _ = self.builder.build_store(f64_ptr, fv);
                                            return Ok(result);
                                        } else {
                                            return Err(Diagnostic::simple_with_span_boxed(
                                                Severity::Error,
                                                "compound assignment result type mismatch for number field",
                                                assign.span.start,
                                            ));
                                        }
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_) => {
                                        if let BasicValueEnum::PointerValue(new_pv) = result {
                                            let slot_ptr_ty =
                                                self.context.ptr_type(AddressSpace::default());
                                            let slot_ptr = self
                                                .builder
                                                .build_pointer_cast(
                                                    field_i8ptr,
                                                    slot_ptr_ty,
                                                    "slot_ptr_cast",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("operation failed")
                                                })?;

                                            // Load old value for RC decrement
                                            let old_val = self
                                                .builder
                                                .build_load(self.i8ptr_t, slot_ptr, "old_field_val")
                                                .map_err(|_| {
                                                    Diagnostic::error("operation failed")
                                                })?;

                                            // Store new value
                                            let _ = self.builder.build_store(slot_ptr, result);

                                            // RC PROTOCOL: Increment new value, then decrement old
                                            let rc_inc = self.get_rc_inc();
                                            let _ = self
                                                .builder
                                                .build_call(
                                                    rc_inc,
                                                    &[new_pv.into()],
                                                    "rc_inc_new_field",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("operation failed")
                                                })?;

                                            if let BasicValueEnum::PointerValue(old_pv) = old_val {
                                                let rc_dec = self.get_rc_dec();
                                                let _ = self
                                                    .builder
                                                    .build_call(
                                                        rc_dec,
                                                        &[old_pv.into()],
                                                        "rc_dec_old_field",
                                                    )
                                                    .map_err(|_| {
                                                        Diagnostic::error("operation failed")
                                                    })?;
                                            }

                                            return Ok(result);
                                        } else {
                                            return Err(Diagnostic::simple_with_span_boxed(
                                                Severity::Error,
                                                "compound assignment result type mismatch for pointer field",
                                                assign.span.start,
                                            ));
                                        }
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            format!(
                                                "compound assignment not supported for field type"
                                            ),
                                            assign.span.start,
                                        ));
                                    }
                                }
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    format!(
                                        "field '{}' not found in class '{}'",
                                        field_name, class_name
                                    ),
                                    assign.span.start,
                                ));
                            }
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                format!("class '{}' has no registered fields", class_name),
                                assign.span.start,
                            ));
                        }
                    } else {
                        // Cannot determine class type at compile time
                        // All types must be known at compile time - this is a type inference failure
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "cannot determine object type for compound assignment: all types must be known at compile time",
                            assign.span.start,
                        ));
                    }
                } else if let MemberProp::Computed(computed_expr) = &member.prop {
                    // Compound assignment for computed member access (e.g., obj["field"] += 5 or obj[0] += 5)
                    // Support both string literal indices and compile-time constant integer indices

                    // Lower the object to get its pointer
                    let obj_val = self.lower_expr(&member.obj, function, param_map, locals)?;
                    let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
                        pv
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "member assignment requires object pointer",
                            assign.span.start,
                        ));
                    };

                    // Determine class name to get field information
                    let mut class_name_opt: Option<String> = None;
                    if let Expr::Ident(ident) = &*member.obj {
                        let ident_name = ident.sym.clone();
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                            self.find_local(locals, &ident_name)
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
                    } else if matches!(&*member.obj, Expr::This(_)) {
                        let fname = function.get_name().to_str().unwrap_or("");
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                            && !param_types.is_empty()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    }

                    // Try to evaluate the computed index as a compile-time constant
                    // Support both string literals (field names) and integer literals (field indices)
                    let (field_idx, field_ty) = if let Some(class_name) = class_name_opt.clone() {
                        if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                            // Try to evaluate as string literal first (field name)
                            if let Ok(crate::codegen::const_eval::ConstValue::Str(s)) =
                                crate::codegen::const_eval::eval_const_expr(
                                    computed_expr,
                                    member.span.start,
                                    &std::collections::HashMap::new(),
                                )
                            {
                                // Look up field by name
                                if let Some((idx, (_name, ty))) =
                                    fields.iter().enumerate().find(|(_, (name, _))| name == &s)
                                {
                                    (idx, ty.clone())
                                } else {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        format!(
                                            "field '{}' not found in class '{}'",
                                            s, class_name
                                        ),
                                        assign.span.start,
                                    ));
                                }
                            } else {
                                // Try to evaluate as integer literal (field index)
                                let idx_val =
                                    self.lower_expr(computed_expr, function, param_map, locals)?;
                                let idx_i64 = match idx_val {
                                    BasicValueEnum::IntValue(iv) => self
                                        .builder
                                        .build_int_cast(iv, self.i64_t, "idx_i64")
                                        .map_err(|_| Diagnostic::error("LLVM builder error"))?,
                                    BasicValueEnum::FloatValue(fv) => self
                                        .builder
                                        .build_float_to_signed_int(fv, self.i64_t, "f2i")
                                        .map_err(|_| Diagnostic::error("LLVM builder error"))?,
                                    _ => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "computed index must be a compile-time constant (string or integer literal)",
                                            assign.span.start,
                                        ));
                                    }
                                };

                                if let Some(const_idx) = idx_i64.get_zero_extended_constant() {
                                    let idx_usize = const_idx as usize;
                                    if let Some((idx, (_name, ty))) =
                                        fields.iter().enumerate().find(|(i, _)| *i == idx_usize)
                                    {
                                        (idx, ty.clone())
                                    } else {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            format!(
                                                "field index {} out of bounds for class '{}'",
                                                idx_usize, class_name
                                            ),
                                            assign.span.start,
                                        ));
                                    }
                                } else {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        "computed index must be a compile-time constant (string or integer literal)",
                                        assign.span.start,
                                    ));
                                }
                            }
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                format!("class '{}' has no registered fields", class_name),
                                assign.span.start,
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "cannot determine object type for computed member assignment: all types must be known at compile time",
                            assign.span.start,
                        ));
                    };

                    // Compute field offset (same as dot-member access)
                    let hdr_size = self.i64_t.const_int(8u64, false);
                    let meta_slot = self.i64_t.const_int(8u64, false);
                    let ptr_sz = self.i64_t.const_int(8u64, false);
                    let idx_const = self.i64_t.const_int(field_idx as u64, false);
                    let mul = self
                        .builder
                        .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                    let tmp = self
                        .builder
                        .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                    let offset = self
                        .builder
                        .build_int_add(tmp, mul, "fld_off")
                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;

                    let field_i8ptr = self
                        .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr")
                        .map_err(|_| Diagnostic::error("operation failed"))?;

                    // Load current field value
                    let current_val = match field_ty {
                        crate::types::OatsType::Number => {
                            let f64_ptr = self
                                .builder
                                .build_pointer_cast(
                                    field_i8ptr,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "f64_ptr_cast",
                                )
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let loaded = self
                                .builder
                                .build_load(self.f64_t, f64_ptr, "field_f64_load")
                                .map_err(|_| Diagnostic::error("operation failed"))?;
                            loaded.as_basic_value_enum()
                        }
                        crate::types::OatsType::String
                        | crate::types::OatsType::NominalStruct(_)
                        | crate::types::OatsType::Array(_) => {
                            let slot_ptr_ty = self.context.ptr_type(AddressSpace::default());
                            let slot_ptr = self
                                .builder
                                .build_pointer_cast(field_i8ptr, slot_ptr_ty, "slot_ptr_cast")
                                .map_err(|_| Diagnostic::error("operation failed"))?;
                            
                            self
                                .builder
                                .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                .map_err(|_| Diagnostic::error("operation failed"))?
                        }
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment not supported for field type".to_string(),
                                assign.span.start,
                            ));
                        }
                    };

                    // Lower right-hand side
                    let rhs_val = self.lower_expr(&assign.right, function, param_map, locals)?;

                    // Convert compound assignment operator to binary operator
                    let bin_op = match assign.op {
                        AssignOp::PlusEq => BinaryOp::Plus,
                        AssignOp::MinusEq => BinaryOp::Minus,
                        AssignOp::MulEq => BinaryOp::Mul,
                        AssignOp::DivEq => BinaryOp::Div,
                        AssignOp::ModEq => BinaryOp::Mod,
                        AssignOp::LShiftEq => BinaryOp::LShift,
                        AssignOp::RShiftEq => BinaryOp::RShift,
                        AssignOp::URShiftEq => BinaryOp::URShift,
                        AssignOp::BitwiseAndEq => BinaryOp::BitwiseAnd,
                        AssignOp::BitwiseOrEq => BinaryOp::BitwiseOr,
                        AssignOp::BitwiseXorEq => BinaryOp::BitwiseXor,
                        AssignOp::ExpEq => BinaryOp::Exp,
                        AssignOp::Eq => unreachable!(),
                    };

                    // Perform binary operation
                    let result = self.lower_binary_expr_with_values(
                        &bin_op,
                        current_val,
                        rhs_val,
                        function,
                        param_map,
                        locals,
                    )?;

                    // Store result back to field
                    match field_ty {
                        crate::types::OatsType::Number => {
                            if let BasicValueEnum::FloatValue(fv) = result {
                                let f64_ptr = self
                                    .builder
                                    .build_pointer_cast(
                                        field_i8ptr,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "f64_ptr_cast",
                                    )
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let _ = self.builder.build_store(f64_ptr, fv);
                                return Ok(result);
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "compound assignment result type mismatch for number field",
                                    assign.span.start,
                                ));
                            }
                        }
                        crate::types::OatsType::String
                        | crate::types::OatsType::NominalStruct(_)
                        | crate::types::OatsType::Array(_) => {
                            if let BasicValueEnum::PointerValue(new_pv) = result {
                                let slot_ptr_ty = self.context.ptr_type(AddressSpace::default());
                                let slot_ptr = self
                                    .builder
                                    .build_pointer_cast(field_i8ptr, slot_ptr_ty, "slot_ptr_cast")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;

                                // Load old value for RC decrement
                                let old_val = self
                                    .builder
                                    .build_load(self.i8ptr_t, slot_ptr, "old_field_val")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;

                                // Store new value
                                let _ = self.builder.build_store(slot_ptr, result);

                                // RC PROTOCOL: Increment new value, then decrement old
                                let rc_inc = self.get_rc_inc();
                                let _ = self
                                    .builder
                                    .build_call(rc_inc, &[new_pv.into()], "rc_inc_new_field")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;

                                if let BasicValueEnum::PointerValue(old_pv) = old_val {
                                    let rc_dec = self.get_rc_dec();
                                    let _ = self
                                        .builder
                                        .build_call(rc_dec, &[old_pv.into()], "rc_dec_old_field")
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                }

                                return Ok(result);
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "compound assignment result type mismatch for pointer field",
                                    assign.span.start,
                                ));
                            }
                        }
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment not supported for field type".to_string(),
                                assign.span.start,
                            ));
                        }
                    }
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "compound assignment to private name member access not yet supported",
                        assign.span.start,
                    ));
                }
            }
        }

        // support simple assignments `ident = expr` where the left side is an identifier
        if let AssignTarget::Pat(Pat::Ident(ident)) = &assign.left {
            let name = ident.sym.clone();
            if let Some((ptr, _ty, _init, is_const, _extra, _nominal, oats_type)) =
                self.find_local(locals, &name)
            {
                // Disallow assigning to immutable locals at compile-time
                if is_const {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "assignment to immutable variable",
                    ));
                }
                if let Ok(mut val) = self.lower_expr(&assign.right, function, param_map, locals) {
                    // If the target variable is a union type, box the value
                    if let Some(crate::types::OatsType::Union(_)) = oats_type {
                        val = match val {
                            BasicValueEnum::FloatValue(fv) => {
                                // Box number into union
                                let box_f64_fn = self.get_union_box_f64();
                                let boxed_call = self
                                    .builder
                                    .build_call(box_f64_fn, &[fv.into()], "box_union_f64")
                                    .map_err(|_| Diagnostic::error("union box failed"))?;

                                if let inkwell::Either::Left(bv) = boxed_call.try_as_basic_value() {
                                    bv
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "union box failed",
                                    ));
                                }
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                // Box pointer into union
                                let box_ptr_fn = self.get_union_box_ptr();
                                let boxed_call = self
                                    .builder
                                    .build_call(box_ptr_fn, &[pv.into()], "box_union_ptr")
                                    .map_err(|_| Diagnostic::error("union box failed"))?;

                                if let inkwell::Either::Left(bv) = boxed_call.try_as_basic_value() {
                                    bv
                                } else {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "union box failed",
                                    ));
                                }
                            }
                            _ => val, // Keep other types as-is
                        };
                    }

                    // RC PROTOCOL: Increment refcount of new value BEFORE storing it.
                    // This ensures the new reference is tracked even if an error occurs later.
                    if _ty == self.i8ptr_t.as_basic_type_enum() {
                        if let BasicValueEnum::PointerValue(newpv) = val
                            && !self.should_elide_rc_for_local(&name)
                            && !self.is_unowned_local(locals, &name)
                        {
                            let rc_inc = self.get_rc_inc();
                            let _ =
                                match self
                                    .builder
                                    .build_call(rc_inc, &[newpv.into()], "rc_inc_new")
                                {
                                    Ok(cs) => cs,
                                    Err(_) => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "operation failed",
                                        ));
                                    }
                                };
                        }

                        // Load old value for RC decrement
                        let old = match self.builder.build_load(self.i8ptr_t, ptr, "old_val") {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "operation failed",
                                ));
                            }
                        };

                        // Store new value
                        let _ = self.builder.build_store(ptr, val);

                        // RC PROTOCOL: Decrement refcount of old value AFTER storing the new one.
                        // This ensures proper cleanup of the previous reference.
                        if _init && !self.should_elide_rc_for_local(&name) {
                            let rc_dec = self.get_rc_dec();
                            let _ = match self.builder.build_call(
                                rc_dec,
                                &[old.into()],
                                "rc_dec_old",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "operation failed",
                                    ));
                                }
                            };
                        }
                    } else {
                        let _ = self.builder.build_store(ptr, val);
                    }
                    // mark initialized after assignment
                    self.set_local_initialized(locals, &name, true);
                    return Ok(val);
                }
            }
        }

        // Handle member assignment: obj.field = expr
        if let AssignTarget::Member(member) = &assign.left {
            // Lower the right-hand side value
            if let Ok(new_val) = self.lower_expr(&assign.right, function, param_map, locals) {
                // Capture RHS origin (if this RHS expression originated from a local)
                let rhs_origin_after_rhs = self.last_expr_origin_local.borrow().clone();
                // Only handle dot-member (obj.prop), not computed (obj[expr])
                use oats_ast::*;
                if let MemberProp::Ident(prop_ident) = &member.prop {
                    let field_name = prop_ident.sym.clone();

                    // Lower the object to get its pointer
                    let lowered_obj_res = self.lower_expr(&member.obj, function, param_map, locals);
                    // Capture object origin (if object expression originated from a local)
                    let obj_origin_after_obj = self.last_expr_origin_local.borrow().clone();
                    if let Ok(BasicValueEnum::PointerValue(obj_ptr)) = lowered_obj_res {
                        // If both RHS and object have origins, and RHS origin maps to a closure ret type,
                        // synthesize a mapping for this object's field so loads can recover the closure type.
                        if let (Some(obj_orig), Some(rhs_orig)) =
                            (obj_origin_after_obj.clone(), rhs_origin_after_rhs.clone())
                            && let Some(rt) = self.closure_local_rettype.borrow().get(&rhs_orig)
                        {
                            let field_key = format!("{}.{}", obj_orig, field_name);
                            self.closure_local_rettype
                                .borrow_mut()
                                .insert(field_key, rt.clone());
                        }
                        // Determine the class name from the object expression
                        let mut class_name_opt: Option<String> = None;

                        // Check if obj is `this` or a named parameter
                        if let Expr::Ident(ident) = &*member.obj {
                            let ident_name = ident.sym.clone();
                            if ident_name == "this" {
                                // If we're inside a constructor function named <Class>_init,
                                // infer the class name from the function name. Otherwise
                                // fall back to fn_param_types map (receiver typed functions).
                                let fname = function.get_name().to_str().unwrap_or("");
                                if let Some(cls) = fname.strip_suffix("_init") {
                                    class_name_opt = Some(cls.to_string());
                                } else if let Some(param_types) =
                                    self.fn_param_types.borrow().get(fname)
                                    && !param_types.is_empty()
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[0]
                                {
                                    class_name_opt = Some(n.clone());
                                }
                            } else if let Some(param_idx) = param_map.get(&ident_name)
                                && let Some(param_types) = self
                                    .fn_param_types
                                    .borrow()
                                    .get(function.get_name().to_str().unwrap_or(""))
                            {
                                let idx = *param_idx as usize;
                                if idx < param_types.len()
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[idx]
                                {
                                    class_name_opt = Some(n.clone());
                                }
                            } else {
                                // Not a parameter: try to resolve a local variable's nominal
                                // class by inspecting locals. Do NOT fall back to scanning
                                // `class_fields` — require the local to carry a nominal
                                // annotation to be used for member lowering.
                                if let Some((
                                    _,
                                    _ty,
                                    _init,
                                    _is_const,
                                    _is_weak,
                                    nominal,
                                    _oats_type,
                                )) = self.find_local(locals, &ident_name)
                                    && let Some(nom) = nominal
                                {
                                    class_name_opt = Some(nom);
                                }
                            }
                        } else if matches!(&*member.obj, Expr::This(_)) {
                            let fname = function.get_name().to_str().unwrap_or("");
                            if let Some(cls) = fname.strip_suffix("_init") {
                                class_name_opt = Some(cls.to_string());
                            } else if let Some(param_types) =
                                self.fn_param_types.borrow().get(fname)
                                && !param_types.is_empty()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                            {
                                class_name_opt = Some(n.clone());
                            }
                        }

                        // Require that we have a nominal class name inferred from
                        // the object expression (from `this`, a typed parameter,
                        // or a local with a nominal). Do not perform fallback
                        // scans over `class_fields` here.
                        let class_name = if let Some(c) = class_name_opt.clone() {
                            c
                        } else {
                            let fname = function.get_name().to_str().unwrap_or("<unknown>");
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                format!(
                                    "unsupported member assignment: could not infer class for field '{}' in function '{}'",
                                    field_name, fname
                                ),
                            ));
                        };

                        // Look up field list for this class
                        if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                            // Find the field by name
                            if let Some((field_idx, (_fname, field_ty))) = fields
                                .iter()
                                .enumerate()
                                .find(|(_, (n, _))| n == &field_name)
                            {
                                // Compute field offset: header (u64) + field_idx * sizeof(ptr)
                                let hdr_size = self
                                    .i64_t
                                    .const_int(std::mem::size_of::<u64>() as u64, false);
                                let ptr_sz = self
                                    .i64_t
                                    .const_int(std::mem::size_of::<usize>() as u64, false);
                                let idx_const = self.i64_t.const_int(field_idx as u64, false);
                                let mul = self
                                    .builder
                                    .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                // Reserve an 8-byte metadata slot after the header so field
                                // offsets are computed as: header_size + meta_slot + idx * ptr_size
                                let meta_slot = self.i64_t.const_int(8u64, false);
                                let tmp = self
                                    .builder
                                    .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let offset = self
                                    .builder
                                    .build_int_add(tmp, mul, "fld_off")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                // Cast offset to i64 (we already have it as i64) and compute i8* pointer
                                let field_ptr = self
                                    .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr_store")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;

                                // Store based on field type
                                match field_ty {
                                    crate::types::OatsType::Number => {
                                        // Coerce RHS to f64 then store into an f64* slot
                                        if let Some(fv) = self.coerce_to_f64(new_val) {
                                            let f64_ptr_ty =
                                                self.context.ptr_type(AddressSpace::default());
                                            let f64_ptr = self
                                                .builder
                                                .build_pointer_cast(
                                                    field_ptr,
                                                    f64_ptr_ty,
                                                    "f64_ptr_cast_store",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("LLVM builder error")
                                                })?;
                                            let _ = self
                                                .builder
                                                .build_store(f64_ptr, fv.as_basic_value_enum());
                                            return Ok(fv.as_basic_value_enum());
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "expected numeric value for number field",
                                            ));
                                        }
                                    }
                                    crate::types::OatsType::Union(_) => {
                                        // Use boxed pointer representation for unions.
                                        // Always store an i8* pointing to a small heap object produced by union_box_*.
                                        // Slot for unions is stored as an i8* value in memory.
                                        // The slot pointer therefore has type i8** (pointer-to-i8*).
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = match self.builder.build_pointer_cast(
                                            field_ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast_store",
                                        ) {
                                            Ok(p) => p,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                ));
                                            }
                                        };

                                        // If new_val is a number, box it with union_box_f64; if pointer, box with union_box_ptr.
                                        let boxed_new = if let BasicValueEnum::FloatValue(fv) =
                                            new_val
                                        {
                                            let box_fn = self.get_union_box_f64();
                                            let cs = match self.builder.build_call(
                                                box_fn,
                                                &[fv.into()],
                                                "union_box_f64_call",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            match cs.try_as_basic_value() {
                                                inkwell::Either::Left(bv) => bv,
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        } else if let BasicValueEnum::PointerValue(pv) = new_val {
                                            let box_fn = self.get_union_box_ptr();
                                            let cs = match self.builder.build_call(
                                                box_fn,
                                                &[pv.into()],
                                                "union_box_ptr_call",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            match cs.try_as_basic_value() {
                                                inkwell::Either::Left(bv) => bv,
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "unsupported union payload type",
                                            ));
                                        };

                                        // Load old value for RC decrement AFTER incrementing new value
                                        let old_val = match self.builder.build_load(
                                            self.i8ptr_t,
                                            slot_ptr,
                                            "old_field_val",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                ));
                                            }
                                        };

                                        // Store new value
                                        let _ = self.builder.build_store(slot_ptr, boxed_new);

                                        // RC PROTOCOL: Decrement refcount of old field value AFTER storing the new one.
                                        if let BasicValueEnum::PointerValue(old_pv) = old_val {
                                            let rc_dec = self.get_rc_dec();
                                            let _ = match self.builder.build_call(
                                                rc_dec,
                                                &[old_pv.into()],
                                                "rc_dec_old_field",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                        }

                                        // RC PROTOCOL: Increment refcount of new field value after storing.
                                        if let BasicValueEnum::PointerValue(new_pv) = boxed_new {
                                            let rc_inc = self.get_rc_inc();
                                            let _ = match self.builder.build_call(
                                                rc_inc,
                                                &[new_pv.into()],
                                                "rc_inc_new_field",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                        }
                                        return Ok(boxed_new);
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_)
                                    | crate::types::OatsType::Option(_)
                                    | crate::types::OatsType::Weak(_)
                                    | crate::types::OatsType::Promise(_) => {
                                        // Cast to pointer type for slot
                                        // Slot for ref-like fields (string/struct/array) stores an i8* value.
                                        // The slot pointer therefore has type i8** (pointer-to-i8*).
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = match self.builder.build_pointer_cast(
                                            field_ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast_store",
                                        ) {
                                            Ok(p) => p,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                ));
                                            }
                                        };

                                        // Load old value for RC decrement
                                        let old_val = match self.builder.build_load(
                                            self.i8ptr_t,
                                            slot_ptr,
                                            "old_field_val",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                ));
                                            }
                                        };

                                        // RC PROTOCOL: Decrement refcount of old field value before overwriting.
                                        if let BasicValueEnum::PointerValue(old_pv) = old_val {
                                            let rc_dec = self.get_rc_dec();
                                            let _ = match self.builder.build_call(
                                                rc_dec,
                                                &[old_pv.into()],
                                                "rc_dec_old_field",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                        }

                                        // Store new value
                                        // Ensure new_val is a pointer before storing
                                        if let BasicValueEnum::PointerValue(new_pv) = new_val {
                                            let _ = self.builder.build_store(slot_ptr, new_val);
                                            // RC PROTOCOL: Increment refcount of new field value after storing.
                                            let rc_inc = self.get_rc_inc();
                                            let _ = match self.builder.build_call(
                                                rc_inc,
                                                &[new_pv.into()],
                                                "rc_inc_new_field",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            return Ok(new_val);
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "expected pointer value for reference field",
                                            ));
                                        }
                                    }
                                    _ => {
                                        // Unsupported field type
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "expression lowering failed",
                                        ))?;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // member.obj did not lower to a pointer value — emit a generic diagnostic
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported member assignment: member object did not lower to a pointer",
                    ));
                }
            } else {
                // member.obj did not lower to a pointer value — emit a generic diagnostic
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported member assignment: member object did not lower to a pointer",
                ));
            }
        }

        // Handle array element assignment: arr[idx] = value
        if let AssignTarget::Member(member) = &assign.left {
            use oats_ast::*;
            if let MemberProp::Computed(computed) = &member.prop {
                // This is arr[idx] = value

                // Get the alloca pointer for the array variable (needed for reallocation)
                let arr_alloca_opt = if let Expr::Ident(ident) = &*member.obj {
                    let name = ident.sym.clone();
                    self.find_local(locals, &name)
                        .map(|(ptr, _, _, _, _, _, _)| ptr)
                } else {
                    None
                };

                let arr_alloca = arr_alloca_opt.ok_or_else(|| {
                    Diagnostic::simple_with_span(
                        Severity::Error,
                        "array element assignment requires array stored in a variable",
                        assign.span.start,
                    )
                })?;

                // Lower the index expression
                let idx_val = self.lower_expr(computed, function, param_map, locals)?;
                let idx_i64 = if let BasicValueEnum::FloatValue(fv) = idx_val {
                    // Convert f64 to i64
                    match self
                        .builder
                        .build_float_to_signed_int(fv, self.i64_t, "idx_i64")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to convert index to i64",
                            ));
                        }
                    }
                } else if let BasicValueEnum::IntValue(iv) = idx_val {
                    // Extend to i64 if needed
                    if iv.get_type().get_bit_width() < 64 {
                        match self.builder.build_int_s_extend(iv, self.i64_t, "idx_i64") {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to extend index to i64",
                                ));
                            }
                        }
                    } else {
                        iv
                    }
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "array index must be a number",
                    ));
                };

                // Lower the value to assign
                let val = self.lower_expr(&assign.right, function, param_map, locals)?;

                // Determine if this is a pointer or numeric assignment
                if let BasicValueEnum::PointerValue(pv) = val {
                    // Pointer assignment: array_set_ptr(arr_alloca, idx, val)
                    let array_set_ptr = self.get_array_set_ptr();
                    let _ = match self.builder.build_call(
                        array_set_ptr,
                        &[arr_alloca.into(), idx_i64.into(), pv.into()],
                        "array_set_ptr",
                    ) {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to call array_set_ptr",
                            ));
                        }
                    };
                } else if let BasicValueEnum::FloatValue(fv) = val {
                    // Numeric assignment: array_set_f64(arr_alloca, idx, val)
                    let array_set_f64 = self.get_array_set_f64();
                    let _ = match self.builder.build_call(
                        array_set_f64,
                        &[arr_alloca.into(), idx_i64.into(), fv.into()],
                        "array_set_f64",
                    ) {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "failed to call array_set_f64",
                            ));
                        }
                    };
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported array element value type",
                    ));
                }

                return Ok(val);
            }
        }

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "unsupported assignment target or pattern",
            assign.span.start,
        ))
    }
}
