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

// Use the shared helper from codegen::helpers
use crate::codegen::helpers::extract_basic_value_from_valuekind;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(super) fn lower_assign_expr(
        &self,
        assign: &oxc_ast::ast::AssignmentExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use oxc_ast::ast::Expression;

        // Handle compound assignment operators (e.g., +=, -=, *=, etc.)
        // For compound assignments, we need to:
        // 1. Load the current value of the left-hand side
        // 2. Perform the binary operation with the right-hand side
        // 3. Store the result back
        use oxc_ast::ast::{AssignmentOperator, SimpleAssignmentTarget};
        if !matches!(assign.operator, AssignmentOperator::Assign) {
            // This is a compound assignment (e.g., +=, -=, *=)
            if let Some(SimpleAssignmentTarget::AssignmentTargetIdentifier(ident)) =
                assign.left.as_simple_assignment_target()
            {
                let name = ident.name.to_string();
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

                    // Handle unsigned right shift (>>>) specially
                    let result =
                        if matches!(assign.operator, AssignmentOperator::ShiftRightZeroFill) {
                            // Unsigned right shift: convert operands to integers and use logical shift
                            let left_i64 = if let Some(fv) = self.coerce_to_f64(current_val) {
                                self.builder
                                    .build_float_to_signed_int(fv, self.i64_t, "f2i_left")
                                    .map_err(|_| {
                                        Diagnostic::error("float to int conversion failed")
                                    })?
                            } else if let BasicValueEnum::IntValue(iv) = current_val {
                                if iv.get_type().get_bit_width() < 64 {
                                    self.builder
                                        .build_int_s_extend(iv, self.i64_t, "extend_left")
                                        .map_err(|_| Diagnostic::error("int extend failed"))?
                                } else {
                                    iv
                                }
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "unsigned right shift requires numeric operand",
                                ));
                            };

                            let right_i64 = if let Some(fv) = self.coerce_to_f64(rhs_val) {
                                self.builder
                                    .build_float_to_signed_int(fv, self.i64_t, "f2i_right")
                                    .map_err(|_| {
                                        Diagnostic::error("float to int conversion failed")
                                    })?
                            } else if let BasicValueEnum::IntValue(iv) = rhs_val {
                                if iv.get_type().get_bit_width() < 64 {
                                    self.builder
                                        .build_int_s_extend(iv, self.i64_t, "extend_right")
                                        .map_err(|_| Diagnostic::error("int extend failed"))?
                                } else {
                                    iv
                                }
                            } else {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "unsigned right shift requires numeric operand",
                                ));
                            };

                            // Perform unsigned (logical) right shift: is_arithmetic=false means logical shift
                            let shifted = self
                                .builder
                                .build_right_shift(left_i64, right_i64, false, "ushift_right")
                                .map_err(|_| Diagnostic::error("unsigned right shift failed"))?;

                            // Convert result back to f64 for consistency with other numeric operations
                            self.builder
                                .build_signed_int_to_float(shifted, self.f64_t, "i2f")
                                .map_err(|_| Diagnostic::error("int to float conversion failed"))?
                                .as_basic_value_enum()
                        } else {
                            // Convert compound assignment operator to binary operator
                            use oxc_ast::ast::BinaryOperator;
                            let bin_op = match assign.operator {
                                AssignmentOperator::Addition => BinaryOperator::Addition,
                                AssignmentOperator::Subtraction => BinaryOperator::Subtraction,
                                AssignmentOperator::Multiplication => {
                                    BinaryOperator::Multiplication
                                }
                                AssignmentOperator::Division => BinaryOperator::Division,
                                AssignmentOperator::Remainder => BinaryOperator::Remainder,
                                AssignmentOperator::ShiftLeft => BinaryOperator::ShiftLeft,
                                AssignmentOperator::ShiftRight => BinaryOperator::ShiftRight,
                                AssignmentOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                                AssignmentOperator::BitwiseOR => BinaryOperator::BitwiseOR,
                                AssignmentOperator::BitwiseXOR => BinaryOperator::BitwiseXOR,
                                AssignmentOperator::Exponential => BinaryOperator::Exponential,
                                AssignmentOperator::LogicalAnd => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "logical AND assignment (&&=) not supported",
                                    ));
                                }
                                AssignmentOperator::LogicalOr => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "logical OR assignment (||=) not supported",
                                    ));
                                }
                                AssignmentOperator::LogicalNullish => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "nullish coalescing assignment (??=) not supported",
                                    ));
                                }
                                AssignmentOperator::Assign => unreachable!(), // Already handled above
                                AssignmentOperator::ShiftRightZeroFill => unreachable!(), // Handled above
                            };

                            // Lower the binary expression using the current value as left operand
                            self.lower_binary_expr_with_values(
                                &bin_op,
                                current_val,
                                rhs_val,
                                function,
                                param_map,
                                locals,
                            )?
                        };

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
            } else if let Some(member_expr) = assign.left.as_member_expression() {
                // Compound assignment for member access (e.g., obj.field += 5)
                // Load the field, perform the operation, and store back

                // Only support dot-member (obj.prop), not computed (obj[expr])
                if let oxc_ast::ast::MemberExpression::StaticMemberExpression(static_member) =
                    member_expr
                {
                    let field_name = static_member.property.name.to_string();

                    // Lower the object to get its pointer
                    let obj_val =
                        self.lower_expr(&static_member.object, function, param_map, locals)?;
                    let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
                        pv
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "member assignment requires object pointer",
                            assign.span.start as usize,
                        ));
                    };

                    // Determine class name to get field information
                    // All types must be known at compile time - if we can't infer the type, it's an error
                    let mut class_name_opt: Option<String> = None;
                    if let Expression::Identifier(ident) = &static_member.object {
                        let ident_name = ident.as_ref().name.to_string();
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                            self.find_local(locals, &ident_name)
                            && let Some(nom) = nominal
                        {
                            class_name_opt = Some(nom);
                        } else if let Some(param_idx) = param_map.get(&ident_name)
                            && let Some(param_types) = self
                                .fn_param_types
                                .borrow()
                                .get(function.get_name().to_str().unwrap_or_default())
                        {
                            let idx = *param_idx as usize;
                            if idx < param_types.len()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                            {
                                class_name_opt = Some(n.clone());
                            }
                        }
                    } else if matches!(&static_member.object, Expression::ThisExpression(_)) {
                        let fname = function.get_name().to_str().unwrap_or_default();
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

                                        self.builder
                                            .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "compound assignment not supported for field type"
                                                .to_string(),
                                            assign.span.start as usize,
                                        ));
                                    }
                                };

                                // Lower right-hand side
                                let rhs_val =
                                    self.lower_expr(&assign.right, function, param_map, locals)?;

                                // Handle unsigned right shift (>>>) specially
                                let result = if matches!(
                                    assign.operator,
                                    AssignmentOperator::ShiftRightZeroFill
                                ) {
                                    // Unsigned right shift: convert operands to integers and use logical shift
                                    let left_i64 = if let Some(fv) = self.coerce_to_f64(current_val)
                                    {
                                        self.builder
                                            .build_float_to_signed_int(fv, self.i64_t, "f2i_left")
                                            .map_err(|_| {
                                                Diagnostic::error("float to int conversion failed")
                                            })?
                                    } else if let BasicValueEnum::IntValue(iv) = current_val {
                                        if iv.get_type().get_bit_width() < 64 {
                                            self.builder
                                                .build_int_s_extend(iv, self.i64_t, "extend_left")
                                                .map_err(|_| {
                                                    Diagnostic::error("int extend failed")
                                                })?
                                        } else {
                                            iv
                                        }
                                    } else {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "unsigned right shift requires numeric operand",
                                            assign.span.start as usize,
                                        ));
                                    };

                                    let right_i64 = if let Some(fv) = self.coerce_to_f64(rhs_val) {
                                        self.builder
                                            .build_float_to_signed_int(fv, self.i64_t, "f2i_right")
                                            .map_err(|_| {
                                                Diagnostic::error("float to int conversion failed")
                                            })?
                                    } else if let BasicValueEnum::IntValue(iv) = rhs_val {
                                        if iv.get_type().get_bit_width() < 64 {
                                            self.builder
                                                .build_int_s_extend(iv, self.i64_t, "extend_right")
                                                .map_err(|_| {
                                                    Diagnostic::error("int extend failed")
                                                })?
                                        } else {
                                            iv
                                        }
                                    } else {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "unsigned right shift requires numeric operand",
                                            assign.span.start as usize,
                                        ));
                                    };

                                    // Perform unsigned (logical) right shift: is_arithmetic=false means logical shift
                                    let shifted = self
                                        .builder
                                        .build_right_shift(
                                            left_i64,
                                            right_i64,
                                            false,
                                            "ushift_right",
                                        )
                                        .map_err(|_| {
                                            Diagnostic::error("unsigned right shift failed")
                                        })?;

                                    // Convert result back to f64 for consistency with other numeric operations
                                    self.builder
                                        .build_signed_int_to_float(shifted, self.f64_t, "i2f")
                                        .map_err(|_| {
                                            Diagnostic::error("int to float conversion failed")
                                        })?
                                        .as_basic_value_enum()
                                } else {
                                    // Convert compound assignment operator to binary operator
                                    use oxc_ast::ast::BinaryOperator;
                                    let bin_op = match assign.operator {
                                        AssignmentOperator::Addition => BinaryOperator::Addition,
                                        AssignmentOperator::Subtraction => {
                                            BinaryOperator::Subtraction
                                        }
                                        AssignmentOperator::Multiplication => {
                                            BinaryOperator::Multiplication
                                        }
                                        AssignmentOperator::Division => BinaryOperator::Division,
                                        AssignmentOperator::Remainder => BinaryOperator::Remainder,
                                        AssignmentOperator::ShiftLeft => BinaryOperator::ShiftLeft,
                                        AssignmentOperator::ShiftRight => {
                                            BinaryOperator::ShiftRight
                                        }
                                        AssignmentOperator::BitwiseAnd => {
                                            BinaryOperator::BitwiseAnd
                                        }
                                        AssignmentOperator::BitwiseOR => BinaryOperator::BitwiseOR,
                                        AssignmentOperator::BitwiseXOR => {
                                            BinaryOperator::BitwiseXOR
                                        }
                                        AssignmentOperator::Exponential => {
                                            BinaryOperator::Exponential
                                        }
                                        AssignmentOperator::LogicalAnd => {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "logical AND assignment (&&=) not supported",
                                            ));
                                        }
                                        AssignmentOperator::LogicalOr => {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "logical OR assignment (||=) not supported",
                                            ));
                                        }
                                        AssignmentOperator::LogicalNullish => {
                                            return Err(Diagnostic::simple_boxed(
                                                Severity::Error,
                                                "nullish coalescing assignment (??=) not supported",
                                            ));
                                        }
                                        AssignmentOperator::Assign => unreachable!(),
                                        AssignmentOperator::ShiftRightZeroFill => unreachable!(), // Handled above
                                    };

                                    // Perform binary operation
                                    self.lower_binary_expr_with_values(
                                        &bin_op,
                                        current_val,
                                        rhs_val,
                                        function,
                                        param_map,
                                        locals,
                                    )?
                                };

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
                                        }
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "compound assignment result type mismatch for number field",
                                            assign.span.start as usize,
                                        ));
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
                                        }
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "compound assignment result type mismatch for pointer field",
                                            assign.span.start as usize,
                                        ));
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_with_span_boxed(
                                            Severity::Error,
                                            "compound assignment not supported for field type"
                                                .to_string(),
                                            assign.span.start as usize,
                                        ));
                                    }
                                }
                            }
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                format!("field '{field_name}' not found in class '{class_name}'"),
                                assign.span.start as usize,
                            ));
                        }
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            format!("class '{class_name}' has no registered fields"),
                            assign.span.start as usize,
                        ));
                    }
                    // Cannot determine class type at compile time
                    // All types must be known at compile time - this is a type inference failure
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "cannot determine object type for compound assignment: all types must be known at compile time",
                        assign.span.start as usize,
                    ));
                } else if let oxc_ast::ast::MemberExpression::ComputedMemberExpression(
                    computed_member,
                ) = member_expr
                {
                    // Compound assignment for computed member access (e.g., obj["field"] += 5 or obj[0] += 5)
                    // Support both string literal indices and compile-time constant integer indices

                    // Lower the object to get its pointer
                    let obj_val =
                        self.lower_expr(&computed_member.object, function, param_map, locals)?;
                    let obj_ptr = if let BasicValueEnum::PointerValue(pv) = obj_val {
                        pv
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "member assignment requires object pointer",
                            assign.span.start as usize,
                        ));
                    };

                    // Determine class name to get field information
                    let mut class_name_opt: Option<String> = None;
                    if let Expression::Identifier(ident) = &computed_member.object {
                        let ident_name = ident.as_ref().name.to_string();
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                            self.find_local(locals, &ident_name)
                            && let Some(nom) = nominal
                        {
                            class_name_opt = Some(nom);
                        } else if let Some(param_idx) = param_map.get(&ident_name)
                            && let Some(param_types) = self
                                .fn_param_types
                                .borrow()
                                .get(function.get_name().to_str().unwrap_or_default())
                        {
                            let idx = *param_idx as usize;
                            if idx < param_types.len()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                            {
                                class_name_opt = Some(n.clone());
                            }
                        }
                    } else if matches!(&computed_member.object, Expression::ThisExpression(_)) {
                        let fname = function.get_name().to_str().unwrap_or_default();
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                            && !param_types.is_empty()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    }

                    // Try to evaluate the computed index as a compile-time constant
                    // Support both string literals (field names) and integer literals (field indices)
                    let (field_idx, field_ty) = if let Some(class_name) = class_name_opt {
                        if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                            // const_eval removed - check for string literal directly
                            if let oxc_ast::ast::Expression::StringLiteral(lit) =
                                &computed_member.expression
                            {
                                let s = lit.value.to_string();
                                // Look up field by name
                                if let Some((idx, (_name, ty))) =
                                    fields.iter().enumerate().find(|(_, (name, _))| name == &s)
                                {
                                    (idx, ty.clone())
                                } else {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        format!("field '{s}' not found in class '{class_name}'"),
                                        assign.span.start as usize,
                                    ));
                                }
                            } else {
                                // Try to evaluate as integer literal (field index)
                                let idx_val = self.lower_expr(
                                    &computed_member.expression,
                                    function,
                                    param_map,
                                    locals,
                                )?;
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
                                            assign.span.start as usize,
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
                                                "field index {idx_usize} out of bounds for class '{class_name}'"
                                            ),
                                            assign.span.start as usize,
                                        ));
                                    }
                                } else {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        "computed index must be a compile-time constant (string or integer literal)",
                                        assign.span.start as usize,
                                    ));
                                }
                            }
                        } else {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                format!("class '{class_name}' has no registered fields"),
                                assign.span.start as usize,
                            ));
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "cannot determine object type for computed member assignment: all types must be known at compile time",
                            assign.span.start as usize,
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

                            self.builder
                                .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                .map_err(|_| Diagnostic::error("operation failed"))?
                        }
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment not supported for field type".to_string(),
                                assign.span.start as usize,
                            ));
                        }
                    };

                    // Lower right-hand side
                    let rhs_val = self.lower_expr(&assign.right, function, param_map, locals)?;

                    // Handle unsigned right shift (>>>) specially
                    let result =
                        if matches!(assign.operator, AssignmentOperator::ShiftRightZeroFill) {
                            // Unsigned right shift: convert operands to integers and use logical shift
                            let left_i64 = if let Some(fv) = self.coerce_to_f64(current_val) {
                                self.builder
                                    .build_float_to_signed_int(fv, self.i64_t, "f2i_left")
                                    .map_err(|_| {
                                        Diagnostic::error("float to int conversion failed")
                                    })?
                            } else if let BasicValueEnum::IntValue(iv) = current_val {
                                if iv.get_type().get_bit_width() < 64 {
                                    self.builder
                                        .build_int_s_extend(iv, self.i64_t, "extend_left")
                                        .map_err(|_| Diagnostic::error("int extend failed"))?
                                } else {
                                    iv
                                }
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unsigned right shift requires numeric operand",
                                    assign.span.start as usize,
                                ));
                            };

                            let right_i64 = if let Some(fv) = self.coerce_to_f64(rhs_val) {
                                self.builder
                                    .build_float_to_signed_int(fv, self.i64_t, "f2i_right")
                                    .map_err(|_| {
                                        Diagnostic::error("float to int conversion failed")
                                    })?
                            } else if let BasicValueEnum::IntValue(iv) = rhs_val {
                                if iv.get_type().get_bit_width() < 64 {
                                    self.builder
                                        .build_int_s_extend(iv, self.i64_t, "extend_right")
                                        .map_err(|_| Diagnostic::error("int extend failed"))?
                                } else {
                                    iv
                                }
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unsigned right shift requires numeric operand",
                                    assign.span.start as usize,
                                ));
                            };

                            // Perform unsigned (logical) right shift: is_arithmetic=false means logical shift
                            let shifted = self
                                .builder
                                .build_right_shift(left_i64, right_i64, false, "ushift_right")
                                .map_err(|_| Diagnostic::error("unsigned right shift failed"))?;

                            // Convert result back to f64 for consistency with other numeric operations
                            self.builder
                                .build_signed_int_to_float(shifted, self.f64_t, "i2f")
                                .map_err(|_| Diagnostic::error("int to float conversion failed"))?
                                .as_basic_value_enum()
                        } else {
                            // Convert compound assignment operator to binary operator
                            use oxc_ast::ast::BinaryOperator;
                            let bin_op = match assign.operator {
                                AssignmentOperator::Addition => BinaryOperator::Addition,
                                AssignmentOperator::Subtraction => BinaryOperator::Subtraction,
                                AssignmentOperator::Multiplication => {
                                    BinaryOperator::Multiplication
                                }
                                AssignmentOperator::Division => BinaryOperator::Division,
                                AssignmentOperator::Remainder => BinaryOperator::Remainder,
                                AssignmentOperator::ShiftLeft => BinaryOperator::ShiftLeft,
                                AssignmentOperator::ShiftRight => BinaryOperator::ShiftRight,
                                AssignmentOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                                AssignmentOperator::BitwiseOR => BinaryOperator::BitwiseOR,
                                AssignmentOperator::BitwiseXOR => BinaryOperator::BitwiseXOR,
                                AssignmentOperator::Exponential => BinaryOperator::Exponential,
                                AssignmentOperator::LogicalAnd => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "logical AND assignment (&&=) not supported",
                                    ));
                                }
                                AssignmentOperator::LogicalOr => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "logical OR assignment (||=) not supported",
                                    ));
                                }
                                AssignmentOperator::LogicalNullish => {
                                    return Err(Diagnostic::simple_boxed(
                                        Severity::Error,
                                        "nullish coalescing assignment (??=) not supported",
                                    ));
                                }
                                AssignmentOperator::Assign => unreachable!(),
                                AssignmentOperator::ShiftRightZeroFill => unreachable!(), // Handled above
                            };

                            // Perform binary operation
                            self.lower_binary_expr_with_values(
                                &bin_op,
                                current_val,
                                rhs_val,
                                function,
                                param_map,
                                locals,
                            )?
                        };

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
                            }
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment result type mismatch for number field",
                                assign.span.start as usize,
                            ));
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
                            }
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment result type mismatch for pointer field",
                                assign.span.start as usize,
                            ));
                        }
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "compound assignment not supported for field type".to_string(),
                                assign.span.start as usize,
                            ));
                        }
                    }
                }
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "compound assignment to private name member access not yet supported",
                    assign.span.start as usize,
                ));
            }
        }

        // support simple assignments `ident = expr` where the left side is an identifier
        if let Some(SimpleAssignmentTarget::AssignmentTargetIdentifier(ident)) =
            assign.left.as_simple_assignment_target()
        {
            let name = ident.name.to_string();
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

                                let vk = boxed_call.try_as_basic_value();
                                // ValueKind is a type that wraps Either<BasicValueEnum, CallSiteValue>
                                // We need to extract BasicValueEnum from it
                                // Since ValueKind and Either have the same memory layout, we can use transmute
                                // But first, let's try to see if ValueKind has conversion methods
                                // For now, use a helper function to extract the value
                                match extract_basic_value_from_valuekind(vk) {
                                    Some(bv) => bv,
                                    None => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union box failed",
                                        ));
                                    }
                                }
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                // Box pointer into union
                                let box_ptr_fn = self.get_union_box_ptr();
                                let boxed_call = self
                                    .builder
                                    .build_call(box_ptr_fn, &[pv.into()], "box_union_ptr")
                                    .map_err(|_| Diagnostic::error("union box failed"))?;

                                let vk = boxed_call.try_as_basic_value();
                                // ValueKind is a type that wraps Either<BasicValueEnum, CallSiteValue>
                                // We need to extract BasicValueEnum from it
                                // Since ValueKind and Either have the same memory layout, we can use transmute
                                // But first, let's try to see if ValueKind has conversion methods
                                // For now, use a helper function to extract the value
                                match extract_basic_value_from_valuekind(vk) {
                                    Some(bv) => bv,
                                    None => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "union box failed",
                                        ));
                                    }
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

        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "unsupported assignment target or pattern",
            assign.span.start as usize,
        ))
    }
}
