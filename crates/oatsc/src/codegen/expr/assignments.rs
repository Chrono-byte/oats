use crate::diagnostics::Diagnostic;
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
        assign: &deno_ast::swc::ast::AssignExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use deno_ast::swc::ast::{AssignTarget, SimpleAssignTarget};

        // support simple assignments `ident = expr` where the left side is an identifier
        if let AssignTarget::Simple(SimpleAssignTarget::Ident(bid)) = &assign.left {
            let name = bid.id.sym.to_string();
            if let Some((ptr, _ty, _init, is_const, _extra, _nominal, oats_type)) =
                self.find_local(locals, &name)
            {
                // Disallow assigning to immutable locals at compile-time
                if is_const {
                    // Use span-aware diagnostic: assign.span.lo is a BytePos wrapper
                    let span_start = assign.span.lo.0 as usize;
                    let _ = crate::diagnostics::report_error_span_and_bail::<()>(
                        None,
                        self.source,
                        span_start,
                        "assignment to immutable variable",
                        Some(
                            "This variable was not declared mutable (use `let mut` to make it mutable).",
                        ),
                    );
                    return Err(Diagnostic::simple_boxed("expression lowering failed"))?;
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
                                    .map_err(|_| Diagnostic::simple("union box failed"))?;

                                if let inkwell::Either::Left(bv) = boxed_call.try_as_basic_value() {
                                    bv
                                } else {
                                    return Err(Diagnostic::simple_boxed("union box failed"));
                                }
                            }
                            BasicValueEnum::PointerValue(pv) => {
                                // Box pointer into union
                                let box_ptr_fn = self.get_union_box_ptr();
                                let boxed_call = self
                                    .builder
                                    .build_call(box_ptr_fn, &[pv.into()], "box_union_ptr")
                                    .map_err(|_| Diagnostic::simple("union box failed"))?;

                                if let inkwell::Either::Left(bv) = boxed_call.try_as_basic_value() {
                                    bv
                                } else {
                                    return Err(Diagnostic::simple_boxed("union box failed"));
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
                                        return Err(Diagnostic::simple_boxed("operation failed"));
                                    }
                                };
                        }

                        // Load old value for RC decrement
                        let old = match self.builder.build_load(self.i8ptr_t, ptr, "old_val") {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed("operation failed"));
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
                                    return Err(Diagnostic::simple_boxed("operation failed"));
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
        // Check if left side is a member expression
        if let AssignTarget::Simple(SimpleAssignTarget::Member(member)) = &assign.left {
            // Lower the right-hand side value
            if let Ok(new_val) = self.lower_expr(&assign.right, function, param_map, locals) {
                // Capture RHS origin (if this RHS expression originated from a local)
                let rhs_origin_after_rhs = self.last_expr_origin_local.borrow().clone();
                // Only handle dot-member (obj.prop), not computed (obj[expr])
                use deno_ast::swc::ast::MemberProp;
                if let MemberProp::Ident(prop_ident) = &member.prop {
                    let field_name = prop_ident.sym.to_string();
                    eprintln!("DEBUG: field_name set to '{}'", field_name);
                    eprintln!("DEBUG: About to do enum check");

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
                        if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                            let ident_name = ident.sym.to_string();
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
                        } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_)) {
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
                            return Err(Diagnostic::simple_boxed(format!(
                                "unsupported member assignment: could not infer class for field '{}' in function '{}'",
                                field_name, fname
                            )));
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
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                // Reserve an 8-byte metadata slot after the header so field
                                // offsets are computed as: header_size + meta_slot + idx * ptr_size
                                let meta_slot = self.i64_t.const_int(8u64, false);
                                let tmp = self
                                    .builder
                                    .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                let offset = self
                                    .builder
                                    .build_int_add(tmp, mul, "fld_off")
                                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                                // Cast offset to i64 (we already have it as i64) and compute i8* pointer
                                let field_ptr = self
                                    .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr_store")
                                    .map_err(|_| Diagnostic::simple("operation failed"))?;

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
                                                    Diagnostic::simple("LLVM builder error")
                                                })?;
                                            let _ = self
                                                .builder
                                                .build_store(f64_ptr, fv.as_basic_value_enum());
                                            return Ok(fv.as_basic_value_enum());
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
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
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            match cs.try_as_basic_value() {
                                                inkwell::Either::Left(bv) => bv,
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
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
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            match cs.try_as_basic_value() {
                                                inkwell::Either::Left(bv) => bv,
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
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
                                                        "operation failed",
                                                    ));
                                                }
                                            };
                                            return Ok(new_val);
                                        } else {
                                            return Err(Diagnostic::simple_boxed(
                                                "expected pointer value for reference field",
                                            ));
                                        }
                                    }
                                    _ => {
                                        // Unsupported field type
                                        return Err(Diagnostic::simple_boxed(
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
                        "unsupported member assignment: member object did not lower to a pointer",
                    ));
                }
            } else {
                // member.obj did not lower to a pointer value — emit a generic diagnostic
                return Err(Diagnostic::simple_boxed(
                    "unsupported member assignment: member object did not lower to a pointer",
                ));
            }
        }

        // Handle array element assignment: arr[idx] = value
        // Check if left side is a subscript expression
        if let AssignTarget::Simple(SimpleAssignTarget::SuperProp(_)) = &assign.left {
            // SuperProp is not supported yet
        } else if let AssignTarget::Simple(SimpleAssignTarget::Member(member)) = &assign.left {
            use deno_ast::swc::ast::MemberProp;
            if let MemberProp::Computed(computed) = &member.prop {
                // This is arr[idx] = value

                // Get the alloca pointer for the array variable (needed for reallocation)
                let arr_alloca_opt = if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                    let name = ident.sym.to_string();
                    self.find_local(locals, &name)
                        .map(|(ptr, _, _, _, _, _, _)| ptr)
                } else {
                    None
                };

                let arr_alloca = arr_alloca_opt.ok_or_else(|| {
                    Diagnostic::simple_with_span(
                        "array element assignment requires array stored in a variable",
                        assign.span.lo.0 as usize,
                    )
                })?;

                // Lower the index expression
                let idx_val = self.lower_expr(&computed.expr, function, param_map, locals)?;
                let idx_i64 = if let BasicValueEnum::FloatValue(fv) = idx_val {
                    // Convert f64 to i64
                    match self
                        .builder
                        .build_float_to_signed_int(fv, self.i64_t, "idx_i64")
                    {
                        Ok(v) => v,
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed("failed to convert index to i64"));
                        }
                    }
                } else if let BasicValueEnum::IntValue(iv) = idx_val {
                    // Extend to i64 if needed
                    if iv.get_type().get_bit_width() < 64 {
                        match self.builder.build_int_s_extend(iv, self.i64_t, "idx_i64") {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    "failed to extend index to i64",
                                ));
                            }
                        }
                    } else {
                        iv
                    }
                } else {
                    return Err(Diagnostic::simple_boxed("array index must be a number"));
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
                            return Err(Diagnostic::simple_boxed("failed to call array_set_ptr"));
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
                            return Err(Diagnostic::simple_boxed("failed to call array_set_f64"));
                        }
                    };
                } else {
                    return Err(Diagnostic::simple_boxed(
                        "unsupported array element value type",
                    ));
                }

                return Ok(val);
            }
        }

        Err(Diagnostic::simple_with_span_boxed(
            "unsupported assignment target or pattern",
            assign.span.lo.0 as usize,
        ))
    }
}
