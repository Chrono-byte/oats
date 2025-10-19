use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use deno_ast::swc::ast;
use deno_ast::swc::ast::MemberProp;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;

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

impl<'a> CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_member_expr(
        &self,
        member: &ast::MemberExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Support both computed member access (obj[expr]) and dot-member (obj.prop)
        match &member.prop {
            MemberProp::Computed(boxed) => {
                eprintln!("DEBUG: Computed member access");
                // lower object and index separately so we can produce clearer diagnostics
                let obj_res = self.lower_expr(&member.obj, function, param_map, locals);
                let idx_res = self.lower_expr(&boxed.expr, function, param_map, locals);
                let obj_val = match obj_res {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to lower member object expression", member.span.lo.0 as usize,
                        ));
                    }
                };
                let idx_val = match idx_res {
                    Ok(v) => v,
                    Err(_) => {
                        return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to lower member index expression", member.span.lo.0 as usize,
                        ));
                    }
                };

                // Only support pointer-array indexing (i8**). Try to
                // infer a nominal class for the object (this may be a
                // tuple shape we registered earlier) so we can lower
                // fixed-field access without calling generic array_get
                // runtime helpers.
                if let BasicValueEnum::PointerValue(arr_ptr) = obj_val {
                    // Try to infer a nominal class name from the object
                    let mut class_name_opt: Option<String> = None;
                    if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                        let ident_name = ident.sym.to_string();
                        // check locals for nominal annotation
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
                    } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_)) {
                        let fname = function.get_name().to_str().unwrap_or("");
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                            && !param_types.is_empty()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    }

                    // If we inferred a nominal type and have its fields registered,
                    // perform field load similar to dot-member lowering.
                    if let Some(class_name) = class_name_opt.clone()
                        && let Some(fields) = self.class_fields.borrow().get(&class_name)
                    {
                        // compute integer index
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
                                return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "unsupported index type for computed member access", member.span.lo.0 as usize,
                                ));
                            }
                        };
                        // If index is a compile-time constant we can use it to lookup the field
                        if let Some(const_idx) = idx_i64.get_zero_extended_constant() {
                            let idx_usize = const_idx as usize;
                            if let Some((field_idx, (_fname, field_ty))) =
                                fields.iter().enumerate().find(|(i, _)| *i == idx_usize)
                            {
                                // compute offset like in dot-member lowering
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
                                let meta_slot = self.i64_t.const_int(8u64, false);
                                let tmp = self
                                    .builder
                                    .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let offset = self
                                    .builder
                                    .build_int_add(tmp, mul, "fld_off")
                                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                let gep_ptr = self
                                    .i8_ptr_from_offset_i64(arr_ptr, offset, "field_i8ptr")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                                match field_ty {
                                    crate::types::OatsType::Number => {
                                        let f64_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "f64_ptr_cast",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::error("LLVM builder error")
                                            })?;
                                        let loaded = self
                                            .builder
                                            .build_load(self.f64_t, f64_ptr, "field_f64_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_) => {
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            )
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        let loaded = self
                                            .builder
                                            .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                    crate::types::OatsType::Union(_) => {
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            )
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        let boxed = self
                                            .builder
                                            .build_load(self.i8ptr_t, slot_ptr, "union_boxed_load")
                                            .map_err(|_| Diagnostic::error("operation failed"))?;
                                        if let BasicValueEnum::PointerValue(boxed_ptr) = boxed {
                                            let unbox_f = self.get_union_unbox_f64();
                                            let cs = self
                                                .builder
                                                .build_call(
                                                    unbox_f,
                                                    &[boxed_ptr.into()],
                                                    "union_unbox_f64_call",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("operation failed")
                                                })?;
                                            if let inkwell::Either::Left(bv) =
                                                cs.try_as_basic_value()
                                            {
                                                return Ok(bv);
                                            }
                                            let unbox_p = self.get_union_unbox_ptr();
                                            let cs2 = self
                                                .builder
                                                .build_call(
                                                    unbox_p,
                                                    &[boxed_ptr.into()],
                                                    "union_unbox_ptr_call",
                                                )
                                                .map_err(|_| {
                                                    Diagnostic::error("operation failed")
                                                })?;
                                            if let inkwell::Either::Left(bv2) =
                                                cs2.try_as_basic_value()
                                            {
                                                return Ok(bv2);
                                            }
                                        }
                                        return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed"));
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(Severity::Error, "unsupported field type for tuple/nominal access",
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    // compute index as i64 for runtime helpers
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
                            return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "unsupported index type for computed member access", member.span.lo.0 as usize,
                            ));
                        }
                    };

                    // If index is numeric, decide whether we should call
                    // `array_get_f64` (for arrays of numbers) or fall back
                    // to `array_get_ptr` (for arrays of pointers). We attempt
                    // to use available type information from locals or
                    // function parameter annotations to make the decision.
                    if matches!(
                        idx_val,
                        BasicValueEnum::IntValue(_) | BasicValueEnum::FloatValue(_)
                    ) {
                        // Try to determine the array's element type from available metadata.
                        let mut prefer_f64 = false;
                        // If the array expression was an identifier, check locals/params
                        if let deno_ast::swc::ast::Expr::Member(_) = &*member.obj {
                            // Member access; we can't easily infer here. Fall through to ptr case.
                        } else if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                            let arr_name = ident.sym.to_string();
                            // Check lexical locals first
                            if let Some((
                                _p,
                                _ty,
                                _init,
                                _is_const,
                                _is_weak,
                                _nominal,
                                oats_type_opt,
                            )) = self.find_local(locals, &arr_name)
                                && let Some(ot) = oats_type_opt
                                && let crate::types::OatsType::Array(inner) = ot
                                && matches!(*inner, crate::types::OatsType::Number)
                            {
                                prefer_f64 = true;
                            }
                            // Check function params if not found in locals
                            if !prefer_f64 && let Some(idx_p) = param_map.get(&arr_name) {
                                let fname = function.get_name().to_str().unwrap_or("");
                                if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
                                    let pidx = *idx_p as usize;
                                    if pidx < param_types.len()
                                        && let crate::types::OatsType::Array(inner) =
                                            &param_types[pidx]
                                        && matches!(**inner, crate::types::OatsType::Number)
                                    {
                                        prefer_f64 = true;
                                    }
                                }
                            }
                        }
                        if prefer_f64 {
                            let array_get = self.get_array_get_f64();
                            let cs = match self.builder.build_call(
                                array_get,
                                &[arr_ptr.into(), idx_i64.into()],
                                "array_get_f64_call",
                            ) {
                                Ok(cs) => cs,
                                Err(_) => {
                                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "operation failed", member.span.lo.0 as usize,
                                    ));
                                }
                            };
                            let either = cs.try_as_basic_value();
                            if let inkwell::Either::Left(bv) = either {
                                return Ok(bv);
                            }
                        }
                    }

                    // Fallback: call runtime helper that returns a pointer and rc_inc's it
                    let array_get_ptr_fn = self.get_array_get_ptr();
                    let cs = match self.builder.build_call(
                        array_get_ptr_fn,
                        &[arr_ptr.into(), idx_i64.into()],
                        "array_get_ptr_call",
                    ) {
                        Ok(cs) => cs,
                        Err(_) => {
                            return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "operation failed", member.span.lo.0 as usize,
                            ));
                        }
                    };
                    let either = cs.try_as_basic_value();
                    if let inkwell::Either::Left(bv) = either {
                        return Ok(bv);
                    }
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "computed member access on non-pointer object", member.span.lo.0 as usize,
                    ));
                }
            }
            MemberProp::Ident(prop_ident) => {
                eprintln!("DEBUG: Entered Ident case");
                eprintln!("DEBUG: Dot member access");
                // dot-member access like obj.prop
                let field_name = prop_ident.sym.to_string();
                eprintln!("DEBUG: field_name created");
                let ident_name = if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                    ident.sym.to_string()
                } else if let deno_ast::swc::ast::Expr::This(_) = &*member.obj {
                    "this".to_string()
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "member access requires identifier or 'this' object", member.span.lo.0 as usize,
                    ));
                };
                // Check if the object is an enum type
                if let Some(OatsType::Enum(_, variants)) =
                    self.symbol_table.borrow().get(&ident_name)
                {
                    // Find the index of the field in the enum variants
                    if let Some(member_index) = variants.iter().position(|v| v == &field_name) {
                        // Return the enum value as a constant integer (0-based index)
                        let enum_value = self.i64_t.const_int(member_index as u64, false);
                        return Ok(enum_value.as_basic_value_enum());
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(Severity::Error, format!("enum '{}' has no member '{}'", ident_name, field_name),
                            member.span.lo.0 as usize,
                        ));
                    }
                }

                // Special-case: `arr.length` for array-like receivers. Load i64 length from array header (+8)
                if field_name == "length" {
                    // Attempt to lower receiver and read length from header for pointer-like values
                    if let Ok(bv) = self.lower_expr(&member.obj, function, param_map, locals)
                        && let BasicValueEnum::PointerValue(pv) = bv
                    {
                        // length is stored as i64 at offset +8 from base
                        let offset = self.i64_t.const_int(8, false);
                        let gep_ptr = match self.i8_ptr_from_offset_i64(pv, offset, "arr_len_i8") {
                            Ok(p) => p,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to compute array length pointer", member.span.lo.0 as usize,
                                ));
                            }
                        };

                        // cast i8* to i64* and load
                        let len_ptr_ty = self.context.ptr_type(AddressSpace::default());
                        let len_ptr = match self
                            .builder
                            .build_pointer_cast(gep_ptr, len_ptr_ty, "len_ptr")
                        {
                            Ok(p) => p,
                            Err(_) => {
                                return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to cast length pointer", member.span.lo.0 as usize,
                                ));
                            }
                        };
                        let loaded =
                            match self.builder.build_load(self.i64_t, len_ptr, "arr_len_load") {
                                Ok(v) => v,
                                Err(_) => {
                                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to load array length", member.span.lo.0 as usize,
                                    ));
                                }
                            };
                        return Ok(loaded.as_basic_value_enum());
                    }
                }
                if let Ok(BasicValueEnum::PointerValue(obj_ptr)) =
                    self.lower_expr(&member.obj, function, param_map, locals)
                {
                    // Determine nominal class name if obj is the `this` parameter
                    // or if function param types carry nominal info.
                    // Check if the object is a param named in param_map
                    // Try to find which param corresponds to this pointer value
                    // If it's `this`, param_map contains "this" -> idx 0 typically.
                    // Use fn_param_types map to get the nominal type for `this`.
                    let mut class_name_opt: Option<String> = None;
                    // Prefer `this` receiver lookup, but also support parameters
                    // that are nominal structs. If member.obj is an Ident and
                    // matches a parameter name, use the declared param type
                    // to infer the nominal class.
                    if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                        let ident_name = ident.sym.to_string();
                        // If ident is `this` and function param types exist
                        if ident_name == "this" {
                            if let Some(param_types) = self
                                .fn_param_types
                                .borrow()
                                .get(function.get_name().to_str().unwrap_or(""))
                                && !param_types.is_empty()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                            {
                                class_name_opt = Some(n.clone());
                            }
                        } else if let Some(param_idx) = param_map.get(&ident_name) {
                            // param_map stores parameter name -> index (u32)
                            if let Some(param_types) = self
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
                            }
                            // If it's not a parameter, check locals for a nominal annotation
                            else if let Some((
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
                    } else if matches!(&*member.obj, deno_ast::swc::ast::Expr::This(_))
                        && let Some(param_types) = self
                            .fn_param_types
                            .borrow()
                            .get(function.get_name().to_str().unwrap_or(""))
                        && !param_types.is_empty()
                        && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                    {
                        class_name_opt = Some(n.clone());
                    } else if let deno_ast::swc::ast::Expr::Member(inner_member) = &*member.obj {
                        // Nested member access: outer.data.value
                        // We need to resolve the type of outer.data to know what fields it has
                        // First, recursively resolve the outer member expression's type
                        class_name_opt =
                            self.resolve_member_type(inner_member, function, param_map, locals);
                    }

                    if let Some(class_name) = class_name_opt {
                        // lookup field list for this class
                        if let Some(fields) = self.class_fields.borrow().get(&class_name) {
                            // find index of field
                            if let Some((field_idx, (_fname, field_ty))) = fields
                                .iter()
                                .enumerate()
                                .find(|(_, (n, _))| n == &field_name)
                            {
                                // field storage layout: after u64 header, fields are stored in pointer-sized slots
                                // compute byte offset = sizeof(u64) + field_idx * sizeof(void*)
                                let hdr_size = self
                                    .i64_t
                                    .const_int(std::mem::size_of::<u64>() as u64, false);
                                let ptr_sz = self
                                    .i64_t
                                    .const_int(std::mem::size_of::<usize>() as u64, false);
                                let idx_const = self.i64_t.const_int(field_idx as u64, false);
                                // offset = hdr_size + idx * ptr_sz
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
                                // We need a i32 index sequence for gep on i8 pointer: cast offset to i64->i32 for index
                                let offset_i64 = offset;
                                let gep_ptr = self
                                    .i8_ptr_from_offset_i64(obj_ptr, offset_i64, "field_i8ptr")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                                // Load based on field type
                                match field_ty {
                                    crate::types::OatsType::Number => {
                                        // Cast to opaque pointer and load as f64
                                        let f64_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                self.context.ptr_type(AddressSpace::default()),
                                                "f64_ptr_cast",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::error("LLVM builder error")
                                            })?;
                                        let loaded = match self.builder.build_load(
                                            self.f64_t,
                                            f64_ptr,
                                            "field_f64_load",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                ));
                                            }
                                        };
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_) => {
                                        // Cast to pointer type and load
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = match self.builder.build_pointer_cast(
                                            gep_ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast",
                                        ) {
                                            Ok(p) => p,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                ));
                                            }
                                        };
                                        // load slot (an i8*)
                                        let loaded = match self.builder.build_load(
                                            self.i8ptr_t,
                                            slot_ptr,
                                            "field_load",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                ));
                                            }
                                        };
                                        // If the object expression had an origin marker,
                                        // check for a synthetic mapping for this object's field
                                        // and, if present, set last_expr_origin_local so
                                        // callers can find a statically-known closure ret type.
                                        if let Some(obj_orig) =
                                            self.last_expr_origin_local.borrow().clone()
                                        {
                                            let field_key = format!("{}.{}", obj_orig, field_name);
                                            if self
                                                .closure_local_rettype
                                                .borrow()
                                                .contains_key(&field_key)
                                            {
                                                self.last_expr_origin_local
                                                    .borrow_mut()
                                                    .replace(field_key);
                                            }
                                        }
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                    crate::types::OatsType::Union(_) => {
                                        // Unions are stored boxed as i8*; load boxed pointer and unbox to requested payload.
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = match self.builder.build_pointer_cast(
                                            gep_ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast",
                                        ) {
                                            Ok(p) => p,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                ));
                                            }
                                        };
                                        let boxed = match self.builder.build_load(
                                            self.i8ptr_t,
                                            slot_ptr,
                                            "union_boxed_load",
                                        ) {
                                            Ok(v) => v,
                                            Err(_) => {
                                                return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                ));
                                            }
                                        };
                                        // Try to unbox as f64 first
                                        if let BasicValueEnum::PointerValue(boxed_ptr) = boxed {
                                            let unbox_f = self.get_union_unbox_f64();
                                            let cs = match self.builder.build_call(
                                                unbox_f,
                                                &[boxed_ptr.into()],
                                                "union_unbox_f64_call",
                                            ) {
                                                Ok(cs) => cs,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                    ));
                                                }
                                            };
                                            let either = cs.try_as_basic_value();
                                            if let inkwell::Either::Left(bv) = either {
                                                return Ok(bv);
                                            }

                                            // Fallback to unbox_ptr
                                            let unbox_p = self.get_union_unbox_ptr();
                                            let cs2 = match self.builder.build_call(
                                                unbox_p,
                                                &[boxed_ptr.into()],
                                                "union_unbox_ptr_call",
                                            ) {
                                                Ok(cs2) => cs2,
                                                Err(_) => {
                                                    return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed",
                                                    ));
                                                }
                                            };
                                            let either2 = cs2.try_as_basic_value();
                                            if let inkwell::Either::Left(bv2) = either2 {
                                                return Ok(bv2);
                                            }
                                        }
                                        return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed"));
                                    }
                                    _ => {
                                        // Unsupported field type
                                        return Err(Diagnostic::simple_boxed(Severity::Error, "expression lowering failed",
                                        ))?;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            MemberProp::PrivateName(_) => {
                // not supported
            }
        }
        {
            // Build helpful debug context about why member lowering failed
            let keys: Vec<String> = self.class_fields.borrow().keys().cloned().collect();
            let mut recv_info = String::from("<unknown>");
            let mut recv_fields: Vec<String> = Vec::new();
            if let deno_ast::swc::ast::Expr::Ident(ident) = &*member.obj {
                let name = ident.sym.to_string();
                if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                    self.find_local(locals, &name)
                {
                    recv_info = format!("ident='{}' local_nominal={:?}", name, nominal);
                    if let Some(n) = nominal
                        && let Some(fields) = self.class_fields.borrow().get(&n)
                    {
                        recv_fields = fields.iter().map(|(n, _)| n.clone()).collect();
                    }
                } else if let Some(idx) = param_map.get(&name) {
                    recv_info = format!("ident='{}' param_idx={}", name, idx);
                    if let Some(param_types) = self
                        .fn_param_types
                        .borrow()
                        .get(function.get_name().to_str().unwrap_or(""))
                    {
                        recv_info.push_str(&format!(" param_types={:?}", param_types));
                    }
                } else {
                    recv_info = format!("ident='{}' not found in locals/params", name);
                }
            }

            // Also include the lowered ABI kind of the receiver expression.
            // If lowering the receiver produced a pointer and the prop is
            // an identifier, attempt a last-resort lookup: scan all
            // registered nominals for one that contains the requested
            // field and perform the field load. This handles cases where
            // earlier inference failed to pick the correct nominal but
            // the field layout is nevertheless registered.
            if let Ok(bv) = self.lower_expr(&member.obj, function, param_map, locals)
                && let BasicValueEnum::PointerValue(obj_ptr) = bv
            {
                // If prop is an identifier, try to find a class that
                // has this field and load it.
                if let deno_ast::swc::ast::MemberProp::Ident(pi) = &member.prop {
                    let target_field = pi.sym.to_string();
                    for (_class_name, fields) in self.class_fields.borrow().iter() {
                        if let Some((field_idx, (_fname, field_ty))) = fields
                            .iter()
                            .enumerate()
                            .find(|(_, (n, _))| n == &target_field)
                        {
                            // compute byte offset = sizeof(u64) + meta_slot + idx * ptr_size
                            let hdr_size = self
                                .i64_t
                                .const_int(std::mem::size_of::<u64>() as u64, false);
                            let ptr_sz = self
                                .i64_t
                                .const_int(std::mem::size_of::<usize>() as u64, false);
                            let idx_const = self.i64_t.const_int(field_idx as u64, false);
                            let mul = match self.builder.build_int_mul(
                                idx_const,
                                ptr_sz,
                                "fld_off_mul",
                            ) {
                                Ok(v) => v,
                                Err(_) => continue,
                            };
                            let meta_slot = self.i64_t.const_int(8u64, false);
                            let tmp = match self.builder.build_int_add(
                                hdr_size,
                                meta_slot,
                                "hdr_plus_meta",
                            ) {
                                Ok(v) => v,
                                Err(_) => continue,
                            };
                            let offset = match self.builder.build_int_add(tmp, mul, "fld_off") {
                                Ok(v) => v,
                                Err(_) => continue,
                            };
                            let gep_ptr =
                                match self.i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr") {
                                    Ok(p) => p,
                                    Err(_) => continue,
                                };
                            match field_ty {
                                crate::types::OatsType::Number => {
                                    let f64_ptr = match self.builder.build_pointer_cast(
                                        gep_ptr,
                                        self.context.ptr_type(AddressSpace::default()),
                                        "f64_ptr_cast",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => continue,
                                    };
                                    if let Ok(loaded) = self.builder.build_load(
                                        self.f64_t,
                                        f64_ptr,
                                        "field_f64_load",
                                    ) {
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                }
                                crate::types::OatsType::String
                                | crate::types::OatsType::NominalStruct(_)
                                | crate::types::OatsType::Array(_) => {
                                    let slot_ptr_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let slot_ptr = match self.builder.build_pointer_cast(
                                        gep_ptr,
                                        slot_ptr_ty,
                                        "slot_ptr_cast",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => continue,
                                    };
                                    if let Ok(loaded) = self.builder.build_load(
                                        self.i8ptr_t,
                                        slot_ptr,
                                        "field_load",
                                    ) {
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                }
                                crate::types::OatsType::Union(_) => {
                                    let slot_ptr_ty =
                                        self.context.ptr_type(AddressSpace::default());
                                    let slot_ptr = match self.builder.build_pointer_cast(
                                        gep_ptr,
                                        slot_ptr_ty,
                                        "slot_ptr_cast",
                                    ) {
                                        Ok(p) => p,
                                        Err(_) => continue,
                                    };
                                    if let Ok(boxed) = self.builder.build_load(
                                        self.i8ptr_t,
                                        slot_ptr,
                                        "union_boxed_load",
                                    ) && let BasicValueEnum::PointerValue(boxed_ptr) = boxed
                                    {
                                        let unbox_f = self.get_union_unbox_f64();
                                        if let Ok(cs) = self.builder.build_call(
                                            unbox_f,
                                            &[boxed_ptr.into()],
                                            "union_unbox_f64_call",
                                        ) && let inkwell::Either::Left(bv) =
                                            cs.try_as_basic_value()
                                        {
                                            return Ok(bv);
                                        }
                                        let unbox_p = self.get_union_unbox_ptr();
                                        if let Ok(cs2) = self.builder.build_call(
                                            unbox_p,
                                            &[boxed_ptr.into()],
                                            "union_unbox_ptr_call",
                                        ) && let inkwell::Either::Left(bv2) =
                                            cs2.try_as_basic_value()
                                        {
                                            return Ok(bv2);
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                }
                // no matching field found; fall through to diagnostic below
            }

            let lowered_recv = match self.lower_expr(&member.obj, function, param_map, locals) {
                Ok(bv) => match bv.get_type() {
                    inkwell::types::BasicTypeEnum::PointerType(_) => "pointer".to_string(),
                    inkwell::types::BasicTypeEnum::FloatType(_) => "float".to_string(),
                    inkwell::types::BasicTypeEnum::IntType(_) => "int".to_string(),
                    _ => "other".to_string(),
                },
                Err(_) => "lower_failed".to_string(),
            };

            let msg = format!(
                "unsupported member access expression; recv={}, lowered_recv={}, recv_fields={:?}, known_nominals={:?}",
                recv_info, lowered_recv, recv_fields, keys
            );
            Err(Diagnostic::simple_with_span_boxed(Severity::Error, msg,
                member.span.lo.0 as usize,
            ))
        }
    }
}
