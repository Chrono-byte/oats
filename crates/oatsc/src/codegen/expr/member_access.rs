use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use inkwell::AddressSpace;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue, ValueKind};
use oxc_ast::ast::*;
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
        member: &oxc_ast::ast::Expression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Support both computed member access (obj[expr]) and dot-member (obj.prop)
        match member {
            Expression::ComputedMemberExpression(computed) => {
                // lower object and index separately so we can produce clearer diagnostics
                let obj_res = self.lower_expr(&computed.object, function, param_map, locals);
                let idx_res = self.lower_expr(&computed.expression, function, param_map, locals);
                let obj_val = obj_res.map_err(|_| {
                    Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to lower member object expression",
                    )
                })?;
                let idx_val = idx_res.map_err(|_| {
                    Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to lower member index expression",
                    )
                })?;

                // Only support pointer-array indexing (i8**). Try to
                // infer a nominal class for the object (this may be a
                // tuple shape we registered earlier) so we can lower
                // fixed-field access without calling generic array_get
                // runtime helpers.
                if let BasicValueEnum::PointerValue(arr_ptr) = obj_val {
                    // Try to infer a nominal class name from the object
                    let mut class_name_opt: Option<String> = None;
                    if let Expression::Identifier(ident) = &computed.object {
                        let ident_name = ident.as_ref().name.to_string();
                        // check locals for nominal annotation
                        if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                            self.find_local(locals, &ident_name)
                            && let Some(nom) = nominal
                        {
                            class_name_opt = Some(nom);
                        } else if let Some(param_idx) = param_map.get(&ident_name) {
                            let fname = function.get_name().to_str().unwrap_or_default();
                            if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
                                let idx = *param_idx as usize;
                                if idx < param_types.len()
                                    && let crate::types::OatsType::NominalStruct(n) =
                                        &param_types[idx]
                                {
                                    class_name_opt = Some(n.clone());
                                }
                            }
                        }
                    } else if matches!(&computed.object, Expression::ThisExpression(_)) {
                        let fname = function.get_name().to_str().unwrap_or_default();
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
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "unsupported index type for computed member access",
                                    computed.span.start as usize,
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
                                            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
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
                                            // Try to unbox as f64 first
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
                                            match cs.try_as_basic_value() {
                                                ValueKind::Basic(bv) => return Ok(bv),
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        }
                                        // Fallback: try unbox_ptr if f64 didn't work
                                        if let BasicValueEnum::PointerValue(boxed_ptr) = boxed {
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
                                            match cs2.try_as_basic_value() {
                                                ValueKind::Basic(bv2) => return Ok(bv2),
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        }
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "operation failed",
                                        ));
                                    }
                                    _ => {
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "unsupported field type for tuple/nominal access",
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
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported index type for computed member access",
                                computed.span.start as usize,
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
                        if matches!(
                            &computed.object,
                            Expression::StaticMemberExpression(_)
                                | Expression::ComputedMemberExpression(_)
                        ) {
                            // Member access; we can't easily infer here. Fall through to ptr case.
                        } else if let Expression::Identifier(ident) = &computed.object {
                            let arr_name = ident.as_ref().name.to_string();
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
                                let fname = function.get_name().to_str().unwrap_or_default();
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
                            let cs = self
                                .builder
                                .build_call(
                                    array_get,
                                    &[arr_ptr.into(), idx_i64.into()],
                                    "array_get_f64_call",
                                )
                                .map_err(|_| {
                                    Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        "operation failed",
                                        computed.span.start as usize,
                                    )
                                })?;
                            match cs.try_as_basic_value() {
                                ValueKind::Basic(bv) => return Ok(bv),
                                _ => {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        "operation failed",
                                        computed.span.start as usize,
                                    ));
                                }
                            }
                        }
                    }

                    // Fallback: call runtime helper that returns a pointer and rc_inc's it
                    let array_get_ptr_fn = self.get_array_get_ptr();
                    let cs = self
                        .builder
                        .build_call(
                            array_get_ptr_fn,
                            &[arr_ptr.into(), idx_i64.into()],
                            "array_get_ptr_call",
                        )
                        .map_err(|_| {
                            Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "operation failed",
                                computed.span.start as usize,
                            )
                        })?;
                    match cs.try_as_basic_value() {
                        ValueKind::Basic(bv) => return Ok(bv),
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "operation failed",
                                computed.span.start as usize,
                            ));
                        }
                    }
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "computed member access on non-pointer object",
                        computed.span.start as usize,
                    ));
                }
            }
            Expression::StaticMemberExpression(static_member) => {
                // dot-member access like obj.prop
                let field_name = static_member.property.name.to_string();
                let ident_name = if let Expression::Identifier(ident) = &static_member.object {
                    ident.as_ref().name.to_string()
                } else if let Expression::ThisExpression(_) = &static_member.object {
                    "this".to_string()
                } else if let Expression::Super(_) = &static_member.object {
                    // Handle super.prop - access parent class field/method
                    return self.lower_super_member_access(
                        member,
                        &field_name,
                        function,
                        param_map,
                        locals,
                    );
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "member access requires identifier, 'this', or 'super' object",
                        static_member.span.start as usize,
                    ));
                };
                // Check if the object is an enum type
                if let Some(OatsType::Enum(_, variants)) =
                    self.symbol_table.borrow().get_type(&ident_name)
                {
                    // Find the index of the field in the enum variants
                    if let Some(member_index) = variants.iter().position(|v| v == &field_name) {
                        // Return the enum value as a constant integer (0-based index)
                        let enum_value = self.i64_t.const_int(member_index as u64, false);
                        return Ok(enum_value.as_basic_value_enum());
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            format!("enum '{}' has no member '{}'", ident_name, field_name),
                            static_member.span.start as usize,
                        ));
                    }
                }

                // Special-case: `arr.length` for array-like receivers. Load i64 length from array header (+8)
                if field_name == "length" {
                    // Attempt to lower receiver and read length from header for pointer-like values
                    if let Ok(bv) =
                        self.lower_expr(&static_member.object, function, param_map, locals)
                        && let BasicValueEnum::PointerValue(pv) = bv
                    {
                        // length is stored as i64 at offset +8 from base
                        let offset = self.i64_t.const_int(8, false);
                        let gep_ptr = self
                            .i8_ptr_from_offset_i64(pv, offset, "arr_len_i8")
                            .map_err(|_| {
                                Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to compute array length pointer",
                                    static_member.span.start as usize,
                                )
                            })?;

                        // cast i8* to i64* and load
                        let len_ptr_ty = self.context.ptr_type(AddressSpace::default());
                        let len_ptr = self
                            .builder
                            .build_pointer_cast(gep_ptr, len_ptr_ty, "len_ptr")
                            .map_err(|_| {
                                Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to cast length pointer",
                                    static_member.span.start as usize,
                                )
                            })?;
                        let loaded = self
                            .builder
                            .build_load(self.i64_t, len_ptr, "arr_len_load")
                            .map_err(|_| {
                                Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "failed to load array length",
                                    static_member.span.start as usize,
                                )
                            })?;
                        return Ok(loaded.as_basic_value_enum());
                    }
                }
                if let Ok(BasicValueEnum::PointerValue(obj_ptr)) =
                    self.lower_expr(&static_member.object, function, param_map, locals)
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
                    if let Expression::Identifier(ident) = &static_member.object {
                        let ident_name = ident.as_ref().name.to_string();
                        // If ident is `this` and function param types exist
                        if ident_name == "this" {
                            let fname = function.get_name().to_str().unwrap_or_default();
                            if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                                && !param_types.is_empty()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                            {
                                class_name_opt = Some(n.clone());
                            }
                        } else if let Some(param_idx) = param_map.get(&ident_name) {
                            // param_map stores parameter name -> index (u32)
                            let fname = function.get_name().to_str().unwrap_or_default();
                            if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
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
                    } else if matches!(&static_member.object, Expression::ThisExpression(_)) {
                        let fname = function.get_name().to_str().unwrap_or_default();
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname)
                            && !param_types.is_empty()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    } else if matches!(
                        &static_member.object,
                        Expression::StaticMemberExpression(_)
                            | Expression::ComputedMemberExpression(_)
                    ) {
                        // Nested member access: outer.data.value
                        // We need to resolve the type of outer.data to know what fields it has
                        // First, recursively resolve the outer member expression's type
                        class_name_opt = self.resolve_member_type(
                            &static_member.object,
                            function,
                            param_map,
                            locals,
                        );
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
                                            .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                        let loaded = self
                                            .builder
                                            .build_load(self.f64_t, f64_ptr, "field_f64_load")
                                            .map_err(|_| {
                                                Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                )
                                            })?;
                                        return Ok(loaded.as_basic_value_enum());
                                    }
                                    crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_) => {
                                        // Cast to pointer type and load
                                        let slot_ptr_ty =
                                            self.context.ptr_type(AddressSpace::default());
                                        let slot_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                )
                                            })?;
                                        // load slot (an i8*)
                                        let loaded = self
                                            .builder
                                            .build_load(self.i8ptr_t, slot_ptr, "field_load")
                                            .map_err(|_| {
                                                Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                )
                                            })?;
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
                                        let slot_ptr = self
                                            .builder
                                            .build_pointer_cast(
                                                gep_ptr,
                                                slot_ptr_ty,
                                                "slot_ptr_cast",
                                            )
                                            .map_err(|_| {
                                                Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                )
                                            })?;
                                        let boxed = self
                                            .builder
                                            .build_load(self.i8ptr_t, slot_ptr, "union_boxed_load")
                                            .map_err(|_| {
                                                Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "operation failed",
                                                )
                                            })?;
                                        // Try to unbox as f64 first
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
                                                    Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    )
                                                })?;
                                            match cs.try_as_basic_value() {
                                                ValueKind::Basic(bv) => return Ok(bv),
                                                _ => {
                                                    return Err(Diagnostic::simple_boxed(
                                                        Severity::Error,
                                                        "operation failed",
                                                    ));
                                                }
                                            }
                                        }
                                        return Err(Diagnostic::simple_boxed(
                                            Severity::Error,
                                            "operation failed",
                                        ));
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
                }
            }
            _ => {
                // Other expression types not handled here
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "unsupported member expression type",
                ));
            }
        }
        // Build helpful debug context about why member lowering failed
        Err(Diagnostic::simple_boxed(
            Severity::Error,
            "member access lowering failed",
        ))
    }

    fn lower_super_member_access(
        &self,
        member: &oxc_ast::ast::Expression<'_>,
        field_name: &str,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Handle super.prop - access parent class field/method
        // Extract object from member expression
        let obj_expr = match member {
            Expression::StaticMemberExpression(m) => &m.object,
            _ => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "super member access requires static member expression",
                ));
            }
        };

        // Verify it's a super expression
        if !matches!(obj_expr, Expression::Super(_)) {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "super member access requires super object",
            ));
        }

        // Find parent class name
        let parent_opt = self.current_class_parent.borrow().clone();
        let parent = parent_opt.ok_or_else(|| {
            Diagnostic::simple_boxed(Severity::Error, "super used but no parent class found")
        })?;

        // Get 'this' pointer
        let (this_ptr, this_ty, _init, _is_const, _extra, _nominal, _oats_type) =
            self.find_local(locals, "this").ok_or_else(|| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    "'this' not found in locals for super member access",
                )
            })?;

        let this_loaded = self
            .builder
            .build_load(this_ty, this_ptr, "this_loaded")
            .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "operation failed"))?;

        // Look up field in parent class
        let fields = self.class_fields.borrow();
        let fields = fields.get(&parent).ok_or_else(|| {
            Diagnostic::simple_boxed(
                Severity::Error,
                format!("parent class '{}' not found in class fields", parent),
            )
        })?;

        let (field_idx, (_fname, field_ty)) = fields
            .iter()
            .enumerate()
            .find(|(_, (n, _))| n == field_name)
            .ok_or_else(|| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("parent class '{}' has no field '{}'", parent, field_name),
                )
            })?;

        // Compute field offset and load
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

        let BasicValueEnum::PointerValue(this_ptr_val) = this_loaded else {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "expected pointer value for super member access",
            ));
        };

        let gep_ptr = self
            .i8_ptr_from_offset_i64(this_ptr_val, offset, "field_i8ptr")
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
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                let loaded = self
                    .builder
                    .build_load(self.f64_t, f64_ptr, "field_f64_load")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                Ok(loaded.as_basic_value_enum())
            }
            crate::types::OatsType::String
            | crate::types::OatsType::NominalStruct(_)
            | crate::types::OatsType::Array(_) => {
                let slot_ptr_ty = self.context.ptr_type(AddressSpace::default());
                let slot_ptr = self
                    .builder
                    .build_pointer_cast(gep_ptr, slot_ptr_ty, "slot_ptr_cast")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                let loaded = self
                    .builder
                    .build_load(self.i8ptr_t, slot_ptr, "field_load")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                Ok(loaded.as_basic_value_enum())
            }
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "unsupported field type for super member access",
            )),
        }
    }

    fn lower_member_assign(
        &self,
        member: &oxc_ast::ast::Expression<'_>,
        value: BasicValueEnum<'a>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        // Handle assignment to member expressions like obj.prop = value
        match member {
            Expression::StaticMemberExpression(static_member) => {
                let field_name = static_member.property.name.to_string();
                let obj_ptr =
                    match self.lower_expr(&static_member.object, function, param_map, locals) {
                        Ok(BasicValueEnum::PointerValue(pv)) => pv,
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "member assignment requires object pointer",
                                static_member.span.start as usize,
                            ));
                        }
                    };

                // Determine nominal class name
                let mut class_name_opt: Option<String> = None;
                if let Expression::Identifier(ident) = &static_member.object {
                    let ident_name = ident.as_ref().name.to_string();
                    if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                        self.find_local(locals, &ident_name)
                        && let Some(nom) = nominal
                    {
                        class_name_opt = Some(nom);
                    } else if let Some(param_idx) = param_map.get(&ident_name) {
                        let fname = function.get_name().to_str().unwrap_or_default();
                        if let Some(param_types) = self.fn_param_types.borrow().get(fname) {
                            let idx = *param_idx as usize;
                            if idx < param_types.len()
                                && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                            {
                                class_name_opt = Some(n.clone());
                            }
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
                } else if matches!(
                    &static_member.object,
                    Expression::StaticMemberExpression(_) | Expression::ComputedMemberExpression(_)
                ) {
                    // Nested member access
                    class_name_opt = self.resolve_member_type(
                        &static_member.object,
                        function,
                        param_map,
                        locals,
                    );
                }

                if let Some(class_name) = class_name_opt
                    && let Some(fields) = self.class_fields.borrow().get(&class_name)
                    && let Some((field_idx, (_fname, field_ty))) = fields
                        .iter()
                        .enumerate()
                        .find(|(_, (n, _))| n == &field_name)
                {
                    // Compute field offset
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
                        .i8_ptr_from_offset_i64(obj_ptr, offset, "field_i8ptr")
                        .map_err(|_| Diagnostic::error("operation failed"))?;

                    // Store based on field type
                    match field_ty {
                        crate::types::OatsType::Number => {
                            let f64_ptr = self
                                .builder
                                .build_pointer_cast(
                                    gep_ptr,
                                    self.context.ptr_type(AddressSpace::default()),
                                    "f64_ptr_cast",
                                )
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            if let BasicValueEnum::FloatValue(fv) = value {
                                self.builder
                                    .build_store(f64_ptr, fv)
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "type mismatch: expected number for field assignment",
                                    static_member.span.start as usize,
                                ));
                            }
                        }
                        crate::types::OatsType::String
                        | crate::types::OatsType::NominalStruct(_)
                        | crate::types::OatsType::Array(_) => {
                            let slot_ptr_ty = self.context.ptr_type(AddressSpace::default());
                            let slot_ptr = self
                                .builder
                                .build_pointer_cast(gep_ptr, slot_ptr_ty, "slot_ptr_cast")
                                .map_err(|_| Diagnostic::error("operation failed"))?;
                            if let BasicValueEnum::PointerValue(pv) = value {
                                // RC protocol: increment new value before storing
                                let rc_inc = self.get_rc_inc();
                                self.builder
                                    .build_call(rc_inc, &[pv.into()], "rc_inc_new")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                                // Load old value and decrement it
                                let old_val = self
                                    .builder
                                    .build_load(self.i8ptr_t, slot_ptr, "old_field_load")
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                                if let BasicValueEnum::PointerValue(old_pv) = old_val {
                                    let rc_dec = self.get_rc_dec();
                                    self.builder
                                        .build_call(rc_dec, &[old_pv.into()], "rc_dec_old")
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                }
                                self.builder
                                    .build_store(slot_ptr, pv)
                                    .map_err(|_| Diagnostic::error("operation failed"))?;
                            } else {
                                return Err(Diagnostic::simple_with_span_boxed(
                                    Severity::Error,
                                    "type mismatch: expected pointer for field assignment",
                                    static_member.span.start as usize,
                                ));
                            }
                        }
                        _ => {
                            return Err(Diagnostic::simple_with_span_boxed(
                                Severity::Error,
                                "unsupported field type for assignment",
                                static_member.span.start as usize,
                            ));
                        }
                    }
                    return Ok(());
                }
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    format!("field '{}' not found or class type unknown", field_name),
                    static_member.span.start as usize,
                ))
            }
            Expression::ComputedMemberExpression(_) => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "assignment to computed member expression not yet supported",
            )),
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "unsupported member expression type for assignment",
            )),
        }
    }
}
