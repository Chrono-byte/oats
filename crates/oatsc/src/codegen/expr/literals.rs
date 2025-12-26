//! Literal expression lowering
//!
//! This module handles lowering of literal expressions including:
//! - Basic literals (numbers, booleans, strings, null)
//! - Array literals
//! - Object literals
//! - Template literals

use crate::diagnostics::{Diagnostic, Severity};
use inkwell::AddressSpace;
use inkwell::values::BasicValue;
use inkwell::values::BasicValueEnum;

use crate::codegen::utils;
use oxc_ast::ast::Expression;

/// Lower a literal expression (number, boolean, string, null)
pub fn lower_lit<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    expr: &oxc_ast::ast::Expression<'_>,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    match expr {
        Expression::NumericLiteral(lit) => {
            // oxc NumberLiteral stores the value as a string or f64
            // We need to parse it or use the raw value
            let value = lit.value;
            let fv = codegen.f64_t.const_float(value);
            Ok(fv.as_basic_value_enum())
        }
        Expression::StringLiteral(lit) => {
            let s = lit.value.to_string();
            let bytes = s.as_bytes();
            let key = s.clone();
            // Check cache first (cache stores the computed pointer)
            if let Some(ptr_val) = codegen.string_literals.borrow().get(&key) {
                return Ok(ptr_val.as_basic_value_enum());
            }

            // String literal layout with header:
            // [u64 header][u64 length][N x i8 data]
            // Header has static bit set (bit 32)
            let str_len = bytes.len();

            // Create a struct type: { i64, i64, [N x i8] }
            let header_ty = codegen.i64_t;
            let len_ty = codegen.i64_t;
            let data_ty = codegen.context.i8_type().array_type((str_len + 1) as u32);
            let struct_ty = codegen
                .context
                .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

            // generate a unique global name for this string literal
            let id = codegen.next_str_id.get();
            let name = format!("strlit.{id}");
            codegen
                .next_str_id
                .set(codegen.next_str_id.get().wrapping_add(1));
            let gv = codegen.module.add_global(struct_ty, None, &name);

            // Initialize: header = (1 << 32) [static bit], length = str_len, data = bytes
            let static_header = codegen.i64_t.const_int(1u64 << 32, false);
            let length_val = codegen.i64_t.const_int(str_len as u64, false);
            let data_val = codegen.context.const_string(bytes, true);

            let initializer = codegen.context.const_struct(
                &[static_header.into(), length_val.into(), data_val.into()],
                false,
            );
            gv.set_initializer(&initializer);

            // Return pointer to the data section (offset +16 bytes)
            let gv_i8 = codegen
                .builder
                .build_pointer_cast(gv.as_pointer_value(), codegen.i8ptr_t, "str_data_base")
                .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "pointer cast failed"))?;
            if let Ok(ptr) = codegen.offset_i8_ptr(gv_i8, 16, "strptr") {
                // store pointer in cache for future reuse
                codegen.string_literals.borrow_mut().insert(key, ptr);
                return Ok(ptr.as_basic_value_enum());
            }
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "failed to lower string literal",
            ))
        }
        Expression::BooleanLiteral(lit) => {
            let b = lit.value;
            let iv = codegen.bool_t.const_int(u64::from(b), false);
            Ok(iv.as_basic_value_enum())
        }
        Expression::NullLiteral(_) => {
            // Represent `null` as a null i8* pointer so it can be
            // stored into pointer-typed fields (strings, objects, arrays).
            let null_ptr = codegen.i8ptr_t.const_null();
            Ok(null_ptr.as_basic_value_enum())
        }
        _ => Err(Diagnostic::simple_boxed(
            Severity::Error,
            format!("Unsupported literal type: {expr:?}"),
        )),
    }
}

/// Lower an array literal expression
pub fn lower_array<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    arr: &oxc_ast::ast::ArrayExpression<'_>,
    function: inkwell::values::FunctionValue<'a>,
    param_map: &std::collections::HashMap<String, u32>,
    locals: &mut crate::codegen::LocalsStackLocal<'a>,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    // Lower array literal: determine element kinds by lowering each elt
    // Pre-allocate with known capacity (some elements might be None, but this is the max)
    let mut lowered_elems: Vec<BasicValueEnum> = Vec::with_capacity(arr.elements.len());
    for elem in &arr.elements {
        use oxc_ast::ast::ArrayExpressionElement;
        if let Some(expr) = elem.as_expression() {
            lowered_elems.push(codegen.lower_expr(expr, function, param_map, locals)?);
        } else {
            match elem {
                ArrayExpressionElement::SpreadElement(_) => {
                    // Spread elements not yet supported
                    return Err(Box::new(Diagnostic::simple(
                        Severity::Error,
                        "spread elements not supported",
                    )));
                }
                ArrayExpressionElement::Elision(_) => {
                    // elided element like [ , ] -> treat as undefined -> unsupported
                    return Err(Box::new(Diagnostic::simple(
                        Severity::Error,
                        "elided array elements not supported",
                    )));
                }
                _ => {
                    // This should not happen if as_expression() is implemented correctly
                    return Err(Box::new(Diagnostic::simple(
                        Severity::Error,
                        "unsupported array element type",
                    )));
                }
            }
        }
    }

    let len = lowered_elems.len() as u64;
    // decide numeric array if every elem is FloatValue
    let all_numbers = lowered_elems
        .iter()
        .all(|v| matches!(v, BasicValueEnum::FloatValue(_)));

    // call runtime array_alloc(i64 len, i32 elem_size, i32 elem_is_number)
    let array_alloc_fn = codegen.get_array_alloc();
    let len_const = codegen.i64_t.const_int(len, false);
    let (elem_size_const, is_number_const) = if all_numbers {
        (
            codegen.i32_t.const_int(8, false),
            codegen.i32_t.const_int(1, false),
        )
    } else {
        // pointer-sized elements
        let ptr_size = 8u64;
        (
            codegen.i32_t.const_int(ptr_size, false),
            utils::constants::zero_i32(codegen),
        )
    };

    let call_site = match codegen.builder.build_call(
        array_alloc_fn,
        &[
            len_const.into(),
            elem_size_const.into(),
            is_number_const.into(),
        ],
        "array_alloc_call",
    ) {
        Ok(cs) => cs,
        Err(_) => {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation failed",
            ));
        }
    };
    let arr_ptr = match call_site.try_as_basic_value() {
        inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
        _ => {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation failed",
            ));
        }
    };

    // compute data pointer: arr_ptr points at header start; data starts after ARRAY_HEADER_SIZE
    // ARRAY_HEADER_SIZE = 3 * sizeof(u64) = 24 bytes (header + length + capacity)
    let header_bytes = (std::mem::size_of::<u64>() * 3) as u64;

    // Compute a pointer to the array data region (i8*) located after the header+length
    let data_ptr_i8 = codegen
        .offset_i8_ptr(arr_ptr, header_bytes, "arr_data_i8")
        .map_err(|_| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "expression lowering failed",
                arr.span.start as usize,
            )
        })?;

    if all_numbers {
        // For each element, compute byte offset = i * 8, GEP from data_ptr_i8, bitcast to f64* and store
        for (i, v) in lowered_elems.into_iter().enumerate() {
            if let BasicValueEnum::FloatValue(fv) = v {
                let byte_off = (i as u64) * 8u64;
                let elem_i8 = codegen
                    .offset_i8_ptr(data_ptr_i8, byte_off, "elem_i8")
                    .map_err(|_| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "expression lowering failed",
                            arr.span.start as usize,
                        )
                    })?;
                // bitcast to f64* (unwrap Result returned by pointer cast)
                let elem_ptr = match codegen.builder.build_pointer_cast(
                    elem_i8,
                    codegen.context.ptr_type(AddressSpace::default()),
                    "elem_f64_ptr",
                ) {
                    Ok(p) => p,
                    Err(_) => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "operation failed",
                        ));
                    }
                };
                let _ = codegen.builder.build_store(elem_ptr, fv);
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "expression lowering failed",
                ))?;
            }
        }
        Ok(arr_ptr.as_basic_value_enum())
    } else {
        // Create a temporary alloca holding the array pointer. We must
        // pass a pointer-to-pointer (i8**) to `array_set_ptr` so the
        // runtime can update the array pointer if it reallocates. After
        // element initialization we will load the (possibly updated)
        // pointer and return it.
        let arr_ptr_tmp = match codegen.builder.build_alloca(codegen.i8ptr_t, "arr_ptr_tmp") {
            Ok(a) => a,
            Err(_) => return Err(Diagnostic::simple_boxed(Severity::Error, "alloca failed")),
        };
        let _ = codegen.builder.build_store(arr_ptr_tmp, arr_ptr);
        let array_set_ptr_fn = codegen.get_array_set_ptr();

        // pointer array: elements stored as machine pointers; element byte offset = i * ptr_size
        let ptr_size = 8u64;
        for (i, v) in lowered_elems.into_iter().enumerate() {
            // Load current array pointer (it may have changed due to reallocation)
            let cur_arr_bv = codegen
                .builder
                .build_load(codegen.i8ptr_t, arr_ptr_tmp, "cur_arr")
                .map_err(|_| Diagnostic::error("failed to load arr ptr"))?;
            let cur_arr = match cur_arr_bv {
                BasicValueEnum::PointerValue(p) => p,
                _ => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to load arr ptr",
                    ));
                }
            };

            // Compute data start from current array pointer
            let data_ptr_i8 = codegen
                .offset_i8_ptr(cur_arr, header_bytes, "arr_data_i8_loop")
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "expression lowering failed",
                        arr.span.start as usize,
                    )
                })?;

            let byte_off = (i as u64) * ptr_size;
            let elem_i8 = codegen
                .offset_i8_ptr(data_ptr_i8, byte_off, "elem_i8")
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "expression lowering failed",
                        arr.span.start as usize,
                    )
                })?;
            // bitcast to i8** (pointer-to-pointer)
            let elem_ptr = match codegen.builder.build_pointer_cast(
                elem_i8,
                codegen.context.ptr_type(AddressSpace::default()),
                "elem_ptrptr",
            ) {
                Ok(p) => p,
                Err(_) => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation failed",
                    ));
                }
            };
            match v {
                BasicValueEnum::PointerValue(pv) => {
                    // call runtime array_set_ptr(arr_ptr_tmp, idx, pv)
                    // pass the address-of (alloca) so runtime can update it
                    let idx_const = codegen.i64_t.const_int(i as u64, false);
                    match codegen.builder.build_call(
                        array_set_ptr_fn,
                        &[arr_ptr_tmp.into(), idx_const.into(), pv.into()],
                        "array_set_ptr_call",
                    ) {
                        Ok(_cs) => (),
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    }
                }
                BasicValueEnum::IntValue(iv) => {
                    // store integer as-is into pointer slot (coerce as i8*)
                    let _ = codegen.builder.build_store(elem_ptr, iv);
                }
                BasicValueEnum::FloatValue(fv) => {
                    // Box numeric payload into a union object and store pointer via array_set_ptr
                    let box_fn = codegen.get_union_box_f64();
                    let cs =
                        match codegen
                            .builder
                            .build_call(box_fn, &[fv.into()], "union_box_f64_call")
                        {
                            Ok(cs) => cs,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "operation failed",
                                ));
                            }
                        };
                    let boxed_ptr = match cs.try_as_basic_value() {
                        inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
                        _ => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    };
                    let idx_const = codegen.i64_t.const_int(i as u64, false);
                    match codegen.builder.build_call(
                        array_set_ptr_fn,
                        &[arr_ptr_tmp.into(), idx_const.into(), boxed_ptr.into()],
                        "array_set_ptr_call",
                    ) {
                        Ok(_cs) => (),
                        Err(_) => {
                            return Err(Diagnostic::simple_boxed(
                                Severity::Error,
                                "operation failed",
                            ));
                        }
                    }
                    // The runtime increments the stored pointer. We must
                    // release our temporary ownership returned by
                    // `union_box_f64` to avoid leaking one refcount.
                    utils::rc::rc_dec_value(codegen, boxed_ptr, "rc_dec_boxed_tmp");
                }
                _ => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "operation failed",
                        arr.span.start as usize,
                    ));
                }
            }
        }
        codegen.returned_ownership.set(true);
        Ok(arr_ptr.as_basic_value_enum())
    }
}

/// Lower an object literal expression
pub fn lower_object<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    obj_lit: &oxc_ast::ast::ObjectExpression<'_>,
    function: inkwell::values::FunctionValue<'a>,
    param_map: &std::collections::HashMap<String, u32>,
    locals: &mut crate::codegen::LocalsStackLocal<'a>,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    // Lower an object literal to a simple heap-allocated struct.
    // We'll allocate header + N fields (8 bytes each) and store
    // each property value in order. Property names are not
    // in the runtime representation (they're positional).

    // Collect lowered values for properties. Support only simple key: expr props.
    let mut field_values: Vec<inkwell::values::BasicValueEnum> = Vec::new();

    for prop in &obj_lit.properties {
        match prop {
            oxc_ast::ast::ObjectPropertyKind::ObjectProperty(prop) => {
                // Support both StaticIdentifier and Identifier keys.
                match &prop.key {
                    oxc_ast::ast::PropertyKey::StaticIdentifier(_)
                    | oxc_ast::ast::PropertyKey::Identifier(_) => {
                        // Lower the value expression
                        let val = codegen.lower_expr(&prop.value, function, param_map, locals)?;
                        field_values.push(val);
                    }
                    _ => {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "unsupported object literal key",
                        ));
                    }
                }
            }
            oxc_ast::ast::ObjectPropertyKind::SpreadProperty(spread) => {
                // Spread properties: { ...obj } copies all enumerable properties from obj
                // For statically known class types, we can copy all fields at compile time

                // Lower the spread expression to get the source object
                let spread_obj_val =
                    codegen.lower_expr(&spread.argument, function, param_map, locals)?;
                let spread_obj_ptr = if let BasicValueEnum::PointerValue(pv) = spread_obj_val {
                    pv
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "spread operator requires an object (pointer type)",
                        spread.span.start as usize,
                    ));
                };

                // Try to infer the class name from the spread expression
                // All types must be known at compile time - if we can't infer the type, it's an error
                let mut class_name_opt: Option<String> = None;
                if let Expression::Identifier(ident) = &spread.argument {
                    let ident_name = ident.name.to_string();
                    // Check locals for nominal annotation
                    if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                        codegen.find_local(locals, &ident_name)
                        && let Some(nom) = nominal
                    {
                        class_name_opt = Some(nom);
                    } else if let Some(param_idx) = param_map.get(&ident_name)
                        && let Some(param_types) = codegen
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
                } else if let Expression::NewExpression(new_expr) = &spread.argument
                    && let Expression::Identifier(ident) = &new_expr.callee
                {
                    // new ClassName() - get class name from callee
                    class_name_opt = Some(ident.name.to_string());
                } else if let Expression::StaticMemberExpression(member) = &spread.argument
                    && let Expression::Identifier(ident) = &member.object
                {
                    // obj.field - try to infer from obj
                    let ident_name = ident.name.to_string();
                    if let Some((_, _ty, _init, _is_const, _is_weak, nominal, _oats_type)) =
                        codegen.find_local(locals, &ident_name)
                        && let Some(nom) = nominal
                    {
                        class_name_opt = Some(nom);
                    }
                }

                // Get field names from class metadata
                if let Some(class_name) = class_name_opt {
                    if let Some(fields) = codegen.class_fields.borrow().get(&class_name) {
                        // Copy each field from the source object
                        for (field_idx, (field_name, field_ty)) in fields.iter().enumerate() {
                            // Compute field offset: header (8) + meta (8) + field_idx * 8
                            let hdr_size = codegen.i64_t.const_int(8u64, false);
                            let meta_slot = codegen.i64_t.const_int(8u64, false);
                            let ptr_sz = codegen.i64_t.const_int(8u64, false);
                            let idx_const = codegen.i64_t.const_int(field_idx as u64, false);
                            let mul = codegen
                                .builder
                                .build_int_mul(idx_const, ptr_sz, "fld_off_mul")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let tmp = codegen
                                .builder
                                .build_int_add(hdr_size, meta_slot, "hdr_plus_meta")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                            let offset = codegen
                                .builder
                                .build_int_add(tmp, mul, "fld_off")
                                .map_err(|_| Diagnostic::error("LLVM builder error"))?;

                            // Get field pointer
                            let field_i8ptr = codegen
                                .i8_ptr_from_offset_i64(
                                    spread_obj_ptr,
                                    offset,
                                    "spread_field_i8ptr",
                                )
                                .map_err(|_| Diagnostic::error("operation failed"))?;

                            // Load field value based on type
                            let field_val = match field_ty {
                                crate::types::OatsType::Number => {
                                    let f64_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_i8ptr,
                                            codegen
                                                .context
                                                .ptr_type(inkwell::AddressSpace::default()),
                                            "f64_ptr_cast",
                                        )
                                        .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                                    let loaded = codegen
                                        .builder
                                        .build_load(codegen.f64_t, f64_ptr, "spread_field_f64_load")
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    loaded.as_basic_value_enum()
                                }
                                crate::types::OatsType::String
                                | crate::types::OatsType::NominalStruct(_)
                                | crate::types::OatsType::Array(_) => {
                                    let slot_ptr_ty =
                                        codegen.context.ptr_type(inkwell::AddressSpace::default());
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_i8ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast",
                                        )
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    let loaded = codegen
                                        .builder
                                        .build_load(codegen.i8ptr_t, slot_ptr, "spread_field_load")
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    // Increment refcount for pointer types
                                    if let BasicValueEnum::PointerValue(pv) = loaded {
                                        let rc_inc = codegen.get_rc_inc();
                                        let _ = codegen.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_spread_field",
                                        );
                                    }
                                    loaded
                                }
                                crate::types::OatsType::Union(_) => {
                                    let slot_ptr_ty =
                                        codegen.context.ptr_type(inkwell::AddressSpace::default());
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_i8ptr,
                                            slot_ptr_ty,
                                            "slot_ptr_cast",
                                        )
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    let boxed = codegen
                                        .builder
                                        .build_load(
                                            codegen.i8ptr_t,
                                            slot_ptr,
                                            "spread_union_boxed_load",
                                        )
                                        .map_err(|_| Diagnostic::error("operation failed"))?;
                                    if let BasicValueEnum::PointerValue(boxed_ptr) = boxed {
                                        // Increment refcount for union boxed values
                                        let rc_inc = codegen.get_rc_inc();
                                        let _ = codegen.builder.build_call(
                                            rc_inc,
                                            &[boxed_ptr.into()],
                                            "rc_inc_spread_union",
                                        );
                                        boxed
                                    } else {
                                        return Err(Diagnostic::simple_boxed(
                                            crate::diagnostics::Severity::Error,
                                            "union field load failed",
                                        ));
                                    }
                                }
                                _ => {
                                    return Err(Diagnostic::simple_with_span_boxed(
                                        Severity::Error,
                                        format!(
                                            "unsupported field type in spread for field '{field_name}'"
                                        ),
                                        spread.span.start as usize,
                                    ));
                                }
                            };
                            field_values.push(field_val);
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            format!("class '{class_name}' has no registered fields"),
                            spread.span.start as usize,
                        ));
                    }
                } else {
                    // Cannot determine class type at compile time
                    // All types must be known at compile time - this is a type inference failure
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "cannot determine object type for spread operator: all types must be known at compile time",
                        spread.span.start as usize,
                    ));
                }
            }
        }
    }

    // Allocate object: layout = [header (8) | meta_slot (8) | fields...]
    // Reserve an 8-byte metadata slot after the header so collector
    // metadata can be stored at offset 8 when appropriate.
    let header_size = 8u64;
    let meta_slot = 8u64;
    let field_count = field_values.len();
    let total_size = header_size + meta_slot + (field_count as u64 * 8);

    let malloc_fn = codegen.get_malloc();
    let size_const = codegen.i64_t.const_int(total_size, false);
    let call_site = codegen
        .builder
        .build_call(malloc_fn, &[size_const.into()], "obj_malloc")
        .map_err(|_| Diagnostic::error("build_call failed"))?;
    let malloc_ret = match call_site.try_as_basic_value() {
        inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
        _ => {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "malloc did not return value",
            ));
        }
    };

    // Initialize header (refcount= refcount=1, no flags)
    // Cast to i8* first for subsequent field GEPs
    let obj_ptr = codegen
        .builder
        .build_pointer_cast(malloc_ret, codegen.i8ptr_t, "obj_ptr")
        .map_err(|_| Diagnostic::error("pointer cast failed"))?;
    // Cast to i64* directly for aligned store
    let header_ptr = codegen
        .builder
        .build_pointer_cast(
            obj_ptr,
            codegen.context.ptr_type(AddressSpace::default()),
            "hdr_ptr",
        )
        .map_err(|_| Diagnostic::error("pointer cast failed"))?;
    let header_val = codegen.i64_t.const_int(1u64, false);
    let store_inst = codegen
        .builder
        .build_store(header_ptr, header_val)
        .map_err(|_| Diagnostic::error("store failed"))?;
    let _ = store_inst.set_alignment(8);

    // Initialize meta_slot (offset 8) to null
    let meta_offset = codegen.i64_t.const_int(8u64, false);
    let meta_ptr_cast = unsafe {
        codegen
            .builder
            .build_gep(codegen.i8_t, obj_ptr, &[meta_offset], "meta_ptr_gep")
            .map_err(|_| Diagnostic::error("GEP failed for meta_slot"))?
    };
    let meta_slot_ptr = codegen
        .builder
        .build_pointer_cast(
            meta_ptr_cast,
            codegen.context.ptr_type(AddressSpace::default()),
            "meta_slot_ptr",
        )
        .map_err(|_| Diagnostic::error("pointer cast failed for meta_slot"))?;
    let null_ptr = codegen.i8ptr_t.const_null();
    let meta_store = codegen
        .builder
        .build_store(meta_slot_ptr, null_ptr)
        .map_err(|_| Diagnostic::error("meta_slot store failed"))?;
    let _ = meta_store.set_alignment(8);

    // Store fields sequentially (fields start after header+meta_slot)
    for (idx, fv) in field_values.into_iter().enumerate() {
        let offset = header_size + meta_slot + (idx as u64 * 8);
        let offset_const = codegen.i64_t.const_int(offset, false);
        // Use GEP instead of ptrtoint/add/inttoptr to preserve pointer provenance
        let field_ptr = unsafe {
            codegen
                .builder
                .build_gep(codegen.i8_t, obj_ptr, &[offset_const], "field_ptr")
                .map_err(|_| Diagnostic::error("GEP failed"))?
        };

        // If it's a float, store as raw f64 in the slot (no boxing).
        if fv.get_type().is_float_type() {
            // Cast slot pointer to f64* and store the float directly
            let f64_slot_ptr = codegen
                .builder
                .build_pointer_cast(
                    field_ptr,
                    codegen.context.ptr_type(AddressSpace::default()),
                    "field_f64_slot",
                )
                .map_err(|_| Diagnostic::error("pointer cast failed"))?;
            // fv is a float value; store it directly
            let _ = codegen.builder.build_store(f64_slot_ptr, fv);
        } else if utils::types::is_pointer_type(&fv) {
            let _ = codegen.builder.build_store(field_ptr, fv);
        } else if utils::types::is_int_type(&fv) {
            // Treat integers as numbers (f64): convert to f64 and store inline
            let intv = fv.into_int_value();
            let fconv = codegen
                .builder
                .build_signed_int_to_float(intv, codegen.f64_t, "i_to_f")
                .map_err(|_| Diagnostic::error("int->float cast failed"))?;
            let f64_slot_ptr = codegen
                .builder
                .build_pointer_cast(
                    field_ptr,
                    codegen.context.ptr_type(AddressSpace::default()),
                    "field_f64_slot",
                )
                .map_err(|_| Diagnostic::error("pointer cast failed"))?;
            let _ = codegen.builder.build_store(f64_slot_ptr, fconv);
        } else {
            // Fallback: attempt to store as is (may fail at runtime)
            let _ = codegen.builder.build_store(field_ptr, fv);
        }
    }

    // Return the base pointer
    codegen.returned_ownership.set(true);
    Ok(malloc_ret.as_basic_value_enum())
}

/// Lower a template literal expression
pub fn lower_template<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    tpl: &oxc_ast::ast::TemplateLiteral<'_>,
    function: inkwell::values::FunctionValue<'a>,
    param_map: &std::collections::HashMap<String, u32>,
    locals: &mut crate::codegen::LocalsStackLocal<'a>,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    // Template literal: `hello ${name}!`
    // Structure: quasis (string parts) and exprs (interpolated expressions)
    // Build result by concatenating: quasis[0] + str(exprs[0]) + quasis[1] + str(exprs[1]) + ...

    // Ensure str_concat is declared
    codegen.gen_str_concat();
    let concat_fn = codegen
        .module
        .get_function("str_concat")
        .ok_or_else(|| Diagnostic::error("str_concat not found"))?;

    // Helper to create a string literal (with header, same as StringLiteral)
    let create_string_literal =
        |codegen: &crate::codegen::CodeGen<'a>,
         s: &str|
         -> crate::diagnostics::DiagnosticResult<inkwell::values::PointerValue<'a>> {
            let bytes = s.as_bytes();
            let key = s.to_string();

            // Check cache first
            if let Some(ptr_val) = codegen.string_literals.borrow().get(&key) {
                return Ok(*ptr_val);
            }

            // String literal layout with header: [u64 header][u64 length][N x i8 data]
            let str_len = bytes.len();
            let header_ty = codegen.i64_t;
            let len_ty = codegen.i64_t;
            let data_ty = codegen.context.i8_type().array_type((str_len + 1) as u32);
            let struct_ty = codegen
                .context
                .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

            let id = codegen.next_str_id.get();
            let name = format!("strlit.{id}");
            codegen.next_str_id.set(id.wrapping_add(1));
            let gv = codegen.module.add_global(struct_ty, None, &name);

            // Initialize with static header
            let static_header = codegen.i64_t.const_int(1u64 << 32, false);
            let length_val = codegen.i64_t.const_int(str_len as u64, false);
            let data_val = codegen.context.const_string(bytes, true);

            let initializer = codegen.context.const_struct(
                &[static_header.into(), length_val.into(), data_val.into()],
                false,
            );
            gv.set_initializer(&initializer);

            // Return pointer to data section (offset +16 bytes)
            let gv_i8 = codegen
                .builder
                .build_pointer_cast(gv.as_pointer_value(), codegen.i8ptr_t, "str_data_base")
                .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "pointer cast failed"))?;
            match codegen.offset_i8_ptr(gv_i8, 16, "strptr") {
                Ok(ptr) => {
                    codegen.string_literals.borrow_mut().insert(key, ptr);
                    Ok(ptr)
                }
                Err(_) => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to create string literal",
                )),
            }
        };

    // Track the running result pointer and whether it is a temporary
    // (i.e., freshly-allocated and owned by this expression). We
    // avoid rc_dec'ing values that originate from locals or static
    // literals.
    let mut result: Option<inkwell::values::PointerValue<'a>> = None;
    let mut result_is_tmp: bool = false;

    // Template literals have quasis (string parts) and expressions (interpolated expressions)
    // quasis.len() = expressions.len() + 1 (there's always one more quasi)
    for (i, quasi) in tpl.quasis.iter().enumerate() {
        // Add the string part
        let quasi_str = match quasi.value.cooked.as_ref() {
            Some(s) => s.to_string(),
            None => String::new(),
        };
        let quasi_string_ptr = create_string_literal(codegen, &quasi_str)?;

        // Concatenate with result so far. If the current `result`
        // is a temporary (previous concat produced a fresh heap
        // string or we earlier created a number->string temp), we
        // will be responsible for rc_dec'ing it once it's consumed.
        result = if let Some(prev) = result {
            let call_site = codegen
                .builder
                .build_call(
                    concat_fn,
                    &[prev.into(), quasi_string_ptr.into()],
                    "tpl_concat_quasi",
                )
                .map_err(|_| Diagnostic::error("failed to build call"))?;
            Some(match call_site.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
                _ => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "concat call returned no value",
                    ));
                }
            })
        } else {
            // Using the quasi literal as the initial result. Quasi
            // literals are static (interned globals) so mark as not tmp.
            result_is_tmp = false;
            Some(quasi_string_ptr)
        };

        // If there's a corresponding expression, evaluate it and convert to string
        if i < tpl.expressions.len() {
            // Lower the interpolated expression and capture its origin
            // We'll also consult `last_expr_origin_local` which other
            // paths set when the lowered expr was an identifier/param.
            // Lowering interpolated expression
            let expr_val = codegen.lower_expr(&tpl.expressions[i], function, param_map, locals)?;
            eprintln!(
                "DEBUG: Template expr[{}] lowered: is_float={}, is_pointer={}, is_int={}",
                i,
                expr_val.is_float_value(),
                expr_val.is_pointer_value(),
                expr_val.is_int_value()
            );
            let mut expr_str_is_num_tmp = false;
            // Determine whether the resulting expr_str is a temporary
            // heap string that we must rc_dec after use.
            // If the last lowered expr was a local name, it's not a tmp.
            let last_origin_local = codegen.last_expr_origin_local.borrow().clone();

            // Convert to string based on type
            let expr_str = if expr_val.is_float_value() {
                eprintln!("DEBUG: Template expr[{}] is float, converting to string", i);
                // Number: use number_to_string
                let num_val = expr_val.into_float_value();
                let num_to_str_fn = codegen.get_number_to_string();
                let call_site = codegen
                    .builder
                    .build_call(num_to_str_fn, &[num_val.into()], "num_to_str")
                    .map_err(|_| {
                        eprintln!("DEBUG: number_to_string call failed");
                        Diagnostic::simple_with_span(
                            Severity::Error,
                            "failed to build call",
                            tpl.span.start as usize,
                        )
                    })?;
                let tmp_ptr = match call_site.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => {
                        eprintln!("DEBUG: number_to_string succeeded, got pointer");
                        bv.into_pointer_value()
                    }
                    _ => {
                        eprintln!("DEBUG: number_to_string returned no value");
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "num_to_str returned no value",
                            tpl.span.start as usize,
                        ));
                    }
                };
                // Number->string returns a fresh heap string; remember
                // that
                // Number->string returns a fresh heap string; remember
                // that so we can rc_dec it. This is always a temporary.
                expr_str_is_num_tmp = true;
                tmp_ptr
            } else if expr_val.is_pointer_value() {
                // Already a string (or object) - use as-is
                // If the lowered expr was an identifier/local, it's not a tmp.
                if last_origin_local.is_some() {
                    // Don't treat as tmp
                    expr_str_is_num_tmp = false;
                }
                expr_val.into_pointer_value()
            } else if expr_val.is_int_value() {
                // Boolean: convert to "true" or "false"
                let bool_val = expr_val.into_int_value();
                let true_str = create_string_literal(codegen, "true")?;
                let false_str = create_string_literal(codegen, "false")?;

                // Use select to pick the right string
                codegen
                    .builder
                    .build_select(bool_val, true_str, false_str, "bool_str")
                    .map_err(|_| {
                        Diagnostic::simple_with_span(
                            Severity::Error,
                            "failed to build select",
                            tpl.span.start as usize,
                        )
                    })?
                    .into_pointer_value()
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unsupported value type in template literal",
                    tpl.span.start as usize,
                ));
            };

            // Concatenate expression string with result
            let left = result.ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "concat left operand missing",
                    tpl.span.start as usize,
                )
            })?;
            // We do not currently maintain origin info for the running
            // `result` across loop iterations; we'll conservatively rc_dec
            // the previous left after concatenation to avoid leaks.
            let call_site = codegen
                .builder
                .build_call(
                    concat_fn,
                    &[left.into(), expr_str.into()],
                    "tpl_concat_expr",
                )
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to build call",
                        tpl.span.start as usize,
                    )
                })?;
            let new_res = match call_site.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
                _ => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "concat call returned no value",
                        tpl.span.start as usize,
                    ));
                }
            };
            // If the expression string was a temporary (number->string)
            // then rc_dec it now that concat consumed it.
            let rc_dec_fn = codegen.get_rc_dec();
            if expr_str_is_num_tmp {
                utils::rc::rc_dec_value(codegen, expr_str, "rc_dec_expr_tmp");
            }

            // The concatenation created a new `new_res`. The previous `left`
            // value is no longer needed. Only rc_dec it if it was a
            // temporary we owned. We track `result_is_tmp` for that.
            if result_is_tmp {
                let _ = codegen
                    .builder
                    .build_call(rc_dec_fn, &[left.into()], "rc_dec_left_tmp")
                    .ok();
            }

            // New result is a freshly-allocated string from concat, so mark tmp=true
            result = Some(new_res);
            result_is_tmp = true;
        }
    }

    codegen.returned_ownership.set(true);
    Ok(result
        .ok_or_else(|| Diagnostic::error("empty template literal"))?
        .as_basic_value_enum())
}
