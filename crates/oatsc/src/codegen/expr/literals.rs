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
use oats_ast::*;

/// Lower a literal expression (number, boolean, string, null)
#[allow(clippy::result_large_err)]
pub fn lower_lit<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    lit: &Lit,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    match lit {
        Lit::F64(n) => {
            let fv = codegen.f64_t.const_float(*n);
            Ok(fv.as_basic_value_enum())
        }
        Lit::F32(n) => {
            let fv = codegen.f32_t.const_float(*n as f64);
            Ok(fv.as_basic_value_enum())
        }
        Lit::I64(n) => {
            let iv = codegen.i64_t.const_int(*n as u64, true);
            Ok(iv.as_basic_value_enum())
        }
        Lit::I32(n) => {
            let iv = codegen.i32_t.const_int(*n as u64, true);
            Ok(iv.as_basic_value_enum())
        }
        Lit::I16(n) => {
            let iv = codegen.i16_t.const_int(*n as u64, true);
            Ok(iv.as_basic_value_enum())
        }
        Lit::I8(n) => {
            let iv = codegen.i8_t.const_int(*n as u64, true);
            Ok(iv.as_basic_value_enum())
        }
        Lit::U64(n) => {
            let iv = codegen.i64_t.const_int(*n, false);
            Ok(iv.as_basic_value_enum())
        }
        Lit::U32(n) => {
            let iv = codegen.i32_t.const_int(*n as u64, false);
            Ok(iv.as_basic_value_enum())
        }
        Lit::U16(n) => {
            let iv = codegen.i16_t.const_int(*n as u64, false);
            Ok(iv.as_basic_value_enum())
        }
        Lit::U8(n) => {
            let iv = codegen.i8_t.const_int(*n as u64, false);
            Ok(iv.as_basic_value_enum())
        }
        // Default numeric literals to F64
        Lit::ISize(n) => {
            let fv = codegen.f64_t.const_float(*n as f64);
            Ok(fv.as_basic_value_enum())
        }
        Lit::USize(n) => {
            let fv = codegen.f64_t.const_float(*n as f64);
            Ok(fv.as_basic_value_enum())
        }
        Lit::Null => {
            // Represent `null` as a null i8* pointer so it can be
            // stored into pointer-typed fields (strings, objects, arrays).
            let null_ptr = codegen.i8ptr_t.const_null();
            Ok(null_ptr.as_basic_value_enum())
        }
        Lit::Bool(b) => {
            let iv = codegen.bool_t.const_int(if *b { 1 } else { 0 }, false);
            Ok(iv.as_basic_value_enum())
        }
        Lit::Str(s) => {
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
            let name = format!("strlit.{}", id);
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

            // Return pointer to the data section (offset +16, field index 2)
            // We need [0, 2, 0] to get: struct base -> field 2 (array) -> element 0
            let zero = utils::constants::zero_i32(codegen);
            let two = utils::constants::i32_const(codegen, 2);
            let indices = &[zero, two, zero];
            let gep = unsafe {
                codegen
                    .builder
                    .build_gep(struct_ty, gv.as_pointer_value(), indices, "strptr")
            };
            if let Ok(ptr) = gep {
                // store pointer in cache for future reuse
                codegen.string_literals.borrow_mut().insert(key, ptr);
                return Ok(ptr.as_basic_value_enum());
            }
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "failed to lower string literal",
            ))
        }
        _ => Err(Diagnostic::simple_boxed(
            Severity::Error,
            "operation not supported",
        )),
    }
}

/// Lower an array literal expression
#[allow(clippy::result_large_err)]
pub fn lower_array<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    arr: &oats_ast::ArrayLit,
    function: inkwell::values::FunctionValue<'a>,
    param_map: &std::collections::HashMap<String, u32>,
    locals: &mut crate::codegen::LocalsStackLocal<'a>,
) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
    // Lower array literal: determine element kinds by lowering each elt
    // Pre-allocate with known capacity (some elements might be None, but this is the max)
    let mut lowered_elems: Vec<BasicValueEnum> = Vec::with_capacity(arr.elems.len());
    for opt in &arr.elems {
        if let Some(expr) = opt {
            if let Ok(ev) = codegen.lower_expr(expr, function, param_map, locals) {
                lowered_elems.push(ev);
            } else {
                // unsupported element lowering
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "expression lowering failed",
                    arr.span.start,
                ));
            }
        } else {
            // elided element like [ , ] -> treat as undefined -> unsupported
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "expression lowering failed",
                arr.span.start,
            ));
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
    let either = call_site.try_as_basic_value();
    let arr_ptr = match either {
        inkwell::Either::Left(bv) => bv.into_pointer_value(),
        _ => {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation failed",
            ));
        }
    };

    // compute data pointer: arr_ptr points at header start; data starts after header+len
    let header_bytes = (std::mem::size_of::<u64>() + std::mem::size_of::<u64>()) as u64;

    // Compute a pointer to the array data region (i8*) located after the header+length
    let offset_const = codegen.i32_t.const_int(header_bytes, false);
    let data_ptr_i8_res = unsafe {
        codegen.builder.build_gep(
            codegen.context.i8_type(),
            arr_ptr,
            &[offset_const],
            "arr_data_i8",
        )
    };
    let data_ptr_i8 = if let Ok(p) = data_ptr_i8_res {
        p
    } else {
        return Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "expression lowering failed",
            arr.span.start,
        ));
    };

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

    if all_numbers {
        // For each element, compute byte offset = i * 8, GEP from data_ptr_i8, bitcast to f64* and store
        for (i, v) in lowered_elems.into_iter().enumerate() {
            if let BasicValueEnum::FloatValue(fv) = v {
                let byte_off = (i as u64) * 8u64;
                let off_const = codegen.i32_t.const_int(byte_off, false);
                let elem_i8_res = unsafe {
                    codegen.builder.build_gep(
                        codegen.context.i8_type(),
                        data_ptr_i8,
                        &[off_const],
                        "elem_i8",
                    )
                };
                let elem_i8 = if let Ok(p) = elem_i8_res {
                    p
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "expression lowering failed",
                        arr.span.start,
                    ));
                };
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
            let offset_const = codegen.i32_t.const_int(header_bytes, false);
            let data_i8_res = unsafe {
                codegen.builder.build_gep(
                    codegen.context.i8_type(),
                    cur_arr,
                    &[offset_const],
                    "arr_data_i8_loop",
                )
            };
            let data_ptr_i8 = if let Ok(p) = data_i8_res {
                p
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "expression lowering failed",
                    arr.span.start,
                ));
            };

            let byte_off = (i as u64) * ptr_size;
            let off_const = codegen.i32_t.const_int(byte_off, false);
            let elem_i8_res = unsafe {
                codegen.builder.build_gep(
                    codegen.context.i8_type(),
                    data_ptr_i8,
                    &[off_const],
                    "elem_i8",
                )
            };
            let elem_i8 = if let Ok(p) = elem_i8_res {
                p
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "expression lowering failed",
                    arr.span.start,
                ));
            };
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
                    };
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
                        inkwell::Either::Left(bv) => bv.into_pointer_value(),
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
                    };
                    // The runtime increments the stored pointer. We must
                    // release our temporary ownership returned by
                    // `union_box_f64` to avoid leaking one refcount.
                    utils::rc::rc_dec_value(codegen, boxed_ptr, "rc_dec_boxed_tmp");
                }
                _ => {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "operation failed",
                        arr.span.start,
                    ));
                }
            }
        }
        Ok(arr_ptr.as_basic_value_enum())
    }
}

/// Lower an object literal expression
#[allow(clippy::result_large_err)]
pub fn lower_object<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    obj_lit: &oats_ast::ObjectLit,
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

    for prop in &obj_lit.props {
        match prop {
            PropOrSpread::Prop(Prop::KeyValue(kv)) => {
                // Only identifier keys are supported currently.
                if let PropName::Ident(_ident) = &kv.key {
                    // Lower the value expression
                    let val = codegen.lower_expr(&kv.value, function, param_map, locals)?;
                    field_values.push(val);
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "unsupported object literal key",
                    ));
                }
            }
            PropOrSpread::Prop(Prop::Shorthand(ident)) => {
                // ES6 shorthand property { x } which is equivalent to { x: x }
                let name = ident.sym.clone();
                // First check parameters
                if let Some(idx) = param_map.get(&name) {
                    if let Some(pv) = function.get_nth_param(*idx) {
                        let bv = pv.as_basic_value_enum();
                        field_values.push(bv);
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to find shorthand param",
                        ));
                    }
                } else if let Some((ptr, ty, _init, _is_const, _extra, _nominal, _oats_type)) =
                    codegen.find_local(locals, &name)
                {
                    // load local value
                    let loaded =
                        match codegen
                            .builder
                            .build_load(ty, ptr, &format!("shorthand_{}", name))
                        {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to load shorthand local value",
                                ));
                            }
                        };
                    field_values.push(loaded);
                } else {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "shorthand property not found in params or locals",
                    ));
                }
            }
            PropOrSpread::Spread(spread) => {
                // Spread properties: { ...obj } copies all enumerable properties from obj
                // For statically known class types, we can copy all fields at compile time

                // Lower the spread expression to get the source object
                let spread_obj_val =
                    codegen.lower_expr(&spread.expr, function, param_map, locals)?;
                let spread_obj_ptr = if let BasicValueEnum::PointerValue(pv) = spread_obj_val {
                    pv
                } else {
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "spread operator requires an object (pointer type)",
                        spread.span.start,
                    ));
                };

                // Try to infer the class name from the spread expression
                // All types must be known at compile time - if we can't infer the type, it's an error
                let mut class_name_opt: Option<String> = None;
                if let Expr::Ident(ident) = &spread.expr {
                    let ident_name = ident.sym.clone();
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
                            .get(function.get_name().to_str().unwrap_or(""))
                    {
                        let idx = *param_idx as usize;
                        if idx < param_types.len()
                            && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                        {
                            class_name_opt = Some(n.clone());
                        }
                    }
                } else if let Expr::New(new_expr) = &spread.expr
                    && let Expr::Ident(ident) = &*new_expr.callee
                {
                    // new ClassName() - get class name from callee
                    class_name_opt = Some(ident.sym.clone());
                } else if let Expr::Member(member) = &spread.expr
                    && let Expr::Ident(ident) = &*member.obj
                {
                    // obj.field - try to infer from obj
                    let ident_name = ident.sym.clone();
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
                                            "unsupported field type in spread for field '{}'",
                                            field_name
                                        ),
                                        spread.span.start,
                                    ));
                                }
                            };
                            field_values.push(field_val);
                        }
                    } else {
                        return Err(Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            format!("class '{}' has no registered fields", class_name),
                            spread.span.start,
                        ));
                    }
                } else {
                    // Cannot determine class type at compile time
                    // All types must be known at compile time - this is a type inference failure
                    return Err(Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "cannot determine object type for spread operator: all types must be known at compile time",
                        spread.span.start,
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
    let malloc_ret = call_site
        .try_as_basic_value()
        .left()
        .ok_or_else(|| Diagnostic::error("malloc did not return value"))?
        .into_pointer_value();

    // Initialize header (refcount=1, no flags)
    let header_ptr = codegen
        .builder
        .build_pointer_cast(malloc_ret, codegen.i8ptr_t, "hdr_ptr")
        .map_err(|_| Diagnostic::error("pointer cast failed"))?;
    let header_val = codegen.i64_t.const_int(1u64, false);
    let _ = codegen.builder.build_store(header_ptr, header_val);

    // Store fields sequentially (fields start after header+meta_slot)
    for (idx, fv) in field_values.into_iter().enumerate() {
        let offset = header_size + meta_slot + (idx as u64 * 8);
        let obj_addr = codegen
            .builder
            .build_ptr_to_int(malloc_ret, codegen.i64_t, "obj_addr")
            .map_err(|_| Diagnostic::error("ptr_to_int failed"))?;
        let offset_const = codegen.i64_t.const_int(offset, false);
        let field_addr = codegen
            .builder
            .build_int_add(obj_addr, offset_const, "field_addr")
            .map_err(|_| Diagnostic::error("int_add failed"))?;
        let field_ptr = codegen
            .builder
            .build_int_to_ptr(field_addr, codegen.i8ptr_t, "field_ptr")
            .map_err(|_| Diagnostic::error("int_to_ptr failed"))?;

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
    Ok(malloc_ret.as_basic_value_enum())
}

/// Lower a template literal expression
#[allow(clippy::result_large_err)]
pub fn lower_template<'a>(
    codegen: &crate::codegen::CodeGen<'a>,
    tpl: &oats_ast::TplExpr,
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

    // Helper to create a string literal (with header, same as Lit::Str)
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
            let name = format!("strlit.{}", id);
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

            // Return pointer to data section (offset +16, field index 2)
            // We need [0, 2, 0] to get: struct base -> field 2 (array) -> element 0
            let zero = utils::constants::zero_i32(codegen);
            let two = utils::constants::i32_const(codegen, 2);
            let indices = &[zero, two, zero];
            let gep = unsafe {
                codegen
                    .builder
                    .build_gep(struct_ty, gv.as_pointer_value(), indices, "strptr")
            };

            if let Ok(ptr) = gep {
                codegen.string_literals.borrow_mut().insert(key, ptr);
                Ok(ptr)
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to create string literal",
                ))
            }
        };

    // Track the running result pointer and whether it is a temporary
    // (i.e., freshly-allocated and owned by this expression). We
    // avoid rc_dec'ing values that originate from locals or static
    // literals.
    let mut result: Option<inkwell::values::PointerValue<'a>> = None;
    let mut result_is_tmp: bool = false;

    // Template literals have quasis (string parts) and exprs (interpolated expressions)
    // quasis.len() = exprs.len() + 1 (there's always one more quasi)
    for (i, quasi) in tpl.quasis.iter().enumerate() {
        // Add the string part
        let quasi_str = quasi.raw.to_string();
        let quasi_ptr = create_string_literal(codegen, &quasi_str)?;

        // Concatenate with result so far. If the current `result`
        // is a temporary (previous concat produced a fresh heap
        // string or we earlier created a number->string temp), we
        // will be responsible for rc_dec'ing it once it's consumed.
        result = if let Some(prev) = result {
            let call_site = codegen
                .builder
                .build_call(
                    concat_fn,
                    &[prev.into(), quasi_ptr.into()],
                    "tpl_concat_quasi",
                )
                .map_err(|_| Diagnostic::error("failed to build call"))?;
            Some(
                call_site
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| Diagnostic::error("concat call returned no value"))?
                    .into_pointer_value(),
            )
        } else {
            // Using the quasi literal as the initial result. Quasi
            // literals are static (interned globals) so mark as not tmp.
            result_is_tmp = false;
            Some(quasi_ptr)
        };

        // If there's a corresponding expression, evaluate it and convert to string
        if i < tpl.exprs.len() {
            // Lower the interpolated expression and capture its origin
            // We'll also consult `last_expr_origin_local` which other
            // paths set when the lowered expr was an identifier/param.
            // Lowering interpolated expression
            let expr_val = codegen.lower_expr(&tpl.exprs[i], function, param_map, locals)?;
            let mut expr_str_is_num_tmp = false;
            // Determine whether the resulting expr_str is a temporary
            // heap string that we must rc_dec after use.
            // If the last lowered expr was a local name, it's not a tmp.
            let last_origin_local = codegen.last_expr_origin_local.borrow().clone();

            // Convert to string based on type
            let expr_str = if expr_val.is_float_value() {
                // Number: use number_to_string
                let num_val = expr_val.into_float_value();
                let num_to_str_fn = codegen.get_number_to_string();
                let call_site = codegen
                    .builder
                    .build_call(num_to_str_fn, &[num_val.into()], "num_to_str")
                    .map_err(|_| {
                        Diagnostic::simple_with_span(
                            Severity::Error,
                            "failed to build call",
                            tpl.span.start,
                        )
                    })?;
                let tmp_ptr = call_site
                    .try_as_basic_value()
                    .left()
                    .ok_or_else(|| {
                        Diagnostic::simple_with_span(
                            Severity::Error,
                            "num_to_str returned no value",
                            tpl.span.start,
                        )
                    })?
                    .into_pointer_value();
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
                            tpl.span.start,
                        )
                    })?
                    .into_pointer_value()
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unsupported value type in template literal",
                    tpl.span.start,
                ));
            };

            // Concatenate expression string with result
            let left = result.ok_or_else(|| {
                Diagnostic::simple_with_span(
                    Severity::Error,
                    "concat left operand missing",
                    tpl.span.start,
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
                    Diagnostic::simple_with_span(
                        Severity::Error,
                        "failed to build call",
                        tpl.span.start,
                    )
                })?;
            let new_res = call_site
                .try_as_basic_value()
                .left()
                .ok_or_else(|| {
                    Diagnostic::simple_with_span(
                        Severity::Error,
                        "concat call returned no value",
                        tpl.span.start,
                    )
                })?
                .into_pointer_value();
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

    Ok(result
        .ok_or_else(|| Diagnostic::error("empty template literal"))?
        .as_basic_value_enum())
}
