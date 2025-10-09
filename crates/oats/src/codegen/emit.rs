//! This module contains the logic for emitting top-level items, such as
//! functions and constructors, into LLVM IR.

use deno_ast::swc::ast;
use inkwell::values::BasicValue;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

type LocalsStackLocal<'a> = Vec<
    HashMap<
        String,
        (
            inkwell::values::PointerValue<'a>,
            inkwell::types::BasicTypeEnum<'a>,
            bool,
            bool,
            bool,
            Option<String>,
        ),
    >,
>;

impl<'a> crate::codegen::CodeGen<'a> {
    // Generates LLVM IR for a function declaration.
    // This is the main entry point for function compilation.
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> Result<FunctionValue<'a>, crate::diagnostics::Diagnostic> {
        // 1. Build the LLVM function type.
        let llvm_param_types: Vec<_> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, ret_type);

        // 2. Add the function to the module and set up the entry block.
        self.gen_str_concat(); // Ensure runtime helpers are available.
        let function = self.module.add_function(func_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // 3. Store metadata and create stack allocations for parameters.
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());
        let (param_map, mut locals_stack) =
            self.create_param_allocas(function, func_decl, &llvm_param_types, receiver_name)?;

        // 4. Lower the function body statements into IR.
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.stmts, function, &param_map, &mut locals_stack)?;
        }

        // 5. Add an implicit `return void` if the function hasn't already returned.
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            self.emit_rc_dec_for_locals(&locals_stack);
            self.builder.build_return(None).map_err(|_| {
                crate::diagnostics::Diagnostic::simple("Failed to build implicit return")
            })?;
        }

        Ok(function)
    }

    // Generate a complete constructor function for a class.
    pub fn gen_constructor_ir(
        &self,
        class_name: &str,
        ctor: &deno_ast::swc::ast::Constructor,
        fields: &[(String, crate::types::OatsType)],
    ) -> Result<(), crate::diagnostics::Diagnostic> {
        use crate::types::OatsType;

        let fname = format!("{}_ctor", class_name);

        let mut param_types_vec: Vec<crate::types::OatsType> = Vec::new();
        let mut param_names: Vec<String> = Vec::new();

        for param in &ctor.params {
            use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
            match param {
                ParamOrTsParamProp::TsParamProp(ts_param) => {
                    if let TsParamPropParam::Ident(binding_ident) = &ts_param.param {
                        let pname = binding_ident.id.sym.to_string();
                        let pty = binding_ident
                            .type_ann
                            .as_ref()
                            .and_then(|ann| crate::types::map_ts_type(&ann.type_ann))
                            .unwrap_or(OatsType::Number);
                        param_types_vec.push(pty);
                        param_names.push(pname);
                    }
                }
                ParamOrTsParamProp::Param(p) => {
                    if let deno_ast::swc::ast::Pat::Ident(bind_ident) = &p.pat {
                        let pname = bind_ident.id.sym.to_string();
                        let pty = bind_ident
                            .type_ann
                            .as_ref()
                            .and_then(|ann| crate::types::map_ts_type(&ann.type_ann))
                            .unwrap_or(OatsType::Number);
                        param_types_vec.push(pty);
                        param_names.push(pname);
                    }
                }
            }
        }

        self.fn_param_types
            .borrow_mut()
            .insert(fname.clone(), param_types_vec.clone());

        let mut llvm_param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
        for pty in &param_types_vec {
            let llvm_ty = match pty {
                OatsType::Number => self.f64_t.into(),
                OatsType::String
                | OatsType::NominalStruct(_)
                | OatsType::Array(_)
                | OatsType::Promise(_)
                | OatsType::Weak(_)
                | OatsType::Option(_) => self.i8ptr_t.into(),
                OatsType::Boolean => self.bool_t.into(),
                OatsType::Union(parts) => {
                    let any_ptr = parts.iter().any(|p| {
                        matches!(
                            p,
                            OatsType::String
                                | OatsType::NominalStruct(_)
                                | OatsType::Array(_)
                                | OatsType::Promise(_)
                                | OatsType::Weak(_)
                                | OatsType::Option(_)
                        )
                    });
                    if any_ptr {
                        self.i8ptr_t.into()
                    } else {
                        self.f64_t.into()
                    }
                }
                OatsType::Void => continue,
            };
            llvm_param_types.push(llvm_ty);
        }

        let fn_ty = self.i8ptr_t.fn_type(&llvm_param_types, false);
        let f = self.module.add_function(&fname, fn_ty, None);

        let entry = self.context.append_basic_block(f, "entry");
        self.builder.position_at_end(entry);

        let header_size = 8u64;
        let field_count = fields.len();
        let total_size = header_size + (field_count as u64 * 8);

        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let call_site = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "call_malloc")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("build_call failed"))?;
        let malloc_ret = call_site
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple("malloc call did not return a value")
            })?
            .into_pointer_value();

        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
        // Set header: rc=1 and type_tag=0 (will encode class type tag non-zero if we emit metadata)
        // We'll set type_tag to 2 to indicate "class with field_map" when we emit metadata below.
        // HEADER_TYPE_TAG_SHIFT == 49 in runtime; shift our chosen tag into position
        let type_tag_val = 2u64 << 49;
        let header_val = self.i64_t.const_int(type_tag_val | 1u64, false);
        let _ = self.builder.build_store(header_ptr, header_val);

        let mut locals: LocalsStackLocal = vec![];
        let mut scope = HashMap::new();

        let this_alloca = self
            .builder
            .build_alloca(self.i8ptr_t, "this")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("alloca failed for this"))?;
        let _ = self.builder.build_store(this_alloca, malloc_ret);
        scope.insert(
            "this".to_string(),
            (
                this_alloca,
                self.i8ptr_t.into(),
                true,
                true,
                false,
                Some(class_name.to_string()),
            ),
        );

        let mut param_map: HashMap<String, u32> = HashMap::new();

        for (i, pname) in param_names.iter().enumerate() {
            let param_val = f.get_nth_param(i as u32).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple(format!(
                    "missing parameter {} for constructor {}",
                    pname, class_name
                ))
            })?;
            let param_ty = param_val.get_type();
            let alloca = self
                .builder
                .build_alloca(param_ty, &format!("param_{}", pname))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple(format!(
                        "alloca failed for param {} in constructor {}",
                        pname, class_name
                    ))
                })?;
            let _ = self.builder.build_store(alloca, param_val);
            // Mark parameter as weak if its declared OatsType is Weak(_)
            let is_param_weak = matches!(param_types_vec.get(i), Some(OatsType::Weak(_)));
            // If the declared param type is a NominalStruct, record its name so
            // member lowering can infer fields without fallback heuristics.
            let nominal = match param_types_vec.get(i) {
                Some(crate::types::OatsType::NominalStruct(n)) => Some(n.clone()),
                _ => None,
            };
            scope.insert(
                pname.clone(),
                (alloca, param_ty, true, true, is_param_weak, nominal),
            );
            param_map.insert(pname.clone(), i as u32);
        }

        locals.push(scope);

        for (field_idx, (field_name, _field_type)) in fields.iter().enumerate() {
            if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
                let param_val = f.get_nth_param(param_idx as u32).ok_or_else(|| {
                    crate::diagnostics::Diagnostic::simple(format!(
                        "missing parameter {} for constructor {}",
                        field_name, class_name
                    ))
                })?;
                let field_offset = header_size + (field_idx as u64 * 8);
                let field_ptr_int = self
                    .builder
                    .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
                let offset_const = self.i64_t.const_int(field_offset, false);
                let field_addr = self
                    .builder
                    .build_int_add(field_ptr_int, offset_const, "field_addr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
                let field_ptr_cast = self
                    .builder
                    .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
                // If the field type is a union, we expect to store a boxed union object.
                match _field_type {
                    OatsType::Union(_) => {
                        // If the incoming param is a float, box it; if it's already a pointer, box via union_box_ptr.
                        if param_val.get_type().is_float_type() {
                            let unboxed_f = param_val.into_float_value();
                            let box_fn = self.get_union_box_f64();
                            let cs = self.builder.build_call(
                                box_fn,
                                &[unboxed_f.into()],
                                "union_box_f64_ctor",
                            );
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                let boxed_bv =
                                    inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                let _ = self.builder.build_store(field_ptr_cast, boxed_bv);
                                // For union-boxed pointers we treat the stored pointer as strong by default.
                                // If the declared field type within the union is Weak, more refined handling
                                // will be added later. For now, use strong rc_inc.
                                let rc_inc_fn = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc_fn,
                                    &[boxed_ptr.into()],
                                    "rc_inc_field",
                                );
                            }
                        } else if param_val.get_type().is_pointer_type() {
                            // box the pointer payload
                            let box_fn = self.get_union_box_ptr();
                            let cs = self.builder.build_call(
                                box_fn,
                                &[param_val.into()],
                                "union_box_ptr_ctor",
                            );
                            if let Ok(cs) = cs
                                && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                            {
                                let boxed_ptr = bv.into_pointer_value();
                                let boxed_bv =
                                    inkwell::values::BasicValueEnum::PointerValue(boxed_ptr);
                                let _ = self.builder.build_store(field_ptr_cast, boxed_bv);
                                let rc_inc_fn = self.get_rc_inc();
                                let _ = self.builder.build_call(
                                    rc_inc_fn,
                                    &[boxed_ptr.into()],
                                    "rc_inc_field",
                                );
                            }
                        } else {
                            // other param ABI not expected for unions
                            let _ = self.builder.build_store(field_ptr_cast, param_val);
                        }
                    }
                    _ => {
                        let _ = self.builder.build_store(field_ptr_cast, param_val);

                        if param_val.get_type().is_pointer_type() {
                            // If the declared field type is Weak<T>, use rc_weak_inc; otherwise rc_inc
                            match _field_type {
                                OatsType::Weak(_) => {
                                    let rc_weak_inc = self.get_rc_weak_inc();
                                    let _ = self.builder.build_call(
                                        rc_weak_inc,
                                        &[param_val.into()],
                                        "rc_weak_inc_field",
                                    );
                                }
                                _ => {
                                    let rc_inc_fn = self.get_rc_inc();
                                    let _ = self.builder.build_call(
                                        rc_inc_fn,
                                        &[param_val.into()],
                                        "rc_inc_field",
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }

        // Emit metadata: list of byte offsets for pointer-like fields so the runtime
        // can traverse object pointer fields. This is a simple array of i64 offsets
        // named `<class_name>_field_map` and will be used by the cycle collector.
        {
            let mut ptr_field_offsets: Vec<inkwell::values::BasicValueEnum> = Vec::new();
            for (field_idx, (_field_name, field_type)) in fields.iter().enumerate() {
                // Determine if this field is pointer-like
                let is_ptr = matches!(
                    field_type,
                    crate::types::OatsType::String
                        | crate::types::OatsType::NominalStruct(_)
                        | crate::types::OatsType::Array(_)
                        | crate::types::OatsType::Promise(_)
                        | crate::types::OatsType::Weak(_)
                        | crate::types::OatsType::Option(_)
                        | crate::types::OatsType::Union(_)
                );
                if is_ptr {
                    let offset = header_size + (field_idx as u64 * 8);
                    let const_off = self.i64_t.const_int(offset, false);
                    ptr_field_offsets.push(const_off.as_basic_value_enum());
                }
            }

            // Create global struct constant containing length + offsets if there are any pointer fields
            if !ptr_field_offsets.is_empty() {
                let len = ptr_field_offsets.len();
                let arr_ty = self.i64_t.array_type(len as u32);
                // Convert BasicValueEnum::IntValue items into IntValue vector
                let int_vals: Vec<inkwell::values::IntValue> = ptr_field_offsets
                    .into_iter()
                    .map(|bv| bv.into_int_value())
                    .collect();

                // Build a struct type: { i64 meta0, [N x i32] }
                // meta0 packs magic/version in high 32 bits and len in low 32 bits.
                let arr_i32_ty = self.context.i32_type().array_type(len as u32);
                let struct_ty = self
                    .context
                    .struct_type(&[self.i64_t.into(), arr_i32_ty.into()], false);

                let magic_u64 = 0x4F415453u64; // 'OATS'
                let meta0_val = (magic_u64 << 32) | (len as u64 & 0xffffffffu64);
                let meta0_const = self.i64_t.const_int(meta0_val, false);

                // Convert offsets to i32 constants
                let int32_vals: Vec<inkwell::values::IntValue> = int_vals
                    .into_iter()
                    .map(|iv| {
                        // Try to extract a constant; fall back to zero if not available.
                        let v = iv.get_zero_extended_constant().unwrap_or(0) as u64;
                        self.i32_t.const_int(v & 0xffffffffu64, false)
                    })
                    .collect();
                let array_const = self.i32_t.const_array(&int32_vals);
                let initializer = self
                    .context
                    .const_struct(&[meta0_const.into(), array_const.into()], false);

                let gv_name = format!("{}_field_map", class_name);
                let gv = self.module.add_global(struct_ty, None, &gv_name);
                gv.set_initializer(&initializer);
                gv.set_constant(true);

                // Store pointer to the field_map global into the object's second word
                // Compute i8* pointer to the global and store at offset +8 from object base
                if let Some(gv_global) = self.module.get_global(&gv_name) {
                    let gv_ptr = gv_global.as_pointer_value();
                    // cast global pointer to i8*
                    if let Ok(gv_i8ptr) =
                        self.builder
                            .build_pointer_cast(gv_ptr, self.i8ptr_t, "field_map_i8ptr")
                    {
                        // compute field slot address: ptr_to_int(malloc_ret) + 8 -> int_to_ptr(i8*)
                        let obj_ptr_int = self
                            .builder
                            .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_meta")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("ptr_to_int failed")
                            })?;
                        let off_const = self.i64_t.const_int(8, false);
                        let field_addr = self
                            .builder
                            .build_int_add(obj_ptr_int, off_const, "meta_field_addr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("int_add failed")
                            })?;
                        let field_ptr = self
                            .builder
                            .build_int_to_ptr(field_addr, self.i8ptr_t, "meta_field_ptr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple("int_to_ptr failed")
                            })?;
                        // store pointer value
                        let _ = self
                            .builder
                            .build_store(field_ptr, gv_i8ptr.as_basic_value_enum());
                    }
                }
            }
        }

        if let Some(body) = &ctor.body {
            for stmt in &body.stmts {
                let _ = self.lower_stmt(stmt, f, &param_map, &mut locals);
            }
        }

        let _ = self.builder.build_return(Some(&malloc_ret));
        Ok(())
    }
}
