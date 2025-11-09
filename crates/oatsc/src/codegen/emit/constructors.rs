//! Constructor generation for code generation.
//!
//! This module contains code generation logic for class constructors.

use crate::diagnostics::Severity;
use crate::codegen::emit::LocalsStackLocal;
use inkwell::values::BasicValue;
use oats_ast::*;
use std::collections::HashMap;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Generate a complete constructor function for a class.
    ///
    /// The constructor created here follows the runtime object layout used
    /// throughout Oats:
    ///
    /// Layout (byte offsets):
    /// - 0: header (8 bytes) — stores reference count and flags. See the
    ///   runtime for the bit layout constants.
    /// - 8: metadata pointer (8 bytes) — points to a `*_field_map` global
    ///   used by the cycle collector/runtime to locate pointer fields.
    /// - 16+: field slots (each 8 bytes)
    ///
    /// The constructor allocates the object via the runtime `malloc` helper,
    /// sets up the header with an initial RC of 1, stores the `field_map`
    /// pointer at offset 8, zero-initializes field slots, and stores any
    /// constructor parameters into fields, performing union-boxing and
    /// reference-count increments as required.
    ///
    /// # Arguments
    /// * `class_name` - nominal class name used to name the generated ctor
    ///   and the `*_field_map` global.
    /// * `ctor` - AST node describing constructor parameters and body.
    /// * `fields` - list of class fields with their `OatsType`s.
    ///
    /// # Returns
    /// Returns `Ok(())` on success or a `Diagnostic` on error.
    #[allow(clippy::result_large_err)]
    pub fn gen_constructor_ir(
        &self,
        class_name: &str,
        ctor: &ConstructorDecl,
        fields: &[(String, crate::types::OatsType)],
        decorators: Option<Vec<String>>,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        use crate::types::OatsType;

        // Check if this class can form reference cycles
        // For now, check for direct self-reference in fields
        let can_form_cycles = fields.iter().any(|(_, field_type)| {
            matches!(field_type, OatsType::NominalStruct(name) if name == class_name)
        });

        // We'll emit an implementation function and a thin wrapper so that
        // decorators can be applied by replacing/indirecting the wrapper at
        // runtime. The impl name holds the original emitted body; the
        // exported `fname` will be the wrapper that calls decorators and
        // forwards to the final constructor pointer.
        let impl_fname = format!("{}_ctor_impl", class_name);
        let fname = format!("{}_ctor", class_name);
        let init_name = format!("{}_init", class_name);

        let mut param_types_vec: Vec<crate::types::OatsType> = Vec::new();
        let mut param_names: Vec<String> = Vec::new();

        for param in &ctor.params {
            let pname = match &param.pat {
                Pat::Ident(ident) => ident.sym.clone(),
                _ => {
                    // Destructuring parameters not yet supported in constructors
                    format!("_param_{}", param.span.start)
                }
            };
            {
                let pty = param
                    .ty
                    .as_ref()
                    .and_then(crate::types::map_ts_type)
                    .unwrap_or(OatsType::Number);
                param_types_vec.push(pty);
                param_names.push(pname);
            }
        }

        self.fn_param_types
            .borrow_mut()
            .insert(fname.clone(), param_types_vec.clone());

        let mut llvm_param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
        for pty in &param_types_vec {
            let llvm_ty = match pty {
                OatsType::Number | OatsType::F64 => self.f64_t.into(),
                OatsType::F32 => self.f32_t.into(),
                OatsType::I64 | OatsType::U64 | OatsType::Isize | OatsType::Usize => {
                    self.i64_t.into()
                }
                OatsType::I32 | OatsType::U32 => self.i32_t.into(),
                OatsType::I16 | OatsType::U16 => self.i16_t.into(),
                OatsType::I8 | OatsType::U8 | OatsType::Char => self.i8_t.into(),
                OatsType::String
                | OatsType::NominalStruct(_)
                | OatsType::StructLiteral(_)
                | OatsType::Array(_)
                | OatsType::Tuple(_)
                | OatsType::Promise(_)
                | OatsType::Weak(_)
                | OatsType::Unowned(_)
                | OatsType::Option(_)
                | OatsType::Enum(_, _) => self.i8ptr_t.into(),
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
                                | OatsType::Unowned(_)
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
                OatsType::Generic(_) => {
                    // Generics should be resolved during specialization.
                    // This is a programming error - generics should be specialized before reaching here.
                    // Return a fallback type to avoid panicking, but this indicates a bug.
                    self.i8ptr_t.into()
                }
                OatsType::GenericInstance { .. } => {
                    // Generic instances are specialized, so treat as pointer types
                    self.i8ptr_t.into()
                }
            };
            llvm_param_types.push(llvm_ty);
        }

        // We'll emit two functions:
        // - `<Class>_init(this, ...params)` which assumes `this` is already
        //    allocated and stores parameters into fields (used by `super(...)`)
        // - `<Class>_init(...params)` which allocates the object and then
        //    calls `<Class>_init` to perform initialization and finally
        //    returns the allocated object.

        let init_param_types = std::iter::once(self.i8ptr_t.into())
            .chain(llvm_param_types.clone())
            .collect::<Vec<_>>();
        let init_fn_ty = self.i8ptr_t.fn_type(&init_param_types, false);
        let init_f = self.module.add_function(&init_name, init_fn_ty, None);

        // Create the implementation function (contains the actual allocation
        // + init logic). The public `fname` wrapper will be created below.
        let ctor_fn_ty = self.i8ptr_t.fn_type(&llvm_param_types, false);
        let impl_f = self.module.add_function(&impl_fname, ctor_fn_ty, None);

        // Emit init function body first
        let init_entry = self.context.append_basic_block(init_f, "entry");
        self.builder.position_at_end(init_entry);

        // Merge constructor parameter properties (TS shorthand `constructor(public x: T)`) into
        // the effective field list so they become true object fields even if not
        // listed in the declared `fields` slice passed in. This ensures the
        // constructor will allocate storage and initialize those fields.
        let mut combined_fields: Vec<(String, crate::types::OatsType)> = fields.to_vec();
        // Detect parameter properties by comparing the ctor param list. We
        // previously collected `param_names` and `param_types_vec`; reconstruct
        // which params were declared as `TsParamProp` by checking the AST again
        // (ctor.params). For simplicity, if a parameter name matches an
        // existing field we leave it alone; otherwise append it.
        for (i, pname) in param_names.iter().enumerate() {
            // If the pname is already a declared field, skip
            if combined_fields.iter().any(|(n, _)| n == pname) {
                continue;
            }
            // Otherwise, this parameter becomes a field (TS param-property)
            // and we use the resolved parameter type as its field type.
            combined_fields.push((pname.clone(), param_types_vec[i].clone()));
        }

        // Layout: [ header (8) | metadata ptr (8) | field0 (8) | field1 (8) | ... ]
        // Reserve an 8-byte metadata slot immediately after the header so the
        // runtime can store a pointer to the class field_map at offset 8.
        let header_size = 8u64;
        let meta_slot = 8u64; // metadata pointer word after header
        let field_count = combined_fields.len();
        let total_size = header_size + meta_slot + (field_count as u64 * 8);

        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let call_site = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "call_malloc")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "build_call failed")
            })?;
        let malloc_ret = call_site
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc call did not return a value",
                )
            })?
            .into_pointer_value();

        // Set header with cycle bit
        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "pointer cast failed")
            })?;
        let type_tag_val = 2u64 << 49;
        let cycle_bit = if can_form_cycles { 0 } else { 1u64 << 48 };
        let header_val = self.i64_t.const_int(type_tag_val | cycle_bit | 1u64, false);
        let _ = self.builder.build_store(header_ptr, header_val);

        // For the init function, `this` is the first parameter; set up locals and store params
        let mut locals: LocalsStackLocal = vec![];
        let mut scope = HashMap::new();

        let this_param = init_f.get_nth_param(0).ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "missing `this` parameter for init",
            )
        })?;
        let this_alloca = self
            .builder
            .build_alloca(self.i8ptr_t, "this")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "alloca failed for this",
                )
            })?;
        let _ = self.builder.build_store(this_alloca, this_param);
        scope.insert(
            "this".to_string(),
            (
                this_alloca,
                self.i8ptr_t.into(),
                true,
                true,
                false,
                Some(class_name.to_string()),
                None, // oats_type
            ),
        );

        // Zero-initialize all field slots to null so subsequent loads aren't garbage.
        for field_idx in 0..field_count {
            let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
            let obj_ptr_int = self
                .builder
                .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_zero")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "ptr_to_int failed",
                    )
                })?;
            let off_const = self.i64_t.const_int(field_offset, false);
            let field_addr = self
                .builder
                .build_int_add(obj_ptr_int, off_const, "field_addr_zero")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "int_add failed")
                })?;
            let field_ptr_cast = self
                .builder
                .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr_zero")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "int_to_ptr failed",
                    )
                })?;
            let null_ptr = self.i8ptr_t.const_null();
            // We store a null i8* into the field slot. For non-pointer fields
            // (e.g. numbers) the representation in object fields is still an
            // i8* when the field is pointer-like; numeric fields are stored
            // directly in registers during computation and only boxed when
            // necessary (by unions). Zero-init prevents uninitialised reads
            // during early phase of constructor execution.
            let _ = self
                .builder
                .build_store(field_ptr_cast, null_ptr.as_basic_value_enum());
        }

        let mut param_map: HashMap<String, u32> = HashMap::new();

        for (i, pname) in param_names.iter().enumerate() {
            // In init function, parameter indices shift by +1 due to `this`
            let param_val = init_f.get_nth_param((i + 1) as u32).ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("missing parameter {} for constructor {}", pname, class_name),
                )
            })?;
            let param_ty = param_val.get_type();
            let alloca = self
                .builder
                .build_alloca(param_ty, &format!("param_{}", pname))
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        format!(
                            "alloca failed for param {} in constructor {}",
                            pname, class_name
                        ),
                    )
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
                (alloca, param_ty, true, true, is_param_weak, nominal, None),
            );
            param_map.insert(pname.clone(), i as u32);
        }

        locals.push(scope);

        for (field_idx, (field_name, _field_type)) in combined_fields.iter().enumerate() {
            if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
                let param_val = init_f
                    .get_nth_param((param_idx + 1) as u32)
                    .ok_or_else(|| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            format!(
                                "missing parameter {} for constructor {}",
                                field_name, class_name
                            ),
                        )
                    })?;
                let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
                let field_ptr_int = self
                    .builder
                    .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr")
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "ptr_to_int failed",
                        )
                    })?;
                let offset_const = self.i64_t.const_int(field_offset, false);
                let field_addr = self
                    .builder
                    .build_int_add(field_ptr_int, offset_const, "field_addr")
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_add failed",
                        )
                    })?;
                let field_ptr_cast = self
                    .builder
                    .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "int_to_ptr failed",
                        )
                    })?;
                // If the field type is a union, we expect to store a boxed union object.
                match _field_type {
                    OatsType::Union(_) => {
                        // If the incoming param is a float, box it; if it's already a pointer, box via union_box_ptr.
                        // We do a small ABI-aware decision here: the constructor
                        // ABI may pass unions as either a float or pointer. If
                        // the parameter's LLVM type is a float then we must
                        // allocate a union container for the f64 and return a
                        // pointer to it (`union_box_f64`). Otherwise, if the
                        // ABI passed an i8* we call `union_box_ptr` which will
                        // wrap the existing pointer in a union tag. The
                        // runtime's union_box_* helpers return an owned pointer
                        // (with RC=1) which we then store into the object and
                        // immediately `rc_inc` to reflect the object's strong
                        // reference to it.
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
                                // The weak/strong distinction will be added later.
                                // Use the strong `rc_inc` path for current allocations.
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

                                // Use rc_weak_inc for weak fields
                                let rc_fn = match _field_type {
                                    OatsType::Weak(_) => self.get_rc_weak_inc(),
                                    _ => self.get_rc_inc(),
                                };
                                let _ =
                                    self.builder
                                        .build_call(rc_fn, &[boxed_ptr.into()], "rc_field");
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
                                    println!("Invoking rc_weak_inc for field: {:?}", field_name);
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
            for (field_idx, (_field_name, field_type)) in combined_fields.iter().enumerate() {
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
                    let offset = header_size + meta_slot + (field_idx as u64 * 8);
                    let const_off = self.i64_t.const_int(offset, false);
                    ptr_field_offsets.push(const_off.as_basic_value_enum());
                }
            }

            // Create global struct constant containing length + offsets if there are any pointer fields
            if !ptr_field_offsets.is_empty() {
                let len = ptr_field_offsets.len();
                let _arr_ty = self.i64_t.array_type(len as u32);
                // Convert BasicValueEnum::IntValue items into IntValue vector
                let int_vals: Vec<inkwell::values::IntValue> = ptr_field_offsets
                    .into_iter()
                    .map(|bv| bv.into_int_value())
                    .collect();

                // Build a struct type: { i64 meta0, [N x i32] }
                // meta0 packs magic/version in high 32 bits and len in low 32 bits.
                // The array contains i32 offsets (relative to object base) for
                // each pointer-like field. The runtime will use this table to
                // walk object fields during cycle collection and serialization.
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
                        let v = iv.get_zero_extended_constant().unwrap_or(0);
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
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "ptr_to_int failed",
                                )
                            })?;
                        let off_const = self.i64_t.const_int(8, false);
                        let field_addr = self
                            .builder
                            .build_int_add(obj_ptr_int, off_const, "meta_field_addr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "int_add failed",
                                )
                            })?;
                        let field_ptr = self
                            .builder
                            .build_int_to_ptr(field_addr, self.i8ptr_t, "meta_field_ptr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "int_to_ptr failed",
                                )
                            })?;
                        // store pointer value
                        let _ = self
                            .builder
                            .build_store(field_ptr, gv_i8ptr.as_basic_value_enum());
                    }
                }
            }
        }

        // Lower ctor body inside init function context (so `super(...)` can call parent init)
        if let Some(body) = &ctor.body {
            for stmt in &body.stmts {
                self.lower_stmt(stmt, init_f, &param_map, &mut locals)?;
            }
        }

        // init returns void (performing stores into this), but we keep signature as i8* to
        // match callers. Return `this` at end of init.
        let this_loaded = match self
            .builder
            .build_load(self.i8ptr_t, this_alloca, "this_ld")
        {
            Ok(v) => v,
            Err(_) => {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to load this",
                ));
            }
        };
        let _ = self.builder.build_return(Some(&this_loaded));

        // Now emit impl ctor function: allocate object then call init
        let ctor_entry = self.context.append_basic_block(impl_f, "entry");
        self.builder.position_at_end(ctor_entry);
        // allocate object

        let malloc_fn = self.get_malloc();
        let size_const = self.i64_t.const_int(total_size, false);
        let call_site2 = self
            .builder
            .build_call(malloc_fn, &[size_const.into()], "call_malloc")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "build_call failed")
            })?;
        let malloc_ret2 = call_site2
            .try_as_basic_value()
            .left()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc call did not return a value",
                )
            })?
            .into_pointer_value();

        // Set header with cycle bit
        let header_ptr = self
            .builder
            .build_pointer_cast(malloc_ret2, self.i8ptr_t, "hdr_ptr")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "pointer cast failed")
            })?;
        let type_tag_val = 2u64 << 49;
        let cycle_bit = if can_form_cycles { 0 } else { 1u64 << 48 };
        let header_val = self.i64_t.const_int(type_tag_val | cycle_bit | 1u64, false);
        let _ = self.builder.build_store(header_ptr, header_val);

        // call init(this, ...) - prepare args
        let mut init_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        init_args.push(malloc_ret2.as_basic_value_enum().into());
        for i in 0..llvm_param_types.len() {
            if let Some(p) = impl_f.get_nth_param(i as u32) {
                init_args.push(p.into());
            } else {
                // pad with null
                init_args.push(self.i8ptr_t.const_null().as_basic_value_enum().into());
            }
        }
        let call_init = self
            .builder
            .build_call(init_f, &init_args, "call_init")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::simple_boxed(Severity::Error, "build_call failed")
            })?;
        let init_result = call_init.try_as_basic_value().left().ok_or_else(|| {
            crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "init call did not return a value",
            )
        })?;
        let _ = self.builder.build_return(Some(&init_result));

        // Build the public wrapper `fname` which will apply decorators (if any)
        // and then indirect-call the final constructor pointer. The wrapper
        // has the same signature as the impl function.
        let wrapper_f = self.module.add_function(&fname, ctor_fn_ty, None);
        let wrapper_entry = self.context.append_basic_block(wrapper_f, "entry");
        self.builder.position_at_end(wrapper_entry);

        // Build a pointer to the impl function as i8*
        let impl_ptr = impl_f.as_global_value().as_pointer_value();
        let mut current_ctor_ptr = impl_ptr;

        // Decorators are applied in reverse order (TS semantics): the last
        // decorator in source order is applied first when composing.
        if let Some(decos) = decorators {
            for dec_name in decos.into_iter().rev() {
                // Ensure decorator function exists (declare if missing)
                let deco_fn = if let Some(f) = self.module.get_function(&dec_name) {
                    f
                } else {
                    // Decorator signature: i8* (i8*) -> i8* ; accept and return ctor ptrs
                    let ty = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                    self.module.add_function(&dec_name, ty, None)
                };

                // Call decorator with current_ctor_ptr (cast to i8*)
                let call_site =
                    self.builder
                        .build_call(deco_fn, &[current_ctor_ptr.into()], "apply_decorator");
                if let Ok(cs) = call_site
                    && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                {
                    current_ctor_ptr = bv.into_pointer_value();
                }
            }
        }

        // Now perform an indirect call to the final constructor pointer.
        // Build a function type matching ctor_fn_ty and perform the call.
        // Collect wrapper parameters and forward them.
        let mut args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
        for i in 0..llvm_param_types.len() {
            if let Some(p) = wrapper_f.get_nth_param(i as u32) {
                args.push(p.into());
            }
        }
        // Call the final constructor pointer (indirect call) and return its result.
        // Both direct calls and indirect calls produce a CallSiteValue we can inspect.
        let call_res = self.builder.build_indirect_call(
            ctor_fn_ty,
            current_ctor_ptr,
            &args,
            "call_decorated_ctor",
        );

        // Extract the CallSiteValue if the builder succeeded, then attempt to
        // get the returned basic value (ctor returns an i8*). Return null on
        // any failure to be conservative.
        if let Ok(cs) = call_res {
            if let inkwell::Either::Left(bv) = cs.try_as_basic_value() {
                let ret_val = bv;
                let _ = self.builder.build_return(Some(&ret_val));
            } else {
                let null_ptr = self.i8ptr_t.const_null().as_basic_value_enum();
                let _ = self.builder.build_return(Some(&null_ptr));
            }
        } else {
            let null_ptr = self.i8ptr_t.const_null().as_basic_value_enum();
            let _ = self.builder.build_return(Some(&null_ptr));
        }

        Ok(())
    }
}
