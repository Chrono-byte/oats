//! Code generation for top-level items.
//!
//! This module implements lowering of high-level declarations (functions,
//! constructors, etc.) into LLVM IR using the shared `CodeGen` helpers.
//!
//! Notes on design and ABI decisions:
//! - Functions use a simple, handwritten ABI mapping defined by `OatsType` ->
//!   LLVM primitive types. Numbers map to `f64`, booleans to `i1`/i8 as
//!   appropriate, and any pointer-like types map to `i8*` so the runtime can
//!   uniformly operate on object pointers. Unions are mapped to either `f64`
//!   or `i8*` depending on whether any branch is pointer-like; this keeps
//!   math-heavy unions efficient while still supporting heap objects.
//! - Constructors return `i8*` (object base pointer). The runtime expects the
//!   object layout described below and the codegen relies on the runtime's
//!   helpers (rc_inc/rc_dec, box/unbox) to manage lifetimes.

use deno_ast::swc::ast;
use inkwell::values::BasicValue;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

// Type alias for the `locals_stack` used during statement lowering.
//
// This is a stack of scopes, where each scope maps local variable names to
// a tuple containing:
// - PointerValue: the alloca or pointer to the variable's storage.
// - BasicTypeEnum: the LLVM type of the variable.
// - bool: is this variable mutable (declared with `let` vs `const`)
// - bool: is this variable a function parameter
// - bool: is this variable declared as Weak<T> (for reference counting)
// - Option<String>: if the variable is of a NominalStruct type, the name of
//   that nominal type; otherwise None. This helps member lowering resolve
//   fields without fallback heuristics.
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
    /// Generates LLVM IR for a function declaration.
    ///
    /// This is the main entry point for compiling a user function into LLVM
    /// IR. Responsibilities include:
    /// - Mapping Oats parameter and return types to LLVM ABI types.
    /// - Creating the LLVM function and entry basic block.
    /// - Registering any anonymous struct-typed parameters as generated
    ///   nominal structs so member lowering can resolve fields.
    /// - Allocating stack slots for parameters and wiring the initial
    ///   `locals_stack` used by statement lowering.
    /// - Lowering the function body via `lower_stmts` and emitting an
    ///   implicit `return` and `rc_dec` cleanup if the body doesn't
    ///   explicitly terminate.
    ///
    /// # Arguments
    /// * `func_name` - exported name to give the generated LLVM function.
    /// * `func_decl` - the AST `Function` node describing parameters and body.
    /// * `param_types` - the list of resolved `OatsType` for parameters.
    /// * `ret_type` - the declared return `OatsType` for ABI mapping.
    /// * `receiver_name` - optional `this` receiver name for methods.
    ///
    /// # Returns
    /// Returns the created `FunctionValue` on success or a `Diagnostic` on
    /// failure. The function emits calls to runtime helpers (for example
    /// for union boxing) and expects the runtime to provide those symbols
    /// during linking.
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

        // 3. If any parameter types are anonymous struct literals, register
        // them as nominal structs in `class_fields` under a generated name so
        // downstream lowering (which expects nominal names) can resolve
        // member accesses. Then store param types in `fn_param_types`.
        let mut param_types_owned = param_types.to_vec();
        for (i, p) in param_types_owned.iter_mut().enumerate() {
            if let crate::types::OatsType::StructLiteral(fields) = p {
                // Create a generated nominal name like <func_name>_param_struct_<i>
                let gen_name = format!("{}_param_struct_{}", func_name, i);
                // Insert into class_fields for lowering
                self.class_fields
                    .borrow_mut()
                    .insert(gen_name.clone(), fields.clone());
                // Replace the param type with NominalStruct so existing lowering uses it
                *p = crate::types::OatsType::NominalStruct(gen_name);
            }
        }
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types_owned.clone());
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
                | OatsType::StructLiteral(_)
                | OatsType::Array(_)
                | OatsType::Tuple(_)
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
        // Set header: rc=1 and type_tag=2 (class with field_map)
        // The header is a packed 64-bit value. Here we set the strong refcount
        // to 1 in the low bits and set a type tag in the high bits so the
        // runtime can recognize class objects quickly. The magic bit layout
        // is defined in the runtime; we mirror its expectations here.
        let type_tag_val = 2u64 << 49;
        let header_val = self.i64_t.const_int(type_tag_val | 1u64, false);
        // Store the header as an i64 at the object base. We intentionally cast
        // the malloc return to an i8* then store the i64 header at offset 0.
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

        // Zero-initialize all field slots to null so subsequent loads aren't garbage.
        for field_idx in 0..field_count {
            let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
            let obj_ptr_int = self
                .builder
                .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
            let off_const = self.i64_t.const_int(field_offset, false);
            let field_addr = self
                .builder
                .build_int_add(obj_ptr_int, off_const, "field_addr_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
            let field_ptr_cast = self
                .builder
                .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr_zero")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
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

        for (field_idx, (field_name, _field_type)) in combined_fields.iter().enumerate() {
            if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
                let param_val = f.get_nth_param(param_idx as u32).ok_or_else(|| {
                    crate::diagnostics::Diagnostic::simple(format!(
                        "missing parameter {} for constructor {}",
                        field_name, class_name
                    ))
                })?;
                let field_offset = header_size + meta_slot + (field_idx as u64 * 8);
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
