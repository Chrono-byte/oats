use crate::types::OatsType;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum};
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool, // is_weak
    Option<String>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> super::CodeGen<'a> {
    // Map our OatsType to an LLVM BasicTypeEnum for parameters/allocas.
    // Note: Void is not a valid parameter type; panic if encountered here.
    pub(crate) fn map_type_to_llvm(&self, t: &OatsType) -> BasicTypeEnum<'a> {
        match t {
            OatsType::Number => self.f64_t.as_basic_type_enum(),
            OatsType::Boolean => self.bool_t.as_basic_type_enum(),
            OatsType::String
            | OatsType::NominalStruct(_)
            | OatsType::StructLiteral(_)
            | OatsType::Array(_)
            | OatsType::Promise(_)
            | OatsType::Weak(_)
            | OatsType::Option(_) => self.i8ptr_t.as_basic_type_enum(),
            OatsType::Union(parts) => {
                // If any part is pointer-like, treat as pointer; otherwise number.
                let any_ptr = parts.iter().any(|p| {
                    matches!(
                        p,
                        OatsType::String
                            | OatsType::NominalStruct(_)
                            | OatsType::StructLiteral(_)
                            | OatsType::Array(_)
                            | OatsType::Promise(_)
                            | OatsType::Weak(_)
                            | OatsType::Option(_)
                    )
                });
                if any_ptr {
                    self.i8ptr_t.as_basic_type_enum()
                } else {
                    self.f64_t.as_basic_type_enum()
                }
            }
            OatsType::Void => panic!("Void cannot be mapped to a BasicTypeEnum for params"),
        }
    }

    // Try to coerce a BasicValueEnum into an f64 FloatValue.
    // Supports FloatValue and IntValue (including i1/bool).
    pub(crate) fn coerce_to_f64(
        &self,
        val: BasicValueEnum<'a>,
    ) -> Option<inkwell::values::FloatValue<'a>> {
        match val {
            BasicValueEnum::FloatValue(fv) => Some(fv),
            BasicValueEnum::IntValue(iv) => self
                .builder
                .build_signed_int_to_float(iv, self.f64_t, "i2f")
                .ok(),
            BasicValueEnum::PointerValue(pv) => {
                // Attempt to unbox a union number from a boxed union pointer
                let unbox_fn = self.get_union_unbox_f64();
                if let Ok(cs) = self.builder.build_call(unbox_fn, &[pv.into()], "union_unbox_f64_call") {
                    let either = cs.try_as_basic_value();
                    if let inkwell::Either::Left(bv) = either
                        && let BasicValueEnum::FloatValue(fv) = bv {
                            return Some(fv);
                        }
                }
                None
            }
            _ => None,
        }
    }

    // Coerce an int/bool value to i64
    pub(crate) fn coerce_to_i64(
        &self,
        val: BasicValueEnum<'a>,
    ) -> Option<inkwell::values::IntValue<'a>> {
        match val {
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() == 64 {
                    Some(iv)
                } else {
                    self.builder.build_int_cast(iv, self.i64_t, "cast_i64").ok()
                }
            }
            BasicValueEnum::FloatValue(fv) => self
                .builder
                .build_float_to_signed_int(fv, self.i64_t, "f2i")
                .ok(),
            _ => None,
        }
    }

    // Lower a value to an i1 condition (as IntValue). Returns None if unsupported.
    pub(crate) fn to_condition_i1(
        &self,
        val: BasicValueEnum<'a>,
    ) -> Option<inkwell::values::IntValue<'a>> {
        // Prefer numeric unboxing: if this value can be coerced to f64 (including union boxed numbers), use numeric truthiness
        if let Some(fv) = self.coerce_to_f64(val) {
            let zero = self.f64_t.const_float(0.0);
            let is_not_zero = self
                .builder
                .build_float_compare(inkwell::FloatPredicate::ONE, fv, zero, "neq0")
                .ok()?;
            let is_not_nan = self
                .builder
                .build_float_compare(inkwell::FloatPredicate::OEQ, fv, fv, "not_nan")
                .ok()?;
            let cond = self
                .builder
                .build_and(is_not_zero, is_not_nan, "num_truth")
                .ok()?;
            return Some(cond);
        }

        match val {
            BasicValueEnum::IntValue(iv) => {
                // If it's already an i1 this is fine; otherwise compare != 0
                if iv.get_type().get_bit_width() == 1 {
                    Some(iv)
                } else {
                    let zero = self.i64_t.const_int(0, false);
                    // widen iv to i64 if needed
                    let iv_wide = if iv.get_type().get_bit_width() < 64 {
                        self.builder
                            .build_int_cast(iv, self.i64_t, "cast_i_wide")
                            .ok()?
                    } else {
                        iv
                    };
                    let cmp = self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, iv_wide, zero, "cond_int_ne0")
                        .ok()?;
                    Some(cmp)
                }
            }
            BasicValueEnum::FloatValue(fv) => {
                // JS-like: not zero and not NaN
                let zero = self.f64_t.const_float(0.0);
                let is_not_zero = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::ONE, fv, zero, "neq0")
                    .ok()?;
                let is_not_nan = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::OEQ, fv, fv, "not_nan")
                    .ok()?;
                let cond = self
                    .builder
                    .build_and(is_not_zero, is_not_nan, "num_truth")
                    .ok()?;
                Some(cond)
            }
            BasicValueEnum::PointerValue(pv) => {
                let is_null = self.builder.build_is_null(pv, "is_null").ok()?;
                let is_not_null = self.builder.build_not(is_null, "not_null").ok()?;
                if let Some(strlen_fn) = self.module.get_function("strlen") {
                    let cs = self
                        .builder
                        .build_call(strlen_fn, &[pv.into()], "strlen_call")
                        .ok()?;
                    let either = cs.try_as_basic_value();
                    if let inkwell::Either::Left(bv) = either {
                        let len = bv.into_int_value();
                        let zero64 = self.i64_t.const_int(0, false);
                        let len_nonzero = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::NE,
                                len,
                                zero64,
                                "len_nonzero",
                            )
                            .ok()?;
                        let cond = self
                            .builder
                            .build_and(is_not_null, len_nonzero, "ptr_truth")
                            .ok()?;
                        return Some(cond);
                    }
                }
                Some(is_not_null)
            }
            _ => None,
        }
    }

    // Build a phi merging two BasicValueEnum values from then_bb and else_bb.
    // Performs simple coercions (int<->float) when possible.
    pub(crate) fn build_phi_merge(
        &self,
        then_bb: inkwell::basic_block::BasicBlock<'a>,
        else_bb: inkwell::basic_block::BasicBlock<'a>,
        tv: BasicValueEnum<'a>,
        ev: BasicValueEnum<'a>,
    ) -> Option<BasicValueEnum<'a>> {
        // Coerce pair to a common type: prefer float if either is float, else int if either is int/bool, else pointer if both pointer
        let tv_ty = tv.get_type();
        let ev_ty = ev.get_type();

        // If either is float, coerce both to float
        if let inkwell::types::BasicTypeEnum::FloatType(_) = tv_ty
            && let Some(ev_f) = self.coerce_to_f64(ev)
        {
            let tv_f = match tv {
                BasicValueEnum::FloatValue(fv) => fv,
                BasicValueEnum::IntValue(iv) => match self
                    .builder
                    .build_signed_int_to_float(iv, self.f64_t, "i2f")
                {
                    Ok(v) => v,
                    Err(_) => return None,
                },
                _ => return None,
            };
            let ty = self.f64_t.as_basic_type_enum();
            let phi_node = match self.builder.build_phi(ty, "phi_tmp") {
                Ok(p) => p,
                Err(_) => return None,
            };
            phi_node.add_incoming(&[
                (&tv_f.as_basic_value_enum(), then_bb),
                (&ev_f.as_basic_value_enum(), else_bb),
            ]);
            return Some(phi_node.as_basic_value());
        }
        if let inkwell::types::BasicTypeEnum::FloatType(_) = ev_ty
            && let Some(tv_f) = self.coerce_to_f64(tv)
        {
            let ev_f = match ev {
                BasicValueEnum::FloatValue(fv) => fv,
                BasicValueEnum::IntValue(iv) => match self
                    .builder
                    .build_signed_int_to_float(iv, self.f64_t, "i2f")
                {
                    Ok(v) => v,
                    Err(_) => return None,
                },
                _ => return None,
            };
            let ty = self.f64_t.as_basic_type_enum();
            let phi_node = match self.builder.build_phi(ty, "phi_tmp") {
                Ok(p) => p,
                Err(_) => return None,
            };
            phi_node.add_incoming(&[
                (&tv_f.as_basic_value_enum(), then_bb),
                (&ev_f.as_basic_value_enum(), else_bb),
            ]);
            return Some(phi_node.as_basic_value());
        }

        // If both pointers, build pointer phi
        if let (
            inkwell::types::BasicTypeEnum::PointerType(_),
            inkwell::types::BasicTypeEnum::PointerType(_),
        ) = (tv_ty, ev_ty)
        {
            let ty = self.i8ptr_t.as_basic_type_enum();
            let phi_node = match self.builder.build_phi(ty, "phi_tmp") {
                Ok(p) => p,
                Err(_) => return None,
            };
            phi_node.add_incoming(&[(&tv, then_bb), (&ev, else_bb)]);
            return Some(phi_node.as_basic_value());
        }

        // Fallback: if either is int/bool, coerce both to i64 and make int phi
        if let (inkwell::types::BasicTypeEnum::IntType(_), _)
        | (_, inkwell::types::BasicTypeEnum::IntType(_)) = (tv_ty, ev_ty)
        {
            let tv_i = self.coerce_to_i64(tv)?;
            let ev_i = self.coerce_to_i64(ev)?;
            let ty = self.i64_t.as_basic_type_enum();
            let phi_node = match self.builder.build_phi(ty, "phi_tmp") {
                Ok(p) => p,
                Err(_) => return None,
            };
            phi_node.add_incoming(&[
                (&tv_i.as_basic_value_enum(), then_bb),
                (&ev_i.as_basic_value_enum(), else_bb),
            ]);
            return Some(phi_node.as_basic_value());
        }

        None
    }

    // Lookup a local by name from the scope stack (searching innermost scope first)
    pub(crate) fn find_local(
        &self,
        locals: &LocalsStackLocal<'a>,
        name: &str,
    ) -> Option<LocalEntry<'a>> {
        for scope in locals.iter().rev() {
            if let Some((ptr, ty, init, is_const, is_weak, nominal)) = scope.get(name) {
                return Some((*ptr, *ty, *init, *is_const, *is_weak, nominal.clone()));
            }
        }
        None
    }

    pub(crate) fn insert_local_current_scope(
        &self,
        locals: &mut LocalsStackLocal<'a>,
        name: String,
        ptr: inkwell::values::PointerValue<'a>,
        ty: BasicTypeEnum<'a>,
        initialized: bool,
        is_const: bool,
        is_weak: bool,
        nominal: Option<String>,
    ) {
        if locals.is_empty() {
            locals.push(HashMap::new());
        }
        // Safe: we just ensured locals is not empty above
        if let Some(scope) = locals.last_mut() {
            scope.insert(name, (ptr, ty, initialized, is_const, is_weak, nominal));
        }
    }

    
    pub(crate) fn set_local_initialized(
        &self,
        locals: &mut LocalsStackLocal<'a>,
        name: &str,
        initialized: bool,
    ) {
        for scope in locals.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                entry.2 = initialized;
                return;
            }
        }
    }
    pub(crate) fn declare_libc(&self) {
        // declare malloc(i64) -> i8*
        if self.module.get_function("malloc").is_none() {
            let i8ptr = self.i8ptr_t;
            let i64t = self.i64_t;
            let malloc_ty = i8ptr.fn_type(&[i64t.into()], false);
            let f = self.module.add_function("malloc", malloc_ty, None);
            self.fn_malloc.borrow_mut().replace(f);
        }

        // declare strlen(i8*) -> i64
        if self.module.get_function("strlen").is_none() {
            let i8ptr = self.i8ptr_t;
            let i64t = self.i64_t;
            let strlen_ty = i64t.fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("strlen", strlen_ty, None);
            self.fn_strlen.borrow_mut().replace(f);
        }


        
        // declare memcpy(i8*, i8*, i64) -> i8*
        if self.module.get_function("memcpy").is_none() {
            let i8ptr = self.i8ptr_t;
            let i64t = self.i64_t;
            let memcpy_ty = i8ptr.fn_type(&[i8ptr.into(), i8ptr.into(), i64t.into()], false);
            let f = self.module.add_function("memcpy", memcpy_ty, None);
            self.fn_memcpy.borrow_mut().replace(f);
        }

        // declare free(i8*) -> void
        if self.module.get_function("free").is_none() {
            let voidt = self.context.void_type();
            let i8ptr = self.i8ptr_t;
            let free_ty = voidt.fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("free", free_ty, None);
            self.fn_free.borrow_mut().replace(f);
        }

        // Declare runtime print helpers so user scripts can call them.
        if self.module.get_function("print_f64").is_none() {
            let print_ty = self
                .context
                .void_type()
                .fn_type(&[self.f64_t.into()], false);
            let f = self.module.add_function("print_f64", print_ty, None);
            self.fn_print_f64.borrow_mut().replace(f);
        }
        if self.module.get_function("print_str").is_none() {
            let i8ptr = self.i8ptr_t;
            let print_str_ty = self.context.void_type().fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("print_str", print_str_ty, None);
            self.fn_print_str.borrow_mut().replace(f);
        }
        // declare sleep_ms(f64) -> void so examples can pause to allow background collector to run
        if self.module.get_function("sleep_ms").is_none() {
            let sleep_ty = self.context.void_type().fn_type(&[self.f64_t.into()], false);
            let f = self.module.add_function("sleep_ms", sleep_ty, None);
            // no dedicated RefCell for this helper; declaration is sufficient
            let _ = f;
        }
        // declare collector_test_enqueue() -> void for demo/testing
        if self.module.get_function("collector_test_enqueue").is_none() {
            let ty = self.context.void_type().fn_type(&[], false);
            let _f = self.module.add_function("collector_test_enqueue", ty, None);
        }
    }

    // Emit rc_dec calls for any initialized pointer locals in the provided
    // locals stack. This is used at function exit or before early returns to
    // ensure pointer locals are decref'd so the runtime can free them.
    pub fn emit_rc_dec_for_locals(&self, locals: &LocalsStackLocal<'a>) {
        // Find the rc_dec declaration (declare if needed)
        let rc_dec = self.get_rc_dec();
        let rc_weak_dec = self.get_rc_weak_dec();
        for scope in locals.iter().rev() {
            for (_name, (ptr, ty, init, _is_const, is_weak, _nominal)) in scope.iter() {
                if *init && let inkwell::types::BasicTypeEnum::PointerType(_) = ty {
                    // load current pointer value
                    if let Ok(loaded) = self.builder.build_load(*ty, *ptr, "drop_load")
                        && let BasicValueEnum::PointerValue(pv) = loaded
                    {
                        // If this local is a weak reference, call rc_weak_dec; otherwise rc_dec
                        if *is_weak {
                            let _ = self.builder.build_call(rc_weak_dec, &[pv.into()], "rc_weak_dec_local");
                        } else {
                            // Silently ignore build_call errors during cleanup
                            let _ = self
                                .builder
                                .build_call(rc_dec, &[pv.into()], "rc_dec_local");
                        }
                    }
                }
            }
        }
    }

    // Emit rc_dec for locals in a suffix of the locals stack starting at
    // `start_index`. This is useful when leaving a loop body early due to
    // `break`/`continue` where only the inner scopes should be decref'd.
    pub(crate) fn _emit_rc_dec_for_locals_from(
        &self,
        locals: &LocalsStackLocal<'a>,
        start_index: usize,
    ) {
        let rc_dec = self.get_rc_dec();
        if start_index >= locals.len() {
            return;
        }
        // iterate from innermost scope down to start_index
        let rc_weak_dec = self.get_rc_weak_dec();
        for scope in locals.iter().rev().take(locals.len() - start_index) {
            for (_name, (ptr, ty, init, _is_const, is_weak, _nominal)) in scope.iter() {
                if *init
                    && let inkwell::types::BasicTypeEnum::PointerType(_) = ty
                    && let Ok(loaded) = self.builder.build_load(*ty, *ptr, "drop_load")
                    && let BasicValueEnum::PointerValue(pv) = loaded
                {
                    if *is_weak {
                        let _ = self.builder.build_call(rc_weak_dec, &[pv.into()], "rc_weak_dec_local");
                    } else {
                        // Silently ignore build_call errors during cleanup
                        let _ = self
                            .builder
                            .build_call(rc_dec, &[pv.into()], "rc_dec_local");
                    }
                }
            }
        }
    }

    pub(crate) fn gen_str_concat(&self) {
        if self.module.get_function("str_concat").is_some() {
            return;
        }
        // Instead of emitting a full definition here (which can cause
        // duplicate-symbol linker errors when we also link the runtime
        // staticlib that provides `str_concat`), only declare the symbol
        // so the runtime implementation will be used at link time.
        let i8ptr = self.i8ptr_t;
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i8ptr.into()], false);

        // Ensure libc declarations exist so calls can be built elsewhere.
        self.declare_libc();

        // Add the function declaration (no body). If a definition is needed
        // later (for a self-contained IR mode) we can change this behavior.
        let _function = self.module.add_function("str_concat", fn_type, None);
    }

    // Allocate a heap object with the standard header/meta/fields layout and
    // store the provided pointer-field values into the object's fields.
    // All values must be BasicValueEnum with pointer type (i8*). Numeric
    // values should be boxed by the caller (e.g., via union_box_f64).
    // Returns the allocated object base pointer (i8*).
    pub(crate) fn heap_alloc_with_ptr_fields(
        &self,
        field_ptrs: &[inkwell::values::BasicValueEnum<'a>],
    ) -> Result<inkwell::values::PointerValue<'a>, crate::diagnostics::Diagnostic> {
        let field_count = field_ptrs.len();
        let header_size = 8u64;
        let meta_slot = 8u64;
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
            .ok_or_else(|| crate::diagnostics::Diagnostic::simple("malloc call did not return a value"))?
            .into_pointer_value();

        // Cast to i8* for stores
        let obj_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "obj_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;

        // Initialize header: rc=1, type_tag=3 (closure/env-like)
        let type_tag_val = 3u64 << 49;
        let header_val = self.i64_t.const_int(type_tag_val | 1u64, false);
        let _ = self.builder.build_store(obj_ptr, header_val);

        // Zero-init meta slot
        let obj_ptr_int = self
            .builder
            .build_ptr_to_int(malloc_ret, self.i64_t, "obj_addr_for_zero")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
        let off_meta = self.i64_t.const_int(header_size, false);
        let meta_addr = self
            .builder
            .build_int_add(obj_ptr_int, off_meta, "meta_addr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
        let meta_ptr = self
            .builder
            .build_int_to_ptr(meta_addr, self.i8ptr_t, "meta_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
        let null_ptr = self.i8ptr_t.const_null();
        let _ = self.builder.build_store(meta_ptr, null_ptr.as_basic_value_enum());

        // Store each field (as i8* word) at offset header+meta_slot + idx*8
        for (i, val) in field_ptrs.iter().enumerate() {
            // compute offset
            let field_offset = header_size + meta_slot + (i as u64 * 8);
            let off_const = self.i64_t.const_int(field_offset, false);
            let field_addr = self
                .builder
                .build_int_add(obj_ptr_int, off_const, "field_addr")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
            let field_ptr_cast = self
                .builder
                .build_int_to_ptr(field_addr, self.i8ptr_t, "field_ptr")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;

            // Ensure val is pointer-like; if it's not, cast could fail — caller responsibility
            let _ = self.builder.build_store(field_ptr_cast, *val);

            // If value is pointer, increment RC to claim ownership in env
            if val.get_type().is_pointer_type()
                && let inkwell::values::BasicValueEnum::PointerValue(pv) = *val {
                    let rc_inc = self.get_rc_inc();
                    let _ = self.builder.build_call(rc_inc, &[pv.into()], "rc_inc_env");
                }
        }

        Ok(obj_ptr)
    }
}
