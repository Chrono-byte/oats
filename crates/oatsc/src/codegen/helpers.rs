//! Small helper utilities used across codegen lowering.
//!
//! This module implements a set of reusable helpers:
//! - Mapping `OatsType` to LLVM ABI types (`map_type_to_llvm`).
//! - Lightweight coercions between ABI kinds (`coerce_to_f64`, `coerce_to_i64`).
//! - Truthiness coercion used for conditionals (`to_condition_i1`).
//! - Local variable stack helpers (`find_local`, `insert_local_current_scope`).
//!
//! The helpers keep lowering code focused and centralize runtime/ABI
//! decisions in one place so they can be updated consistently as the ABI
//! evolves.

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
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

/// Parameters for inserting a local variable into the current scope.
pub(crate) struct LocalVarInfo<'a> {
    pub name: String,
    pub ptr: inkwell::values::PointerValue<'a>,
    pub ty: BasicTypeEnum<'a>,
    pub initialized: bool,
    pub is_const: bool,
    pub is_weak: bool,
    pub nominal: Option<String>,
    pub oats_type: Option<crate::types::OatsType>,
}

impl<'a> super::CodeGen<'a> {
    /// Map an `OatsType` to an LLVM `BasicTypeEnum` for parameter/alloca ABI.
    ///
    /// This follows the current ABI decisions:
    /// - Numeric types map to `f64`.
    /// - Pointer-like types (strings, arrays, objects, option, weak, etc.) map
    ///   to `i8*`.
    /// - Unions map to `i8*` if any arm is pointer-like, otherwise `f64`.
    ///
    /// Panics for `Void` as it cannot be used as a parameter/alloca type.
    pub(crate) fn map_type_to_llvm(&self, t: &OatsType) -> BasicTypeEnum<'a> {
        match t {
            OatsType::Number => self.f64_t.as_basic_type_enum(),
            OatsType::Boolean => self.bool_t.as_basic_type_enum(),
            OatsType::String
            | OatsType::NominalStruct(_)
            | OatsType::StructLiteral(_)
            | OatsType::Array(_)
            | OatsType::Tuple(_)
            | OatsType::Promise(_)
            | OatsType::Weak(_)
            | OatsType::Enum(_, _) => self.i8ptr_t.as_basic_type_enum(),
            OatsType::Option(inner) => {
                // If the inner type is numeric (or a union of numeric-only arms),
                // represent Option<number> using the numeric ABI (f64). This lets
                // Option<number> be lowered efficiently and enables correct
                // monomorphization for generics like T | undefined where T=number.
                match &**inner {
                    OatsType::Number => self.f64_t.as_basic_type_enum(),
                    OatsType::Union(parts) => {
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
                                    | OatsType::Enum(_, _)
                            )
                        });
                        if any_ptr {
                            self.i8ptr_t.as_basic_type_enum()
                        } else {
                            self.f64_t.as_basic_type_enum()
                        }
                    }
                    _ => self.i8ptr_t.as_basic_type_enum(),
                }
            }
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
                            | OatsType::Enum(_, _)
                    )
                });
                if any_ptr {
                    self.i8ptr_t.as_basic_type_enum()
                } else {
                    self.f64_t.as_basic_type_enum()
                }
            }
            OatsType::Void => {
                // Defensive fallback: Void should not appear as a parameter/alloca
                // type. Returning an i8* avoids panics in unexpected edge cases
                // and keeps lowering resilient. Call sites that encounter Void
                // in parameter positions are likely buggy and should be
                // diagnosed upstream.
                self.i8ptr_t.as_basic_type_enum()
            }
            OatsType::Generic(_) => {
                // Generics are not directly mappable to LLVM types.
                // This should be resolved during specialization.
                panic!("Generic types must be specialized before mapping to LLVM types");
            }
            OatsType::GenericInstance { .. } => {
                // Generic instances are specialized, so treat as pointer types
                self.i8ptr_t.as_basic_type_enum()
            }
        }
    }

    /// Try to coerce a `BasicValueEnum` into an `f64` FloatValue.
    ///
    /// Supported inputs:
    /// - `FloatValue` -> returned as-is
    /// - `IntValue` -> converted with signed int->float
    /// - `PointerValue` -> attempt to `union_unbox_f64` (unbox boxed union)
    ///
    /// Returns `None` when coercion isn't possible.
    pub(crate) fn coerce_to_f64(
        &self,
        val: inkwell::values::BasicValueEnum<'a>,
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
                if let Ok(cs) =
                    self.builder
                        .build_call(unbox_fn, &[pv.into()], "union_unbox_f64_call")
                {
                    let either = cs.try_as_basic_value();
                    if let inkwell::Either::Left(bv) = either
                        && let BasicValueEnum::FloatValue(fv) = bv
                    {
                        return Some(fv);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Coerce an int/bool/float value to an `i64` IntValue when possible.
    ///
    /// This is used when building phi nodes or integer comparisons where a
    /// canonical `i64` representation is convenient.
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

    /// Lower a value to an `i1` condition (`IntValue`). Returns `None` if
    /// the value cannot be coerced to a boolean.
    ///
    /// Truthiness rules are JS-like: numbers are true when not 0/NaN, pointers
    /// are true when non-null (and strings also require non-zero length), and
    /// ints are true when non-zero.
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
                // Prefer to read the length field stored at offset +8 from object base.
                // This covers arrays (len stored at +8) and strings (len at +8),
                // and yields a sensible truthiness for other objects (meta ptr non-null -> truthy).
                let is_null = self.builder.build_is_null(pv, "is_null").ok()?;
                let is_not_null = self.builder.build_not(is_null, "not_null").ok()?;

                // Compute pointer to i64 length at offset +8
                let offset = self.i64_t.const_int(8, false);
                let gep_res = self.i8_ptr_from_offset_i64(pv, offset, "len_i8ptr").ok();
                if let Some(gep_ptr) = gep_res {
                    // cast i8* -> i64*
                    let len_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
                    if let Ok(len_ptr) =
                        self.builder
                            .build_pointer_cast(gep_ptr, len_ptr_ty, "len_ptr_cast")
                        && let Ok(loaded) = self.builder.build_load(self.i64_t, len_ptr, "len_load")
                    {
                        let len_iv = loaded.into_int_value();
                        let zero64 = self.i64_t.const_int(0, false);
                        if let Ok(len_nonzero) = self.builder.build_int_compare(
                            inkwell::IntPredicate::NE,
                            len_iv,
                            zero64,
                            "len_nonzero",
                        ) && let Ok(cond) =
                            self.builder
                                .build_and(is_not_null, len_nonzero, "ptr_truth")
                        {
                            return Some(cond);
                        }
                    }
                }
                // Fallback: treat non-null pointer as truthy
                Some(is_not_null)
            }
            _ => None,
        }
    }

    /// Build a phi merging two `BasicValueEnum` values coming from `then_bb`
    /// and `else_bb` respectively.
    ///
    /// The helper performs simple coercions to a common type when possible:
    /// - If either value is float, coerce both to `f64`.
    /// - Else if both are pointers, build a pointer phi.
    /// - Else if either is integer-like, coerce both to `i64` and build an
    ///   integer phi.
    ///
    /// Returns `None` when no sensible coercion/phi can be constructed.
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

        // If either is a pointer, prefer a pointer phi and box numeric arms as needed.
        if let (inkwell::types::BasicTypeEnum::PointerType(_), _)
        | (_, inkwell::types::BasicTypeEnum::PointerType(_)) = (tv_ty, ev_ty)
        {
            let ty = self.i8ptr_t.as_basic_type_enum();
            // helper to ensure a BasicValueEnum is a pointer value; box floats/ints into i8* via union_box_f64
            let ensure_ptr = |bv: BasicValueEnum<'a>| -> Option<BasicValueEnum<'a>> {
                match bv {
                    BasicValueEnum::PointerValue(p) => Some(p.as_basic_value_enum()),
                    BasicValueEnum::FloatValue(fv) => {
                        let box_fn = self.get_union_box_f64();
                        if let Ok(cs) = self.builder.build_call(box_fn, &[fv.into()], "box_for_phi")
                            && let inkwell::Either::Left(bv2) = cs.try_as_basic_value()
                        {
                            return Some(bv2);
                        }
                        None
                    }
                    BasicValueEnum::IntValue(iv) => {
                        // convert bool/int to f64 then box
                        let fv = match self
                            .builder
                            .build_signed_int_to_float(iv, self.f64_t, "i2f_box")
                        {
                            Ok(v) => v,
                            Err(_) => return None,
                        };
                        let box_fn = self.get_union_box_f64();
                        if let Ok(cs) = self.builder.build_call(box_fn, &[fv.into()], "box_for_phi")
                            && let inkwell::Either::Left(bv2) = cs.try_as_basic_value()
                        {
                            return Some(bv2);
                        }
                        None
                    }
                    _ => None,
                }
            };

            let tvp = ensure_ptr(tv)?;
            let evp = ensure_ptr(ev)?;
            let phi_node = match self.builder.build_phi(ty, "phi_tmp") {
                Ok(p) => p,
                Err(_) => return None,
            };
            phi_node.add_incoming(&[(&tvp, then_bb), (&evp, else_bb)]);
            return Some(phi_node.as_basic_value());
        }

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

    /// Lookup a local by name from the lexical scope stack (innermost first).
    pub(crate) fn find_local(
        &self,
        locals: &LocalsStackLocal<'a>,
        name: &str,
    ) -> Option<LocalEntry<'a>> {
        for scope in locals.iter().rev() {
            if let Some((ptr, ty, init, is_const, is_weak, nominal, oats_type)) = scope.get(name) {
                return Some((
                    *ptr,
                    *ty,
                    *init,
                    *is_const,
                    *is_weak,
                    nominal.clone(),
                    oats_type.clone(),
                ));
            }
        }
        // DEBUG: print locals stack keys for troubleshooting lookups
        {
            let mut all_keys: Vec<Vec<String>> = Vec::new();
            for scope in locals.iter() {
                let keys: Vec<String> = scope.keys().cloned().collect();
                all_keys.push(keys);
            }
            eprintln!(
                "[debug find_local] name='{}' locals_scopes={:?}",
                name, all_keys
            );
        }
        None
    }

    /// Insert a new local into the current lexical scope.
    ///
    /// Creates the scope if none exists. The `nominal` argument carries an
    /// optional nominal struct/class name which helps member access lowering
    /// when the static type is available.
    pub(crate) fn insert_local_current_scope(
        &self,
        locals: &mut LocalsStackLocal<'a>,
        info: LocalVarInfo<'a>,
    ) {
        if locals.is_empty() {
            locals.push(HashMap::new());
        }
        // Safe: we just ensured locals is not empty above
        if let Some(scope) = locals.last_mut() {
            let key = info.name.clone();
            scope.insert(
                info.name,
                (
                    info.ptr,
                    info.ty,
                    info.initialized,
                    info.is_const,
                    info.is_weak,
                    info.nominal.clone(),
                    info.oats_type.clone(),
                ),
            );
            // DEBUG: print insertion
            #[cfg(debug_assertions)]
            {
                let keys: Vec<String> = scope.keys().cloned().collect();
                let entry = scope.get(&key).cloned();
                let nominal_str = entry
                    .and_then(|(_, _, _, _, _, nom, _)| nom)
                    .unwrap_or("<none>".to_string());
                eprintln!(
                    "[debug insert_local] inserted='{}' nominal='{}' keys_now={:?}",
                    key, nominal_str, keys
                );
            }
            // If this new local was initialized from a previously-tracked
            // expression origin that we recorded as a freshly-created closure,
            // propagate its return-type mapping so future calls can use the
            // statically-known return type.
            if info.initialized
                && let Some(orig) = self.last_expr_origin_local.borrow().clone()
                && let Some(rt) = self.closure_local_rettype.borrow().get(&orig)
            {
                self.closure_local_rettype
                    .borrow_mut()
                    .insert(key, rt.clone());
            }
        }
    }

    /// Insert a new local into the function-level scope (for `var` declarations).
    ///
    /// This is used for `var` declarations which are function-scoped and hoisted
    /// to the top of the function, unlike `let`/`const` which are block-scoped.
    pub(crate) fn insert_local_function_scope(
        &self,
        locals: &mut LocalsStackLocal<'a>,
        info: LocalVarInfo<'a>,
    ) {
        // Ensure we have at least one scope (the function scope)
        if locals.is_empty() {
            locals.push(HashMap::new());
        }
        // Insert into the function-level scope (index 0)
        let scope = &mut locals[0];
        #[cfg(debug_assertions)]
        let _key = info.name.clone();
        scope.insert(
            info.name,
            (
                info.ptr,
                info.ty,
                info.initialized,
                info.is_const,
                info.is_weak,
                info.nominal.clone(),
                info.oats_type.clone(),
            ),
        );
        // DEBUG: print insertion
        #[cfg(debug_assertions)]
        {
            let keys: Vec<String> = scope.keys().cloned().collect();
            let entry = scope.get(&_key).cloned();
            let nominal_str = entry
                .and_then(|(_, _, _, _, _, nom, _)| nom)
                .unwrap_or("<none>".to_string());
            eprintln!(
                "[debug insert_local_function] inserted='{}' nominal='{}' keys_now={:?}",
                _key, nominal_str, keys
            );
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
    pub fn declare_libc(&self) {
        // Declare external libc/runtime helper functions used by codegen.
        // These are added as declarations (no body) so the runtime or
        // system libc provides the implementation at link time.
        // Examples: malloc, strlen, memcpy, free and runtime print helpers.
        // Declarations are idempotent: we only add a function if it isn't
        // already present in the module.
        //
        // Note: these declarations are intentionally minimal to avoid
        // duplicate-symbol issues when linking against the runtime staticlib.
        // If a definition is required for self-contained IR builds, the
        // behavior can be adjusted.
        //
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
        // declare print_f64_no_nl(f64) -> void
        if self.module.get_function("print_f64_no_nl").is_none() {
            let print_ty = self
                .context
                .void_type()
                .fn_type(&[self.f64_t.into()], false);
            let _f = self.module.add_function("print_f64_no_nl", print_ty, None);
        }
        if self.module.get_function("print_str").is_none() {
            let i8ptr = self.i8ptr_t;
            let print_str_ty = self.context.void_type().fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("print_str", print_str_ty, None);
            self.fn_print_str.borrow_mut().replace(f);
        }
        // declare print_str_no_nl(i8*) -> void
        if self.module.get_function("print_str_no_nl").is_none() {
            let i8ptr = self.i8ptr_t;
            let print_str_ty = self.context.void_type().fn_type(&[i8ptr.into()], false);
            let _f = self
                .module
                .add_function("print_str_no_nl", print_str_ty, None);
        }
        // declare print_newline() -> void
        if self.module.get_function("print_newline").is_none() {
            let voidt = self.context.void_type();
            let _f = self
                .module
                .add_function("print_newline", voidt.fn_type(&[], false), None);
        }
        // declare array_to_string(i8*) -> i8* (runtime helper that converts
        // array objects to a printable heap string). The runtime provides an
        // implementation; declare it here for codegen to call.
        if self.module.get_function("array_to_string").is_none() {
            let i8ptr = self.i8ptr_t;
            let fn_ty = i8ptr.fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("array_to_string", fn_ty, None);
            let _ = f;
        }
        // declare rc_dec_str(i8*) -> void so codegen can release temporary
        // heap strings produced by helpers like `array_to_string`.
        if self.module.get_function("rc_dec_str").is_none() {
            let voidt = self.context.void_type();
            let i8ptr = self.i8ptr_t;
            let fn_ty = voidt.fn_type(&[i8ptr.into()], false);
            let f = self.module.add_function("rc_dec_str", fn_ty, None);
            let _ = f;
        }
        // declare sleep_ms(f64) -> void so examples can pause to allow the
        // background collector to run during demos/tests.
        if self.module.get_function("sleep_ms").is_none() {
            let sleep_ty = self
                .context
                .void_type()
                .fn_type(&[self.f64_t.into()], false);
            let f = self.module.add_function("sleep_ms", sleep_ty, None);
            // no dedicated RefCell for this helper; declaration is sufficient
            let _ = f;
        }
        // Declare test-only helper `collector_test_enqueue()` when the
        // workspace enables the `collector-test` Cargo feature. This avoids
        // exposing test/debug symbols in normal release builds.
        if cfg!(feature = "collector-test")
            && self.module.get_function("collector_test_enqueue").is_none()
        {
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
            for (_name, (ptr, ty, init, _is_const, is_weak, _nominal, _oats_type)) in scope.iter() {
                if *init && let inkwell::types::BasicTypeEnum::PointerType(_) = ty {
                    // load current pointer value
                    if let Ok(loaded) = self.builder.build_load(*ty, *ptr, "drop_load")
                        && let BasicValueEnum::PointerValue(pv) = loaded
                    {
                        // If this local is a weak reference, call rc_weak_dec; otherwise rc_dec
                        if *is_weak {
                            let _ = self.builder.build_call(
                                rc_weak_dec,
                                &[pv.into()],
                                "rc_weak_dec_local",
                            );
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
            for (_name, (ptr, ty, init, _is_const, is_weak, _nominal, _oats_type)) in scope.iter() {
                if *init
                    && let inkwell::types::BasicTypeEnum::PointerType(_) = ty
                    && let Ok(loaded) = self.builder.build_load(*ty, *ptr, "drop_load")
                    && let BasicValueEnum::PointerValue(pv) = loaded
                {
                    if *is_weak {
                        let _ =
                            self.builder
                                .build_call(rc_weak_dec, &[pv.into()], "rc_weak_dec_local");
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
    pub fn heap_alloc_with_ptr_fields(
        &self,
        field_ptrs: &[(inkwell::values::BasicValueEnum<'a>, bool)],
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
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple("malloc call did not return a value")
            })?
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
        let _ = self
            .builder
            .build_store(meta_ptr, null_ptr.as_basic_value_enum());

        // Store each field (as i8* word) at offset header+meta_slot + idx*8
        for (i, (val, is_weak)) in field_ptrs.iter().enumerate() {
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

            // Ensure val is pointer-like; if it's not an i8* (heap object pointer),
            // cast it to i8* for storage but DO NOT adjust refcounts for non-heap
            // pointers (e.g., function pointers). Only increment refcounts when
            // the original value is an i8* heap pointer.
            let mut store_val = *val;
            // If it's a pointer and not already i8*, cast to i8* for storage
            if val.get_type().is_pointer_type()
                && val.get_type() != self.i8ptr_t.as_basic_type_enum()
                && let inkwell::values::BasicValueEnum::PointerValue(pv) = *val
            {
                let casted = self
                    .builder
                    .build_pointer_cast(pv, self.i8ptr_t, "cast_to_i8ptr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
                store_val = casted.as_basic_value_enum();
            }

            let _ = self.builder.build_store(field_ptr_cast, store_val);

            // Only increment RC when the original value type is i8* (heap object)
            if val.get_type() == self.i8ptr_t.as_basic_type_enum()
                && let inkwell::values::BasicValueEnum::PointerValue(pv) = *val
            {
                if *is_weak {
                    let rc_weak_inc = self.get_rc_weak_inc();
                    let _ = self
                        .builder
                        .build_call(rc_weak_inc, &[pv.into()], "rc_weak_inc_env");
                } else {
                    let rc_inc = self.get_rc_inc();
                    let _ = self.builder.build_call(rc_inc, &[pv.into()], "rc_inc_env");
                }
            }
        }

        Ok(obj_ptr)
    }

    // Emit a packed field_map global named `gv_name` containing meta0 and i32 offsets.
    // Returns an i8* pointer value to the global (suitable for storing into meta slot).
    pub(crate) fn emit_field_map_global(
        &self,
        gv_name: &str,
        offsets: &[u64],
    ) -> Result<inkwell::values::PointerValue<'a>, crate::diagnostics::Diagnostic> {
        // If global already exists, cast and return pointer
        if let Some(g) = self.module.get_global(gv_name) {
            let gv_ptr = g.as_pointer_value();
            let gv_i8 = self
                .builder
                .build_pointer_cast(gv_ptr, self.i8ptr_t, "field_map_i8ptr")
                .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
            return Ok(gv_i8);
        }

        let len = offsets.len();
        let arr_i32_ty = self.context.i32_type().array_type(len as u32);
        let struct_ty = self
            .context
            .struct_type(&[self.i64_t.into(), arr_i32_ty.into()], false);

        let magic_u64 = 0x4F415453u64; // 'OATS'
        let meta0_val = (magic_u64 << 32) | (len as u64 & 0xffffffffu64);
        let meta0_const = self.i64_t.const_int(meta0_val, false);

        let mut int32_vals: Vec<inkwell::values::IntValue<'a>> = Vec::new();
        for &off in offsets {
            let v = off & 0xffffffffu64;
            int32_vals.push(self.i32_t.const_int(v, false));
        }
        let array_const = self.i32_t.const_array(&int32_vals);
        let initializer = self
            .context
            .const_struct(&[meta0_const.into(), array_const.into()], false);

        let gv = self.module.add_global(struct_ty, None, gv_name);
        gv.set_initializer(&initializer);
        gv.set_constant(true);

        let gv_ptr = gv.as_pointer_value();
        let gv_i8 = self
            .builder
            .build_pointer_cast(gv_ptr, self.i8ptr_t, "field_map_i8ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
        Ok(gv_i8)
    }

    // Helper: compute an i8* pointer by adding a 64-bit byte offset to a base i8*.
    // We prefer ptr_to_int + add + int_to_ptr sequences for consistent byte-offset
    // arithmetic across the codegen (avoids subtle differences when using GEP
    // with i8 element indices). `base` must be an i8* pointer value.
    pub(crate) fn i8_ptr_from_offset_i64(
        &self,
        base: inkwell::values::PointerValue<'a>,
        offset: inkwell::values::IntValue<'a>,
        name: &str,
    ) -> Result<inkwell::values::PointerValue<'a>, crate::diagnostics::Diagnostic> {
        // ptrtoint base -> i64
        let obj_ptr_int = self
            .builder
            .build_ptr_to_int(base, self.i64_t, "obj_addr_for_gep")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("ptr_to_int failed"))?;
        // add offset
        let field_addr = self
            .builder
            .build_int_add(obj_ptr_int, offset, "field_addr_for_gep")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
        // inttoptr -> i8*
        let field_ptr = self
            .builder
            .build_int_to_ptr(field_addr, self.i8ptr_t, name)
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
        Ok(field_ptr)
    }

    // Compatibility wrapper: accept a simple slice of BasicValueEnum and treat all
    // fields as strong (is_weak = false). This avoids changing many call sites
    // at once when migrating to the per-field weak flag API.
    pub fn heap_alloc_with_ptr_fields_simple(
        &self,
        field_ptrs: &[inkwell::values::BasicValueEnum<'a>],
    ) -> Result<inkwell::values::PointerValue<'a>, crate::diagnostics::Diagnostic> {
        let mut vec: Vec<(inkwell::values::BasicValueEnum<'a>, bool)> = Vec::new();
        for v in field_ptrs.iter() {
            vec.push((*v, false));
        }
        self.heap_alloc_with_ptr_fields(vec.as_slice())
    }

    // Allocate a closure object with two pointer fields (fn_ptr, env_ptr) and
    // an additional i64 return-type tag stored in the third 8-byte slot. The
    // tag is not included in the field_map (so the collector treats only the
    // first two slots as pointer fields). Returns the i8* object pointer.
    pub fn heap_alloc_closure_with_rettag(
        &self,
        fn_ptr: inkwell::values::BasicValueEnum<'a>,
        env_ptr: inkwell::values::BasicValueEnum<'a>,
        ret_tag: u64,
    ) -> Result<inkwell::values::PointerValue<'a>, crate::diagnostics::Diagnostic> {
        // We'll allocate space for 3 * 8-byte slots after header+meta.
        let header_size = 8u64;
        let meta_slot = 8u64;
        let field_count = 3u64; // include the non-pointer tag slot
        let total_size = header_size + meta_slot + (field_count * 8);

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

        let obj_ptr = self
            .builder
            .build_pointer_cast(malloc_ret, self.i8ptr_t, "obj_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;

        // Initialize header
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
        let _ = self
            .builder
            .build_store(meta_ptr, null_ptr.as_basic_value_enum());

        // Store fn_ptr (idx 0) and env_ptr (idx 1) as pointer fields, and store
        // ret_tag (i64) into idx 2.
        // compute base int
        for (i, slot_val) in [fn_ptr, env_ptr].iter().enumerate() {
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

            // Cast slot_val to i8* if necessary and store
            let mut store_val = *slot_val;
            if slot_val.get_type().is_pointer_type()
                && slot_val.get_type() != self.i8ptr_t.as_basic_type_enum()
                && let inkwell::values::BasicValueEnum::PointerValue(pv) = *slot_val
            {
                let casted = self
                    .builder
                    .build_pointer_cast(pv, self.i8ptr_t, "cast_to_i8ptr")
                    .map_err(|_| crate::diagnostics::Diagnostic::simple("pointer cast failed"))?;
                store_val = casted.as_basic_value_enum();
            }
            let _ = self.builder.build_store(field_ptr_cast, store_val);

            // increment RC for pointer fields
            if slot_val.get_type() == self.i8ptr_t.as_basic_type_enum()
                && let inkwell::values::BasicValueEnum::PointerValue(pv) = *slot_val
            {
                let rc_inc = self.get_rc_inc();
                let _ = self
                    .builder
                    .build_call(rc_inc, &[pv.into()], "rc_inc_closure");
            }
        }

        // store ret_tag into idx 2 as i64
        let ret_offset = header_size + meta_slot + (2u64 * 8);
        let off_ret = self.i64_t.const_int(ret_offset, false);
        let ret_addr = self
            .builder
            .build_int_add(obj_ptr_int, off_ret, "ret_addr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_add failed"))?;
        let ret_ptr_ty = self.context.ptr_type(inkwell::AddressSpace::default());
        let ret_ptr = self
            .builder
            .build_int_to_ptr(ret_addr, ret_ptr_ty, "ret_ptr")
            .map_err(|_| crate::diagnostics::Diagnostic::simple("int_to_ptr failed"))?;
        let ret_const = self.i64_t.const_int(ret_tag, false);
        let _ = self
            .builder
            .build_store(ret_ptr, ret_const.as_basic_value_enum());

        Ok(obj_ptr)
    }
}
