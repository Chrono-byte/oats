//! Constant global value emission.
//!
//! This module handles emitting LLVM globals for compile-time constant values
//! (strings, arrays, objects). It includes interning logic to avoid duplicate
//! globals for identical constant values.

use crate::codegen::CodeGen;
use crate::codegen::const_eval::ConstValue;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::PointerValue;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

impl<'a> CodeGen<'a> {
    /// Emits an LLVM global for a compile-time constant value.
    ///
    /// Returns a pointer to the global's data. Uses interning to avoid
    /// duplicate globals for identical values.
    ///
    /// # Parameters
    /// - `name`: Base name for the global (may be modified for uniqueness)
    /// - `val`: The constant value to emit
    pub fn emit_const_global(
        &self,
        name: &str,
        val: &ConstValue,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        // Helper: stable string key for a ConstValue suitable for interning.
        fn const_value_key(v: &ConstValue) -> String {
            match v {
                ConstValue::Number(n) => format!("N:{:.17}", n),
                ConstValue::F32(n) => format!("F32:{:.17}", n),
                ConstValue::I64(n) => format!("I64:{}", n),
                ConstValue::I32(n) => format!("I32:{}", n),
                ConstValue::I16(n) => format!("I16:{}", n),
                ConstValue::I8(n) => format!("I8:{}", n),
                ConstValue::U64(n) => format!("U64:{}", n),
                ConstValue::U32(n) => format!("U32:{}", n),
                ConstValue::U16(n) => format!("U16:{}", n),
                ConstValue::U8(n) => format!("U8:{}", n),
                ConstValue::Char(c) => format!("C:{}", *c as u32),
                ConstValue::Bool(b) => format!("B:{}", if *b { 1 } else { 0 }),
                ConstValue::Str(s) => {
                    let mut out = String::with_capacity(s.len() * 2 + 2);
                    out.push_str("S:");
                    for &b in s.as_bytes() {
                        out.push_str(&format!("{:02x}", b));
                    }
                    out
                }
                ConstValue::Array(elems) => {
                    let mut parts: Vec<String> = Vec::with_capacity(elems.len());
                    for e in elems {
                        parts.push(const_value_key(e));
                    }
                    format!("A:[{}]", parts.join(","))
                }
                ConstValue::Object(map) => {
                    let mut keys: Vec<&String> = map.keys().collect();
                    keys.sort();
                    let mut parts: Vec<String> = Vec::with_capacity(keys.len());
                    for k in keys {
                        if let Some(v) = map.get(k) {
                            parts.push(format!("{}:{}", k, const_value_key(v)));
                        }
                    }
                    format!("O:{{{}}}", parts.join(","))
                }
            }
        }

        // If already interned, return the existing global
        let key = const_value_key(val);
        if let Some(ptr) = self.const_interns.borrow().get(&key) {
            return Ok(*ptr);
        }

        match val {
            ConstValue::Str(s) => self.emit_const_string(&key, s),
            ConstValue::Array(elems) => self.emit_const_array(&key, name, elems),
            ConstValue::Object(map) => self.emit_const_object(&key, name, map),
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "unsupported const global kind",
            )),
        }
    }

    fn emit_const_string(
        &self,
        key: &str,
        s: &str,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let bytes = s.as_bytes();
        let len = bytes.len();
        let header_t = self.i64_t;
        let len_t = self.i64_t;
        let data_t = self.context.i8_type().array_type((len + 1) as u32);
        let struct_t = self
            .context
            .struct_type(&[header_t.into(), len_t.into(), data_t.into()], false);
        // Use a stable interned global name derived from the key hash.
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        let gname = format!("const.intern.{:016x}", h);
        let gv = self.module.add_global(struct_t, None, &gname);
        let static_header = self.i64_t.const_int(1u64 << 32, false);
        let len_val = self.i64_t.const_int(len as u64, false);
        let data_val = self.context.const_string(bytes, true);
        let init = self.context.const_struct(
            &[static_header.into(), len_val.into(), data_val.into()],
            false,
        );
        gv.set_initializer(&init);
        let pv = gv.as_pointer_value();
        self.const_interns.borrow_mut().insert(key.to_string(), pv);
        Ok(pv)
    }

    fn emit_const_array(
        &self,
        key: &str,
        name: &str,
        elems: &[ConstValue],
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        if elems.is_empty() {
            return self.emit_const_empty_array(key);
        }

        let first = &elems[0];
        match first {
            ConstValue::Number(_) => self.emit_const_number_array(key, elems),
            ConstValue::Str(_) => self.emit_const_string_array(key, name, elems),
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "unsupported const array element type",
            )),
        }
    }

    fn emit_const_empty_array(
        &self,
        key: &str,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let header_t = self.i64_t;
        let len_t = self.i64_t;
        let elem_array_t = self.i8ptr_t.array_type(0);
        let struct_t = self
            .context
            .struct_type(&[header_t.into(), len_t.into(), elem_array_t.into()], false);
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        let gname = format!("const.intern.{:016x}", h);
        let gv = self.module.add_global(struct_t, None, &gname);
        let static_header = self.i64_t.const_int(1u64 << 32, false);
        let len_val = self.i64_t.const_int(0, false);
        let arr = self.i8ptr_t.const_array(&[]);
        let init = self
            .context
            .const_struct(&[static_header.into(), len_val.into(), arr.into()], false);
        gv.set_initializer(&init);
        let pv = gv.as_pointer_value();
        self.const_interns.borrow_mut().insert(key.to_string(), pv);
        Ok(pv)
    }

    fn emit_const_number_array(
        &self,
        key: &str,
        elems: &[ConstValue],
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let elem_count = elems.len();
        let header_t = self.i64_t;
        let len_t = self.i64_t;
        let elem_array_t = self.context.f64_type().array_type(elem_count as u32);
        let struct_t = self
            .context
            .struct_type(&[header_t.into(), len_t.into(), elem_array_t.into()], false);
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        let gname = format!("const.intern.{:016x}", h);
        let gv = self.module.add_global(struct_t, None, &gname);
        let static_header = self.i64_t.const_int(1u64 << 32, false);
        let len_val = self.i64_t.const_int(elem_count as u64, false);
        let mut float_vals = Vec::new();
        for e in elems {
            if let ConstValue::Number(n) = e {
                float_vals.push(self.context.f64_type().const_float(*n));
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "mixed array element types in const array",
                ));
            }
        }
        let arr = self.context.f64_type().const_array(&float_vals);
        let init = self
            .context
            .const_struct(&[static_header.into(), len_val.into(), arr.into()], false);
        gv.set_initializer(&init);
        let pv = gv.as_pointer_value();
        self.const_interns.borrow_mut().insert(key.to_string(), pv);
        Ok(pv)
    }

    fn emit_const_string_array(
        &self,
        key: &str,
        name: &str,
        elems: &[ConstValue],
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let elem_count = elems.len();
        let ptr_t = self.i8ptr_t;
        let array_ty = ptr_t.array_type(elem_count as u32);
        let struct_t = self.context.struct_type(
            &[self.i64_t.into(), self.i64_t.into(), array_ty.into()],
            false,
        );
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        let gname = format!("const.intern.{:016x}", h);
        let gv = self.module.add_global(struct_t, None, &gname);
        let static_header = self.i64_t.const_int(1u64 << 32, false);
        let len_val = self.i64_t.const_int(elem_count as u64, false);
        let mut ptr_vals = Vec::new();
        for (i, e) in elems.iter().enumerate() {
            if let ConstValue::Str(_) = e {
                let child_name = format!("{}_{}_str", name, i);
                let child_ptr = self.emit_const_global(&child_name, e)?;
                ptr_vals.push(child_ptr);
            } else {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "mixed types in const array",
                ));
            }
        }
        let arr_const = self.i8ptr_t.const_array(&ptr_vals);
        let init = self.context.const_struct(
            &[static_header.into(), len_val.into(), arr_const.into()],
            false,
        );
        gv.set_initializer(&init);
        let pv = gv.as_pointer_value();
        self.const_interns.borrow_mut().insert(key.to_string(), pv);
        Ok(pv)
    }

    fn emit_const_object(
        &self,
        key: &str,
        name: &str,
        map: &std::collections::HashMap<String, ConstValue>,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        if map.is_empty() {
            return Err(Diagnostic::simple_boxed(
                Severity::Error,
                "empty const object not supported",
            ));
        }

        let mut keys: Vec<_> = map.keys().cloned().collect();
        keys.sort();

        // Build field types and initializer values
        let mut field_types: Vec<inkwell::types::BasicTypeEnum<'a>> = Vec::new();
        let mut field_vals = Vec::new();

        for k in &keys {
            let v = map.get(k).ok_or_else(|| {
                Diagnostic::simple_boxed(Severity::Error, "key not found in const map")
            })?;
            match v {
                ConstValue::Number(n) => {
                    field_types.push(self.f64_t.into());
                    field_vals.push(self.f64_t.const_float(*n).into());
                }
                ConstValue::F32(n) => {
                    field_types.push(self.f32_t.into());
                    field_vals.push(self.f32_t.const_float(*n as f64).into());
                }
                ConstValue::I64(n) => {
                    field_types.push(self.i64_t.into());
                    field_vals.push(self.i64_t.const_int(*n as u64, true).into());
                }
                ConstValue::I32(n) => {
                    field_types.push(self.i32_t.into());
                    field_vals.push(self.i32_t.const_int(*n as u64, true).into());
                }
                ConstValue::I16(n) => {
                    field_types.push(self.i16_t.into());
                    field_vals.push(self.i16_t.const_int(*n as u64, true).into());
                }
                ConstValue::I8(n) => {
                    field_types.push(self.i8_t.into());
                    field_vals.push(self.i8_t.const_int(*n as u64, true).into());
                }
                ConstValue::U64(n) => {
                    field_types.push(self.i64_t.into());
                    field_vals.push(self.i64_t.const_int(*n, false).into());
                }
                ConstValue::U32(n) => {
                    field_types.push(self.i32_t.into());
                    field_vals.push(self.i32_t.const_int(*n as u64, false).into());
                }
                ConstValue::U16(n) => {
                    field_types.push(self.i16_t.into());
                    field_vals.push(self.i16_t.const_int(*n as u64, false).into());
                }
                ConstValue::U8(n) => {
                    field_types.push(self.i8_t.into());
                    field_vals.push(self.i8_t.const_int(*n as u64, false).into());
                }
                ConstValue::Char(c) => {
                    field_types.push(self.i8_t.into());
                    field_vals.push(self.i8_t.const_int(*c as u64, false).into());
                }
                ConstValue::Bool(b) => {
                    field_types.push(self.bool_t.into());
                    field_vals.push(self.bool_t.const_int(if *b { 1 } else { 0 }, false).into());
                }
                ConstValue::Str(_) | ConstValue::Array(_) | ConstValue::Object(_) => {
                    let child_name = format!("{}_{}", name, k);
                    let child_ptr = self.emit_const_global(&child_name, v)?;
                    field_types.push(self.i8ptr_t.into());
                    field_vals.push(inkwell::values::BasicValueEnum::PointerValue(child_ptr));
                }
            }
        }

        // Build struct type: header + meta_ptr + field types
        let mut members = Vec::new();
        members.push(self.i64_t.into()); // header
        members.push(self.i8ptr_t.into()); // meta slot (pointer)
        members.extend(field_types.iter().cloned());
        let struct_t = self.context.struct_type(&members, false);

        // Compute metadata for pointer fields: offsets (in bytes) from object base
        let mut meta_offsets = Vec::new();
        for (idx, k) in keys.iter().enumerate() {
            let v = map.get(k).ok_or_else(|| {
                Diagnostic::simple_boxed(Severity::Error, "key not found in const map")
            })?;
            match v {
                ConstValue::Str(_) | ConstValue::Array(_) | ConstValue::Object(_) => {
                    let off = 16 + (idx * 8);
                    meta_offsets.push(off as i32);
                }
                _ => {}
            }
        }

        // Emit metadata global if there are pointer fields
        let meta_ptr_val = if !meta_offsets.is_empty() {
            let meta_name = format!("{}_meta", name);
            let offsets_count = meta_offsets.len();
            let offsets_array_t = self.context.i32_type().array_type(offsets_count as u32);
            let meta_struct_t = self
                .context
                .struct_type(&[self.i64_t.into(), offsets_array_t.into()], false);
            let meta_gv = self.module.add_global(meta_struct_t, None, &meta_name);

            let meta_magic: u64 = 0x4F415453u64; // 'OATS'
            let meta0_val = self
                .i64_t
                .const_int((meta_magic << 32) | (offsets_count as u64), false);

            let mut off_ints = Vec::new();
            for &o in &meta_offsets {
                off_ints.push(self.context.i32_type().const_int(o as u64, false));
            }
            let off_array_const = self.context.i32_type().const_array(&off_ints);

            let meta_init = self
                .context
                .const_struct(&[meta0_val.into(), off_array_const.into()], false);
            meta_gv.set_initializer(&meta_init);
            meta_gv.set_constant(true);

            inkwell::values::BasicValueEnum::PointerValue(meta_gv.as_pointer_value())
        } else {
            inkwell::values::BasicValueEnum::PointerValue(self.i8ptr_t.const_null())
        };

        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let h = hasher.finish();
        let gname = format!("const.intern.{:016x}", h);
        let gv = self.module.add_global(struct_t, None, &gname);
        let static_header = self.i64_t.const_int(1u64 << 32, false);

        let mut init_vals = Vec::new();
        init_vals.push(static_header.into());
        init_vals.push(meta_ptr_val);
        for fv in field_vals {
            init_vals.push(fv);
        }

        let init = self.context.const_struct(&init_vals, false);
        gv.set_initializer(&init);
        gv.set_constant(true);
        let pv = gv.as_pointer_value();
        self.const_interns.borrow_mut().insert(key.to_string(), pv);

        Ok(pv)
    }
}
