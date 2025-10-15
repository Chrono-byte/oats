//! Top-level codegen module
//!
//! This module provides the `CodeGen` structure which holds the LLVM
//! `Context`, `Module`, `Builder`, and a number of caches for frequently
//! used LLVM types and runtime helper function declarations. The codegen
//! pipeline is organized across submodules:
//! - `emit` : top-level item lowering (functions, constructors)
//! - `expr` : expression lowering
//! - `stmt` : statement lowering
//! - `helpers` : small re-usable utilities
//!
//! `CodeGen` also exposes getters for runtime helper functions (for
//! example `get_rc_inc`, `get_union_box_f64`) which lazily add declarations
//! to the LLVM module. This centralization ensures consistent ABI types for
//! these helpers and avoids duplicate declarations.

use deno_ast::swc::ast;
use inkwell::AddressSpace;
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub mod const_eval;
pub mod emit;
pub mod escape;
pub mod expr;
pub mod helpers;
pub mod stmt;

use crate::codegen::const_eval::ConstValue;

// Locals are represented as a tuple (ptr, ty, initialized, is_const) in many
// helper modules. Use the same alias here so different files agree on the
// in-memory representation of the locals stack.
// LocalEntry now includes an `is_weak` bool as the fifth element.
// LocalEntry now carries an optional nominal type name as the sixth element.
// LocalEntry now carries an optional OatsType as the seventh element for union tracking.
type LocalEntry<'a> = (
    PointerValue<'a>,               // alloca ptr
    BasicTypeEnum<'a>,              // LLVM type
    bool,                           // is_initialized
    bool,                           // is_const
    bool,                           // is_weak
    Option<String>,                 // nominal type name
    Option<crate::types::OatsType>, // OatsType for unions
);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

// Loop context for tracking break/continue targets
#[derive(Clone)]
pub struct LoopContext<'a> {
    pub continue_block: inkwell::basic_block::BasicBlock<'a>,
    pub break_block: inkwell::basic_block::BasicBlock<'a>,
    // Index into the `locals_stack` marking the first scope that belongs to
    // the loop. When breaking/continuing we should only emit rc_decs for
    // locals added from this index onward.
    pub locals_start: usize,
    // Optional label for labeled break/continue
    pub label: Option<String>,
}

// The main code generation structure, holding the LLVM context, module,
// builder, and various caches for types and functions.
pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub next_str_id: Cell<u32>,
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    pub f64_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub bool_t: inkwell::types::IntType<'a>,
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    pub fn_number_to_string: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_box_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_box_ptr: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_unbox_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_unbox_ptr: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_get_discriminant: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_weak_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_weak_dec: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_weak_upgrade: RefCell<Option<FunctionValue<'a>>>,
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    pub loop_context_stack: RefCell<Vec<LoopContext<'a>>>,
    pub current_label: RefCell<Option<String>>,
    // Optional mapping for the currently-emitted constructor's parent class name.
    // Used to lower `super(...)` calls inside constructors to call the
    // parent's `<Parent>_init(this, ...)` initializer.
    pub current_class_parent: RefCell<Option<String>>,
    // Map local names (alloca/local identifiers) that currently hold a
    // freshly-created closure object to the closure's return type. This lets
    // call-lowering emit statically-typed indirect calls when the callee is a
    // local variable referencing a known closure.
    pub closure_local_rettype: RefCell<HashMap<String, crate::types::OatsType>>,
    // Helper to record the origin local name for the last-lowered expression
    // when that expression was a simple load from a local. Used to propagate
    // closure-local typing across assignments.
    pub last_expr_origin_local: RefCell<Option<String>>,
    // Async-lowering context populated while lowering an `async fn` into
    // a poll-state machine. These are intentionally optional so the
    // expression lowering paths can fall back to the Phase-0 behaviour
    // when not present.
    pub async_await_live_sets: RefCell<Option<Vec<std::collections::HashSet<String>>>>,
    pub async_local_name_to_slot: RefCell<Option<std::collections::HashMap<String, usize>>>,
    pub async_resume_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    pub async_cont_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    pub async_poll_function: RefCell<Option<FunctionValue<'a>>>,
    pub async_await_counter: Cell<u32>,
    pub async_param_count: Cell<u32>,
    pub async_local_slot_count: Cell<usize>,
    // Locals stack from the poll function so resume blocks can access
    // the poll's allocas and restore values into them.
    pub async_poll_locals: RefCell<Option<LocalsStackLocal<'a>>>,
    pub source: &'a str,
    // Byte indices (span.lo) of VarDecl AST nodes that include a `mut`
    // token. Populated by the parser and referenced here so codegen
    // can decide mutability without scanning the source text.
    pub mut_var_decls: std::collections::HashSet<usize>,
    // Track the current function's return type so return statements can box
    // values when the function returns a union type.
    pub current_function_return_type: RefCell<Option<crate::types::OatsType>>,
    // Track whether the last lowered expression is already a boxed union
    // (e.g., from a function call that returns a union). This prevents double-boxing.
    pub last_expr_is_boxed_union: Cell<bool>,
    // Global map for function signatures (parameter and return types).
    pub global_function_signatures:
        RefCell<HashMap<String, (Vec<crate::types::OatsType>, crate::types::OatsType)>>,
    // Symbol table for type information
    pub symbol_table: RefCell<crate::types::SymbolTable>,
    // Map for compile-time evaluated const items: name -> ConstValue
    pub const_items: RefCell<HashMap<String, ConstValue>>,
    // Emitted LLVM globals for complime-time const heap objects (strings/arrays/objects)
    pub const_globals: RefCell<HashMap<String, PointerValue<'a>>>,
    // Interning map for nested/repeated const values: stable key -> emitted global ptr
    pub const_interns: RefCell<HashMap<String, PointerValue<'a>>>,
    // Optional escape analysis info for the currently-lowered function.
    pub current_escape_info: RefCell<Option<crate::codegen::escape::EscapeInfo>>,
    // Nested generic functions collected during lowering. Keyed by the
    // declared name. Value is the AST `Function` and its strictness
    // `FunctionSig` so we can monomorphize per-call-site.
    pub nested_generic_fns:
        RefCell<HashMap<String, (deno_ast::swc::ast::Function, crate::types::FunctionSig)>>,
    // Map of monomorphized specialized function keys -> generated name so
    // we avoid regenerating the same specialization multiple times.
    pub monomorphized_map: RefCell<HashMap<String, String>>,
    // RTA results for optimization
    pub rta_results: Option<crate::rta::RTAResults>,
}

impl<'a> CodeGen<'a> {
    /// Emit an LLVM global for a ConstValue and return a pointer to its data
    pub fn emit_const_global(
        &self,
        name: &str,
        val: &crate::codegen::const_eval::ConstValue,
    ) -> Result<PointerValue<'a>, crate::diagnostics::Diagnostic> {
        use crate::codegen::const_eval::ConstValue;
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        // Helper: stable string key for a ConstValue suitable for interning.
        fn const_value_key(v: &ConstValue) -> String {
            match v {
                ConstValue::Number(n) => format!("N:{:.17}", n),
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
            ConstValue::Str(s) => {
                // reuse logic: create struct [i64 header, i64 len, [i8 x N]]
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
                let zero = self.i32_t.const_int(0, false);
                let two = self.i32_t.const_int(2, false);
                let _indices = &[zero, two, zero];
                let pv = gv.as_pointer_value();
                self.const_interns.borrow_mut().insert(key.clone(), pv);
                Ok(pv)
            }
            ConstValue::Array(elems) => {
                // Only support numeric arrays and string arrays for now. We'll emit
                // a runtime array object: header + i64 len + elements (either f64 or ptrs)
                if elems.is_empty() {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "empty const arrays not supported yet",
                    ));
                }
                // detect element kind
                let first = &elems[0];
                match first {
                    ConstValue::Number(_) => {
                        // create data block: header + len + f64 * N
                        let elem_count = elems.len();
                        let header_t = self.i64_t;
                        let len_t = self.i64_t;
                        let elem_array_t = self.context.f64_type().array_type(elem_count as u32);
                        let struct_t = self.context.struct_type(
                            &[header_t.into(), len_t.into(), elem_array_t.into()],
                            false,
                        );
                        let mut hasher = DefaultHasher::new();
                        key.hash(&mut hasher);
                        let h = hasher.finish();
                        let gname = format!("const.intern.{:016x}", h);
                        let gv = self.module.add_global(struct_t, None, &gname);
                        let static_header = self.i64_t.const_int(1u64 << 32, false);
                        let len_val = self.i64_t.const_int(elem_count as u64, false);
                        let mut float_vals: Vec<inkwell::values::FloatValue> = Vec::new();
                        for e in elems {
                            if let ConstValue::Number(n) = e {
                                float_vals.push(self.context.f64_type().const_float(*n));
                            } else {
                                return Err(crate::diagnostics::Diagnostic::simple(
                                    "mixed array element types in const array",
                                ));
                            }
                        }
                        let arr = self.context.f64_type().const_array(&float_vals);
                        let init = self.context.const_struct(
                            &[static_header.into(), len_val.into(), arr.into()],
                            false,
                        );
                        gv.set_initializer(&init);
                        let zero = self.i32_t.const_int(0, false);
                        let two = self.i32_t.const_int(2, false);
                        let _indices = &[zero, two, zero];
                        let pv = gv.as_pointer_value();
                        self.const_interns.borrow_mut().insert(key.clone(), pv);
                        Ok(pv)
                    }
                    ConstValue::Str(_) => {
                        // For string arrays, allocate an array of i8* pointers with each element pointing to a const string global.
                        let elem_count = elems.len();
                        let ptr_t = self.i8ptr_t;
                        let array_ty = ptr_t.array_type(elem_count as u32);
                        // Create a struct type: header + len + pointer-array
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
                        // For each string element, recursively emit a string global and collect its pointer
                        let mut ptr_vals: Vec<inkwell::values::PointerValue> = Vec::new();
                        for (i, e) in elems.iter().enumerate() {
                            if let ConstValue::Str(_) = e {
                                let child_name = format!("{}_{}_str", name, i);
                                let child_ptr = self.emit_const_global(&child_name, e)?;
                                ptr_vals.push(child_ptr);
                            } else {
                                return Err(crate::diagnostics::Diagnostic::simple(
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
                        let zero = self.i32_t.const_int(0, false);
                        let two = self.i32_t.const_int(2, false);
                        let _indices = &[zero, two, zero];
                        let pv = gv.as_pointer_value();
                        self.const_interns.borrow_mut().insert(key.clone(), pv);
                        Ok(pv)
                    }
                    _ => Err(crate::diagnostics::Diagnostic::simple(
                        "unsupported const array element type",
                    )),
                }
            }
            ConstValue::Object(map) => {
                // Emit a nominal object: [i64 header][i64 meta_ptr][fields...]
                // Fields may be numbers (f64) or pointers (i8*). We emit a
                // deterministic field order by sorting keys.
                if map.is_empty() {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "empty const object not supported",
                    ));
                }

                let mut keys: Vec<String> = map.keys().cloned().collect();
                keys.sort();

                // Build field types and initializer values
                let mut field_types: Vec<inkwell::types::BasicTypeEnum> = Vec::new();
                let mut field_vals: Vec<inkwell::values::BasicValueEnum> = Vec::new();

                for k in &keys {
                    let v = map.get(k).unwrap();
                    match v {
                        ConstValue::Number(n) => {
                            field_types.push(self.f64_t.into());
                            field_vals.push(self.f64_t.const_float(*n).into());
                        }
                        ConstValue::Str(_) | ConstValue::Array(_) | ConstValue::Object(_) => {
                            // Recursively emit child globals for pointer-like fields
                            let child_name = format!("{}_{}", name, k);
                            let child_ptr = self.emit_const_global(&child_name, v)?;
                            field_types.push(self.i8ptr_t.into());
                            field_vals
                                .push(inkwell::values::BasicValueEnum::PointerValue(child_ptr));
                        }
                        ConstValue::Bool(b) => {
                            // Represent bools as i1/i8? Use i64 slot sized as f64 for simplicity
                            // We'll store as i64 0/1 in an i64 slot to keep layout simple.
                            field_types.push(self.i64_t.into());
                            let iv = self.i64_t.const_int(if *b { 1 } else { 0 }, false);
                            field_vals.push(iv.into());
                        }
                    }
                }

                // Build struct type: header + meta_ptr + field types
                let mut members: Vec<inkwell::types::BasicTypeEnum> = Vec::new();
                members.push(self.i64_t.into()); // header
                members.push(self.i8ptr_t.into()); // meta slot (pointer)
                members.extend(field_types.iter().cloned());
                let struct_t = self.context.struct_type(&members, false);

                // Compute metadata for pointer fields: offsets (in bytes) from object base
                // Pointer fields start at offset 16 (header 8 + meta 8), each field is 8 bytes
                let mut meta_offsets: Vec<i32> = Vec::new();
                for (idx, k) in keys.iter().enumerate() {
                    let v = map.get(k).unwrap();
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
                    // meta name
                    let meta_name = format!("{}_meta", name);
                    // meta struct: [i64 meta0, [i32 offsets[len]]]
                    let offsets_count = meta_offsets.len();
                    let offsets_array_t = self.context.i32_type().array_type(offsets_count as u32);
                    let meta_struct_t = self
                        .context
                        .struct_type(&[self.i64_t.into(), offsets_array_t.into()], false);
                    let meta_gv = self.module.add_global(meta_struct_t, None, &meta_name);

                    // meta0 = (META_MAGIC << 32) | len
                    let meta_magic: u64 = 0x4F415453u64; // 'OATS'
                    let meta0_val = self
                        .i64_t
                        .const_int((meta_magic << 32) | (offsets_count as u64), false);

                    // Build i32 const array for offsets
                    let mut off_ints: Vec<inkwell::values::IntValue> = Vec::new();
                    for &o in &meta_offsets {
                        off_ints.push(self.context.i32_type().const_int(o as u64, false));
                    }
                    let off_array_const = self.context.i32_type().const_array(&off_ints);

                    let meta_init = self
                        .context
                        .const_struct(&[meta0_val.into(), off_array_const.into()], false);
                    meta_gv.set_initializer(&meta_init);
                    meta_gv.set_constant(true);

                    // Return as a pointer value to store in object meta slot
                    inkwell::values::BasicValueEnum::PointerValue(meta_gv.as_pointer_value())
                } else {
                    // No pointer fields -> null meta
                    inkwell::values::BasicValueEnum::PointerValue(self.i8ptr_t.const_null())
                };

                let mut hasher = DefaultHasher::new();
                key.hash(&mut hasher);
                let h = hasher.finish();
                let gname = format!("const.intern.{:016x}", h);
                let gv = self.module.add_global(struct_t, None, &gname);
                let static_header = self.i64_t.const_int(1u64 << 32, false);

                // Build initializer array: header, meta_ptr, fields...
                let mut init_vals: Vec<inkwell::values::BasicValueEnum> = Vec::new();
                init_vals.push(static_header.into());
                init_vals.push(meta_ptr_val);
                for fv in field_vals {
                    init_vals.push(fv);
                }

                let init = self.context.const_struct(&init_vals, false);
                gv.set_initializer(&init);
                gv.set_constant(true);
                let pv = gv.as_pointer_value();
                self.const_interns.borrow_mut().insert(key.clone(), pv);

                // Return the global's base pointer as a PointerValue
                Ok(pv)
            }

            _ => Err(crate::diagnostics::Diagnostic::simple(
                "unsupported const global kind",
            )),
        }
    }
    // --- Runtime Helper Function Getters ---

    fn get_array_alloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_array_alloc.borrow() {
            return f;
        }
        let fn_type = self.i8ptr_t.fn_type(
            &[self.i64_t.into(), self.i32_t.into(), self.i32_t.into()],
            false,
        );
        let f = self.module.add_function("array_alloc", fn_type, None);
        *self.fn_array_alloc.borrow_mut() = Some(f);
        f
    }

    pub fn get_rc_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_inc.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_inc", fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    /// Decide whether RC operations for a named local can be elided.
    /// Controlled by the environment variable `OATS_ELIDE_ARC=1` and the
    /// optional `current_escape_info` computed before lowering the function.
    pub fn should_elide_rc_for_local(&self, local_name: &str) -> bool {
        if std::env::var("OATS_ELIDE_ARC").unwrap_or_default() != "1" {
            return false;
        }
        if let Some(info) = &*self.current_escape_info.borrow() {
            // elide only when the local is present and NOT marked as escaping
            return !info.escapes(local_name);
        }
        false
    }

    fn get_promise_resolve(&self) -> FunctionValue<'a> {
        // promise_resolve(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("promise_resolve")
            .unwrap_or_else(|| self.module.add_function("promise_resolve", fn_type, None))
    }

    fn get_promise_poll_into(&self) -> FunctionValue<'a> {
        // promise_poll_into(i8*, i8*) -> i32
        let fn_type = self
            .i32_t
            .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
        self.module
            .get_function("promise_poll_into")
            .unwrap_or_else(|| self.module.add_function("promise_poll_into", fn_type, None))
    }

    #[allow(dead_code)]
    fn get_promise_new_from_state(&self) -> FunctionValue<'a> {
        // promise_new_from_state(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("promise_new_from_state")
            .unwrap_or_else(|| {
                self.module
                    .add_function("promise_new_from_state", fn_type, None)
            })
    }

    #[allow(dead_code)]
    fn get_executor_enqueue(&self) -> FunctionValue<'a> {
        // executor_enqueue(i8*) -> void
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("executor_enqueue")
            .unwrap_or_else(|| self.module.add_function("executor_enqueue", fn_type, None))
    }

    #[allow(dead_code)]
    fn get_executor_run(&self) -> FunctionValue<'a> {
        // executor_run() -> void
        let fn_type = self.context.void_type().fn_type(&[], false);
        self.module
            .get_function("executor_run")
            .unwrap_or_else(|| self.module.add_function("executor_run", fn_type, None))
    }

    #[allow(dead_code)]
    fn get_waker_create_for_task(&self) -> FunctionValue<'a> {
        // waker_create_for_task(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("waker_create_for_task")
            .unwrap_or_else(|| {
                self.module
                    .add_function("waker_create_for_task", fn_type, None)
            })
    }

    #[allow(dead_code)]
    fn get_waker_wake(&self) -> FunctionValue<'a> {
        // waker_wake(i8*) -> void
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("waker_wake")
            .unwrap_or_else(|| self.module.add_function("waker_wake", fn_type, None))
    }

    fn get_math_random(&self) -> FunctionValue<'a> {
        self.module.get_function("math_random").unwrap_or_else(|| {
            let fn_type = self.f64_t.fn_type(&[], false);
            self.module.add_function("math_random", fn_type, None)
        })
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_dec", fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_number_to_string(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_to_string.borrow() {
            return f;
        }
        // number_to_string(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function("number_to_string", fn_type, None);
        *self.fn_number_to_string.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_f64.borrow() {
            return f;
        }
        // union_box_f64(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function("union_box_f64", fn_type, None);
        *self.fn_union_box_f64.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_ptr.borrow() {
            return f;
        }
        // union_box_ptr(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_box_ptr", fn_type, None);
        *self.fn_union_box_ptr.borrow_mut() = Some(f);
        f
    }

    fn get_union_unbox_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_unbox_f64.borrow() {
            return f;
        }
        // union_unbox_f64(i8*) -> f64
        let fn_type = self.f64_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_unbox_f64", fn_type, None);
        *self.fn_union_unbox_f64.borrow_mut() = Some(f);
        f
    }

    /// Emit an unconditional branch to `bb` only if the current insertion
    /// block does not already have a terminator. This guards against
    /// accidentally emitting duplicate branch instructions when lowering
    /// constructs that may have emitted branches earlier.
    pub(crate) fn ensure_unconditional_branch(&self, bb: inkwell::basic_block::BasicBlock<'a>) {
        if let Some(cur) = self.builder.get_insert_block()
            && cur.get_terminator().is_none()
        {
            let _ = self.builder.build_unconditional_branch(bb);
        }
    }

    fn get_union_unbox_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_unbox_ptr.borrow() {
            return f;
        }
        // union_unbox_ptr(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_unbox_ptr", fn_type, None);
        *self.fn_union_unbox_ptr.borrow_mut() = Some(f);
        f
    }

    fn get_union_get_discriminant(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_get_discriminant.borrow() {
            return f;
        }
        // declare union_get_discriminant(i8*) -> i64
        let fn_type = self.i64_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self
            .module
            .add_function("union_get_discriminant", fn_type, None);
        *self.fn_union_get_discriminant.borrow_mut() = Some(f);
        f
    }

    fn get_rc_weak_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_weak_inc.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_weak_inc", fn_type, None);
        *self.fn_rc_weak_inc.borrow_mut() = Some(f);
        f
    }

    fn get_rc_weak_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_weak_dec.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_weak_dec", fn_type, None);
        *self.fn_rc_weak_dec.borrow_mut() = Some(f);
        f
    }

    fn get_rc_weak_upgrade(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_weak_upgrade.borrow() {
            return f;
        }
        // rc_weak_upgrade(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_weak_upgrade", fn_type, None);
        *self.fn_rc_weak_upgrade.borrow_mut() = Some(f);
        f
    }

    fn get_array_get_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_f64")
            .unwrap_or_else(|| {
                let fn_type = self
                    .f64_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_f64", fn_type, None)
            })
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_ptr")
            .unwrap_or_else(|| {
                let fn_type = self
                    .i8ptr_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_ptr", fn_type, None)
            })
    }

    fn get_array_push_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_push_f64")
            .unwrap_or_else(|| {
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[self.i8ptr_t.into(), self.f64_t.into()], false);
                self.module.add_function("array_push_f64", fn_type, None)
            })
    }

    fn get_array_pop_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_pop_f64")
            .unwrap_or_else(|| {
                let fn_type = self.f64_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_pop_f64", fn_type, None)
            })
    }

    fn get_array_push_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_push_ptr")
            .unwrap_or_else(|| {
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
                self.module.add_function("array_push_ptr", fn_type, None)
            })
    }

    #[allow(dead_code)]
    fn get_array_push_ptr_weak(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_push_ptr_weak")
            .unwrap_or_else(|| {
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
                self.module
                    .add_function("array_push_ptr_weak", fn_type, None)
            })
    }

    fn get_array_pop_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_pop_ptr")
            .unwrap_or_else(|| {
                let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_pop_ptr", fn_type, None)
            })
    }

    fn get_array_set_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_set_f64")
            .unwrap_or_else(|| {
                // array_set_f64(arr_ptr: i8**, idx: i64, v: f64) -> void
                // Takes pointer-to-pointer to allow reallocation
                // Represent i8** as pointer-to (i8*) type
                // Build i8** by asking the Context for a pointer type in the default address space
                let ptr_ptr_t = self.context.ptr_type(AddressSpace::default());
                let fn_type = self.context.void_type().fn_type(
                    &[ptr_ptr_t.into(), self.i64_t.into(), self.f64_t.into()],
                    false,
                );
                self.module.add_function("array_set_f64", fn_type, None)
            })
    }

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_set_ptr")
            .unwrap_or_else(|| {
                // array_set_ptr(arr_ptr: i8**, idx: i64, p: i8*) -> void
                // Takes pointer-to-pointer to allow reallocation
                // Represent i8** as pointer-to (i8*) type
                // Build i8** by asking the Context for a pointer type in the default address space
                let ptr_ptr_t = self.context.ptr_type(AddressSpace::default());
                let fn_type = self.context.void_type().fn_type(
                    &[ptr_ptr_t.into(), self.i64_t.into(), self.i8ptr_t.into()],
                    false,
                );
                self.module.add_function("array_set_ptr", fn_type, None)
            })
    }
    #[allow(dead_code)]
    fn get_array_set_ptr_weak(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_set_ptr_weak")
            .unwrap_or_else(|| {
                // array_set_ptr_weak(arr: i8*, idx: i64, p: i8*) -> void
                let fn_type = self.context.void_type().fn_type(
                    &[self.i8ptr_t.into(), self.i64_t.into(), self.i8ptr_t.into()],
                    false,
                );
                self.module
                    .add_function("array_set_ptr_weak", fn_type, None)
            })
    }

    // --- Main Function Generation ---

    // Function generation moved to emit.rs

    // --- Function Generation Helpers ---

    // Helper to build the LLVM function type from parameter and return types.
    fn build_llvm_fn_type(
        &self,
        llvm_param_types: &[BasicTypeEnum<'a>],
        ret_type: &crate::types::OatsType,
    ) -> inkwell::types::FunctionType<'a> {
        let args: Vec<_> = llvm_param_types.iter().map(|&t| t.into()).collect();
        match ret_type {
            crate::types::OatsType::Void => self.context.void_type().fn_type(&args, false),
            _ => self.map_type_to_llvm(ret_type).fn_type(&args, false),
        }
    }

    // Intern a string literal in the module and return a pointer to the data
    // section (same layout as Lit::Str handling in expression lowering). This
    // reuses the `string_literals` cache so identical literals share the same
    // global and pointer value.
    pub fn intern_string_literal(&self, s: &str) -> inkwell::values::PointerValue<'a> {
        // Check cache first
        if let Some(ptr) = self.string_literals.borrow().get(s) {
            return *ptr;
        }

        let bytes = s.as_bytes();
        let str_len = bytes.len();
        let header_ty = self.i64_t;
        let len_ty = self.i64_t;
        let data_ty = self.context.i8_type().array_type((str_len + 1) as u32);
        let struct_ty = self
            .context
            .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

        let id = self.next_str_id.get();
        let name = format!("strlit.{}", id);
        self.next_str_id.set(id.wrapping_add(1));
        let gv = self.module.add_global(struct_ty, None, &name);

        // static header bit set at bit 32
        let static_header = self.i64_t.const_int(1u64 << 32, false);
        let length_val = self.i64_t.const_int(str_len as u64, false);
        let data_val = self.context.const_string(bytes, true);

        let initializer = self.context.const_struct(
            &[static_header.into(), length_val.into(), data_val.into()],
            false,
        );
        gv.set_initializer(&initializer);

        // Return pointer to data section (field index 2)
        let zero = self.i32_t.const_int(0, false);
        let two = self.i32_t.const_int(2, false);
        let indices = &[zero, two];
        let gep = unsafe {
            self.builder
                .build_gep(struct_ty, gv.as_pointer_value(), indices, "strptr")
        };
        if let Ok(ptr) = gep {
            self.string_literals.borrow_mut().insert(s.to_string(), ptr);
            return ptr;
        }

        // Fallback: null pointer (shouldn't happen)
        self.context
            .ptr_type(inkwell::AddressSpace::default())
            .const_null()
    }

    // Creates stack allocations (`alloca`) for all function parameters, making them
    // accessible like local variables and handling initial reference counting.
    fn create_param_allocas(
        &self,
        function: FunctionValue<'a>,
        func_decl: &ast::Function,
        llvm_param_types: &[BasicTypeEnum<'a>],
        receiver_name: Option<&str>,
    ) -> Result<(HashMap<String, u32>, LocalsStackLocal<'a>), crate::diagnostics::Diagnostic> {
        let mut param_map = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            use deno_ast::swc::ast;
            match &p.pat {
                ast::Pat::Ident(ident) => {
                    let name = ident.id.sym.to_string();
                    let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                    param_map.insert(name, idx);
                }
                // Support default parameters like `x = 0` where the pattern is an Assign
                ast::Pat::Assign(assign) => {
                    if let ast::Pat::Ident(ident) = &*assign.left {
                        let name = ident.id.sym.to_string();
                        let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                        param_map.insert(name, idx);
                    }
                }
                _ => {}
            }
        }

        // Initialize an empty locals stack. Allocas for `let` bindings will be added later.
        let locals_stack: LocalsStackLocal<'a> = vec![HashMap::new()];

        // We no longer create allocas for parameters. Still, for parameters
        // that have a union type and require pointer ABI, box numeric params
        // into union objects and increment RC on the boxed value. We consult
        // `fn_param_types` map to find the declared parameter types for this
        // function (if present).
        for idx_ref in param_map.values() {
            let idx = *idx_ref;
            if let Some(param_types) = self
                .fn_param_types
                .borrow()
                .get(function.get_name().to_str().unwrap_or(""))
            {
                let idx_usize = idx as usize;
                if idx_usize < param_types.len() {
                    let pty = &param_types[idx_usize];
                    if let crate::types::OatsType::Union(parts) = pty {
                        // If any part is pointer-like, the ABI for this param is i8*
                        let any_ptr = parts.iter().any(|p| {
                            matches!(
                                p,
                                crate::types::OatsType::String
                                    | crate::types::OatsType::NominalStruct(_)
                                    | crate::types::OatsType::Array(_)
                                    | crate::types::OatsType::Promise(_)
                            )
                        });
                        if any_ptr {
                            // Get the raw argument
                            if let Some(arg_val) = function.get_nth_param(idx) {
                                // If arg is float, box it
                                if arg_val.get_type().is_float_type() {
                                    let box_fn = self.get_union_box_f64();
                                    let cs = self.builder.build_call(
                                        box_fn,
                                        &[arg_val.into()],
                                        "union_box_f64_param",
                                    );
                                    if let Ok(cs) = cs
                                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                        && let inkwell::values::BasicValueEnum::PointerValue(pv) =
                                            bv
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_param",
                                        );
                                    }
                                } else if arg_val.get_type().is_pointer_type() {
                                    // box pointer payload
                                    let box_fn = self.get_union_box_ptr();
                                    let cs = self.builder.build_call(
                                        box_fn,
                                        &[arg_val.into()],
                                        "union_box_ptr_param",
                                    );
                                    if let Ok(cs) = cs
                                        && let inkwell::Either::Left(bv) = cs.try_as_basic_value()
                                        && let inkwell::values::BasicValueEnum::PointerValue(pv) =
                                            bv
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        let _ = self.builder.build_call(
                                            rc_inc,
                                            &[pv.into()],
                                            "rc_inc_param",
                                        );
                                    }
                                }
                            }
                        } else {
                            // union of numbers -> nothing special (f64 ABI)
                        }
                    } else {
                        // non-union types: previous behavior only incremented RC for pointer types
                        if let Some(param_ty) = llvm_param_types.get(idx as usize)
                            && param_ty.is_pointer_type()
                            && let Some(pv) = function.get_nth_param(idx)
                        {
                            let rc_inc = self.get_rc_inc();
                            if self
                                .builder
                                .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                                .is_err()
                            {
                                return Err(crate::diagnostics::Diagnostic::simple(
                                    "rc_inc param call failed",
                                ));
                            }
                        }
                    }
                }
            }
        }
        Ok((param_map, locals_stack))
    }

    // Lowers a slice of AST statements into the current basic block.
    // Statement lowering functions have been moved to stmt.rs

    // --- Host Main Generation ---

    pub fn emit_host_main(
        &self,
        _params: &[crate::types::OatsType],
        _ret: &crate::types::OatsType,
    ) -> bool {
        // Emit a simple C-compatible `main` function that calls the
        // generated `oats_main` symbol. This covers the common case where
        // user scripts export `function main(): number` with no params.
        // Return `true` to indicate we emitted a host main so the driver
        // will skip linking an external `rt_main.o`.

        // Only handle the simple case: no params, integer return or void.
        // Build: int main(int argc, char** argv) { int r = oats_main(); return r; }
        let i32_t = self.i32_t;
        let i8ptr_t = self.i8ptr_t;

        // Build function type: i32 (int) with (i32, i8**) params
        let fn_type = i32_t.fn_type(&[i32_t.into(), i8ptr_t.as_basic_type_enum().into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => {
                // No oats_main available; emit an empty main that returns 1
                let const_one = i32_t.const_int(1, false);
                let _ = self.builder.build_return(Some(&const_one));
                return true;
            }
        };

        // Call oats_main(); we only support a no-arg oats_main here.
        let call_site = match self.builder.build_call(oats_main_fn, &[], "call_oats_main") {
            Ok(cs) => cs,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple("failed to build call to oats_main"),
                    Some(self.source),
                );
                let const_zero = i32_t.const_int(0, false);
                let _ = self.builder.build_return(Some(&const_zero));
                return true;
            }
        };

        // Run the async executor to process any enqueued promises
        let executor_run_fn = self.get_executor_run();
        let _ = self
            .builder
            .build_call(executor_run_fn, &[], "call_executor");

        // Interpret the result depending on its type. If the function returns an i64 or f64
        // we coerce/truncate to i32; if void, return 0.
        let either = call_site.try_as_basic_value();
        if let inkwell::Either::Left(bv) = either {
            let ret_val = if bv.get_type().is_int_type() {
                // Truncate or bitcast to i32 if needed
                let rv_int = bv.into_int_value();
                let cast = match self
                    .builder
                    .build_int_truncate_or_bit_cast(rv_int, i32_t, "ret_i32")
                {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "int cast failed when building host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(cast)
            } else if bv.get_type().is_float_type() {
                // cast float to i32 via fptosi
                let fv = bv.into_float_value();
                let conv = match self.builder.build_float_to_signed_int(fv, i32_t, "f_to_i") {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "float->int conversion failed in host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(conv)
            } else if bv.get_type().is_pointer_type() {
                // pointer return -> return 0
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            } else {
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            };
            let _ = self.builder.build_return(Some(&ret_val));
        } else {
            // No basic return (void), return 0
            let const_zero = i32_t.const_int(0, false);
            let _ = self.builder.build_return(Some(&const_zero));
        }

        true
    }

    // Generate a complete constructor function for a class.
    // The constructor allocates memory for the header + fields, initializes the header
    // with refcount=1, and runs the constructor body (which may initialize fields via
    // assignments or use constructor parameters).
    //
    // # Arguments
    // * `class_name` - Name of the class (e.g., "Point")
    // * `ctor` - The constructor AST node
    // * `fields` - Ordered list of (field_name, field_type) tuples
    //
    // The emitted function signature is:
    //   `ClassName_ctor(param1, param2, ...) -> i8*`

    fn get_malloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_malloc.borrow() {
            return f;
        }
        let fn_type = self.i8ptr_t.fn_type(&[self.i64_t.into()], false);
        let f = self.module.add_function("malloc", fn_type, None);
        *self.fn_malloc.borrow_mut() = Some(f);
        f
    }

    // Specialize a generic type based on type parameters and arguments
    pub fn specialize_generic(
        &self,
        _type_params: &[crate::types::OatsType],
        _args: &[deno_ast::swc::ast::ExprOrSpread],
    ) -> Result<BasicValueEnum<'a>, crate::diagnostics::Diagnostic> {
        // Logic to specialize the generic type
        Ok(self
            .context
            .ptr_type(AddressSpace::default())
            .const_null()
            .into())
    }

    /// Infer return type from arrow function body
    pub fn infer_return_type_from_arrow_body(
        &self,
        body: &deno_ast::swc::ast::BlockStmtOrExpr,
    ) -> Result<crate::types::OatsType, crate::diagnostics::Diagnostic> {
        match body {
            deno_ast::swc::ast::BlockStmtOrExpr::Expr(expr) => {
                // Simple inference for single expression body
                self.infer_type_from_expr(expr)
            }
            deno_ast::swc::ast::BlockStmtOrExpr::BlockStmt(_block) => {
                // For block, look for return statements
                // For simplicity, default to Number if no returns or complex
                // TODO: Implement full inference from return statements
                Ok(crate::types::OatsType::Number)
            }
        }
    }

    /// Simple type inference from expression
    fn infer_type_from_expr(
        &self,
        expr: &deno_ast::swc::ast::Expr,
    ) -> Result<crate::types::OatsType, crate::diagnostics::Diagnostic> {
        match expr {
            deno_ast::swc::ast::Expr::Lit(lit) => match lit {
                deno_ast::swc::ast::Lit::Num(_) => Ok(crate::types::OatsType::Number),
                deno_ast::swc::ast::Lit::Str(_) => Ok(crate::types::OatsType::String),
                deno_ast::swc::ast::Lit::Bool(_) => Ok(crate::types::OatsType::Number), // bool as number
                _ => Ok(crate::types::OatsType::Number),                                // default
            },
            deno_ast::swc::ast::Expr::Ident(_) => Ok(crate::types::OatsType::Number), // unknown
            _ => Ok(crate::types::OatsType::Number), // default for complex expr
        }
    }
}
