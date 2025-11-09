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
//! - `const_eval` : compile-time constant evaluation
//! - `escape` : escape analysis for RC optimization
//!
//! `CodeGen` also exposes getters for runtime helper functions (for
//! example `get_rc_inc`, `get_union_box_f64`) which lazily add declarations
//! to the LLVM module. This centralization ensures consistent ABI types for
//! these helpers and avoids duplicate declarations.
//!
//! # RTA Integration
//!
//! The `rta_results` field stores Rapid Type Analysis results which are used
//! to optimize code generation:
//! - Dead code elimination: Methods that are not reachable are skipped
//! - Future: Devirtualization, method inlining decisions
//!
//! # Memory Management
//!
//! Code generation emits reference counting operations (`rc_inc`/`rc_dec`) for
//! heap-allocated objects. Escape analysis (`escape.rs`) can elide unnecessary
//! RC operations for locals that don't escape the function scope.

use crate::diagnostics::Severity;
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
pub mod utils;

use crate::codegen::const_eval::ConstValue;

// Locals are represented as a tuple (ptr, ty, initialized, is_const) in many
// helper modules. Use the same alias here so different files agree on the
// in-memory representation of the locals stack.
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
    pub f32_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub i16_t: inkwell::types::IntType<'a>,
    pub i8_t: inkwell::types::IntType<'a>,
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
    // Enum metadata: enum name -> (variant names, variant field types)
    pub enum_variants: RefCell<HashMap<String, Vec<(String, Vec<crate::types::OatsType>)>>>,
    // Class hierarchy: class name -> parent class name (None if no parent)
    pub class_parents: RefCell<HashMap<String, Option<String>>>,
    // Type aliases: alias name -> (type parameters, aliased type)
    // For non-generic aliases, type_params is None
    pub type_aliases: RefCell<HashMap<String, (Option<Vec<String>>, oats_ast::TsType)>>,
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
    // Cache for declared external std functions
    pub external_std_fns: RefCell<HashMap<String, FunctionValue<'a>>>,
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
        RefCell<HashMap<String, (oats_ast::Function, crate::types::FunctionSig)>>,
    // Map of monomorphized specialized function keys -> generated name so
    // we avoid regenerating the same specialization multiple times.
    pub monomorphized_map: RefCell<HashMap<String, String>>,
    // RTA results for optimization
    pub rta_results: Option<crate::rta::RTAResults>,
    // Track whether this compilation unit uses async/await features
    pub uses_async: Cell<bool>,
}

impl<'a> CodeGen<'a> {
    /// Emit an LLVM global for a ConstValue and return a pointer to its data
    pub fn emit_const_global(
        &self,
        name: &str,
        val: &crate::codegen::const_eval::ConstValue,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        use crate::codegen::const_eval::ConstValue;
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

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
                    // For empty arrays, emit a global with len=0 and empty data
                    // Assume pointer array since we don't know the element type
                    let header_t = self.i64_t;
                    let len_t = self.i64_t;
                    // Empty array of pointers
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
                    self.const_interns.borrow_mut().insert(key.clone(), pv);
                    Ok(pv)
                } else {
                    // detect element kind
                    let first = &elems[0];
                    match first {
                        ConstValue::Number(_) => {
                            // create data block: header + len + f64 * N
                            let elem_count = elems.len();
                            let header_t = self.i64_t;
                            let len_t = self.i64_t;
                            let elem_array_t =
                                self.context.f64_type().array_type(elem_count as u32);
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
                                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                        Severity::Error,
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
                                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
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
                            let zero = self.i32_t.const_int(0, false);
                            let two = self.i32_t.const_int(2, false);
                            let _indices = &[zero, two, zero];
                            let pv = gv.as_pointer_value();
                            self.const_interns.borrow_mut().insert(key.clone(), pv);
                            Ok(pv)
                        }
                        _ => Err(crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "unsupported const array element type",
                        )),
                    }
                }
            }
            ConstValue::Object(map) => {
                // Emit a nominal object: [i64 header][i64 meta_ptr][fields...]
                // Fields may be numbers (f64) or pointers (i8*). We emit a
                // deterministic field order by sorting keys.
                if map.is_empty() {
                    return Err(crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "empty const object not supported",
                    ));
                }

                let mut keys: Vec<String> = map.keys().cloned().collect();
                keys.sort();

                // Build field types and initializer values
                let mut field_types: Vec<inkwell::types::BasicTypeEnum> = Vec::new();
                let mut field_vals: Vec<inkwell::values::BasicValueEnum> = Vec::new();

                for k in &keys {
                    let v = map.get(k).ok_or_else(|| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "key not found in const map",
                        )
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
                            field_vals
                                .push(self.bool_t.const_int(if *b { 1 } else { 0 }, false).into());
                        }
                        ConstValue::Str(_) | ConstValue::Array(_) | ConstValue::Object(_) => {
                            // Recursively emit child globals for pointer-like fields
                            let child_name = format!("{}_{}", name, k);
                            let child_ptr = self.emit_const_global(&child_name, v)?;
                            field_types.push(self.i8ptr_t.into());
                            field_vals
                                .push(inkwell::values::BasicValueEnum::PointerValue(child_ptr));
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
                    let v = map.get(k).ok_or_else(|| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "key not found in const map",
                        )
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

            _ => Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
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
        let f =
            self.module
                .add_function(crate::runtime_functions::names::ARRAY_ALLOC, fn_type, None);
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
        let f = self
            .module
            .add_function(crate::runtime_functions::names::RC_INC, fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    /// Decide whether RC operations for a named local can be elided.
    /// Controlled by the environment variable `OATS_ELIDE_ARC=1` and the
    /// optional `current_escape_info` computed before lowering the function.
    pub fn should_elide_rc_for_local(&self, local_name: &str) -> bool {
        // Use escape analysis to determine if we can elide RC ops for this local.
        // If a variable doesn't escape the function scope, we skip both rc_inc
        // and rc_dec operations since the variable will be cleaned up automatically
        // by LLVM at function exit.
        if let Some(info) = &*self.current_escape_info.borrow() {
            // elide only when the local is present and NOT marked as escaping
            return !info.escapes(local_name);
        }
        // If no escape info available, be conservative and don't elide
        false
    }

    pub fn is_unowned_local(&self, locals: &LocalsStackLocal<'a>, local_name: &str) -> bool {
        // Check if the local is an unowned reference
        for scope in locals.iter().rev() {
            if let Some((_ptr, _ty, _init, _is_const, _is_weak, _nominal, oats_type)) =
                scope.get(local_name)
            {
                return matches!(oats_type, Some(crate::types::OatsType::Unowned(_)));
            }
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

    fn get_executor_run(&self) -> FunctionValue<'a> {
        // executor_run() -> void
        let fn_type = self.context.void_type().fn_type(&[], false);
        self.module
            .get_function("executor_run")
            .unwrap_or_else(|| self.module.add_function("executor_run", fn_type, None))
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self
            .module
            .add_function(crate::runtime_functions::names::RC_DEC, fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_number_to_string(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_to_string.borrow() {
            return f;
        }
        // number_to_string(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_TO_STRING,
            fn_type,
            None,
        );
        *self.fn_number_to_string.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_f64.borrow() {
            return f;
        }
        // union_box_f64(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::UNION_BOX_F64,
            fn_type,
            None,
        );
        *self.fn_union_box_f64.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_ptr.borrow() {
            return f;
        }
        // union_box_ptr(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::UNION_BOX_PTR,
            fn_type,
            None,
        );
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
        let f =
            self.module
                .add_function(crate::runtime_functions::names::RC_WEAK_INC, fn_type, None);
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
        let f =
            self.module
                .add_function(crate::runtime_functions::names::RC_WEAK_DEC, fn_type, None);
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

    fn get_array_get_length(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_length")
            .unwrap_or_else(|| {
                // array_get_length(arr_ptr: i8*) -> i64
                let fn_type = self.i64_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_get_length", fn_type, None)
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
        let name = format!(
            "{}{}",
            crate::runtime_functions::naming::STRING_LITERAL_PREFIX,
            id
        );
        self.next_str_id.set(id.wrapping_add(1));
        let gv = self.module.add_global(struct_ty, None, &name);

        // static header bit set at bit 32
        let static_header = self
            .i64_t
            .const_int(crate::constants::STATIC_HEADER_BIT, false);
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
        func_decl: &oats_ast::Function,
        llvm_param_types: &[BasicTypeEnum<'a>],
        receiver_name: Option<&str>,
    ) -> crate::diagnostics::DiagnosticResult<(HashMap<String, u32>, LocalsStackLocal<'a>)> {
        let mut param_map = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            use oats_ast::*;
            match &p.pat {
                Pat::Ident(ident) => {
                    let name = ident.sym.clone();
                    let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                    param_map.insert(name, idx);
                }
                _ => {
                    // Destructuring parameters not yet supported in parameter mapping
                }
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
                                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
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

        // Build function type: i32 (int) with (i32, i8**) params
        let argv_type = self.context.ptr_type(inkwell::AddressSpace::default());
        let fn_type = i32_t.fn_type(&[i32_t.into(), argv_type.into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self
            .module
            .get_function(crate::runtime_functions::names::OATS_MAIN)
        {
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
                    &crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to build call to oats_main",
                    ),
                    Some(self.source),
                );
                let const_zero = i32_t.const_int(0, false);
                let _ = self.builder.build_return(Some(&const_zero));
                return true;
            }
        };

        // Run the async executor to process any enqueued promises
        if self.uses_async.get() {
            let executor_run_fn = self.get_executor_run();
            let _ = self
                .builder
                .build_call(executor_run_fn, &[], "call_executor");
        }

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
                            &crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
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
                            &crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
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

            // Convert to proper exit code: 0 for success, 1 for failure
            let ret_int_val = ret_val.into_int_value();
            let zero = i32_t.const_int(0, false);
            let one = i32_t.const_int(1, false);
            let is_success = match self.builder.build_int_compare(
                inkwell::IntPredicate::EQ,
                ret_int_val,
                zero,
                "is_success",
            ) {
                Ok(cmp) => cmp,
                Err(_) => {
                    crate::diagnostics::emit_diagnostic(
                        &crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to build int compare for exit code",
                        ),
                        Some(self.source),
                    );
                    zero
                }
            };
            let exit_code = match self
                .builder
                .build_select(is_success, zero, one, "exit_code")
            {
                Ok(sel) => sel,
                Err(_) => {
                    crate::diagnostics::emit_diagnostic(
                        &crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to build select for exit code",
                        ),
                        Some(self.source),
                    );
                    inkwell::values::BasicValueEnum::IntValue(zero)
                }
            };
            let _ = self.builder.build_return(Some(&exit_code));
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
        let f = self
            .module
            .add_function(crate::runtime_functions::names::MALLOC, fn_type, None);
        *self.fn_malloc.borrow_mut() = Some(f);
        f
    }

    // Specialize a generic type based on type parameters and arguments
    pub fn specialize_generic(
        &self,
        _type_params: &[crate::types::OatsType],
        _args: &[oats_ast::Expr],
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Logic to specialize the generic type
        Ok(self
            .context
            .ptr_type(AddressSpace::default())
            .const_null()
            .into())
    }

    /// Monomorphize a generic function with the given type arguments
    pub fn monomorphize_function(
        &self,
        func_name: &str,
        type_args: &[crate::types::OatsType],
    ) -> crate::diagnostics::DiagnosticResult<String> {
        // Monomorphizing function: func='{}' type_args={:?}
        // Create a unique key for this monomorphization
        let mut key_parts = vec![func_name.to_string()];
        for arg in type_args {
            key_parts.push(format!("{:?}", arg));
        }
        let key = key_parts.join("_");

        // Check cache
        if let Some(existing_name) = self.monomorphized_map.borrow().get(&key) {
            return Ok(existing_name.clone());
        }

        // Lookup the generic function AST and its signature
        let (func_ast, fsig) = self
            .nested_generic_fns
            .borrow()
            .get(func_name)
            .cloned()
            .ok_or_else(|| {
                crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    format!("Generic function '{}' not found", func_name),
                )
            })?;

        // Obtain type parameter names: prefer fsig.type_params
        let tp_names: Vec<String> = fsig.type_params.clone();

        if tp_names.len() != type_args.len() {
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                format!(
                    "Expected {} type arguments for generic function '{}', got {}",
                    tp_names.len(),
                    func_name,
                    type_args.len()
                ),
            ));
        }

        // Build substitution map name -> OatsType
        let mut subst: std::collections::HashMap<String, crate::types::OatsType> =
            std::collections::HashMap::new();
        for (n, t) in tp_names.iter().zip(type_args.iter()) {
            subst.insert(n.clone(), t.clone());
        }

        // Compute concrete parameter types by applying the substitution to the
        // AST parameter annotations. Fall back to the fsig params when needed.
        let mut concrete_params: Vec<crate::types::OatsType> = Vec::new();
        for param in &func_ast.params {
            use oats_ast::*;
            // Only handle simple identifier parameters for now
            if !matches!(&param.pat, Pat::Ident(_)) {
                continue; // Skip destructuring parameters
            }
            let type_ann_opt = param.ty.as_ref();
            if let Some(type_ann) = type_ann_opt
                && let Some(mapped) = crate::types::map_ts_type_with_subst(type_ann, &subst)
            {
                concrete_params.push(mapped);
                continue;
            }
            // fallback: use corresponding entry from fsig.params if present
            if let Some(p) = fsig.params.get(concrete_params.len()) {
                concrete_params.push(p.clone());
            } else {
                // As a last resort, assume Number
                concrete_params.push(crate::types::OatsType::Number);
            }
        }

        // Compute concrete return type
        let concrete_ret = if let Some(rt) = &func_ast.return_type {
            if let Some(mapped) = crate::types::map_ts_type_with_subst(rt, &subst) {
                mapped
            } else {
                fsig.ret.clone()
            }
        } else {
            fsig.ret.clone()
        };

        // Build specialized name matching expected `_mono` pattern
        let mut specialized_name = format!("{}_mono", func_name);
        for arg in type_args {
            specialized_name.push_str(
                &format!("_{:?}", arg)
                    .replace(' ', "")
                    .replace(['(', ')'], "")
                    .replace(',', "_"),
            );
        }

        // Reserve mapping before emission to avoid recursion
        self.monomorphized_map
            .borrow_mut()
            .insert(key.clone(), specialized_name.clone());
        // Reserved monomorphization: '{}' (key={}) module_has_before={}

        // Emit the specialized function IR. gen_function_ir mutates the builder
        // insert point, so preserve and restore it.
        let prev_block = self.builder.get_insert_block();
        if let Err(diag) = self.gen_function_ir(
            &specialized_name,
            &func_ast,
            &concrete_params,
            &concrete_ret,
            None,
        ) {
            // On error, remove the reserved mapping and return the diagnostic
            self.monomorphized_map.borrow_mut().remove(&key);
            // Monomorphization failed for '{}': {}
            return Err(diag);
        }
        if let Some(pb) = prev_block {
            self.builder.position_at_end(pb);
        }
        // Monomorphization emitted: '{}' present_in_module={}

        Ok(specialized_name)
    }

    /// Infer return type from arrow function body
    pub fn infer_return_type_from_arrow_body(
        &self,
        body: &oats_ast::ArrowBody,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        match body {
            oats_ast::ArrowBody::Expr(expr) => {
                // Simple inference for single expression body
                self.infer_type_from_expr(expr)
            }
            oats_ast::ArrowBody::Block(block) => {
                // Collect all return expressions from the block
                let mut return_exprs = Vec::new();
                self.collect_return_exprs(&block.stmts, &mut return_exprs);

                if return_exprs.is_empty() {
                    // No returns, infer as void
                    Ok(crate::types::OatsType::Void)
                } else {
                    // Infer types from all return expressions
                    let mut return_types = Vec::new();
                    for expr in &return_exprs {
                        return_types.push(self.infer_type_from_expr(expr)?);
                    }

                    // Deduplicate types
                    let mut unique_types = Vec::new();
                    for ty in return_types {
                        if !unique_types.contains(&ty) {
                            unique_types.push(ty);
                        }
                    }

                    // If only one unique type, return it; otherwise create union
                    if unique_types.len() == 1 {
                        unique_types.into_iter().next().ok_or_else(|| {
                            crate::diagnostics::Diagnostic::simple_boxed(
                                Severity::Error,
                                "expected one unique type",
                            )
                        })
                    } else {
                        Ok(crate::types::OatsType::Union(unique_types))
                    }
                }
            }
        }
    }

    /// Collect all return expressions from a list of statements
    fn collect_return_exprs(&self, stmts: &[oats_ast::Stmt], out: &mut Vec<oats_ast::Expr>) {
        for stmt in stmts {
            self.collect_return_exprs_from_stmt(stmt, out);
        }
    }

    fn collect_return_exprs_from_stmt(&self, stmt: &oats_ast::Stmt, out: &mut Vec<oats_ast::Expr>) {
        use oats_ast::*;
        match stmt {
            Stmt::Return(ret) => {
                if let Some(expr) = &ret.arg {
                    out.push(expr.clone());
                }
            }
            Stmt::Block(block) => {
                self.collect_return_exprs(&block.stmts, out);
            }
            Stmt::If(if_stmt) => {
                self.collect_return_exprs_from_stmt(&if_stmt.cons, out);
                if let Some(alt) = &if_stmt.alt {
                    self.collect_return_exprs_from_stmt(alt, out);
                }
            }
            Stmt::For(for_stmt) => {
                self.collect_return_exprs_from_stmt(&for_stmt.body, out);
            }
            Stmt::While(while_stmt) => {
                self.collect_return_exprs_from_stmt(&while_stmt.body, out);
            }
            Stmt::DoWhile(do_while) => {
                self.collect_return_exprs_from_stmt(&do_while.body, out);
            }
            Stmt::Switch(switch) => {
                for case in &switch.cases {
                    self.collect_return_exprs(&case.cons, out);
                }
            }
            Stmt::Try(try_stmt) => {
                self.collect_return_exprs(&try_stmt.block.stmts, out);
                if let Some(handler) = &try_stmt.handler {
                    self.collect_return_exprs(&handler.body.stmts, out);
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    self.collect_return_exprs(&finalizer.stmts, out);
                }
            }
            _ => {}
        }
    }

    /// Simple type inference from expression
    fn infer_type_from_expr(
        &self,
        expr: &oats_ast::Expr,
    ) -> crate::diagnostics::DiagnosticResult<crate::types::OatsType> {
        match expr {
            oats_ast::Expr::Lit(lit) => match lit {
                oats_ast::Lit::F64(_)
                | oats_ast::Lit::F32(_)
                | oats_ast::Lit::I8(_)
                | oats_ast::Lit::I16(_)
                | oats_ast::Lit::I32(_)
                | oats_ast::Lit::I64(_)
                | oats_ast::Lit::I128(_)
                | oats_ast::Lit::ISize(_)
                | oats_ast::Lit::U8(_)
                | oats_ast::Lit::U16(_)
                | oats_ast::Lit::U32(_)
                | oats_ast::Lit::U64(_)
                | oats_ast::Lit::U128(_)
                | oats_ast::Lit::USize(_) => Ok(crate::types::OatsType::Number),
                oats_ast::Lit::Str(_) => Ok(crate::types::OatsType::String),
                oats_ast::Lit::Bool(_) => Ok(crate::types::OatsType::Boolean),
                _ => Ok(crate::types::OatsType::Number), // default
            },
            oats_ast::Expr::Ident(_) => Ok(crate::types::OatsType::Number), // unknown
            _ => Ok(crate::types::OatsType::Number), // default for complex expr
        }
    }
}
