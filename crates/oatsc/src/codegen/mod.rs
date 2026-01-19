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
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub mod common_types;
pub mod emit;
pub mod escape;
pub mod expr;
#[macro_use]
pub mod error_macros;
pub mod generics;
pub mod helpers;
pub mod host_main;
pub mod runtime_decls;
#[macro_use]
pub mod runtime_macros;
pub mod stmt;
pub mod utils;

// Import common types to maintain backward compatibility
pub use common_types::{EnumVariantMap, LocalEntry, LocalsStackLocal, LoopContext, TypeAliasMap};

/// Main code generation context holding LLVM state and caches.
///
/// See module-level documentation for architecture overview.
pub struct CodeGen<'a> {
    /// LLVM context
    pub context: &'a Context,
    /// LLVM module being generated
    pub module: Module<'a>,
    /// LLVM IR builder
    pub builder: Builder<'a>,
    /// Next unique string literal ID
    pub next_str_id: Cell<u32>,
    /// Cached string literal pointers
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    /// Cached LLVM type: f64
    pub f64_t: inkwell::types::FloatType<'a>,
    /// Cached LLVM type: f32
    pub f32_t: inkwell::types::FloatType<'a>,
    /// Cached LLVM type: i64
    pub i64_t: inkwell::types::IntType<'a>,
    /// Cached LLVM type: i32
    pub i32_t: inkwell::types::IntType<'a>,
    /// Cached LLVM type: i16
    pub i16_t: inkwell::types::IntType<'a>,
    /// Cached LLVM type: i8
    pub i8_t: inkwell::types::IntType<'a>,
    /// Cached LLVM type: bool (i1)
    pub bool_t: inkwell::types::IntType<'a>,
    /// Cached LLVM type: i8*
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    /// Runtime function: `print_f64`
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `print_str`
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: strlen
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: malloc
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: memcpy
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: free
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `array_alloc`
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `rc_inc`
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `rc_dec`
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_to_string`
    pub fn_number_to_string: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_new`
    pub fn_number_new: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_is_nan_static`
    pub fn_number_is_nan_static: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_is_finite_static`
    pub fn_number_is_finite_static: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_is_integer`
    pub fn_number_is_integer: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_parse_float_static`
    pub fn_number_parse_float_static: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_parse_int`
    pub fn_number_parse_int: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_epsilon`
    pub fn_number_epsilon: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_max_safe_integer`
    pub fn_number_max_safe_integer: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_max_value`
    pub fn_number_max_value: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_min_safe_integer`
    pub fn_number_min_safe_integer: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_min_value`
    pub fn_number_min_value: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_nan`
    pub fn_number_nan: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_negative_infinity`
    pub fn_number_negative_infinity: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_positive_infinity`
    pub fn_number_positive_infinity: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_is_safe_integer`
    pub fn_number_is_safe_integer: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_to_fixed`
    pub fn_number_to_fixed: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_to_exponential`
    pub fn_number_to_exponential: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_to_precision`
    pub fn_number_to_precision: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_to_locale_string`
    pub fn_number_to_locale_string: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `number_value_of`
    pub fn_number_value_of: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `union_box_f64`
    pub fn_union_box_f64: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `union_box_ptr`
    pub fn_union_box_ptr: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `union_unbox_f64`
    pub fn_union_unbox_f64: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `union_unbox_ptr`
    pub fn_union_unbox_ptr: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `union_get_discriminant`
    pub fn_union_get_discriminant: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `rc_weak_inc`
    pub fn_rc_weak_inc: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `rc_weak_dec`
    pub fn_rc_weak_dec: RefCell<Option<FunctionValue<'a>>>,
    /// Runtime function: `rc_weak_upgrade`
    pub fn_rc_weak_upgrade: RefCell<Option<FunctionValue<'a>>>,
    /// Class name -> (field name, field type) pairs
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    /// Interface name -> (property name, property type) pairs
    pub interface_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    /// Enum name -> (variant name, variant field types) pairs
    pub enum_variants: RefCell<EnumVariantMap>,
    /// Class name -> parent class name (None if no parent)
    pub class_parents: RefCell<HashMap<String, Option<String>>>,
    /// Type alias name -> (type parameters, aliased type). None `type_params` for non-generic aliases
    pub type_aliases: RefCell<TypeAliasMap<'a>>,
    /// Function name -> parameter types
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    /// Stack of active loop contexts for break/continue
    pub loop_context_stack: RefCell<Vec<LoopContext<'a>>>,
    /// Current labeled statement name
    pub current_label: RefCell<Option<String>>,
    /// Parent class name for current constructor (for `super(...)` lowering)
    pub current_class_parent: RefCell<Option<String>>,
    /// Local name -> closure return type (for typed indirect calls)
    pub closure_local_rettype: RefCell<HashMap<String, crate::types::OatsType>>,
    /// Last expression's origin local name (for closure type propagation)
    pub last_expr_origin_local: RefCell<Option<String>>,
    /// Last expression's `OatsType` (for union unboxing decisions)
    pub last_expr_oats_type: RefCell<Option<crate::types::OatsType>>,
    /// Async: live variable sets per await point
    pub async_await_live_sets: RefCell<Option<Vec<std::collections::HashSet<String>>>>,
    /// Async: local name -> slot index mapping
    pub async_local_name_to_slot: RefCell<Option<std::collections::HashMap<String, usize>>>,
    /// Async: resume blocks for state machine
    pub async_resume_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    /// Async: continuation blocks
    pub async_cont_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    /// Async: poll function being generated
    pub async_poll_function: RefCell<Option<FunctionValue<'a>>>,
    /// Async: await counter for unique IDs
    pub async_await_counter: Cell<u32>,
    /// Async: parameter count
    pub async_param_count: Cell<u32>,
    /// Async: local slot count
    pub async_local_slot_count: Cell<usize>,
    /// Async: poll function's locals stack (for resume block access)
    pub async_poll_locals: RefCell<Option<LocalsStackLocal<'a>>>,
    /// Generator: live variable sets per yield point
    pub generator_yield_live_sets: RefCell<Option<Vec<std::collections::HashSet<String>>>>,
    /// Generator: local name -> slot index mapping
    pub generator_local_name_to_slot: RefCell<Option<std::collections::HashMap<String, usize>>>,
    /// Generator: resume blocks for state machine
    pub generator_resume_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    /// Generator: continuation blocks
    pub generator_cont_blocks: RefCell<Option<Vec<BasicBlock<'a>>>>,
    /// Generator: next function being generated
    pub generator_next_function: RefCell<Option<FunctionValue<'a>>>,
    /// Generator: yield counter for unique IDs
    pub generator_yield_counter: Cell<u32>,
    /// Generator: parameter count
    pub generator_param_count: Cell<u32>,
    /// Generator: local slot count
    pub generator_local_slot_count: Cell<usize>,
    /// Generator: next function's locals stack (for resume block access)
    pub generator_next_locals: RefCell<Option<LocalsStackLocal<'a>>>,
    /// Source code being compiled
    pub source: &'a str,
    /// Byte indices of `mut` `VarDecl` nodes (from parser)
    pub mut_var_decls: std::collections::HashSet<usize>,
    /// Current function's return type (for union boxing)
    pub current_function_return_type: RefCell<Option<crate::types::OatsType>>,
    /// Whether last expression is already a boxed union (prevents double-boxing)
    pub last_expr_is_boxed_union: Cell<bool>,
    /// Function name -> (parameter types, return type)
    pub global_function_signatures:
        RefCell<HashMap<String, (Vec<crate::types::OatsType>, crate::types::OatsType)>>,
    /// Type information symbol table
    pub symbol_table: RefCell<crate::types::SymbolTable>,
    /// Cache of declared external std functions
    pub external_std_fns: RefCell<HashMap<String, FunctionValue<'a>>>,
    /// Compile-time const items: name -> `ConstValue`
    /// Emitted LLVM globals for const heap objects
    pub const_globals: RefCell<HashMap<String, PointerValue<'a>>>,
    /// Interned const values: stable key -> global pointer
    pub const_interns: RefCell<HashMap<String, PointerValue<'a>>>,
    /// Escape analysis info for current function
    pub current_escape_info: RefCell<Option<crate::codegen::escape::EscapeInfo>>,
    /// Nested generic functions: name -> (AST, signature)
    pub nested_generic_fns:
        RefCell<HashMap<String, (&'a oxc_ast::ast::Function<'a>, crate::types::FunctionSig)>>,
    /// Nested generic classes: name -> (AST, `type_params`)
    pub nested_generic_classes:
        RefCell<HashMap<String, (&'a oxc_ast::ast::Class<'a>, Vec<String>)>>,
    /// Monomorphization cache: key -> specialized function name
    pub monomorphized_map: RefCell<HashMap<String, String>>,
    /// Rapid Type Analysis results for optimization
    pub rta_results: Option<crate::rta::RTAResults>,
    /// Whether this compilation uses async/await
    pub uses_async: Cell<bool>,
    /// Whether the last expression returned a +1 ownership stake
    pub returned_ownership: Cell<bool>,
}

impl<'a> CodeGen<'a> {
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
        self.current_escape_info
            .borrow()
            .as_ref()
            .is_some_and(|info| !info.escapes(local_name))
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
        self.module.get_function("promise_resolve").map_or_else(
            || self.module.add_function("promise_resolve", fn_type, None),
            std::convert::identity,
        )
    }

    fn get_promise_poll_into(&self) -> FunctionValue<'a> {
        // promise_poll_into(i8*, i8*) -> i32
        let fn_type = self
            .i32_t
            .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
        self.module.get_function("promise_poll_into").map_or_else(
            || self.module.add_function("promise_poll_into", fn_type, None),
            std::convert::identity,
        )
    }

    fn get_promise_new_from_state(&self) -> FunctionValue<'a> {
        // promise_new_from_state(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        self.module
            .get_function("promise_new_from_state")
            .map_or_else(
                || {
                    self.module
                        .add_function("promise_new_from_state", fn_type, None)
                },
                std::convert::identity,
            )
    }

    fn get_executor_enqueue(&self) -> FunctionValue<'a> {
        // executor_enqueue(i8*) -> void
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        self.module.get_function("executor_enqueue").map_or_else(
            || self.module.add_function("executor_enqueue", fn_type, None),
            std::convert::identity,
        )
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

    fn get_number_new(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_new.borrow() {
            return f;
        }
        // number_new(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f =
            self.module
                .add_function(crate::runtime_functions::names::NUMBER_NEW, fn_type, None);
        *self.fn_number_new.borrow_mut() = Some(f);
        f
    }

    fn get_number_is_nan_static(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_is_nan_static.borrow() {
            return f;
        }
        // number_is_nan_static(f64) -> i64
        let fn_type = self.i64_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_IS_NAN_STATIC,
            fn_type,
            None,
        );
        *self.fn_number_is_nan_static.borrow_mut() = Some(f);
        f
    }

    fn get_number_is_finite_static(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_is_finite_static.borrow() {
            return f;
        }
        // number_is_finite_static(f64) -> i64
        let fn_type = self.i64_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_IS_FINITE_STATIC,
            fn_type,
            None,
        );
        *self.fn_number_is_finite_static.borrow_mut() = Some(f);
        f
    }

    fn get_number_is_integer(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_is_integer.borrow() {
            return f;
        }
        // number_is_integer(f64) -> i64
        let fn_type = self.i64_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_IS_INTEGER,
            fn_type,
            None,
        );
        *self.fn_number_is_integer.borrow_mut() = Some(f);
        f
    }

    fn get_number_parse_float_static(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_parse_float_static.borrow() {
            return f;
        }
        // number_parse_float_static(i8*) -> f64
        let fn_type = self.f64_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_PARSE_FLOAT_STATIC,
            fn_type,
            None,
        );
        *self.fn_number_parse_float_static.borrow_mut() = Some(f);
        f
    }

    fn get_number_parse_int(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_parse_int.borrow() {
            return f;
        }
        // number_parse_int(i8*, f64) -> f64
        let fn_type = self
            .f64_t
            .fn_type(&[self.i8ptr_t.into(), self.f64_t.into()], false);
        let f = self.module.add_function(
            crate::runtime_functions::names::NUMBER_PARSE_INT,
            fn_type,
            None,
        );
        *self.fn_number_parse_int.borrow_mut() = Some(f);
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
        self.module.get_function("array_get_f64").map_or_else(
            || {
                let fn_type = self
                    .f64_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_f64", fn_type, None)
            },
            std::convert::identity,
        )
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        self.module.get_function("array_get_ptr").map_or_else(
            || {
                let fn_type = self
                    .i8ptr_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_ptr", fn_type, None)
            },
            std::convert::identity,
        )
    }

    fn get_array_get_length(&self) -> FunctionValue<'a> {
        self.module.get_function("array_get_length").map_or_else(
            || {
                // array_get_length(arr_ptr: i8*) -> i64
                let fn_type = self.i64_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_get_length", fn_type, None)
            },
            std::convert::identity,
        )
    }

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        self.module.get_function("array_set_ptr").map_or_else(
            || {
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
            },
            std::convert::identity,
        )
    }

    fn get_array_to_string(&self) -> FunctionValue<'a> {
        self.module.get_function("array_to_string").map_or_else(
            || {
                // array_to_string(arr_ptr: i8*) -> i8*
                let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_to_string", fn_type, None)
            },
            std::convert::identity,
        )
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
    pub fn intern_string_literal(
        &self,
        s: &str,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::PointerValue<'a>> {
        // Check cache first
        if let Some(ptr) = self.string_literals.borrow().get(s) {
            return Ok(*ptr);
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

        // Return pointer to data section (offset 16 bytes: header + len)
        let gv_i8 = self
            .builder
            .build_pointer_cast(gv.as_pointer_value(), self.i8ptr_t, "str_data_base")
            .map_err(|_| {
                crate::diagnostics::Diagnostic::error("failed to cast string literal global to i8*")
            })?;
        let ptr = self.offset_i8_ptr(gv_i8, 16, "strptr").map_err(|_| {
            crate::diagnostics::Diagnostic::error(
                "failed to offset string literal pointer to data section",
            )
        })?;
        self.string_literals.borrow_mut().insert(s.to_string(), ptr);
        Ok(ptr)
    }

    // Creates stack allocations (`alloca`) for all function parameters, making them
    // accessible like local variables and handling initial reference counting.
    fn create_param_allocas(
        &self,
        function: FunctionValue<'a>,
        func_decl: &oxc_ast::ast::Function,
        llvm_param_types: &[BasicTypeEnum<'a>],
        receiver_name: Option<&str>,
    ) -> crate::diagnostics::DiagnosticResult<(HashMap<String, u32>, LocalsStackLocal<'a>)> {
        let mut param_map = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.items.iter().enumerate() {
            match &p.pattern.kind {
                oxc_ast::ast::BindingPatternKind::BindingIdentifier(ident) => {
                    let name = ident.name.to_string();
                    let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                    param_map.insert(name, idx);
                }
                oxc_ast::ast::BindingPatternKind::AssignmentPattern(assign) => {
                    if let oxc_ast::ast::BindingPatternKind::BindingIdentifier(ident) =
                        &assign.left.kind
                    {
                        let name = ident.name.to_string();
                        let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                        param_map.insert(name, idx);
                    }
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
            let function_name = function.get_name().to_str().unwrap_or_default();
            if let Some(param_types) = self.fn_param_types.borrow().get(function_name) {
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
                                    | crate::types::OatsType::StructLiteral(_)
                                    | crate::types::OatsType::Array(_)
                                    | crate::types::OatsType::Tuple(_)
                                    | crate::types::OatsType::Promise(_)
                                    | crate::types::OatsType::Weak(_)
                                    | crate::types::OatsType::Unowned(_)
                                    | crate::types::OatsType::Option(_)
                                    | crate::types::OatsType::Enum(_, _)
                                    | crate::types::OatsType::ProcessId
                                    | crate::types::OatsType::MonitorRef
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
                                        && let inkwell::values::ValueKind::Basic(bv) =
                                            cs.try_as_basic_value()
                                        && let inkwell::values::BasicValueEnum::PointerValue(pv) =
                                            bv
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        self.builder
                                            .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "failed to build rc_inc call",
                                                )
                                            })?;
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
                                        && let inkwell::values::ValueKind::Basic(bv) =
                                            cs.try_as_basic_value()
                                        && let inkwell::values::BasicValueEnum::PointerValue(pv) =
                                            bv
                                    {
                                        let rc_inc = self.get_rc_inc();
                                        self.builder
                                            .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                                            .map_err(|_| {
                                                crate::diagnostics::Diagnostic::simple_boxed(
                                                    Severity::Error,
                                                    "failed to build rc_inc call",
                                                )
                                            })?;
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

    // Host main generation is implemented in host_main.rs for better organization

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

    fn get_number_epsilon(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_EPSILON)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_EPSILON,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_max_safe_integer(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_MAX_SAFE_INTEGER)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_MAX_SAFE_INTEGER,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_max_value(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_MAX_VALUE)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_MAX_VALUE,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_min_safe_integer(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_MIN_SAFE_INTEGER)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_MIN_SAFE_INTEGER,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_min_value(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_MIN_VALUE)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_MIN_VALUE,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_nan(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_NAN)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_NAN,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_negative_infinity(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_NEGATIVE_INFINITY)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_NEGATIVE_INFINITY,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_positive_infinity(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_POSITIVE_INFINITY)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_POSITIVE_INFINITY,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_is_safe_integer(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_IS_SAFE_INTEGER)
            .map_or_else(
                || {
                    let fn_type = self.i64_t.fn_type(&[self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_IS_SAFE_INTEGER,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_to_fixed(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_TO_FIXED)
            .map_or_else(
                || {
                    let fn_type = self
                        .i8ptr_t
                        .fn_type(&[self.f64_t.into(), self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_TO_FIXED,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_to_exponential(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_TO_EXPONENTIAL)
            .map_or_else(
                || {
                    let fn_type = self
                        .i8ptr_t
                        .fn_type(&[self.f64_t.into(), self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_TO_EXPONENTIAL,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_to_precision(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_TO_PRECISION)
            .map_or_else(
                || {
                    let fn_type = self
                        .i8ptr_t
                        .fn_type(&[self.f64_t.into(), self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_TO_PRECISION,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_to_locale_string(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_TO_LOCALE_STRING)
            .map_or_else(
                || {
                    let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_TO_LOCALE_STRING,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    fn get_number_value_of(&self) -> FunctionValue<'a> {
        self.module
            .get_function(crate::runtime_functions::names::NUMBER_VALUE_OF)
            .map_or_else(
                || {
                    let fn_type = self.f64_t.fn_type(&[self.f64_t.into()], false);
                    self.module.add_function(
                        crate::runtime_functions::names::NUMBER_VALUE_OF,
                        fn_type,
                        None,
                    )
                },
                std::convert::identity,
            )
    }

    // Generic function monomorphization and type inference methods are
    // implemented in generics.rs for better organization
}
