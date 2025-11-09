//! Runtime function name constants
//!
//! This module centralizes all runtime function names used during code generation.
//! This improves Locality of Behaviour by making it obvious what function names
//! are being generated when reading codegen code.
//!
//! All runtime functions must be declared in `crates/runtime/src/lib.rs` with
//! matching names.

/// Runtime function names used in LLVM IR generation
pub mod names {
    /// Increment reference count: `rc_inc(i8*) -> void`
    pub const RC_INC: &str = "rc_inc";

    /// Decrement reference count: `rc_dec(i8*) -> void`
    pub const RC_DEC: &str = "rc_dec";

    /// Increment weak reference count: `rc_weak_inc(i8*) -> void`
    pub const RC_WEAK_INC: &str = "rc_weak_inc";

    /// Decrement weak reference count: `rc_weak_dec(i8*) -> void`
    pub const RC_WEAK_DEC: &str = "rc_weak_dec";

    /// Upgrade weak reference to strong: `rc_weak_upgrade(i8*) -> i8*`
    pub const RC_WEAK_UPGRADE: &str = "rc_weak_upgrade";

    /// Allocate array: `array_alloc(i64 len, i32 elem_kind, i32 init_kind) -> i8*`
    pub const ARRAY_ALLOC: &str = "array_alloc";

    /// Get array element (f64): `array_get_f64(i8*, i64) -> f64`
    pub const ARRAY_GET_F64: &str = "array_get_f64";

    /// Get array element (pointer): `array_get_ptr(i8*, i64) -> i8*`
    pub const ARRAY_GET_PTR: &str = "array_get_ptr";

    /// Get array length: `array_get_length(i8*) -> i64`
    pub const ARRAY_GET_LENGTH: &str = "array_get_length";

    /// Set array element (f64): `array_set_f64(i8**, i64, f64) -> void`
    pub const ARRAY_SET_F64: &str = "array_set_f64";

    /// Set array element (pointer): `array_set_ptr(i8**, i64, i8*) -> void`
    pub const ARRAY_SET_PTR: &str = "array_set_ptr";

    /// Convert number to string: `number_to_string(f64) -> i8*`
    pub const NUMBER_TO_STRING: &str = "number_to_string";

    /// Box f64 into union: `union_box_f64(f64) -> i8*`
    pub const UNION_BOX_F64: &str = "union_box_f64";

    /// Box pointer into union: `union_box_ptr(i8*) -> i8*`
    pub const UNION_BOX_PTR: &str = "union_box_ptr";

    /// Unbox f64 from union: `union_unbox_f64(i8*) -> f64`
    pub const UNION_UNBOX_F64: &str = "union_unbox_f64";

    /// Unbox pointer from union: `union_unbox_ptr(i8*) -> i8*`
    pub const UNION_UNBOX_PTR: &str = "union_unbox_ptr";

    /// Get union discriminant: `union_get_discriminant(i8*) -> i64`
    pub const UNION_GET_DISCRIMINANT: &str = "union_get_discriminant";

    /// Promise resolve: `promise_resolve(i8*) -> i8*`
    pub const PROMISE_RESOLVE: &str = "promise_resolve";

    /// Promise poll into: `promise_poll_into(i8*, i8*) -> i32`
    pub const PROMISE_POLL_INTO: &str = "promise_poll_into";

    /// Promise new from state: `promise_new_from_state(i8*) -> i8*`
    pub const PROMISE_NEW_FROM_STATE: &str = "promise_new_from_state";

    /// Executor enqueue: `executor_enqueue(i8*) -> void`
    pub const EXECUTOR_ENQUEUE: &str = "executor_enqueue";

    /// Executor run: `executor_run() -> void`
    pub const EXECUTOR_RUN: &str = "executor_run";

    /// Standard library console log: `oats_std_console_log(i8*) -> void`
    pub const STD_CONSOLE_LOG: &str = "oats_std_console_log";

    /// Standard malloc: `malloc(i64) -> i8*`
    pub const MALLOC: &str = "malloc";

    /// Standard free: `free(i8*) -> void`
    pub const FREE: &str = "free";

    /// Standard memcpy: `memcpy(i8*, i8*, i64) -> void`
    pub const MEMCPY: &str = "memcpy";

    /// Standard strlen: `strlen(i8*) -> i64`
    pub const STRLEN: &str = "strlen";

    /// Standard print f64: `print_f64(f64) -> void`
    pub const PRINT_F64: &str = "print_f64";

    /// Standard print string: `print_str(i8*) -> void`
    pub const PRINT_STR: &str = "print_str";

    /// String concatenation: `str_concat(i8*, i8*) -> i8*`
    pub const STR_CONCAT: &str = "str_concat";

    /// Main entry point function: `oats_main() -> ...`
    pub const OATS_MAIN: &str = "oats_main";
}

/// Constants for code generation naming patterns
pub mod naming {
    /// Prefix for string literal globals: `strlit.{id}`
    pub const STRING_LITERAL_PREFIX: &str = "strlit.";

    /// Suffix for monomorphized functions: `{name}_mono_{types}`
    pub const MONO_SUFFIX: &str = "_mono";

    /// Suffix for constructor implementation: `{class}_ctor_impl`
    pub const CTOR_IMPL_SUFFIX: &str = "_ctor_impl";

    /// Suffix for constructor: `{class}_ctor`
    pub const CTOR_SUFFIX: &str = "_ctor";

    /// Suffix for initializer: `{class}_init`
    pub const INIT_SUFFIX: &str = "_init";

    /// Prefix for method names: `{class}_{method}`
    pub const METHOD_SEPARATOR: &str = "_";

    /// Prefix for parameter struct types: `{func}_param_struct_{i}`
    pub const PARAM_STRUCT_PREFIX: &str = "_param_struct_";

    /// Prefix for generated method IDs: `method_{name}_{id}`
    pub const METHOD_ID_PREFIX: &str = "method_";
}

/// Constants for module and test names
pub mod modules {
    /// Default module name for AOT compilation
    pub const OATS_AOT: &str = "oats_aot";

    /// Default module name for tests
    pub const TEST_MODULE: &str = "test_module";
}

