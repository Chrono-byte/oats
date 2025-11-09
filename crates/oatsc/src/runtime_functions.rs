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

    // ========== Process Model Functions ==========

    /// Process spawn: `process_spawn(i32 priority) -> i8*`
    pub const PROCESS_SPAWN: &str = "process_spawn";

    /// Process spawn named: `process_spawn_named(i8*, i32 priority) -> i8*`
    pub const PROCESS_SPAWN_NAMED: &str = "process_spawn_named";

    /// Process send: `process_send(i64* to_pid, i64* from_pid, i8* payload, i64 type_id) -> i32`
    pub const PROCESS_SEND: &str = "process_send";

    /// Process send to name: `process_send_to_name(i8* name, i64* from_pid, i8* payload, i64 type_id) -> i32`
    pub const PROCESS_SEND_TO_NAME: &str = "process_send_to_name";

    /// Process receive: `process_receive(i64* pid, i64 type_id) -> i8*`
    pub const PROCESS_RECEIVE: &str = "process_receive";

    /// Process self: `process_self() -> i8*`
    pub const PROCESS_SELF: &str = "process_self";

    /// Process exit: `process_exit(i64* pid, i32 reason_type, i8* reason_str) -> void`
    pub const PROCESS_EXIT: &str = "process_exit";

    /// Process whereis: `process_whereis(i8* name) -> i8*`
    pub const PROCESS_WHEREIS: &str = "process_whereis";

    /// Process register: `process_register(i64* pid, i8* name) -> i32`
    pub const PROCESS_REGISTER: &str = "process_register";

    /// Process unregister: `process_unregister(i8* name) -> void`
    pub const PROCESS_UNREGISTER: &str = "process_unregister";

    /// Process link: `process_link(i64* pid1, i64* pid2) -> i32`
    pub const PROCESS_LINK: &str = "process_link";

    /// Process unlink: `process_unlink(i64* pid1, i64* pid2) -> void`
    pub const PROCESS_UNLINK: &str = "process_unlink";

    /// Process monitor: `process_monitor(i64* monitor_pid, i64* target_pid) -> i8*`
    pub const PROCESS_MONITOR: &str = "process_monitor";

    /// Process demonitor: `process_demonitor(i64* monitor_pid, i64* monitor_ref) -> i32`
    pub const PROCESS_DEMONITOR: &str = "process_demonitor";

    /// Process wait for promise: `process_wait_for_promise(i64* pid, i8* promise) -> void`
    pub const PROCESS_WAIT_FOR_PROMISE: &str = "process_wait_for_promise";

    /// Process wait for timeout: `process_wait_for_timeout(i64* pid, i64 timeout_ms) -> void`
    pub const PROCESS_WAIT_FOR_TIMEOUT: &str = "process_wait_for_timeout";

    /// Process scheduler run: `process_scheduler_run() -> i32`
    pub const PROCESS_SCHEDULER_RUN: &str = "process_scheduler_run";

    /// Process check promises: `process_check_promises() -> void`
    pub const PROCESS_CHECK_PROMISES: &str = "process_check_promises";

    /// Process check timeouts: `process_check_timeouts() -> void`
    pub const PROCESS_CHECK_TIMEOUTS: &str = "process_check_timeouts";

    /// Process register supervisor: `process_register_supervisor(i64* supervisor_pid, i32 strategy, i32 max_restarts, i64 max_seconds) -> i32`
    pub const PROCESS_REGISTER_SUPERVISOR: &str = "process_register_supervisor";

    /// Process add supervisor child: `process_add_supervisor_child(i64* supervisor_pid, i64* child_pid, i8* child_id, i32 restart, i64 shutdown_timeout) -> i32`
    pub const PROCESS_ADD_SUPERVISOR_CHILD: &str = "process_add_supervisor_child";

    /// Process set max processes: `process_set_max_processes(i64 max) -> void`
    pub const PROCESS_SET_MAX_PROCESSES: &str = "process_set_max_processes";

    /// Process get count: `process_get_count() -> i64`
    pub const PROCESS_GET_COUNT: &str = "process_get_count";

    /// Process scheduler init multi-threaded: `process_scheduler_init_multi_threaded(i64 num_threads) -> i32`
    pub const PROCESS_SCHEDULER_INIT_MULTI_THREADED: &str = "process_scheduler_init_multi_threaded";

    /// Process scheduler stop: `process_scheduler_stop() -> void`
    pub const PROCESS_SCHEDULER_STOP: &str = "process_scheduler_stop";

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

