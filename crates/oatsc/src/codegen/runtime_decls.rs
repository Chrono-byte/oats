//! Runtime function declarations.
//!
//! This module handles declaring external runtime functions used by codegen.
//! These declarations are idempotent - functions are only added if they don't
//! already exist in the module.

use crate::codegen::CodeGen;
use inkwell::values::FunctionValue;

impl<'a> CodeGen<'a> {
    /// Declare external libc/runtime helper functions used by codegen.
    ///
    /// These are added as declarations (no body) so the runtime or system libc
    /// provides the implementation at link time. Declarations are idempotent:
    /// we only add a function if it isn't already present in the module.
    pub fn declare_libc(&self) {
        self.declare_libc_functions();
        self.declare_print_functions();
        self.declare_temporal_functions();
        self.declare_process_functions();
    }

    fn declare_libc_functions(&self) {
        // declare malloc(i64) -> i8*
        self.declare_function_if_missing(
            crate::runtime_functions::names::MALLOC,
            &[self.i64_t.into()],
            Some(self.i8ptr_t.into()),
            |f| {
                self.fn_malloc.borrow_mut().replace(f);
            },
        );

        // declare strlen(i8*) -> i64
        self.declare_function_if_missing(
            crate::runtime_functions::names::STRLEN,
            &[self.i8ptr_t.into()],
            Some(self.i64_t.into()),
            |f| {
                self.fn_strlen.borrow_mut().replace(f);
            },
        );

        // declare memcpy(i8*, i8*, i64) -> i8*
        self.declare_function_if_missing(
            crate::runtime_functions::names::MEMCPY,
            &[self.i8ptr_t.into(), self.i8ptr_t.into(), self.i64_t.into()],
            Some(self.i8ptr_t.into()),
            |f| {
                self.fn_memcpy.borrow_mut().replace(f);
            },
        );

        // declare free(i8*) -> void
        self.declare_function_if_missing(
            crate::runtime_functions::names::FREE,
            &[self.i8ptr_t.into()],
            None,
            |f| {
                self.fn_free.borrow_mut().replace(f);
            },
        );

        // declare array_to_string(i8*) -> i8*
        self.declare_function_if_missing(
            "array_to_string",
            &[self.i8ptr_t.into()],
            Some(self.i8ptr_t.into()),
            |_| {},
        );

        // declare rc_dec_str(i8*) -> void
        self.declare_function_if_missing("rc_dec_str", &[self.i8ptr_t.into()], None, |_| {});

        // declare sleep_ms(f64) -> void
        self.declare_function_if_missing("sleep_ms", &[self.f64_t.into()], None, |_| {});

        // Declare test-only helper when feature is enabled
        if cfg!(feature = "collector-test") {
            self.declare_function_if_missing("collector_test_enqueue", &[], None, |_| {});
        }
    }

    fn declare_print_functions(&self) {
        // declare print_f64(f64) -> void
        self.declare_function_if_missing(
            crate::runtime_functions::names::PRINT_F64,
            &[self.f64_t.into()],
            None,
            |f| {
                self.fn_print_f64.borrow_mut().replace(f);
            },
        );

        // declare print_f64_no_nl(f64) -> void
        self.declare_function_if_missing("print_f64_no_nl", &[self.f64_t.into()], None, |_| {});

        // declare print_str(i8*) -> void
        self.declare_function_if_missing(
            crate::runtime_functions::names::PRINT_STR,
            &[self.i8ptr_t.into()],
            None,
            |f| {
                self.fn_print_str.borrow_mut().replace(f);
            },
        );

        // declare print_str_no_nl(i8*) -> void
        self.declare_function_if_missing("print_str_no_nl", &[self.i8ptr_t.into()], None, |_| {});

        // declare print_newline() -> void
        self.declare_function_if_missing("print_newline", &[], None, |_| {});
    }

    fn declare_temporal_functions(&self) {
        self.declare_function_if_missing(
            "oats_std_temporal_now",
            &[],
            Some(self.i8ptr_t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            "oats_std_temporal_now_instant",
            &[],
            Some(self.i8ptr_t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            "oats_std_temporal_instant_from_epoch_nanoseconds",
            &[self.i64_t.into()],
            Some(self.i8ptr_t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            "oats_std_temporal_instant_epoch_nanoseconds",
            &[self.i8ptr_t.into()],
            Some(self.i64_t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            "oats_std_temporal_instant_add",
            &[self.i8ptr_t.into(), self.i64_t.into()],
            Some(self.i8ptr_t.into()),
            |_| {},
        );
    }

    fn declare_process_functions(&self) {
        let i32t = self.i32_t;
        let i64t = self.i64_t;
        let i8ptr = self.i8ptr_t;
        let i64ptr = self.context.ptr_type(inkwell::AddressSpace::default());

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SPAWN,
            &[i32t.into()],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SPAWN_NAMED,
            &[i8ptr.into(), i32t.into()],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SEND,
            &[i64ptr.into(), i64ptr.into(), i8ptr.into(), i64t.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SEND_TO_NAME,
            &[i8ptr.into(), i64ptr.into(), i8ptr.into(), i64t.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_RECEIVE,
            &[i64ptr.into(), i64t.into()],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SELF,
            &[],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_EXIT,
            &[i64ptr.into(), i32t.into(), i8ptr.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_WHEREIS,
            &[i8ptr.into()],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_REGISTER,
            &[i64ptr.into(), i8ptr.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_UNREGISTER,
            &[i8ptr.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_LINK,
            &[i64ptr.into(), i64ptr.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_UNLINK,
            &[i64ptr.into(), i64ptr.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_MONITOR,
            &[i64ptr.into(), i64ptr.into()],
            Some(i8ptr.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_DEMONITOR,
            &[i64ptr.into(), i64ptr.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_WAIT_FOR_PROMISE,
            &[i64ptr.into(), i8ptr.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_WAIT_FOR_TIMEOUT,
            &[i64ptr.into(), i64t.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SCHEDULER_RUN,
            &[],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_CHECK_PROMISES,
            &[],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_CHECK_TIMEOUTS,
            &[],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_REGISTER_SUPERVISOR,
            &[i64ptr.into(), i32t.into(), i32t.into(), i64t.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_ADD_SUPERVISOR_CHILD,
            &[
                i64ptr.into(),
                i64ptr.into(),
                i8ptr.into(),
                i32t.into(),
                i64t.into(),
            ],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SET_MAX_PROCESSES,
            &[i64t.into()],
            None,
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_GET_COUNT,
            &[],
            Some(i64t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SCHEDULER_INIT_MULTI_THREADED,
            &[i64t.into()],
            Some(i32t.into()),
            |_| {},
        );

        self.declare_function_if_missing(
            crate::runtime_functions::names::PROCESS_SCHEDULER_STOP,
            &[],
            None,
            |_| {},
        );
    }

    /// Declares a function if it doesn't already exist in the module.
    ///
    /// # Parameters
    /// - `name`: Function name
    /// - `param_types`: Parameter types
    /// - `return_type`: Return type (None for void)
    /// - `on_declare`: Callback invoked when function is declared
    fn declare_function_if_missing<F>(
        &self,
        name: &str,
        param_types: &[inkwell::types::BasicTypeEnum<'a>],
        return_type: Option<inkwell::types::BasicTypeEnum<'a>>,
        on_declare: F,
    ) where
        F: FnOnce(FunctionValue<'a>),
    {
        if self.module.get_function(name).is_none() {
            use inkwell::types::BasicType;
            let params: Vec<_> = param_types.iter().map(|&t| t.into()).collect();
            let ret_ty = if let Some(rt) = return_type {
                rt.fn_type(&params, false)
            } else {
                self.context.void_type().fn_type(&params, false)
            };
            let f = self.module.add_function(name, ret_ty, None);
            on_declare(f);
        }
    }
}
