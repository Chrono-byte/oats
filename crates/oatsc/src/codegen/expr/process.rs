//! Process model expression lowering
//!
//! This module provides codegen for process-related expressions:
//! spawn, send, receive, link, monitor, exit, self, whereis, register, unregister

use crate::diagnostics::{Diagnostic, Severity};
use crate::runtime_functions;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;

type LocalEntry<'a> = (
    PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Lower spawn expression: spawn() or spawn(name, priority)
    pub(super) fn lower_spawn_expr(
        &self,
        spawn_expr: &oats_ast::SpawnExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc(); // Ensure process functions are declared

        // Default priority is Normal (0)
        let priority = if let Some(priority_expr) = &spawn_expr.priority {
            let priority_val = self.lower_expr(priority_expr, function, param_map, locals)?;
            if let BasicValueEnum::IntValue(iv) = priority_val {
                iv
            } else {
                // Coerce to i32 if needed
                self.builder.build_int_cast(
                    priority_val.try_into().map_err(|_| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "spawn priority must be a number",
                            spawn_expr.span.start,
                        )
                    })?,
                    self.i32_t,
                    "priority_cast",
                )
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to cast spawn priority",
                        spawn_expr.span.start,
                    )
                })?
            }
        } else {
            self.i32_t.const_int(0, false) // Normal priority
        };

        let (spawn_fn, name_ptr_opt) = if spawn_expr.name.is_some() {
            // Use process_spawn_named if name is provided
            let name_expr = spawn_expr.name.as_ref().unwrap();
            let name_val = self.lower_expr(name_expr, function, param_map, locals)?;
            let name_ptr = if let BasicValueEnum::PointerValue(pv) = name_val {
                pv
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "spawn name must be a string",
                    spawn_expr.span.start,
                ));
            };

            let fn_val = self.module
                .get_function(runtime_functions::names::PROCESS_SPAWN_NAMED)
                .ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "process_spawn_named not found",
                        spawn_expr.span.start,
                    )
                })?;
            (fn_val, Some(name_ptr))
        } else {
            // Use process_spawn if no name
            let fn_val = self.module
                .get_function(runtime_functions::names::PROCESS_SPAWN)
                .ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "process_spawn not found",
                        spawn_expr.span.start,
                    )
                })?;
            (fn_val, None)
        };

        let result = if let Some(name_ptr) = name_ptr_opt {
            self.builder
                .build_call(spawn_fn, &[name_ptr.into(), priority.into()], "spawn_result")
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to call process_spawn_named",
                        spawn_expr.span.start,
                    )
                })?
        } else {
            self.builder
                .build_call(spawn_fn, &[priority.into()], "spawn_result")
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to call process_spawn",
                        spawn_expr.span.start,
                    )
                })?
        };

        // Result is i8* (pointer to heap-allocated u64 containing ProcessId)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    /// Lower send expression: send(to, message)
    pub(super) fn lower_send_expr(
        &self,
        send_expr: &oats_ast::SendExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let to_val = self.lower_expr(&send_expr.to, function, param_map, locals)?;
        let message_val = self.lower_expr(&send_expr.message, function, param_map, locals)?;

        // Get current process ID (self)
        let self_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SELF)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_self not found",
                    send_expr.span.start,
                )
            })?;
        let self_pid_ptr_val = self
            .builder
            .build_call(self_fn, &[], "self_pid")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_self",
                    send_expr.span.start,
                )
            })?
            .try_as_basic_value()
            .unwrap_left();

        let self_pid_ptr = if let BasicValueEnum::PointerValue(pv) = self_pid_ptr_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "process_self returned invalid type",
                send_expr.span.start,
            ));
        };

        // Determine if 'to' is a ProcessId or a string name
        let to_pid_ptr = if let BasicValueEnum::PointerValue(pv) = to_val {
            // Assume it's a ProcessId pointer (i8* to u64)
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "send target must be ProcessId or string",
                send_expr.span.start,
            ));
        };

        let message_ptr = if let BasicValueEnum::PointerValue(pv) = message_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "send message must be a pointer",
                send_expr.span.start,
            ));
        };

        // Type ID: 0 for now (can be extended later)
        let type_id = self.i64_t.const_int(0, false);

        let send_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SEND)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_send not found",
                    send_expr.span.start,
                )
            })?;

        // Convert pointers to i64* for the FFI call
        let to_pid_i64ptr = self
            .builder
            .build_pointer_cast(to_pid_ptr, self.context.ptr_type(inkwell::AddressSpace::default()), "to_pid_i64ptr")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast to_pid pointer",
                    send_expr.span.start,
                )
            })?;
        let self_pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                self_pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "self_pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast self_pid pointer",
                    send_expr.span.start,
                )
            })?;

        let result = self.builder.build_call(
            send_fn,
            &[
                to_pid_i64ptr.into(),
                self_pid_i64ptr.into(),
                message_ptr.into(),
                type_id.into(),
            ],
            "send_result",
        )
        .map_err(|_| {
            Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "failed to call process_send",
                send_expr.span.start,
            )
        })?;

        // Result is i32 (0 = success, non-zero = error)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    /// Lower receive expression: receive() or receive(type_id)
    pub(super) fn lower_receive_expr(
        &self,
        receive_expr: &oats_ast::ReceiveExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        // Get current process ID (self)
        let self_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SELF)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_self not found",
                    receive_expr.span.start,
                )
            })?;
        let self_pid_ptr_val = self
            .builder
            .build_call(self_fn, &[], "self_pid")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_self",
                    receive_expr.span.start,
                )
            })?
            .try_as_basic_value()
            .unwrap_left();

        let self_pid_ptr = if let BasicValueEnum::PointerValue(pv) = self_pid_ptr_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "process_self returned invalid type",
                receive_expr.span.start,
            ));
        };

        let type_id = if let Some(type_id_expr) = &receive_expr.type_id {
            let type_id_val = self.lower_expr(type_id_expr, function, param_map, locals)?;
            if let BasicValueEnum::IntValue(iv) = type_id_val {
                iv
            } else {
                self.builder.build_int_cast(
                    type_id_val.try_into().map_err(|_| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "receive type_id must be a number",
                            receive_expr.span.start,
                        )
                    })?,
                    self.i64_t,
                    "type_id_cast",
                )
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to cast receive type_id",
                        receive_expr.span.start,
                    )
                })?
            }
        } else {
            self.i64_t.const_int(0, false) // Default type ID
        };

        let receive_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_RECEIVE)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_receive not found",
                    receive_expr.span.start,
                )
            })?;

        let self_pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                self_pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "self_pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast self_pid pointer",
                    receive_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(receive_fn, &[self_pid_i64ptr.into(), type_id.into()], "receive_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_receive",
                    receive_expr.span.start,
                )
            })?;

        // Result is i8* (message payload or null)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    /// Lower process self expression: self()
    pub(super) fn lower_process_self_expr(
        &self,
        _self_expr: &oats_ast::ProcessSelfExpr,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let self_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SELF)
            .ok_or_else(|| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    "process_self not found",
                )
            })?;

        let result = self.builder.build_call(self_fn, &[], "self_pid")
            .map_err(|_| {
                Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to call process_self",
                )
            })?;

        // Result is i8* (pointer to heap-allocated u64 containing ProcessId)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    // Additional process expression lowering functions would go here...
    // For brevity, I'll add stubs for the remaining expressions
    // Full implementation would follow similar patterns

    pub(super) fn lower_process_exit_expr(
        &self,
        exit_expr: &oats_ast::ProcessExitExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        // Get process ID (default to self if not provided)
        let pid_ptr = if let Some(pid_expr) = &exit_expr.pid {
            let pid_val = self.lower_expr(pid_expr, function, param_map, locals)?;
            if let BasicValueEnum::PointerValue(pv) = pid_val {
                pv
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "exit pid must be a ProcessId",
                    exit_expr.span.start,
                ));
            }
        } else {
            // Use self() if no pid provided
            let self_fn = self
                .module
                .get_function(runtime_functions::names::PROCESS_SELF)
                .ok_or_else(|| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "process_self not found",
                        exit_expr.span.start,
                    )
                })?;
            let self_val = self
                .builder
                .build_call(self_fn, &[], "self_pid")
                .map_err(|_| {
                    Diagnostic::simple_with_span_boxed(
                        Severity::Error,
                        "failed to call process_self",
                        exit_expr.span.start,
                    )
                })?;
            if let BasicValueEnum::PointerValue(pv) = self_val.try_as_basic_value().unwrap_left() {
                pv
            } else {
                return Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_self returned invalid type",
                    exit_expr.span.start,
                ));
            }
        };

        // Determine reason type and reason string
        let (reason_type, reason_str_ptr) = if let Some(reason_expr) = &exit_expr.reason {
            let reason_val = self.lower_expr(reason_expr, function, param_map, locals)?;
            // Check if reason is a string or number
            if let BasicValueEnum::PointerValue(pv) = reason_val {
                // Assume it's a string
                (self.i32_t.const_int(1, false), pv) // Error type
            } else {
                // Assume it's a number - treat as normal exit (0)
                (self.i32_t.const_int(0, false), self.i8ptr_t.const_null())
            }
        } else {
            // No reason provided - normal exit
            (self.i32_t.const_int(0, false), self.i8ptr_t.const_null())
        };

        // Convert pid pointer to i64* for FFI
        let pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid pointer",
                    exit_expr.span.start,
                )
            })?;

        let exit_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_EXIT)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_exit not found",
                    exit_expr.span.start,
                )
            })?;

        self.builder
            .build_call(
                exit_fn,
                &[
                    pid_i64ptr.into(),
                    reason_type.into(),
                    reason_str_ptr.into(),
                ],
                "exit_call",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_exit",
                    exit_expr.span.start,
                )
            })?;

        // Exit returns void, return a harmless zero
        Ok(self.f64_t.const_float(0.0).as_basic_value_enum())
    }

    pub(super) fn lower_process_link_expr(
        &self,
        link_expr: &oats_ast::ProcessLinkExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let pid1_val = self.lower_expr(&link_expr.pid1, function, param_map, locals)?;
        let pid2_val = self.lower_expr(&link_expr.pid2, function, param_map, locals)?;

        let pid1_ptr = if let BasicValueEnum::PointerValue(pv) = pid1_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "link pid1 must be a ProcessId",
                link_expr.span.start,
            ));
        };

        let pid2_ptr = if let BasicValueEnum::PointerValue(pv) = pid2_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "link pid2 must be a ProcessId",
                link_expr.span.start,
            ));
        };

        // Convert to i64* for FFI
        let pid1_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid1_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid1_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid1 pointer",
                    link_expr.span.start,
                )
            })?;

        let pid2_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid2_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid2_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid2 pointer",
                    link_expr.span.start,
                )
            })?;

        let link_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_LINK)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_link not found",
                    link_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(link_fn, &[pid1_i64ptr.into(), pid2_i64ptr.into()], "link_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_link",
                    link_expr.span.start,
                )
            })?;

        // Result is i32 (0 = success, non-zero = error)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    pub(super) fn lower_process_unlink_expr(
        &self,
        unlink_expr: &oats_ast::ProcessUnlinkExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let pid1_val = self.lower_expr(&unlink_expr.pid1, function, param_map, locals)?;
        let pid2_val = self.lower_expr(&unlink_expr.pid2, function, param_map, locals)?;

        let pid1_ptr = if let BasicValueEnum::PointerValue(pv) = pid1_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unlink pid1 must be a ProcessId",
                unlink_expr.span.start,
            ));
        };

        let pid2_ptr = if let BasicValueEnum::PointerValue(pv) = pid2_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unlink pid2 must be a ProcessId",
                unlink_expr.span.start,
            ));
        };

        // Convert to i64* for FFI
        let pid1_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid1_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid1_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid1 pointer",
                    unlink_expr.span.start,
                )
            })?;

        let pid2_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid2_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid2_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid2 pointer",
                    unlink_expr.span.start,
                )
            })?;

        let unlink_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_UNLINK)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_unlink not found",
                    unlink_expr.span.start,
                )
            })?;

        self.builder
            .build_call(unlink_fn, &[pid1_i64ptr.into(), pid2_i64ptr.into()], "unlink_call")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_unlink",
                    unlink_expr.span.start,
                )
            })?;

        // Unlink returns void, return a harmless zero
        Ok(self.f64_t.const_float(0.0).as_basic_value_enum())
    }

    pub(super) fn lower_process_monitor_expr(
        &self,
        monitor_expr: &oats_ast::ProcessMonitorExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let target_val = self.lower_expr(&monitor_expr.target, function, param_map, locals)?;
        let target_ptr = if let BasicValueEnum::PointerValue(pv) = target_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "monitor target must be a ProcessId",
                monitor_expr.span.start,
            ));
        };

        // Get current process ID (self)
        let self_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SELF)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_self not found",
                    monitor_expr.span.start,
                )
            })?;
        let self_pid_ptr_val = self
            .builder
            .build_call(self_fn, &[], "self_pid")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_self",
                    monitor_expr.span.start,
                )
            })?;
        let self_pid_ptr = if let BasicValueEnum::PointerValue(pv) = self_pid_ptr_val.try_as_basic_value().unwrap_left() {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "process_self returned invalid type",
                monitor_expr.span.start,
            ));
        };

        // Convert to i64* for FFI
        let monitor_pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                self_pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "monitor_pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast monitor_pid pointer",
                    monitor_expr.span.start,
                )
            })?;

        let target_pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                target_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "target_pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast target_pid pointer",
                    monitor_expr.span.start,
                )
            })?;

        let monitor_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_MONITOR)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_monitor not found",
                    monitor_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(
                monitor_fn,
                &[monitor_pid_i64ptr.into(), target_pid_i64ptr.into()],
                "monitor_result",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_monitor",
                    monitor_expr.span.start,
                )
            })?;

        // Result is i8* (pointer to heap-allocated u64 containing MonitorRef)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    pub(super) fn lower_process_demonitor_expr(
        &self,
        demonitor_expr: &oats_ast::ProcessDemonitorExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let monitor_ref_val = self.lower_expr(&demonitor_expr.monitor_ref, function, param_map, locals)?;
        let monitor_ref_ptr = if let BasicValueEnum::PointerValue(pv) = monitor_ref_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "demonitor monitor_ref must be a MonitorRef",
                demonitor_expr.span.start,
            ));
        };

        // Get current process ID (self)
        let self_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_SELF)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_self not found",
                    demonitor_expr.span.start,
                )
            })?;
        let self_pid_ptr_val = self
            .builder
            .build_call(self_fn, &[], "self_pid")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_self",
                    demonitor_expr.span.start,
                )
            })?;
        let self_pid_ptr = if let BasicValueEnum::PointerValue(pv) = self_pid_ptr_val.try_as_basic_value().unwrap_left() {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "process_self returned invalid type",
                demonitor_expr.span.start,
            ));
        };

        // Convert to i64* for FFI
        let monitor_pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                self_pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "monitor_pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast monitor_pid pointer",
                    demonitor_expr.span.start,
                )
            })?;

        let monitor_ref_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                monitor_ref_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "monitor_ref_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast monitor_ref pointer",
                    demonitor_expr.span.start,
                )
            })?;

        let demonitor_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_DEMONITOR)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_demonitor not found",
                    demonitor_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(
                demonitor_fn,
                &[monitor_pid_i64ptr.into(), monitor_ref_i64ptr.into()],
                "demonitor_result",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_demonitor",
                    demonitor_expr.span.start,
                )
            })?;

        // Result is i32 (0 = success, non-zero = error)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    pub(super) fn lower_process_whereis_expr(
        &self,
        whereis_expr: &oats_ast::ProcessWhereisExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let name_val = self.lower_expr(&whereis_expr.name, function, param_map, locals)?;
        let name_ptr = if let BasicValueEnum::PointerValue(pv) = name_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "whereis name must be a string",
                whereis_expr.span.start,
            ));
        };

        let whereis_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_WHEREIS)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_whereis not found",
                    whereis_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(whereis_fn, &[name_ptr.into()], "whereis_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_whereis",
                    whereis_expr.span.start,
                )
            })?;

        // Result is i8* (pointer to heap-allocated u64 containing ProcessId, or null)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    pub(super) fn lower_process_register_expr(
        &self,
        register_expr: &oats_ast::ProcessRegisterExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let pid_val = self.lower_expr(&register_expr.pid, function, param_map, locals)?;
        let name_val = self.lower_expr(&register_expr.name, function, param_map, locals)?;

        let pid_ptr = if let BasicValueEnum::PointerValue(pv) = pid_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "register pid must be a ProcessId",
                register_expr.span.start,
            ));
        };

        let name_ptr = if let BasicValueEnum::PointerValue(pv) = name_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "register name must be a string",
                register_expr.span.start,
            ));
        };

        // Convert pid to i64* for FFI
        let pid_i64ptr: PointerValue<'a> = self
            .builder
            .build_pointer_cast(
                pid_ptr,
                self.context.ptr_type(inkwell::AddressSpace::default()),
                "pid_i64ptr",
            )
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to cast pid pointer",
                    register_expr.span.start,
                )
            })?;

        let register_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_REGISTER)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_register not found",
                    register_expr.span.start,
                )
            })?;

        let result = self
            .builder
            .build_call(register_fn, &[pid_i64ptr.into(), name_ptr.into()], "register_result")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_register",
                    register_expr.span.start,
                )
            })?;

        // Result is i32 (0 = success, non-zero = error)
        Ok(result.try_as_basic_value().unwrap_left().into())
    }

    pub(super) fn lower_process_unregister_expr(
        &self,
        unregister_expr: &oats_ast::ProcessUnregisterExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        self.declare_libc();

        let name_val = self.lower_expr(&unregister_expr.name, function, param_map, locals)?;
        let name_ptr = if let BasicValueEnum::PointerValue(pv) = name_val {
            pv
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unregister name must be a string",
                unregister_expr.span.start,
            ));
        };

        let unregister_fn = self
            .module
            .get_function(runtime_functions::names::PROCESS_UNREGISTER)
            .ok_or_else(|| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "process_unregister not found",
                    unregister_expr.span.start,
                )
            })?;

        self.builder
            .build_call(unregister_fn, &[name_ptr.into()], "unregister_call")
            .map_err(|_| {
                Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "failed to call process_unregister",
                    unregister_expr.span.start,
                )
            })?;

        // Unregister returns void, return a harmless zero
        Ok(self.f64_t.const_float(0.0).as_basic_value_enum())
    }
}

