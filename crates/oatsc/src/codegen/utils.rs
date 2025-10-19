//! Shared utilities for code generation operations.
//!
//! This module contains common patterns and helper functions extracted from
//! expression lowering code to reduce duplication and improve maintainability.

use inkwell::values::FunctionValue;
use inkwell::values::{BasicValueEnum, PointerValue};
use std::collections::HashMap;

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;

/// Reference counting operation helpers
pub mod rc {
    use super::*;

    /// Increment reference count for a pointer value
    pub fn rc_inc_value<'a>(codegen: &CodeGen<'a>, ptr: PointerValue<'a>, name: &str) {
        let rc_inc = codegen.get_rc_inc();
        let _ = codegen.builder.build_call(rc_inc, &[ptr.into()], name);
    }

    /// Decrement reference count for a pointer value
    pub fn rc_dec_value<'a>(codegen: &CodeGen<'a>, ptr: PointerValue<'a>, name: &str) {
        let rc_dec = codegen.get_rc_dec();
        let _ = codegen.builder.build_call(rc_dec, &[ptr.into()], name);
    }
}

/// Type checking utilities for LLVM types
pub mod types {
    /// Check if an LLVM value is a pointer type (heap allocated)
    pub fn is_pointer_type<'a>(value: &inkwell::values::BasicValueEnum<'a>) -> bool {
        value.get_type().is_pointer_type()
    }

    /// Check if an LLVM value is a float type
    pub fn is_float_type<'a>(value: &inkwell::values::BasicValueEnum<'a>) -> bool {
        value.get_type().is_float_type()
    }

    /// Check if an LLVM value is an integer type
    pub fn is_int_type<'a>(value: &inkwell::values::BasicValueEnum<'a>) -> bool {
        value.get_type().is_int_type()
    }
}

/// Runtime function access helpers
pub mod runtime {
    use super::*;

    /// Get the array allocation function
    pub fn get_array_alloc<'a>(codegen: &CodeGen<'a>) -> inkwell::values::FunctionValue<'a> {
        codegen.get_array_alloc()
    }

    /// Get the malloc function
    pub fn get_malloc<'a>(codegen: &CodeGen<'a>) -> inkwell::values::FunctionValue<'a> {
        codegen.get_malloc()
    }

    /// Get the string concatenation function
    pub fn get_str_concat<'a>(
        codegen: &CodeGen<'a>,
    ) -> crate::diagnostics::DiagnosticResult<inkwell::values::FunctionValue<'a>> {
        codegen.gen_str_concat();
        codegen.module.get_function("str_concat").ok_or_else(|| {
            Diagnostic::simple_boxed(Severity::Error, "str_concat function not found")
        })
    }

    /// Get the number to string function
    pub fn get_number_to_string<'a>(codegen: &CodeGen<'a>) -> inkwell::values::FunctionValue<'a> {
        codegen.get_number_to_string()
    }
}

/// Memory allocation helpers
pub mod memory {
    use super::*;

    /// Allocate memory using the runtime malloc function
    pub fn allocate_memory<'a>(
        codegen: &CodeGen<'a>,
        size: inkwell::values::IntValue<'a>,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let malloc_fn = codegen.get_malloc();
        let call_site = codegen
            .builder
            .build_call(malloc_fn, &[size.into()], name)
            .map_err(|_| Diagnostic::error("failed to build malloc call"))?;
        let ptr = call_site
            .try_as_basic_value()
            .left()
            .ok_or_else(|| Diagnostic::error("malloc call returned no value"))?;
        Ok(ptr.into_pointer_value())
    }

    /// Allocate an array using the runtime array_alloc function
    pub fn allocate_array<'a>(
        codegen: &CodeGen<'a>,
        length: inkwell::values::IntValue<'a>,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let array_alloc_fn = codegen.get_array_alloc();
        let call_site = codegen
            .builder
            .build_call(array_alloc_fn, &[length.into()], name)
            .map_err(|_| Diagnostic::error("failed to build array_alloc call"))?;
        let ptr = call_site
            .try_as_basic_value()
            .left()
            .ok_or_else(|| Diagnostic::error("array_alloc call returned no value"))?;
        Ok(ptr.into_pointer_value())
    }
}

/// LLVM constant creation helpers
pub mod constants {
    use super::*;

    /// Create an i32 constant
    pub fn i32_const<'a>(codegen: &CodeGen<'a>, value: u64) -> inkwell::values::IntValue<'a> {
        codegen.i32_t.const_int(value, false)
    }

    /// Create an i64 constant
    pub fn i64_const<'a>(codegen: &CodeGen<'a>, value: u64) -> inkwell::values::IntValue<'a> {
        codegen.i64_t.const_int(value, false)
    }

    /// Create a zero i32 constant
    pub fn zero_i32<'a>(codegen: &CodeGen<'a>) -> inkwell::values::IntValue<'a> {
        i32_const(codegen, 0)
    }

    /// Create a zero i64 constant
    pub fn zero_i64<'a>(codegen: &CodeGen<'a>) -> inkwell::values::IntValue<'a> {
        i64_const(codegen, 0)
    }
}

/// Variable resolution helpers
pub mod variables {
    use super::*;

    /// Information about a local variable in the codegen context
    type LocalVarInfo<'a> = (
        PointerValue<'a>,
        inkwell::types::BasicTypeEnum<'a>,
        bool,
        bool,
        Option<String>,
        Option<String>,
        OatsType,
    );

    /// Resolve a variable by name, checking parameters first, then locals
    pub fn resolve_variable<'a>(
        codegen: &CodeGen<'a>,
        name: &str,
        param_map: &HashMap<String, usize>,
        function: FunctionValue<'a>,
        locals: &HashMap<String, LocalVarInfo<'a>>,
    ) -> Option<BasicValueEnum<'a>> {
        // Check parameters first
        if let Some(&idx) = param_map.get(name)
            && let Some(param) = function.get_nth_param(idx as u32)
        {
            return Some(param);
        }

        // Check locals
        if let Some((ptr, ty, _init, _is_const, _extra, _nominal, _oats_type)) = locals.get(name) {
            // Load the local variable
            if let Ok(loaded) = codegen.builder.build_load(*ty, *ptr, name) {
                return Some(loaded);
            }
        }

        None
    }
}
