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
    use super::{CodeGen, Diagnostic, PointerValue, Severity};

    /// Increment reference count for a pointer value
    pub fn rc_inc_value<'a>(
        codegen: &CodeGen<'a>,
        ptr: PointerValue<'a>,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        let rc_inc = codegen.get_rc_inc();
        codegen
            .builder
            .build_call(rc_inc, &[ptr.into()], name)
            .map_err(|_| {
                Diagnostic::simple_boxed(Severity::Error, "failed to build rc_inc call")
            })?;
        Ok(())
    }

    /// Decrement reference count for a pointer value
    pub fn rc_dec_value<'a>(
        codegen: &CodeGen<'a>,
        ptr: PointerValue<'a>,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        let rc_dec = codegen.get_rc_dec();
        codegen
            .builder
            .build_call(rc_dec, &[ptr.into()], name)
            .map_err(|_| {
                Diagnostic::simple_boxed(Severity::Error, "failed to build rc_dec call")
            })?;
        Ok(())
    }
}

/// Type checking utilities for LLVM types
pub mod types {
    /// Check if an LLVM value is a pointer type (heap allocated)
    #[must_use]
    pub fn is_pointer_type(value: &inkwell::values::BasicValueEnum<'_>) -> bool {
        value.get_type().is_pointer_type()
    }

    /// Check if an LLVM value is a float type
    #[must_use]
    pub fn is_float_type(value: &inkwell::values::BasicValueEnum<'_>) -> bool {
        value.get_type().is_float_type()
    }

    /// Check if an LLVM value is an integer type
    #[must_use]
    pub fn is_int_type(value: &inkwell::values::BasicValueEnum<'_>) -> bool {
        value.get_type().is_int_type()
    }
}

/// Runtime function access helpers
pub mod runtime {
    use super::{CodeGen, Diagnostic, Severity};

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
    use super::{CodeGen, Diagnostic, PointerValue, Severity};

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
            .map_err(|_| {
                Diagnostic::simple_boxed(Severity::Error, "failed to build malloc call")
            })?;
        let ptr = match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
            _ => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc call returned no value",
                ));
            }
        };
        Ok(ptr)
    }

    /// Allocate an array using the runtime `array_alloc` function
    pub fn allocate_array<'a>(
        codegen: &CodeGen<'a>,
        length: inkwell::values::IntValue<'a>,
        name: &str,
    ) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
        let array_alloc_fn = codegen.get_array_alloc();
        let call_site = codegen
            .builder
            .build_call(array_alloc_fn, &[length.into()], name)
            .map_err(|_| {
                Diagnostic::simple_boxed(Severity::Error, "failed to build array_alloc call")
            })?;
        let ptr = match call_site.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(bv) => bv.into_pointer_value(),
            _ => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "array_alloc call returned no value",
                ));
            }
        };
        Ok(ptr)
    }
}

/// LLVM constant creation helpers
pub mod constants {
    use super::CodeGen;

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

/// String builder optimization helpers
pub mod string_builder {
    use super::{CodeGen, Diagnostic, PointerValue, Severity};

    /// A string builder that pre-allocates a buffer and avoids multiple concatenations
    pub struct StringBuilder<'a> {
        codegen: &'a CodeGen<'a>,
        buffer_ptr: Option<PointerValue<'a>>,
        capacity: inkwell::values::IntValue<'a>,
        length: inkwell::values::IntValue<'a>,
        is_temporary: bool,
    }

    impl<'a> StringBuilder<'a> {
        /// Create a new string builder with estimated capacity
        pub fn new(
            codegen: &'a CodeGen<'a>,
            estimated_capacity: u64,
        ) -> crate::diagnostics::DiagnosticResult<Self> {
            let buffer_ptr = super::memory::allocate_memory(
                codegen,
                codegen.i64_t.const_int(estimated_capacity, false),
                "str_builder_buf",
            )?;

            let header_ptr = unsafe {
                codegen
                    .builder
                    .build_gep(
                        codegen.i8_t,
                        buffer_ptr,
                        &[codegen.i64_t.const_int(0, false)],
                        "str_builder_header",
                    )
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "failed to build GEP for header")
                    })?
            };
            let header_i64_ptr = codegen
                .builder
                .build_pointer_cast(
                    header_ptr,
                    codegen.context.ptr_type(inkwell::AddressSpace::default()),
                    "header_i64_ptr",
                )
                .map_err(|_| {
                    Diagnostic::simple_boxed(Severity::Error, "failed to cast header pointer")
                })?;
            let header_val = codegen.i64_t.const_int(1, false);
            codegen
                .builder
                .build_store(header_i64_ptr, header_val)
                .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to store header"))?;

            let length_ptr = unsafe {
                codegen
                    .builder
                    .build_gep(
                        codegen.i8_t,
                        buffer_ptr,
                        &[codegen.i64_t.const_int(8, false)],
                        "str_builder_len",
                    )
                    .map_err(|_| {
                        Diagnostic::simple_boxed(Severity::Error, "failed to build GEP for length")
                    })?
            };
            let length_i64_ptr = codegen
                .builder
                .build_pointer_cast(
                    length_ptr,
                    codegen.context.ptr_type(inkwell::AddressSpace::default()),
                    "length_i64_ptr",
                )
                .map_err(|_| {
                    Diagnostic::simple_boxed(Severity::Error, "failed to cast length pointer")
                })?;
            let length_val = codegen.i64_t.const_int(0, false);
            codegen
                .builder
                .build_store(length_i64_ptr, length_val)
                .map_err(|_| Diagnostic::simple_boxed(Severity::Error, "failed to store length"))?;

            Ok(Self {
                codegen,
                buffer_ptr: Some(buffer_ptr),
                capacity: codegen.i64_t.const_int(estimated_capacity, false),
                length: length_val,
                is_temporary: true,
            })
        }

        /// Append a string to the builder (simplified version)
        pub fn append_str(
            &mut self,
            str_ptr: PointerValue<'a>,
        ) -> crate::diagnostics::DiagnosticResult<()> {
            let strcat_fn = self
                .codegen
                .module
                .get_function("str_concat")
                .ok_or_else(|| {
                    Diagnostic::simple_boxed(Severity::Error, "str_concat function not found")
                })?;

            let current_ptr = self.get_current_string()?;
            let call_site = self
                .codegen
                .builder
                .build_call(
                    strcat_fn,
                    &[current_ptr.into(), str_ptr.into()],
                    "builder_append",
                )
                .map_err(|_| {
                    Diagnostic::simple_boxed(Severity::Error, "failed to append to string builder")
                })?;

            match call_site.try_as_basic_value() {
                inkwell::values::ValueKind::Basic(bv) => {
                    self.buffer_ptr = Some(bv.into_pointer_value());
                    Ok(())
                }
                _ => Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "str_concat returned non-pointer in builder",
                )),
            }
        }

        /// Get current string pointer
        fn get_current_string(&self) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
            if let Some(ptr) = self.buffer_ptr {
                self.codegen.offset_i8_ptr(ptr, 16, "current_str")
            } else {
                self.codegen.intern_string_literal("")
            }
        }

        /// Finalize and return the built string
        pub fn finalize(self) -> crate::diagnostics::DiagnosticResult<PointerValue<'a>> {
            self.get_current_string()
        }
    }
}

/// Function attribute optimizations
pub mod function_attributes {
    use super::CodeGen;

    /// Add optimization attributes to a function for better LLVM optimization
    pub fn add_optimization_attributes<'a>(
        codegen: &CodeGen<'a>,
        function: inkwell::values::FunctionValue<'a>,
        is_pure: bool,
        is_readonly: bool,
        is_nofree: bool,
        will_return: bool,
    ) {
        use inkwell::attributes::Attribute;

        let add_attr = |attr_name: &str| {
            let attr_id = Attribute::get_named_enum_kind_id(attr_name);
            function.add_attribute(
                inkwell::attributes::AttributeLoc::Function,
                codegen.context.create_enum_attribute(attr_id, 0),
            );
        };

        add_attr("nounwind");

        if is_pure {
            add_attr("pure");
        }

        if is_readonly {
            add_attr("readonly");
        }

        if is_nofree {
            add_attr("nofree");
        }

        if will_return {
            add_attr("willreturn");
        }
    }
}

/// Constant folding optimization helpers
pub mod constant_folding {
    use super::CodeGen;
    use inkwell::values::BasicValue;

    /// Try to fold a binary operation at compile time
    pub fn try_fold_binary_op<'a>(
        codegen: &CodeGen<'a>,
        left: inkwell::values::BasicValueEnum<'a>,
        right: inkwell::values::BasicValueEnum<'a>,
        op: &oxc_ast::ast::BinaryOperator,
    ) -> Option<inkwell::values::BasicValueEnum<'a>> {
        use oxc_ast::ast::BinaryOperator;

        if !left.is_const() || !right.is_const() {
            return None;
        }

        match (left, right) {
            (
                inkwell::values::BasicValueEnum::FloatValue(lf),
                inkwell::values::BasicValueEnum::FloatValue(rf),
            ) => {
                let (l, _) = lf.get_constant()?;
                let (r, _) = rf.get_constant()?;

                let result = match op {
                    BinaryOperator::Addition => l + r,
                    BinaryOperator::Subtraction => l - r,
                    BinaryOperator::Multiplication => l * r,
                    BinaryOperator::Division => l / r,
                    BinaryOperator::Remainder => l % r,
                    _ => return None,
                };

                Some(codegen.f64_t.const_float(result).as_basic_value_enum())
            }
            _ => None,
        }
    }
}

/// Variable resolution helpers
pub mod variables {
    use super::{BasicValueEnum, CodeGen, FunctionValue, HashMap, OatsType, PointerValue};

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
