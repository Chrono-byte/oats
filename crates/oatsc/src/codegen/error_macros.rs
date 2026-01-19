//! Error handling helper macros
//!
//! This module provides macros to eliminate boilerplate error handling code
//! across the codebase, making error messages consistent and reducing redundancy.

/// Macro to handle builder errors with consistent error messages
#[macro_export]
macro_rules! build_call_with_error {
    ($builder:expr, $function:expr, $args:expr, $name:expr, $error_msg:expr) => {
        $builder
            .build_call($function, $args, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
    ($builder:expr, $function:expr, $args:expr, $name:expr) => {
        build_call_with_error!($builder, $function, $args, $name, "LLVM builder error")
    };
}

/// Macro to handle instruction building with consistent error messages
#[macro_export]
macro_rules! build_instruction_with_error {
    ($builder:expr, $method:ident, $($arg:expr),*) => {
        $builder
            .$method($($arg),*)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("LLVM builder error"))?
    };
    ($builder:expr, $method:ident, $($arg:expr),*, $error_msg:expr) => {
        $builder
            .$method($($arg),*)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for alloca operations with error handling
#[macro_export]
macro_rules! build_alloca_with_error {
    ($codegen:expr, $ty:expr, $name:expr) => {
        $codegen
            .builder
            .build_alloca($ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("alloca failed"))?
    };
    ($codegen:expr, $ty:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_alloca($ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for GEP (get element pointer) operations with error handling
#[macro_export]
macro_rules! build_gep_with_error {
    ($codegen:expr, $ptr:expr, $indices:expr, $name:expr) => {
        $codegen
            .builder
            .build_gep($ptr, $indices, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("GEP failed"))?
    };
    ($codegen:expr, $ptr:expr, $indices:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_gep($ptr, $indices, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for load operations with error handling
#[macro_export]
macro_rules! build_load_with_error {
    ($codegen:expr, $ptr:expr, $name:expr) => {
        $codegen
            .builder
            .build_load($ptr, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("load failed"))?
    };
    ($codegen:expr, $ptr:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_load($ptr, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for store operations with error handling
#[macro_export]
macro_rules! build_store_with_error {
    ($codegen:expr, $value:expr, $ptr:expr) => {
        $codegen
            .builder
            .build_store($ptr, $value)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("store failed"))?
    };
    ($codegen:expr, $value:expr, $ptr:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_store($ptr, $value)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for pointer cast operations with error handling
#[macro_export]
macro_rules! build_pointer_cast_with_error {
    ($codegen:expr, $value:expr, $ty:expr, $name:expr) => {
        $codegen
            .builder
            .build_pointer_cast($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("pointer cast failed"))?
    };
    ($codegen:expr, $value:expr, $ty:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_pointer_cast($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for integer cast operations with error handling
#[macro_export]
macro_rules! build_int_cast_with_error {
    ($codegen:expr, $value:expr, $ty:expr, $name:expr) => {
        $codegen
            .builder
            .build_int_cast($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("int cast failed"))?
    };
    ($codegen:expr, $value:expr, $ty:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_int_cast($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}

/// Macro for integer to float cast operations with error handling
#[macro_export]
macro_rules! build_int_to_float_with_error {
    ($codegen:expr, $value:expr, $ty:expr, $name:expr) => {
        $codegen
            .builder
            .build_int_to_float($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error("int to float cast failed"))?
    };
    ($codegen:expr, $value:expr, $ty:expr, $name:expr, $error_msg:expr) => {
        $codegen
            .builder
            .build_int_to_float($value, $ty, $name)
            .map_err(|_| $crate::diagnostics::Diagnostic::error($error_msg))?
    };
}
