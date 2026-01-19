//! Runtime function getter macros
//!
//! This module provides macros to eliminate boilerplate code for runtime function
//! getter methods in the `CodeGen` struct.

/// Macro to generate a runtime function getter with caching
///
/// # Arguments
/// - `$field_name`: Name of the `RefCell`<Option<FunctionValue>> field in `CodeGen`
/// - `$function_name`: Name of the runtime function constant
/// - `$param_types`: Array of parameter types for the function signature
/// - `$return_type`: Return type of the function
/// - `$doc`: Documentation string for the functio
#[macro_export]
macro_rules! declare_runtime_function {
    (
        $(#[doc = $doc:expr])?
        fn $field_name:ident,
        $function_name:expr,
        &$param_types:expr,
        $return_type:expr
    ) => {
        $(#[doc = $doc])?
        pub fn $field_name(&self) -> inkwell::values::FunctionValue<'a> {
            if let Some(f) = *self.$field_name.borrow() {
                return f;
            }
            let fn_type = $return_type.fn_type($param_types, false);
            let f = self
                .module
                .add_function($function_name, fn_type, None);
            *self.$field_name.borrow_mut() = Some(f);
            f
        }
    };
}

/// Macro to generate runtime function getters with module existence check
#[macro_export]
macro_rules! declare_runtime_function_with_check {
    (
        $(#[doc = $doc:expr])?
        fn $field_name:ident,
        $function_name:expr,
        &$param_types:expr,
        $return_type:expr
    ) => {
        $(#[doc = $doc])?
        fn $field_name(&self) -> inkwell::values::FunctionValue<'a> {
            self.module.get_function($function_name).map_or_else(
                || {
                    let fn_type = $return_type.fn_type($param_types, false);
                    self.module.add_function($function_name, fn_type, None)
                },
                std::convert::identity,
            )
        }
    };
}
