//! Code generation for super expressions.
//!
//! Super expressions (`super`, `super.prop`, `super.method()`) are used to
//! access parent class members. This module handles standalone `super` expressions,
//! while `super.prop` and `super.method()` are handled in member_access.rs and calls.rs.

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oats_ast::*;
use std::collections::HashMap;

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    /// Lowers a super expression.
    ///
    /// Standalone `super` expressions return the `this` pointer, which can then
    /// be used for member access or method calls. In practice, `super` is usually
    /// used as `super.prop` or `super.method()`, which are handled by member_access
    /// and calls modules respectively.
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_super_expr(
        &self,
        _super_expr: &SuperExpr,
        function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Super expressions in isolation should return `this`
        // Get the first parameter which should be `this` for methods/constructors
        if let Some(this_param) = function.get_nth_param(0) {
            if let BasicValueEnum::PointerValue(this_ptr) = this_param {
                Ok(this_ptr.as_basic_value_enum())
            } else {
                Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "super used but 'this' parameter is not a pointer",
                ))
            }
        } else {
            Err(Diagnostic::simple_boxed(
                Severity::Error,
                "super used in function with no 'this' parameter",
            ))
        }
    }
}
