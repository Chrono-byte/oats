//! Code generation for optional chaining expressions (`?.`).
//!
//! Optional chaining (`obj?.prop`, `arr?.[0]`) safely accesses properties/methods
//! when the object might be null or undefined. If the object is null/undefined,
//! the entire expression evaluates to `undefined` (represented as a null pointer).

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValueEnum, FunctionValue};
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
    /// Lowers an optional member expression (`obj?.prop` or `arr?.[0]`).
    ///
    /// Generates code that checks for null/undefined before accessing the member.
    /// If the object is null/undefined, returns null pointer (undefined).
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_optional_member_expr(
        &self,
        _optional: &oxc_ast::ast::ChainExpression<'_>,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Lower the expression from the chain
        // ChainExpression has an expression field which is a ChainElement
        // We need to extract the Expression from the ChainElement
        // For now, return an error since ChainElement structure needs proper handling
        // TODO: Properly handle ChainElement to extract Expression
        // ChainElement is an enum that may contain different types of chain elements
        // We need to match on it to extract the underlying Expression
        Err(Diagnostic::simple_boxed(
            Severity::Error,
            "Optional chaining (ChainExpression) not yet fully implemented - ChainElement structure needs handling",
        ))
    }
}
