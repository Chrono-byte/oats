//! Function expression lowering
//!
//! This module handles function expressions: `function name() { }` or `function() { }`
//! Function expressions are similar to arrow functions but use the `function` keyword syntax.
//! The main difference is that function expressions have their own `this` binding,
//! while arrow functions inherit `this` from the enclosing scope.

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValueEnum, FunctionValue};
use std::collections::HashMap;

#[allow(dead_code)]
type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    /// Lowers a function expression.
    ///
    /// Function expressions contain `Box<Function>` which has a different structure
    /// than `ArrowFunctionExpression`. We convert it to a closure similar to arrow functions,
    /// but note that function expressions have their own `this` binding (unlike arrow functions).
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_fn_expr(
        &self,
        fn_expr: &oxc_ast::ast::Function<'_>,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // FunctionExpression contains Box<Function>, which has:
        // - id: Option<Identifier> (function name, if any)
        // - params: FormalParameters
        // - body: FunctionBody
        // - generator: bool
        // - async: bool
        // - etc.

        // For now, we'll treat function expressions similarly to arrow functions
        // by creating a closure. The main difference is `this` binding:
        // - Arrow functions: inherit `this` from enclosing scope
        // - Function expressions: have their own `this` (bound when called)

        // Since we can't easily convert Function to ArrowFunctionExpression,
        // we'll use the arrow function lowering logic but note that `this` handling
        // may need adjustment in the future.

        // Extract parameters and body from Function
        // Function.params is FormalParameters, similar to ArrowFunctionExpression
        // Function.body is FunctionBody, which contains statements

        // For now, return an error indicating this needs more work
        // A full implementation would need to:
        // 1. Extract params from fn_expr.params
        // 2. Extract body from fn_expr.body
        // 3. Create a closure (similar to arrow functions)
        // 4. Handle `this` binding appropriately

        // Since this is complex and FunctionExpression is less common than arrow functions,
        // we'll provide a basic implementation that reuses arrow function logic where possible
        // but note the limitations

        // Function expressions are not yet fully supported. A complete implementation would:
        // 1. Create a synthetic ArrowFunctionExpression-like structure from Function
        // 2. Call lower_arrow_expr with that structure
        // 3. Handle `this` binding differences (function expressions have their own `this` binding)
        // 4. Handle function name (for recursion) if fn_expr.id is Some
        Err(Diagnostic::simple_with_span_boxed(
            Severity::Error,
            "FunctionExpression support is not yet implemented - function expressions need conversion to closure format",
            fn_expr.span.start as usize,
        ))
    }
}
