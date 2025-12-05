//! Function expression lowering
//!
//! This module handles function expressions: `function name() { }` or `function() { }`
//! Function expressions are similar to arrow functions but use the `function` keyword syntax.

use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
use inkwell::values::{BasicValueEnum, FunctionValue};
use oxc_ast::ast::*;
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
    /// Lowers a function expression.
    ///
    /// Function expressions are similar to arrow functions but use named function syntax.
    /// They can be anonymous or have a name (for recursion).
    /// For now, we treat them similarly to arrow functions but may need different handling
    /// for `this` binding and hoisting behavior.
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_fn_expr(
        &self,
        fn_expr: &oxc_ast::ast::FunctionExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Function expressions are similar to arrow functions but use the `function` keyword syntax.
        // They can be anonymous or have a name (for recursion).
        // For now, we treat them similarly to arrow functions but may need different handling
        // for `this` binding and hoisting behavior.

        // Extract parameters - FunctionExpression has function which has params
        let params = fn_expr.function.params.iter().map(|p| p.pattern.clone()).collect();

        // Extract body - Function has body
        let body = match &fn_expr.function.body {
            Some(oxc_ast::ast::FunctionBody::BlockStatement(block)) => {
                crate::codegen::expr::arrow_expr::ArrowBody::Block(block.clone())
            }
            _ => {
                return Err(Diagnostic::simple_with_span_boxed(
                    crate::diagnostics::Severity::Error,
                    "function expression must have a body",
                    fn_expr.span.start,
                ));
            }
        };

        // Create an ArrowFunctionExpression-like structure to reuse arrow function lowering logic
        // Note: This is a simplification - function expressions have different `this` semantics
        let arrow_expr = ArrowFunctionExpression {
            span: fn_expr.span,
            r#async: false,
            generator: false,
            params,
            body,
            return_type: fn_expr.function.return_type.clone(),
            span: fn_expr.span.clone(),
        };

        // Use arrow function lowering (may need adjustments for `this` binding later)
        self.lower_arrow_expr(&arrow_expr, function, param_map, locals)
    }
}
