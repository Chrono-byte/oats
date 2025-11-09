//! Function expression lowering
//!
//! This module handles function expressions: `function name() { }` or `function() { }`
//! Function expressions are similar to arrow functions but use the `function` keyword syntax.

use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
use inkwell::values::{BasicValueEnum, FunctionValue};
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
    /// Lowers a function expression.
    ///
    /// Function expressions are similar to arrow functions but use named function syntax.
    /// They can be anonymous or have a name (for recursion).
    /// For now, we treat them similarly to arrow functions but may need different handling
    /// for `this` binding and hoisting behavior.
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_fn_expr(
        &self,
        fn_expr: &FnExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Convert FnExpr to ArrowExpr-like structure for lowering
        // Function expressions have similar semantics to arrow functions in terms of
        // closure capture, but differ in `this` binding and hoisting

        // Extract parameters - FnExpr has Function which has params: Vec<Param>
        let params = fn_expr.function.params.clone();

        // Extract body - Function has body: Option<BlockStmt>
        let body = if let Some(block) = &fn_expr.function.body {
            ArrowBody::Block(block.clone())
        } else {
            return Err(Diagnostic::simple_with_span_boxed(
                crate::diagnostics::Severity::Error,
                "function expression must have a body",
                fn_expr.span.start,
            ));
        };

        // Create an ArrowExpr to reuse arrow function lowering logic
        // Note: This is a simplification - function expressions have different `this` semantics
        // but for initial implementation, we can reuse the arrow function infrastructure
        let arrow_expr = ArrowExpr {
            params,
            body,
            return_type: fn_expr.function.return_type.clone(),
            span: fn_expr.span.clone(),
        };

        // Use arrow function lowering (may need adjustments for `this` binding later)
        self.lower_arrow_expr(&arrow_expr, function, param_map, locals)
    }
}
