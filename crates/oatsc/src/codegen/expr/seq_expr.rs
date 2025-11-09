//! Sequence expression lowering
//!
//! This module handles sequence expressions (comma operator): `(expr1, expr2, expr3)`
//! Sequence expressions evaluate all expressions left-to-right and return the value of the last expression.

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
    /// Lowers a sequence expression (comma operator).
    ///
    /// Evaluates all expressions in order, discarding all but the last value.
    /// The last expression's value is returned.
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_seq_expr(
        &self,
        seq: &SeqExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        if seq.exprs.is_empty() {
            return Err(Diagnostic::simple_with_span_boxed(
                crate::diagnostics::Severity::Error,
                "empty sequence expression",
                seq.span.start,
            ));
        }

        // Evaluate all expressions except the last (for side effects)
        for expr in &seq.exprs[..seq.exprs.len() - 1] {
            self.lower_expr(expr, function, param_map, locals)?;
        }

        // Return the value of the last expression
        self.lower_expr(&seq.exprs[seq.exprs.len() - 1], function, param_map, locals)
    }
}
