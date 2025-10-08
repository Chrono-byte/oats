use crate::diagnostics::Diagnostic;
use deno_ast::swc::ast::Expr;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;
use crate::codegen::helpers::*;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Thin adapter that converts the existing Option-based `lower_expr` into
    /// a Result carrying a `Diagnostic` so callers can centrally report errors.
    pub fn lower_expr_result(
        &self,
        expr: &Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut Vec<HashMap<String, (inkwell::values::PointerValue<'a>, inkwell::types::BasicTypeEnum<'a>, bool, bool)>>,
        ctx_name: Option<&str>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        if let Some(v) = self.lower_expr(expr, function, param_map, locals) {
            Ok(v)
        } else {
            let msg = format!("failed to lower expression in context '{}'", ctx_name.unwrap_or("<unknown>"));
            Err(Diagnostic::simple(msg))
        }
    }
}
