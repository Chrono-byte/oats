use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use crate::types::OatsType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oxc_ast::ast::*;
use std::collections::HashMap;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
#[allow(dead_code)]
type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> CodeGen<'a> {
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_new_expr(
        &self,
        new_expr: &NewExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        if let Expression::Identifier(ident) = &new_expr.callee {
            let name = ident.name.to_string();
            // Resolve class name through symbol table
            let class_name = if let Some(crate::types::Symbol::Type { mangled_name }) =
                self.symbol_table.borrow().get(&name)
            {
                mangled_name.clone()
            } else {
                name
            };

            let ctor_name = format!("{}_ctor", class_name);
            if let Some(fv) = self.module.get_function(&ctor_name) {
                let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                for arg in &new_expr.arguments {
                    // Spread arguments are not yet supported in lowering.
                    let expr = arg.as_expression().ok_or_else(|| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "spread arguments are not supported",
                            new_expr.span.start as usize,
                        )
                    })?;
                    if let Ok(val) = self.lower_expr(expr, function, param_map, locals) {
                        lowered_args.push(val.into());
                    } else {
                        return Err(Diagnostic::simple_boxed(
                            Severity::Error,
                            "expression lowering failed",
                        ))?;
                    }
                }
                // Ensure the number of arguments matches the constructor's
                // declared parameter count. Truncate extra args or pad with
                // nulls if the function expects more parameters. This fixes
                // ABI mismatches where the constructor signature and callsite
                // disagree.
                let expected = fv.count_params() as usize;
                let mut call_args = lowered_args.clone();
                if call_args.len() > expected {
                    call_args.truncate(expected);
                } else if call_args.len() < expected {
                    // pad with null pointers
                    while call_args.len() < expected {
                        call_args.push(self.i8ptr_t.const_null().as_basic_value_enum().into());
                    }
                }

                let cs = self
                    .builder
                    .build_call(fv, &call_args, "new_call")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation not supported",
                    )),
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unknown constructor or missing `<Name>_ctor` function",
                    new_expr.span.start as usize,
                ))
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unsupported `new` callee: only identifier constructors are supported",
                new_expr.span.start as usize,
            ))
        }
    }
}
