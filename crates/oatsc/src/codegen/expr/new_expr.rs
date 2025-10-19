use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
use crate::types::OatsType;
use deno_ast::swc::ast;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use inkwell::types::BasicTypeEnum;
use std::collections::HashMap;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
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
    pub(super) fn lower_new_expr(
        &self,
        new_expr: &ast::NewExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        if let ast::Expr::Ident(ident) = &*new_expr.callee {
            let ctor_name = format!("{}_ctor", ident.sym);
            if let Some(fv) = self.module.get_function(&ctor_name) {
                let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> =
                    Vec::new();
                if let Some(args) = &new_expr.args {
                    for a in args {
                        if let Ok(val) =
                            self.lower_expr(&a.expr, function, param_map, locals)
                        {
                            lowered_args.push(val.into());
                        } else {
                            return Err(Diagnostic::simple("expression lowering failed"))?;
                        }
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
                        call_args
                            .push(self.i8ptr_t.const_null().as_basic_value_enum().into());
                    }
                }

                let cs = self
                    .builder
                    .build_call(fv, &call_args, "new_call")
                    .map_err(|_| Diagnostic::simple("LLVM builder error"))?;
                let either = cs.try_as_basic_value();
                match either {
                    inkwell::Either::Left(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple("operation not supported")),
                }
            } else {
                Err(Diagnostic::simple_with_span(
                    "unknown constructor or missing `<Name>_ctor` function",
                    new_expr.span.lo.0 as usize,
                ))
            }
        } else {
            Err(Diagnostic::simple_with_span(
                "unsupported `new` callee: only identifier constructors are supported",
                new_expr.span.lo.0 as usize,
            ))
        }
    }
}
