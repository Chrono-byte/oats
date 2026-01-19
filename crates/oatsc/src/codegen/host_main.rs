//! Main function generation.
//!
//! This module handles emitting the C-compatible `main` entry point that directly
//! contains the user's main function body, without creating a separate function.

use crate::codegen::CodeGen;
use crate::diagnostics::Diagnostic;
use std::collections::HashMap;

impl<'a> CodeGen<'a> {
    /// Emit the main function with C ABI, inlining the user's main function body directly.
    ///
    /// This generates:
    ///   define i32 @main(i32 %argc, ptr %argv) {
    ///     entry:
    ///       <user's main body statements, excluding final return>
    ///       ret i32 0
    ///   }
    pub fn emit_main_function(
        &self,
        func_decl: &'a oxc_ast::ast::Function<'a>,
        _ret_type: &crate::types::OatsType,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        let i32_t = self.context.i32_type();
        let argv_type = self.context.ptr_type(inkwell::AddressSpace::default());

        let main_fn = self.module.add_function(
            "main",
            i32_t.fn_type(&[i32_t.into(), argv_type.into()], false),
            None,
        );

        main_fn.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            self.context.create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id("nounwind"),
                0,
            ),
        );

        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Scan for nested functions and arrow functions in the body
        if let Some(body) = &func_decl.body {
            self.gen_nested_decls(&body.statements, "main")?;
        }

        // Set up empty parameter map since main has no parameters from user perspective
        let param_map: HashMap<String, u32> = HashMap::new();
        let mut locals_stack = vec![HashMap::new()];

        // Lower the main function body directly into the entry block,
        // but exclude the final return statement if present (we'll emit our own i32 return)
        if let Some(body) = &func_decl.body {
            let statements_to_lower = if body.statements.is_empty() {
                &body.statements[..]
            } else {
                // Check if the last statement is a return statement
                match &body.statements[body.statements.len() - 1] {
                    oxc_ast::ast::Statement::ReturnStatement(_) => {
                        // Exclude the final return statement
                        &body.statements[..body.statements.len() - 1]
                    }
                    _ => &body.statements[..],
                }
            };

            let _emitted_terminator =
                self.lower_stmts(statements_to_lower, main_fn, &param_map, &mut locals_stack)?;
        }

        // Always emit a return i32 0 for the C entry point
        self.emit_rc_dec_for_locals(&locals_stack)?;
        let _ = self.builder.build_return(Some(&i32_t.const_int(0, false)));

        Ok(())
    }
}
