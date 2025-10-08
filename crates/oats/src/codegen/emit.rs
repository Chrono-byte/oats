// Top-level IR emission helpers
// Moved from mod.rs during modularization

use inkwell::values::FunctionValue;
use inkwell::types::BasicType;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Generates LLVM IR for a function declaration.
    /// This is the main entry point for function compilation.
    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &deno_ast::swc::ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
        receiver_name: Option<&str>,
    ) -> Result<FunctionValue<'a>, crate::diagnostics::Diagnostic> {
        // 1. Build the LLVM function type.
        let llvm_param_types: Vec<_> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();
        let fn_type = self.build_llvm_fn_type(&llvm_param_types, ret_type);

        // 2. Add the function to the module and set up the entry block.
        self.gen_str_concat(); // Ensure runtime helpers are available.
        let function = self.module.add_function(func_name, fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // 3. Store metadata and create stack allocations for parameters.
        self.fn_param_types
            .borrow_mut()
            .insert(func_name.to_string(), param_types.to_vec());
        let (param_map, mut locals_stack) =
            self.create_param_allocas(function, func_decl, &llvm_param_types, receiver_name)?;

        // 4. Lower the function body statements into IR.
        let mut emitted_terminator = false;
        if let Some(body) = &func_decl.body {
            emitted_terminator =
                self.lower_stmts(&body.stmts, function, &param_map, &mut locals_stack)?;
        }

        // 5. Add an implicit `return void` if the function hasn't already returned.
        if !emitted_terminator
            && self
                .builder
                .get_insert_block()
                .is_none_or(|b| b.get_terminator().is_none())
        {
            // Helpers live in helpers.rs; emit rc decs for locals before returning
            self.emit_rc_dec_for_locals(&locals_stack);
            if let Err(_) = self.builder.build_return(None) {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple("failed to build implicit return"),
                    Some(self.source),
                );
            }
        }

        Ok(function)
    }

    pub fn emit_host_main(&self, main_fn_name: &str) {
        // This emits a C-compatible main function that calls oats_main
        let main_ty = self.context.void_type().fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_ty, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Call oats_main (exported main)
        let oats_main_fn = self.module.get_function(main_fn_name).unwrap();
        let _ = self.builder.build_call(oats_main_fn, &[], "call_oats_main");

        // Return void
        let _ = self.builder.build_return(None);
    }

    // Helper methods for internal use (avoiding conflicts with helpers.rs)
    fn map_type_to_llvm_basic(&self, t: &crate::types::OatsType) -> inkwell::types::BasicMetadataTypeEnum<'a> {
        use crate::types::OatsType;
        match t {
            OatsType::Number => self.f64_t.into(),
            OatsType::String | OatsType::NominalStruct(_) | OatsType::Array(_) | OatsType::Promise(_) => self.i8ptr_t.into(),
            OatsType::Boolean => self.bool_t.into(),
            OatsType::Void => self.i8ptr_t.into(), // fallback for void
        }
    }

    fn build_llvm_fn_type_internal(
        &self,
        param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        ret_type: &crate::types::OatsType
    ) -> inkwell::types::FunctionType<'a> {
        use crate::types::OatsType;
        match ret_type {
            OatsType::Number => self.f64_t.fn_type(param_types, false),
            OatsType::String | OatsType::NominalStruct(_) | OatsType::Array(_) | OatsType::Promise(_) => self.i8ptr_t.fn_type(param_types, false),
            OatsType::Boolean => self.bool_t.fn_type(param_types, false),
            OatsType::Void => self.context.void_type().fn_type(param_types, false),
        }
    }

    fn create_param_allocas_internal(
        &self,
        function: inkwell::values::FunctionValue<'a>,
        func_decl: &deno_ast::swc::ast::Function,
        llvm_param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        receiver_name: Option<&str>
    ) -> Result<(
        std::collections::HashMap<String, u32>,
        Vec<std::collections::HashMap<String, crate::codegen::LocalEntry<'a>>>
    ), crate::diagnostics::Diagnostic> {
        let mut param_map = std::collections::HashMap::new();
        let locals_stack = vec![std::collections::HashMap::new()];

        for (i, p) in func_decl.params.iter().enumerate() {
            if let deno_ast::swc::ast::Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                param_map.insert(name, idx);
            }
        }

        Ok((param_map, locals_stack))
    }
}

    // Helper methods for internal use (avoiding conflicts with helpers.rs)
    fn map_type_to_llvm_basic(&self, t: &crate::types::OatsType) -> inkwell::types::BasicMetadataTypeEnum<'a> {
        use crate::types::OatsType;
        match t {
            OatsType::Number => self.f64_t.into(),
            OatsType::String | OatsType::NominalStruct(_) | OatsType::Array(_) | OatsType::Promise(_) => self.i8ptr_t.into(),
            OatsType::Boolean => self.bool_t.into(),
            OatsType::Void => self.i8ptr_t.into(), // fallback for void
        }
    }

    fn build_llvm_fn_type_internal(
        &self,
        param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        ret_type: &crate::types::OatsType
    ) -> inkwell::types::FunctionType<'a> {
        use crate::types::OatsType;
        match ret_type {
            OatsType::Number => self.f64_t.fn_type(param_types, false),
            OatsType::String | OatsType::NominalStruct(_) | OatsType::Array(_) | OatsType::Promise(_) => self.i8ptr_t.fn_type(param_types, false),
            OatsType::Boolean => self.bool_t.fn_type(param_types, false),
            OatsType::Void => self.context.void_type().fn_type(param_types, false),
        }
    }

    fn create_param_allocas_internal(
        &self,
        function: inkwell::values::FunctionValue<'a>,
        func_decl: &deno_ast::swc::ast::Function,
        llvm_param_types: &[inkwell::types::BasicMetadataTypeEnum<'a>],
        receiver_name: Option<&str>
    ) -> Result<(
        std::collections::HashMap<String, u32>,
        Vec<std::collections::HashMap<String, crate::codegen::LocalEntry<'a>>>
    ), crate::diagnostics::Diagnostic> {
        let mut param_map = std::collections::HashMap::new();
        let locals_stack = vec![std::collections::HashMap::new()];

        for (i, p) in func_decl.params.iter().enumerate() {
            if let deno_ast::swc::ast::Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                param_map.insert(name, idx);
            }
        }

        Ok((param_map, locals_stack))
    }

    fn lower_stmts_internal(
        &self,
        stmts: &[deno_ast::swc::ast::Stmt],
        function: inkwell::values::FunctionValue<'a>,
        param_map: &std::collections::HashMap<String, u32>,
        locals_stack: &mut Vec<std::collections::HashMap<String, crate::codegen::LocalEntry<'a>>>
    ) -> Result<bool, crate::diagnostics::Diagnostic> {
        // Delegate to the actual implementation in stmt.rs
        self.lower_stmts(stmts, function, param_map, locals_stack)
    }

    fn emit_rc_dec_for_locals_internal(&self, locals_stack: &Vec<std::collections::HashMap<String, crate::codegen::LocalEntry<'a>>>) {
        // Delegate to the actual implementation in helpers.rs
        self.emit_rc_dec_for_locals(locals_stack)
    }
}
    pub fn emit_host_main(
        &self,
        _params: &[crate::types::OatsType],
        _ret: &crate::types::OatsType,
    ) -> bool {
        // Emit a simple C-compatible `main` function that calls the
        // generated `oats_main` symbol. This covers the common case where
        // user scripts export `function main(): number` with no params.
        // Return `true` to indicate we emitted a host main so the driver
        // will skip linking an external `rt_main.o`.

        // Only handle the simple case: no params, integer return or void.
        // Build: int main(int argc, char** argv) { int r = oats_main(); return r; }
        let i32_t = self.i32_t;
        let i8ptr_t = self.i8ptr_t;

        // Build function type: i32 (int) with (i32, i8**) params
        let fn_type = i32_t.fn_type(&[i32_t.into(), i8ptr_t.as_basic_type_enum().into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => {
                // No oats_main available; emit an empty main that returns 1
                let const_one = i32_t.const_int(1, false);
                let _ = self.builder.build_return(Some(&const_one));
                return true;
            }
        };

        // Call oats_main(); we only support a no-arg oats_main here.
        let call_site = match self.builder.build_call(oats_main_fn, &[], "call_oats_main") {
            Ok(cs) => cs,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple("failed to build call to oats_main"),
                    Some(self.source),
                );
                let const_zero = i32_t.const_int(0, false);
                let _ = self.builder.build_return(Some(&const_zero));
                return true;
            }
        };
        // Interpret the result depending on its type. If the function returns an i64 or f64
        // we coerce/truncate to i32; if void, return 0.
        let either = call_site.try_as_basic_value();
        if let inkwell::Either::Left(bv) = either {
            let ret_val = if bv.get_type().is_int_type() {
                // Truncate or bitcast to i32 if needed
                let rv_int = bv.into_int_value();
                let cast = match self
                    .builder
                    .build_int_truncate_or_bit_cast(rv_int, i32_t, "ret_i32")
                {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "int cast failed when building host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(cast)
            } else if bv.get_type().is_float_type() {
                // cast float to i32 via fptosi
                let fv = bv.into_float_value();
                let conv = match self.builder.build_float_to_signed_int(fv, i32_t, "f_to_i") {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "float->int conversion failed in host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(conv)
            } else if bv.get_type().is_pointer_type() {
                // pointer return -> return 0
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            } else {
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            };
            let _ = self.builder.build_return(Some(&ret_val));
        } else {
            // No basic return (void), return 0
            let const_zero = i32_t.const_int(0, false);
            let _ = self.builder.build_return(Some(&const_zero));
        }

        true
    }
}