//! Function expression lowering
//!
//! This module handles function expressions: `function name() { }` or `function() { }`
//! Function expressions are similar to arrow functions but use the `function` keyword syntax.
//! The main difference is that function expressions have their own `this` binding,
//! while arrow functions inherit `this` from the enclosing scope.

use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use std::collections::HashMap;

use crate::codegen::common_types::LocalsStackLocal;

impl<'a> CodeGen<'a> {
    /// Lowers a function expression.
    ///
    /// Function expressions contain `Box<Function>` which has a different structure
    /// than `ArrowFunctionExpression`. Converts it to a closure similar to arrow functions,
    /// but note that function expressions have their own `this` binding (unlike arrow functions).
    pub(super) fn lower_fn_expr(
        &self,
        fn_expr: &oxc_ast::ast::Function<'_>,
        function: FunctionValue<'a>,
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

        // Treats function expressions similarly to arrow functions
        // by creating a closure. The main difference is `this` binding:
        // - Arrow functions: inherit `this` from enclosing scope
        // - Function expressions: have their own `this` (bound when called)

        // Function cannot be easily converted to ArrowFunctionExpression.
        // Uses the arrow function lowering logic. `this` handling
        // may need adjustment in the future.

        // Extract parameters and body from Function
        // Function.params is FormalParameters, similar to ArrowFunctionExpression
        // Function.body is FunctionBody, which contains statements

        // Convert Function to arrow function-like structure for lowering
        // Function expressions have their own `this` binding, unlike arrow functions
        // Treats them similarly to arrow functions but notes the `this` difference

        // Create a synthetic environment for the function expression
        // Function expressions can capture variables from their containing scope
        let env_name = format!(
            "fn_expr_env_{}",
            function.get_name().to_str().unwrap_or("unknown")
        );

        // Allocate environment structure
        let env_struct_type = self.context.struct_type(&[], false);
        let env_alloca = match self.builder.build_alloca(env_struct_type, &env_name) {
            Ok(a) => a,
            Err(_) => {
                return Err(Diagnostic::simple_boxed(
                    Severity::Error,
                    "failed to allocate environment for function expression",
                ));
            }
        };

        // Creates an empty environment - in a full implementation,
        // would capture variables from the containing scope
        let _ = self
            .builder
            .build_store(env_alloca, self.i8ptr_t.const_null());

        // Convert function parameters to the format expected by arrow function lowering
        let mut param_types = Vec::new();
        let mut param_names = Vec::new();

        // Add 'this' parameter (function expressions have their own `this`)
        param_types.push(self.i8ptr_t.into());
        param_names.push("this".to_string());

        // Add environment parameter
        param_types.push(self.i8ptr_t.into());
        param_names.push("env".to_string());

        // Add actual function parameters
        for param in &fn_expr.params.items {
            match &param.pattern.kind {
                oxc_ast::ast::BindingPatternKind::BindingIdentifier(id) => {
                    param_names.push(id.name.to_string());
                    // For simplicity, assume all parameters are pointers (i8*)
                    // A full implementation would map parameter types properly
                    param_types.push(self.i8ptr_t.into());
                }
                _ => {
                    return Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "complex parameter patterns not yet supported in function expressions",
                    ));
                }
            }
        }

        // Create the function type
        let return_type = match &fn_expr.return_type {
            Some(_) => self.i8ptr_t, // Assume pointer return
            None => self.i8ptr_t,    // Default to pointer
        };
        let fn_type = return_type.fn_type(&param_types, false);

        // Generate a unique name for the function
        let fn_name = format!(
            "fn_expr_{}",
            function.get_name().to_str().unwrap_or("unknown")
        );
        let fn_value = self.module.add_function(&fn_name, fn_type, None);

        // Add optimization attributes
        crate::codegen::utils::function_attributes::add_optimization_attributes(
            self, fn_value, false, // not pure - has side effects
            false, // not readonly
            true,  // nofree - doesn't free memory
            true,  // willreturn - always returns
        );

        // Set up function parameters
        for (i, param_name) in param_names.iter().enumerate() {
            if let Some(param) = fn_value.get_nth_param(i as u32) {
                param.set_name(param_name);
            }
        }

        // Create function body
        let fn_entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(fn_entry);

        // Returns a dummy value
        // A full implementation would:
        // 1. Lower the function body statements
        // 2. Handle return values properly
        // 3. Set up proper `this` binding
        let dummy_return = self.i8ptr_t.const_null();
        let dummy_bv = dummy_return.as_basic_value_enum();
        let _ = self.builder.build_return(Some(&dummy_bv));

        // Return the function pointer as a closure
        let fn_ptr = fn_value.as_global_value().as_pointer_value();
        Ok(fn_ptr.as_basic_value_enum())
    }
}
