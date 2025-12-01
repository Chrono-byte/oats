//! Design notes and conventions used throughout this module:
//! - ABI/Return values: lowering returns a `BasicValueEnum` that represents
//!   the ABI value for an expression: numeric results are `f64` (float),
//!   pointer-like values are `i8*`. For callers that expect a value from a
//!   void-returning IR call we return a harmless `f64` zero so the surrounding
//!   lowering can remain uniform.
//! - Unions: when a union has any pointer-like arm we choose a pointer
//!   representation for the ABI (i8*). Numeric arms are boxed via
//!   `union_box_f64`/`union_box_ptr` when stored in heap fields or captured.
//! - Last-expression origin: `last_expr_origin_local` is used as a small
//!   heuristic to record that the last-lowered expression came from a named
//!   temporary local (for example a freshly constructed closure). This lets
//!   subsequent lowering paths conservatively recover more static information
//!   (e.g., closure return types) without heavy analysis.

pub mod assignments;

use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

pub mod arrow_expr;
pub mod async_expr;
pub mod binary_ops;
pub mod calls;
pub mod control_flow_expr;
pub mod fn_expr;
pub mod ident;
pub mod literals;
pub mod member_access;
pub mod new_expr;
pub mod optional_chaining;
pub mod paren;
pub mod process;
pub mod seq_expr;
pub mod super_expr;
pub mod this;
pub mod unary_ops;
pub mod yield_expr;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Main expression lowering function.
    ///
    /// Traverses an AST expression and emits LLVM IR values for the
    /// expression using the `CodeGen`'s `builder` and runtime helper
    /// functions. The returned `BasicValueEnum` represents the ABI value
    /// for the expression (for example `f64` for numbers or `i8*` for
    /// pointer-like values). On error a `Diagnostic` is returned and the
    /// caller decides whether to continue lowering other parts of the
    /// module.
    ///
    /// Contract/Notes:
    /// - The lowering keeps boxing/unboxing explicit: unions with pointer-
    ///   like arms use `i8*` slots and numeric arms may be boxed into
    ///   runtime objects using `union_box_f64`.
    /// - Short-circuiting logical operations create basic blocks and
    ///   phi nodes to merge results when needed.
    /// - `lower_expr` may emit calls to runtime helpers (e.g. `strlen`,
    ///   `array_get_f64`, `str_concat`). Those functions are declared via
    ///   `helpers::declare_libc` or created lazily by `CodeGen` getters.
    ///
    /// # Arguments
    /// * `expr` - AST expression to lower.
    /// * `function` - current LLVM function for block creation.
    /// * `param_map` - map of function parameter names to indices.
    /// * `locals` - mutable lexical locals stack used for allocas and RC.
    ///
    /// # Returns
    /// A lowered `BasicValueEnum` on success, or a `Diagnostic` on failure.
    pub fn lower_expr(
        &self,
        expr: &oats_ast::Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        use oats_ast::*;

        match expr {
            // Removed duplicate `Expr::Call(call)` match arm at line 482 to fix unreachable pattern warning.
            // Removed unused `parent` variable to resolve the warning.
            Expr::Bin(bin) => self.lower_binary_expr(bin, function, param_map, locals),
            // Identifier lookup and TDZ handling
            //
            // Identifiers can refer to function parameters (which are
            // passed in via `param_map`) or to locals created by `let` bindings.
            // For locals we track an `initialized` flag and trap (emit
            // unreachable) for Temporal Dead Zone reads. We also record the
            // origin local name for the last-lowered expression which helps
            // propagate closure-local typing information.
            Expr::Ident(id) => self.lower_ident_expr(id, function, param_map, locals),
            Expr::This(this_expr) => self.lower_this_expr(this_expr, function, param_map, locals),
            Expr::Super(super_expr) => {
                self.lower_super_expr(super_expr, function, param_map, locals)
            }
            Expr::Call(call) => self.lower_call_expr(call, function, param_map, locals),
            // (Duplicate closure-call lowering removed; handled in the primary Call arm above.)
            Expr::Assign(assign) => self.lower_assign_expr(assign, function, param_map, locals),
            Expr::Paren(paren) => self.lower_paren_expr(paren, function, param_map, locals),
            Expr::Cond(cond) => self.lower_cond_expr(cond, function, param_map, locals),
            Expr::Lit(lit) => Ok(crate::codegen::expr::literals::lower_lit(self, lit)?),
            Expr::Array(arr) => Ok(literals::lower_array(
                self, arr, function, param_map, locals,
            )?),
            Expr::Member(member) => self.lower_member_expr(member, function, param_map, locals),
            Expr::OptionalMember(optional) => {
                self.lower_optional_member_expr(optional, function, param_map, locals)
            }
            Expr::New(new_expr) => self.lower_new_expr(new_expr, function, param_map, locals),
            Expr::Await(await_expr) => {
                self.lower_await_expr(await_expr, function, param_map, locals)
            }
            Expr::Arrow(arrow) => self.lower_arrow_expr(arrow, function, param_map, locals),
            Expr::Fn(fn_expr) => self.lower_fn_expr(fn_expr, function, param_map, locals),
            Expr::Seq(seq) => self.lower_seq_expr(seq, function, param_map, locals),
            Expr::Object(obj_lit) => Ok(literals::lower_object(
                self, obj_lit, function, param_map, locals,
            )?),
            Expr::Tpl(tpl) => Ok(literals::lower_template(
                self, tpl, function, param_map, locals,
            )?),
            Expr::Unary(unary) => self.lower_unary_expr(unary, function, param_map, locals),
            Expr::Update(update) => self.lower_update_expr(update, function, param_map, locals),
            // Process model expressions
            Expr::Spawn(spawn) => self.lower_spawn_expr(spawn, function, param_map, locals),
            Expr::Send(send) => self.lower_send_expr(send, function, param_map, locals),
            Expr::Receive(receive) => self.lower_receive_expr(receive, function, param_map, locals),
            Expr::ProcessSelf(self_expr) => self.lower_process_self_expr(self_expr),
            Expr::ProcessExit(exit) => {
                self.lower_process_exit_expr(exit, function, param_map, locals)
            }
            Expr::ProcessLink(link) => {
                self.lower_process_link_expr(link, function, param_map, locals)
            }
            Expr::ProcessUnlink(unlink) => {
                self.lower_process_unlink_expr(unlink, function, param_map, locals)
            }
            Expr::ProcessMonitor(monitor) => {
                self.lower_process_monitor_expr(monitor, function, param_map, locals)
            }
            Expr::ProcessDemonitor(demonitor) => {
                self.lower_process_demonitor_expr(demonitor, function, param_map, locals)
            }
            Expr::ProcessWhereis(whereis) => {
                self.lower_process_whereis_expr(whereis, function, param_map, locals)
            }
            Expr::ProcessRegister(register) => {
                self.lower_process_register_expr(register, function, param_map, locals)
            }
            Expr::ProcessUnregister(unregister) => {
                self.lower_process_unregister_expr(unregister, function, param_map, locals)
            }
            Expr::Yield(yield_expr) => {
                self.lower_yield_expr(yield_expr, function, param_map, locals)
            }
            _ => Err(Diagnostic::simple_boxed(
                Severity::Error,
                "operation not supported",
            )),
        }
    }

    /// Resolve the nominal type of a member expression.
    /// For example, given `outer.data`, this returns the nominal type of the `data` field.
    fn resolve_member_type(
        &self,
        member: &oats_ast::MemberExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Option<String> {
        self.resolve_member_type_with_depth(member, function, param_map, locals, 0)
    }

    /// Internal helper with recursion depth tracking to prevent stack overflow.
    fn resolve_member_type_with_depth(
        &self,
        member: &oats_ast::MemberExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
        depth: usize,
    ) -> Option<String> {
        use oats_ast::*;

        // Prevent stack overflow from deeply nested member access chains
        const MAX_MEMBER_DEPTH: usize = 128;
        if depth > MAX_MEMBER_DEPTH {
            return None;
        }

        // First, determine the nominal type of member.obj
        let obj_nominal = if let Expr::Ident(ident) = &*member.obj {
            let ident_name = ident.sym.clone();
            // Check if it's `this`
            if ident_name == "this" {
                if let Some(param_types) = self
                    .fn_param_types
                    .borrow()
                    .get(function.get_name().to_str().unwrap_or(""))
                    && !param_types.is_empty()
                    && let crate::types::OatsType::NominalStruct(n) = &param_types[0]
                {
                    Some(n.clone())
                } else {
                    None
                }
            }
            // Check params
            else if let Some(param_idx) = param_map.get(&ident_name) {
                if let Some(param_types) = self
                    .fn_param_types
                    .borrow()
                    .get(function.get_name().to_str().unwrap_or(""))
                {
                    let idx = *param_idx as usize;
                    if idx < param_types.len()
                        && let crate::types::OatsType::NominalStruct(n) = &param_types[idx]
                    {
                        Some(n.clone())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            // Check locals
            else if let Some((_, _, _, _, _, nominal, _oats_type)) =
                self.find_local(locals, &ident_name)
            {
                nominal
            } else {
                None
            }
        } else if let Expr::Member(inner_member) = &*member.obj {
            // Recursively resolve nested member expressions with depth tracking
            self.resolve_member_type_with_depth(
                inner_member,
                function,
                param_map,
                locals,
                depth + 1,
            )
        } else {
            None
        };

        // Now look up the field type in the class_fields map
        if let Some(obj_type_name) = obj_nominal
            && let Some(fields) = self.class_fields.borrow().get(&obj_type_name)
        {
            // Get the property name from member.prop
            if let MemberProp::Ident(prop_ident) = &member.prop {
                let field_name = prop_ident.sym.clone();
                // Find the field and return its type if it's a NominalStruct
                for (fname, ftype) in fields {
                    if fname == &field_name {
                        if let crate::types::OatsType::NominalStruct(n) = ftype {
                            return Some(n.clone());
                        } else {
                            // Field exists but is not a nominal struct
                            return None;
                        }
                    }
                }
            }
        }

        None
    }
}
