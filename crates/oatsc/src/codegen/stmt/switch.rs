//! Switch statement lowering
//!
//! This module handles lowering of switch statements into LLVM IR.
//! Switch statements use LLVM's switch instruction for efficient
//! multi-way branching based on integer values.

use crate::diagnostics::Diagnostic;
use inkwell::values::FunctionValue;
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
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_switch_stmt(
        &self,
        switch_stmt: &oats_ast::SwitchStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Lower the discriminant expression
        let disc_val = self
            .lower_expr(&switch_stmt.discriminant, function, param_map, locals_stack)
            .map_err(|_| {
                Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "failed to lower switch discriminant",
                )
            })?;

        // Convert discriminant to i64 for switch instruction
        let disc_i64 = self.coerce_to_i64(disc_val).ok_or_else(|| {
            Diagnostic::simple_boxed(
                crate::diagnostics::Severity::Error,
                "switch discriminant must be a number",
            )
        })?;

        // Find the default case (if any)
        let default_case_idx = switch_stmt
            .cases
            .iter()
            .position(|case| case.test.is_none());

        // Create basic blocks for each case and a merge block
        let mut case_blocks = Vec::new();
        let default_block = if default_case_idx.is_some() {
            Some(self.context.append_basic_block(function, "switch.default"))
        } else {
            None
        };
        let merge_block = self.context.append_basic_block(function, "switch.merge");

        // Create blocks for each case
        for (i, case) in switch_stmt.cases.iter().enumerate() {
            if case.test.is_some() {
                let case_bb = self
                    .context
                    .append_basic_block(function, &format!("switch.case.{}", i));
                case_blocks.push((i, case_bb));
            }
        }

        // Build switch instruction with cases
        // LLVM switch requires constant integer values, so we try to evaluate
        // case tests as constants first. If any case is non-constant, we fall
        // back to an if-else chain.
        let mut switch_cases: Vec<(
            inkwell::values::IntValue<'a>,
            inkwell::basic_block::BasicBlock<'a>,
        )> = Vec::new();
        let mut non_constant_cases: Vec<(usize, &oats_ast::SwitchCase)> = Vec::new();
        let const_map = self.const_items.borrow();
        let const_map_ref = &*const_map;

        // Process each case to build switch cases
        for (case_idx, case) in switch_stmt.cases.iter().enumerate() {
            if let Some(test_expr) = &case.test {
                // Try to evaluate as a constant first
                match crate::codegen::const_eval::eval_const_expr(
                    test_expr,
                    case.span.start,
                    const_map_ref,
                ) {
                    Ok(const_val) => {
                        // Convert constant value to i64
                        let int_val = match const_val {
                            crate::codegen::const_eval::ConstValue::Number(n) => {
                                self.i64_t.const_int(n as i64 as u64, true)
                            }
                            crate::codegen::const_eval::ConstValue::I64(n) => {
                                self.i64_t.const_int(n as u64, true)
                            }
                            crate::codegen::const_eval::ConstValue::I32(n) => {
                                self.i64_t.const_int(n as u64, true)
                            }
                            crate::codegen::const_eval::ConstValue::I16(n) => {
                                self.i64_t.const_int(n as u64, true)
                            }
                            crate::codegen::const_eval::ConstValue::I8(n) => {
                                self.i64_t.const_int(n as u64, true)
                            }
                            crate::codegen::const_eval::ConstValue::U64(n) => {
                                self.i64_t.const_int(n, false)
                            }
                            crate::codegen::const_eval::ConstValue::U32(n) => {
                                self.i64_t.const_int(n as u64, false)
                            }
                            crate::codegen::const_eval::ConstValue::U16(n) => {
                                self.i64_t.const_int(n as u64, false)
                            }
                            crate::codegen::const_eval::ConstValue::U8(n) => {
                                self.i64_t.const_int(n as u64, false)
                            }
                            _ => {
                                // Non-numeric constant, treat as non-constant
                                non_constant_cases.push((case_idx, case));
                                continue;
                            }
                        };

                        // Find the corresponding case block
                        if let Some(case_block) = case_blocks
                            .iter()
                            .find(|(idx, _)| *idx == case_idx)
                            .map(|(_, bb)| *bb)
                        {
                            switch_cases.push((int_val, case_block));
                        }
                    }
                    Err(_) => {
                        // Not a constant - will use if-else chain
                        non_constant_cases.push((case_idx, case));
                    }
                }
            }
        }

        // If we have non-constant cases, we need to use an if-else chain
        // Start with the switch instruction for constant cases, then chain if-else for non-constants
        let current_block = if non_constant_cases.is_empty() {
            // All cases are constant - use pure switch
            let default_target = default_block.unwrap_or(merge_block);
            let _switch_inst = self
                .builder
                .build_switch(disc_i64, default_target, switch_cases.as_slice())
                .map_err(|_| {
                    Diagnostic::simple_boxed(
                        crate::diagnostics::Severity::Error,
                        "failed to build switch instruction",
                    )
                })?;
            None // No need for additional if-else chain
        } else {
            // We have non-constant cases - need if-else chain
            // First, handle constant cases with switch, defaulting to first non-constant check
            let first_non_const_bb = case_blocks
                .iter()
                .find(|(idx, _)| non_constant_cases.iter().any(|&(nc_idx, _)| nc_idx == *idx))
                .map(|(_, bb)| *bb)
                .unwrap_or_else(|| {
                    // No matching case block found, create a new one for the if-else chain
                    self.context
                        .append_basic_block(function, "switch.ifelse.start")
                });

            let default_target = if non_constant_cases.is_empty() {
                default_block.unwrap_or(merge_block)
            } else {
                first_non_const_bb
            };

            let _switch_inst = self
                .builder
                .build_switch(disc_i64, default_target, switch_cases.as_slice())
                .map_err(|_| {
                    Diagnostic::simple_boxed(
                        crate::diagnostics::Severity::Error,
                        "failed to build switch instruction",
                    )
                })?;

            Some(first_non_const_bb)
        };

        // Handle non-constant cases with if-else chain
        if let Some(mut ifelse_bb) = current_block {
            for (nc_idx, &(case_idx, case)) in non_constant_cases.iter().enumerate() {
                self.builder.position_at_end(ifelse_bb);

                // Lower the test expression
                if let Some(test_expr) = &case.test {
                    let test_val = self
                        .lower_expr(test_expr, function, param_map, locals_stack)
                        .map_err(|_| {
                            Diagnostic::simple_boxed(
                                crate::diagnostics::Severity::Error,
                                "failed to lower switch case test",
                            )
                        })?;

                    let test_i64 = self.coerce_to_i64(test_val).ok_or_else(|| {
                        Diagnostic::simple_boxed(
                            crate::diagnostics::Severity::Error,
                            "switch case test must be a number",
                        )
                    })?;

                    // Compare with discriminant
                    let cmp = self
                        .builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            disc_i64,
                            test_i64,
                            "switch_case_cmp",
                        )
                        .map_err(|_| {
                            Diagnostic::simple_boxed(
                                crate::diagnostics::Severity::Error,
                                "failed to build comparison",
                            )
                        })?;

                    // Find case block
                    let case_bb = case_blocks
                        .iter()
                        .find(|(idx, _)| *idx == case_idx)
                        .map(|(_, bb)| *bb)
                        .unwrap_or(merge_block);

                    // Create next block for remaining cases
                    let next_bb = if nc_idx + 1 < non_constant_cases.len() {
                        self.context
                            .append_basic_block(function, &format!("switch.ifelse.{}", nc_idx + 1))
                    } else {
                        default_block.unwrap_or(merge_block)
                    };

                    // Branch: if equal, go to case block; otherwise, check next case
                    let _ = self.builder.build_conditional_branch(cmp, case_bb, next_bb);
                    ifelse_bb = next_bb;
                }
            }
        }

        // Process each case body
        let mut all_terminated = true;

        for (case_idx, case) in switch_stmt.cases.iter().enumerate() {
            let case_block = if case.test.is_some() {
                case_blocks
                    .iter()
                    .find(|(idx, _)| *idx == case_idx)
                    .map(|(_, bb)| *bb)
                    .unwrap_or(merge_block)
            } else {
                default_block.unwrap_or(merge_block)
            };

            self.builder.position_at_end(case_block);

            // Lower the case body statements
            let case_terminated =
                self.lower_stmts(&case.cons, function, param_map, locals_stack)?;

            // If this case didn't terminate, we need to branch to the next case or merge
            if !case_terminated {
                // Check if there's a next case to fall through to
                let next_case_idx = case_idx + 1;
                if next_case_idx < switch_stmt.cases.len() {
                    // Fall through to next case
                    let next_case = &switch_stmt.cases[next_case_idx];
                    let next_block = if next_case.test.is_some() {
                        case_blocks
                            .iter()
                            .find(|(idx, _)| *idx == next_case_idx)
                            .map(|(_, bb)| *bb)
                            .unwrap_or(merge_block)
                    } else {
                        default_block.unwrap_or(merge_block)
                    };
                    let _ = self.builder.build_unconditional_branch(next_block);
                } else {
                    // Last case, branch to merge
                    let _ = self.builder.build_unconditional_branch(merge_block);
                }
            }

            if !case_terminated {
                all_terminated = false;
            }
        }

        // Position at merge block
        self.builder.position_at_end(merge_block);

        // If all cases terminated, the merge block is unreachable
        if all_terminated {
            let _ = self.builder.build_unreachable();
        }

        Ok(all_terminated)
    }
}
