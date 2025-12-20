//! Codegen statement tests for the Oats compiler.
//!
//! This module tests statement lowering to LLVM IR, covering all statement
//! types including variable declarations, control flow, loops, switch, try/catch,
//! return, break/continue, and labeled statements.

mod test_utils;

use test_utils::*;

#[test]
fn test_codegen_variable_declaration() {
    let code = "let x: number = 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_variable_declaration_no_init() {
    let code = "let x: number;";
    assert_compiles(code);
}

#[test]
fn test_codegen_const_declaration() {
    let code = "const x: number = 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_mut_declaration() {
    let code = "var x: number = 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_expression_statement() {
    let code = "1 + 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_block_statement() {
    let code = "{ let x = 1; let y = 2; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_if_statement() {
    let code = "if (true) { let x = 1; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_if_else_statement() {
    let code = "if (true) { let x = 1; } else { let y = 2; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_if_else_if_statement() {
    let code = "if (false) { } else if (true) { } else { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_while_loop() {
    let code = "while (true) { break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_while_loop_with_body() {
    let code = "var i = 0; while (i < 10) { i = i + 1; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_do_while_loop() {
    let code = "do { break; } while (false);";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop() {
    let code = "for (let i = 0; i < 10; i++) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_no_init() {
    let code = "var i = 0; for (; i < 10; i++) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_no_condition() {
    let code = "for (let i = 0; ; i++) { break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_no_increment() {
    let code = "for (let i = 0; i < 10; ) { i = i + 1; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_empty() {
    let code = "for (;;) { break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_in_loop() {
    let code = "let obj = { a: 1, b: 2 }; for (let key in obj) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_of_loop() {
    let code = "let arr = [1, 2, 3]; for (let item of arr) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_switch_statement() {
    let code = "switch (1) { case 1: break; default: break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_switch_multiple_cases() {
    let code = "switch (x) { case 1: break; case 2: break; default: break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_switch_no_default() {
    let code = "switch (1) { case 1: break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_try_catch() {
    let code = "try { } catch (e) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_try_catch_finally() {
    let code = "try { } catch (e) { } finally { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_try_finally() {
    let code = "try { } finally { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_return_statement() {
    let code = "function f(): void { return; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_return_with_value() {
    let code = "function f(): number { return 42; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_return_expression() {
    let code = "function f(): number { return 1 + 2; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_break_statement() {
    let code = "while (true) { break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_continue_statement() {
    let code = "while (true) { continue; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_labeled_break() {
    let code = "label: while (true) { break label; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_labeled_continue() {
    let code = "label: while (true) { continue label; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_throw_statement() {
    let code = "throw new Error('message');";
    assert_compiles(code);
}

#[test]
fn test_codegen_throw_string() {
    let code = "throw 'error';";
    assert_compiles(code);
}

#[test]
fn test_codegen_labeled_statement() {
    let code = "label: if (true) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_if() {
    let code = "if (true) { if (false) { } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_loops() {
    let code = "for (let i = 0; i < 10; i++) { for (let j = 0; j < 10; j++) { } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_switch() {
    let code = "switch (x) { case 1: switch (y) { case 2: break; } break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_loop_with_if() {
    let code = "for (let i = 0; i < 10; i++) { if (i % 2 == 0) { continue; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_if_with_loop() {
    let code = "if (true) { while (false) { break; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_multiple_returns() {
    let code = "function f(x: number): number { if (x > 0) { return 1; } return 0; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_early_return() {
    let code = "function f(x: number): number { if (x < 0) { return -1; } return x; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_switch_fallthrough() {
    let code = "switch (x) { case 1: case 2: break; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_try_catch_nested() {
    let code = "try { try { } catch (e) { } } catch (e) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_catch_with_type() {
    let code = "try { } catch (e: Error) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_break() {
    let code = "for (let i = 0; i < 10; i++) { if (i > 5) { break; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_for_loop_continue() {
    let code = "for (let i = 0; i < 10; i++) { if (i % 2 == 0) { continue; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_while_break() {
    let code = "var i = 0; while (true) { if (i > 10) { break; } i = i + 1; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_while_continue() {
    let code = "var i = 0; while (i < 10) { i = i + 1; if (i % 2 == 0) { continue; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_do_while_break() {
    let code = "do { break; } while (true);";
    assert_compiles(code);
}

#[test]
fn test_codegen_do_while_continue() {
    let code = "var i = 0; do { i = i + 1; if (i % 2 == 0) { continue; } } while (i < 10);";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_labeled_break() {
    let code = "outer: for (let i = 0; i < 10; i++) { inner: for (let j = 0; j < 10; j++) { break outer; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_labeled_continue() {
    let code = "outer: for (let i = 0; i < 10; i++) { inner: for (let j = 0; j < 10; j++) { continue outer; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_variable_in_block() {
    let code = "{ let x = 1; { let y = 2; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_variable_shadowing() {
    let code = "let x = 1; { let x = 2; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_multiple_statements() {
    let code = "let x = 1; let y = 2; let z = 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_empty_block() {
    let code = "{ }";
    assert_compiles(code);
}

#[test]
fn test_codegen_empty_if() {
    let code = "if (true) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_empty_while() {
    let code = "while (false) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_empty_for() {
    let code = "for (let i = 0; i < 10; i++) { }";
    assert_compiles(code);
}

#[test]
fn test_codegen_complex_control_flow() {
    let code = "function f(x: number): number { if (x > 0) { for (let i = 0; i < x; i++) { if (i % 2 == 0) { continue; } } return 1; } return 0; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_switch_with_returns() {
    let code = "function f(x: number): number { switch (x) { case 1: return 1; case 2: return 2; default: return 0; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_try_catch_with_returns() {
    let code = "function f(): number { try { return 1; } catch (e) { return 0; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_finally_with_return() {
    let code = "function f(): number { try { return 1; } finally { } }";
    assert_compiles(code);
}

