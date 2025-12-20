//! Escape analysis tests for the Oats compiler.
//!
//! This module tests local variable escape detection and RC optimization scenarios.

mod test_utils;

use test_utils::*;

// Note: Escape analysis is an optimization that determines which local variables
// escape their function scope. Variables that don't escape can skip RC operations.
// These tests focus on code patterns that should trigger different escape analysis results.

#[test]
fn test_escape_local_not_escaping() {
    // Local variable that doesn't escape
    let code = "function f(): void { let x = 'hello'; }";
    assert_compiles(code);
}

#[test]
fn test_escape_local_returned() {
    // Local variable that escapes via return
    let code = "function f(): string { let x = 'hello'; return x; }";
    assert_compiles(code);
}

#[test]
fn test_escape_local_assigned_to_field() {
    // Local variable that escapes via field assignment
    let code = r#"
        class Container {
            value: string;
        }
        function f(): void {
            let x = 'hello';
            let c = new Container();
            c.value = x;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_local_passed_to_function() {
    // Local variable that escapes via function call
    let code = r#"
        function process(s: string): void {}
        function f(): void {
            let x = 'hello';
            process(x);
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_local_in_array() {
    // Local variable that escapes via array element
    let code = r#"
        function f(): void {
            let x = 'hello';
            let arr: string[] = [];
            arr[0] = x;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_parameter() {
    // Function parameter (always escapes)
    let code = "function f(x: string): void { }";
    assert_compiles(code);
}

#[test]
fn test_escape_async_await() {
    // Variable that lives across await point
    let code = r#"
        async function f(): Promise<void> {
            let x = 'hello';
            await Promise.resolve();
            // x is live across await
            let y = x;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_nested_function() {
    // Variable captured in closure
    let code = r#"
        function outer(): void {
            let x = 'hello';
            function inner(): void {
                let y = x; // x is captured
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_arrow_function() {
    // Variable captured in arrow function
    let code = r#"
        function f(): void {
            let x = 'hello';
            let g = () => x; // x is captured
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_complex_control_flow() {
    // Variable used in multiple branches
    let code = r#"
        function f(cond: boolean): void {
            let x = 'hello';
            if (cond) {
                return x; // escapes in one branch
            }
            // x doesn't escape in other branch
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_loop_variable() {
    // Variable in loop that doesn't escape
    let code = r#"
        function f(): void {
            for (let i = 0; i < 10; i++) {
                let x = 'hello';
                // x doesn't escape
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_loop_variable_escaping() {
    // Variable in loop that escapes
    let code = r#"
        function f(): string[] {
            let result: string[] = [];
            for (let i = 0; i < 10; i++) {
                let x = 'hello';
                result.push(x); // x escapes
            }
            return result;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_multiple_locals() {
    // Multiple locals with different escape behavior
    let code = r#"
        function f(): void {
            let a = 'local'; // doesn't escape
            let b = 'returned';
            return b; // escapes
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_object_property() {
    // Object property access
    let code = r#"
        function f(): void {
            let obj = { prop: 'value' };
            let x = obj.prop; // doesn't escape (new value)
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_object_itself() {
    // Object itself that escapes
    let code = r#"
        function f(): any {
            let obj = { prop: 'value' };
            return obj; // obj escapes
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_array_element() {
    // Array element access
    let code = r#"
        function f(): void {
            let arr = ['a', 'b', 'c'];
            let x = arr[0]; // doesn't escape (new value)
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_array_itself() {
    // Array itself that escapes
    let code = r#"
        function f(): string[] {
            let arr = ['a', 'b', 'c'];
            return arr; // arr escapes
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_conditional_return() {
    // Variable that conditionally escapes
    let code = r#"
        function f(flag: boolean): string | void {
            let x = 'hello';
            if (flag) {
                return x; // escapes conditionally
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_try_catch() {
    // Variable in try-catch
    let code = r#"
        function f(): void {
            try {
                let x = 'hello';
                // x doesn't escape
            } catch (e) {
                // x not accessible here
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_switch_case() {
    // Variable in switch case
    let code = r#"
        function f(x: number): void {
            switch (x) {
                case 1: {
                    let y = 'hello';
                    // y doesn't escape
                    break;
                }
                default: break;
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_nested_blocks() {
    // Variable in nested blocks
    let code = r#"
        function f(): void {
            {
                {
                    let x = 'hello';
                    // x doesn't escape
                }
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_parameter_passed_through() {
    // Parameter passed to another function
    let code = r#"
        function g(s: string): void {}
        function f(x: string): void {
            g(x); // x escapes (but it's a parameter, so already escaped)
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_this_reference() {
    // 'this' reference in method
    let code = r#"
        class C {
            method(): void {
                let x = this; // this escapes (but it's 'this')
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_constructor_parameter() {
    // Constructor parameter assigned to field
    let code = r#"
        class Point {
            x: number;
            constructor(x: number) {
                this.x = x; // x escapes to field
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_generator_yield() {
    // Variable yielded from generator
    let code = r#"
        function* gen(): Generator<string> {
            let x = 'hello';
            yield x; // x escapes via yield
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_escape_promise_resolve() {
    // Variable in promise
    let code = r#"
        async function f(): Promise<string> {
            let x = 'hello';
            return Promise.resolve(x); // x escapes
        }
    "#;
    assert_compiles(code);
}

