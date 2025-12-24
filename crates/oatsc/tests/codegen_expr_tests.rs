//! Codegen expression tests for the Oats compiler.
//!
//! This module tests expression lowering to LLVM IR, covering all expression
//! types including literals, binary/unary operations, function calls, member
//! access, async expressions, and more.

mod test_utils;

use test_utils::*;

// Note: These tests focus on ensuring codegen paths are exercised.
// Full integration tests would verify the generated IR is correct.

#[test]
fn test_codegen_number_literal() {
    let code = "let x: number = 42;";
    assert_compiles(code);
}

#[test]
fn test_codegen_string_literal() {
    let code = "let x: string = 'hello';";
    assert_compiles(code);
}

#[test]
fn test_codegen_boolean_literal() {
    let code = "let x: boolean = true;";
    assert_compiles(code);
}

#[test]
fn test_codegen_array_literal() {
    let code = "let x: number[] = [1, 2, 3];";
    assert_compiles(code);
}

#[test]
fn test_codegen_object_literal() {
    let code = "let x = { a: 1, b: 2 };";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_add() {
    let code = "let x = 1 + 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_subtract() {
    let code = "let x = 5 - 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_multiply() {
    let code = "let x = 2 * 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_divide() {
    let code = "let x = 10 / 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_modulo() {
    let code = "let x = 10 % 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_equals() {
    let code = "let x = 1 == 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_not_equals() {
    let code = "let x = 1 != 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_less_than() {
    let code = "let x = 1 < 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_greater_than() {
    let code = "let x = 2 > 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_less_equal() {
    let code = "let x = 1 <= 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_greater_equal() {
    let code = "let x = 2 >= 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_logical_and() {
    let code = "let x = true && false;";
    assert_compiles(code);
}

#[test]
fn test_codegen_binary_logical_or() {
    let code = "let x = true || false;";
    assert_compiles(code);
}

#[test]
fn test_codegen_unary_negate() {
    let code = "let x = -1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_unary_plus() {
    let code = "let x = +1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_unary_not() {
    let code = "let x = !true;";
    assert_compiles(code);
}

#[test]
fn test_codegen_unary_bitwise_not() {
    let code = "let x = ~1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_function_call() {
    let code = "function f(): void {} f();";
    assert_compiles(code);
}

#[test]
fn test_codegen_function_call_with_args() {
    let code = "function f(x: number): void {} f(1);";
    assert_compiles(code);
}

#[test]
fn test_codegen_method_call() {
    let code = "let obj = { method(): void {} }; obj.method();";
    assert_compiles(code);
}

#[test]
fn test_codegen_member_access_property() {
    let code = "let obj = { x: 1 }; let y = obj.x;";
    assert_compiles(code);
}

#[test]
fn test_codegen_member_access_computed() {
    let code = "let obj = { x: 1 }; let y = obj['x'];";
    assert_compiles(code);
}

#[test]
fn test_codegen_member_access_numeric_index() {
    let code = "let arr = [1, 2, 3]; let x = arr[0];";
    assert_compiles(code);
}

#[test]
fn test_codegen_new_expression() {
    let code = "class Point { constructor(public x: number) {} } let p = new Point(1);";
    assert_compiles(code);
}

#[test]
fn test_codegen_this_expression() {
    let code = "class C { x: number; getX(): number { return this.x; } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_super_expression() {
    let code = "class Parent { method(): void {} } class Child extends Parent { method(): void { super.method(); } }";
    assert_compiles(code);
}

#[test]
fn test_codegen_optional_chaining_property() {
    let code = "let obj: any = null; let x = obj?.prop;";
    assert_compiles(code);
}

#[test]
fn test_codegen_optional_chaining_computed() {
    let code = "let obj: any = null; let x = obj?.['key'];";
    assert_compiles(code);
}

#[test]
fn test_codegen_optional_chaining_call() {
    let code = "let obj: any = null; obj?.method();";
    assert_compiles(code);
}

#[test]
fn test_codegen_ternary_expression() {
    let code = "let x = true ? 1 : 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_sequence_expression() {
    let code = "let x = (1, 2, 3);";
    assert_compiles(code);
}

#[test]
fn test_codegen_parenthesized_expression() {
    let code = "let x = (1 + 2) * 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment() {
    let code = "var x = 1; x = 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment_add() {
    let code = "var x = 1; x += 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment_subtract() {
    let code = "var x = 5; x -= 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment_multiply() {
    let code = "var x = 2; x *= 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment_divide() {
    let code = "var x = 10; x /= 2;";
    assert_compiles(code);
}

#[test]
fn test_codegen_assignment_modulo() {
    let code = "var x = 10; x %= 3;";
    assert_compiles(code);
}

#[test]
fn test_codegen_arrow_function() {
    let code = "let f = (x: number) => x + 1;";
    assert_compiles(code);
}

#[test]
fn test_codegen_arrow_function_no_params() {
    let code = "let f = () => 42;";
    assert_compiles(code);
}

#[test]
fn test_codegen_arrow_function_multiple_params() {
    let code = "let f = (x: number, y: number) => x + y;";
    assert_compiles(code);
}

#[test]
fn test_codegen_async_await() {
    let code = "async function f(): Promise<number> { return await Promise.resolve(1); }";
    assert_compiles(code);
}

#[test]
fn test_codegen_async_await_expression() {
    let code =
        "async function f(): Promise<number> { let x = await Promise.resolve(1); return x; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_nested_expressions() {
    let code = "let x = (1 + 2) * (3 - 4) / 5;";
    assert_compiles(code);
}

#[test]
fn test_codegen_chained_member_access() {
    let code = "let obj = { nested: { value: 1 } }; let x = obj.nested.value;";
    assert_compiles(code);
}

#[test]
fn test_codegen_chained_method_calls() {
    let code = "let obj = { method(): any { return this; } }; obj.method().method();";
    assert_compiles(code);
}

#[test]
fn test_codegen_complex_binary_expression() {
    let code = "let x = 1 + 2 * 3 - 4 / 5;";
    assert_compiles(code);
}

#[test]
fn test_codegen_union_type_expression() {
    let code = "function f(x: number | string): void {} f(1); f('hello');";
    assert_compiles(code);
}

#[test]
fn test_codegen_array_access() {
    let code = "let arr = [1, 2, 3]; let x = arr[0]; let y = arr[1];";
    assert_compiles(code);
}

#[test]
fn test_codegen_array_length() {
    let code = "let arr = [1, 2, 3]; let len = arr.length;";
    assert_compiles(code);
}

#[test]
fn test_codegen_string_concatenation() {
    let code = "let x = 'hello' + ' ' + 'world';";
    assert_compiles(code);
}

#[test]
fn test_codegen_template_literal() {
    let code = "let name = 'world'; let x = `hello ${name}`;";
    assert_compiles(code);
}

#[test]
fn test_codegen_template_literal_complex() {
    let code = "let a = 1; let b = 2; let x = `a=${a}, b=${b}, sum=${a + b}`;";
    assert_compiles(code);
}

#[test]
fn test_codegen_identifier_variable() {
    let code = "let x = 1; let y = x;";
    assert_compiles(code);
}

#[test]
fn test_codegen_identifier_parameter() {
    let code = "function f(x: number): number { return x; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_logical_and_short_circuit() {
    let code = "let x = true && false;";
    assert_compiles(code);
}

#[test]
fn test_codegen_logical_or_short_circuit() {
    let code = "let x = false || true;";
    assert_compiles(code);
}

#[test]
fn test_codegen_null_coalescing() {
    let code = "let x: number | null = null; let y = x ?? 0;";
    assert_compiles(code);
}

#[test]
fn test_codegen_type_assertion() {
    let code = "let x: any = 1; let y = <number>x;";
    assert_compiles(code);
}

#[test]
fn test_codegen_type_assertion_as() {
    let code = "let x: any = 1; let y = x as number;";
    assert_compiles(code);
}

#[test]
fn test_codegen_spread_operator_array() {
    let code = "let arr1 = [1, 2]; let arr2 = [...arr1, 3];";
    assert_compiles(code);
}

#[test]
fn test_codegen_spread_operator_object() {
    let code = "let obj1 = { a: 1 }; let obj2 = { ...obj1, b: 2 };";
    assert_compiles(code);
}

#[test]
fn test_codegen_spread_operator_call() {
    let code = "function f(...args: number[]): void {} f(1, 2, 3);";
    assert_compiles(code);
}

#[test]
fn test_codegen_destructuring_array() {
    let code = "let arr = [1, 2]; let [x, y] = arr;";
    assert_compiles(code);
}

#[test]
fn test_codegen_destructuring_object() {
    let code = "let obj = { a: 1, b: 2 }; let { a, b } = obj;";
    assert_compiles(code);
}

#[test]
fn test_codegen_destructuring_renamed() {
    let code = "let obj = { a: 1 }; let { a: x } = obj;";
    assert_compiles(code);
}

#[test]
fn test_codegen_computed_property_name() {
    let code = "let key = 'x'; let obj = { [key]: 1 };";
    assert_compiles(code);
}

#[test]
fn test_codegen_shorthand_property() {
    let code = "let x = 1; let obj = { x };";
    assert_compiles(code);
}

#[test]
fn test_codegen_method_shorthand() {
    let code = "let obj = { method() { return 1; } };";
    assert_compiles(code);
}

#[test]
fn test_codegen_void_expression() {
    let code = "function f(): void {} let x = f();";
    assert_compiles(code);
}

#[test]
fn test_codegen_void_return() {
    let code = "function f(): void { return; }";
    assert_compiles(code);
}

#[test]
fn test_codegen_undefined() {
    let code = "let x = undefined;";
    assert_compiles(code);
}

#[test]
fn test_codegen_null() {
    let code = "let x = null;";
    assert_compiles(code);
}

#[test]
fn test_codegen_void_keyword() {
    let code = "let x: void = undefined;";
    assert_compiles(code);
}
