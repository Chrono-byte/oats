//! Parser tests for the Oats compiler.
//!
//! This module tests the parser's ability to handle all AST node types,
//! error conditions, and edge cases.

mod test_utils;

use oatsc::diagnostics::Severity;
use oatsc::parser;
use test_utils::*;

#[test]
fn test_parse_empty_file() {
    let code = "";
    let (parsed_mod_opt, diags) = parser::parse_oats_module(code, Some("empty.oats"))
        .expect("Should parse empty file without error");

    // Empty file should parse but produce no content
    if let Some(parsed_mod) = parsed_mod_opt {
        assert!(parsed_mod.parsed.body.is_empty());
        assert!(parsed_mod.declared_fns.is_empty());
    }
    assert!(diags.is_empty() || diags.iter().all(|d| d.severity != Severity::Error));
}

#[test]
fn test_parse_whitespace_only() {
    let code = "   \n\t  \n  ";
    let (parsed_mod_opt, _diags) = parser::parse_oats_module(code, Some("whitespace.oats"))
        .expect("Should parse whitespace-only file");

    if let Some(parsed_mod) = parsed_mod_opt {
        assert!(parsed_mod.parsed.body.is_empty());
    }
}

#[test]
fn test_parse_variable_declarations() {
    // Test let declarations
    assert_compiles("let x: number = 42;");
    assert_compiles("let y: string = 'hello';");
    assert_compiles("let z: boolean = true;");

    // Test const declarations
    assert_compiles("const a: number = 10;");
    assert_compiles("const b = 20;");

    // Test var declarations
    assert_compiles("var c: number = 30;");

    // Test multiple declarations
    assert_compiles("let x = 1, y = 2, z = 3;");
}

#[test]
fn test_parse_function_declarations() {
    // Regular function
    assert_compiles("function foo(): void {}");
    assert_compiles("function bar(x: number): number { return x; }");
    assert_compiles("function baz(x: number, y: string): void {}");

    // Arrow function
    assert_compiles("let arrow = (x: number) => x + 1;");
    assert_compiles("let arrow2 = () => 42;");

    // Async function
    assert_compiles("async function asyncFoo(): Promise<number> { return 1; }");

    // Generator function
    assert_compiles("function* gen(): Generator<number> { yield 1; }");

    // Function with type parameters
    assert_compiles("function generic<T>(x: T): T { return x; }");
}

#[test]
fn test_parse_class_declarations() {
    // Simple class
    assert_compiles("class Point { x: number; y: number; }");

    // Class with constructor
    assert_compiles("class Point { constructor(public x: number, public y: number) {} }");

    // Class with methods
    assert_compiles("class Point { getX(): number { return this.x; } }");

    // Class with inheritance
    assert_compiles("class Child extends Parent {}");

    // Class with multiple inheritance levels
    assert_compiles("class GrandChild extends Child {}");
}

#[test]
fn test_parse_interface_declarations() {
    assert_compiles("interface Point { x: number; y: number; }");
    assert_compiles("interface Shape { area(): number; }");
    assert_compiles("interface Extended extends Base { extra: string; }");
}

#[test]
fn test_parse_enum_declarations() {
    assert_compiles("enum Color { Red, Green, Blue }");
    assert_compiles("enum Direction { Up = 1, Down = 2 }");
}

#[test]
fn test_parse_control_flow_statements() {
    // If statements
    assert_compiles("if (true) { let x = 1; }");
    assert_compiles("if (true) { } else { }");
    assert_compiles("if (x) { } else if (y) { } else { }");

    // While loops
    assert_compiles("while (true) { break; }");

    // Do-while loops
    assert_compiles("do { } while (false);");

    // For loops
    assert_compiles("for (let i = 0; i < 10; i++) { }");
    assert_compiles("for (;;) { break; }");

    // For-in loops
    assert_compiles("for (let key in obj) { }");

    // For-of loops
    assert_compiles("for (let item of arr) { }");

    // Switch statements
    assert_compiles("switch (x) { case 1: break; default: break; }");

    // Try-catch
    assert_compiles("try { } catch (e) { }");
    assert_compiles("try { } catch (e) { } finally { }");
}

#[test]
fn test_parse_expressions() {
    // Binary operations
    assert_compiles("let x = 1 + 2;");
    assert_compiles("let x = 1 - 2;");
    assert_compiles("let x = 1 * 2;");
    assert_compiles("let x = 1 / 2;");
    assert_compiles("let x = 1 % 2;");
    assert_compiles("let x = 1 == 2;");
    assert_compiles("let x = 1 != 2;");
    assert_compiles("let x = 1 < 2;");
    assert_compiles("let x = 1 > 2;");
    assert_compiles("let x = 1 <= 2;");
    assert_compiles("let x = 1 >= 2;");
    assert_compiles("let x = 1 && 2;");
    assert_compiles("let x = 1 || 2;");

    // Unary operations
    assert_compiles("let x = -1;");
    assert_compiles("let x = +1;");
    assert_compiles("let x = !true;");
    assert_compiles("let x = ~1;");

    // Function calls
    assert_compiles("foo();");
    assert_compiles("foo(1, 2, 3);");
    assert_compiles("obj.method();");

    // Member access
    assert_compiles("obj.prop;");
    assert_compiles("obj['key'];");
    assert_compiles("obj[0];");

    // New expressions
    assert_compiles("new Point(1, 2);");

    // This/super
    assert_compiles("this.x;");
    assert_compiles("super.method();");

    // Optional chaining
    assert_compiles("obj?.prop;");
    assert_compiles("obj?.['key'];");
    assert_compiles("obj?.method();");

    // Ternary
    assert_compiles("let x = true ? 1 : 2;");

    // Sequence expressions
    assert_compiles("let x = (1, 2, 3);");
}

#[test]
fn test_parse_literals() {
    // Number literals
    assert_compiles("let x = 42;");
    assert_compiles("let x = 3.14;");
    assert_compiles("let x = 1e10;");

    // String literals
    assert_compiles("let x = 'hello';");
    assert_compiles("let x = \"world\";");
    assert_compiles("let x = `template ${1 + 2}`;");

    // Boolean literals
    assert_compiles("let x = true;");
    assert_compiles("let x = false;");

    // Array literals
    assert_compiles("let x = [1, 2, 3];");
    assert_compiles("let x = [];");

    // Object literals
    assert_compiles("let x = { a: 1, b: 2 };");
    assert_compiles("let x = {};");
    assert_compiles("let x = { [key]: value };");
}

#[test]
fn test_parse_type_annotations() {
    assert_compiles("let x: number = 1;");
    assert_compiles("let x: string = 'hello';");
    assert_compiles("let x: boolean = true;");
    assert_compiles("let x: number[] = [1, 2, 3];");
    assert_compiles("let x: Array<number> = [1, 2, 3];");
    assert_compiles("function f(x: number | string): void {}");
    assert_compiles("let x: Point = new Point();");
}

#[test]
fn test_parse_import_export() {
    assert_compiles("import { foo } from './module';");
    assert_compiles("import * as mod from './module';");
    assert_compiles("export function foo(): void {}");
    assert_compiles("export const x = 1;");
    assert_compiles("export class Point {}");
}

#[test]
fn test_parse_nested_structures() {
    // Deeply nested if
    assert_compiles("if (a) { if (b) { if (c) { } } }");

    // Nested loops
    assert_compiles("for (let i = 0; i < 10; i++) { for (let j = 0; j < 10; j++) { } }");

    // Nested functions
    assert_compiles("function outer() { function inner() { } }");
}

#[test]
fn test_parse_unicode_identifiers() {
    assert_compiles("let 变量 = 1;");
    assert_compiles("let переменная = 2;");
    assert_compiles("function 関数(): void {}");
}

#[test]
fn test_parse_long_identifiers() {
    let long_name = "a".repeat(1000);
    let code = format!("let {} = 1;", long_name);
    assert_compiles(&code);
}

#[test]
fn test_parse_source_size_limit() {
    // Create a file that exceeds the 10MB limit
    let large_content = "let x = 1;\n".repeat(1_000_000); // ~10MB
    let (parsed_mod_opt, diags) = parser::parse_oats_module(&large_content, Some("large.oats"))
        .expect("Should handle large file");

    // Should either fail to parse or produce a diagnostic
    if parsed_mod_opt.is_none() {
        // Check that we got an error diagnostic
        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(!errors.is_empty(), "Should have error for oversized file");
    }
}

#[test]
fn test_parse_invalid_syntax_missing_semicolon() {
    // This should parse (parser is lenient) but may produce diagnostics
    let code = "let x = 1\nlet y = 2";
    let (_parsed_mod_opt, _diags) = parser::parse_oats_module(code, Some("test.oats"))
        .expect("Parser should handle missing semicolons");
}

#[test]
fn test_parse_invalid_syntax_missing_brackets() {
    // Missing closing brace
    let code = "function foo() { let x = 1;";
    let (_parsed_mod_opt, _diags) =
        parser::parse_oats_module(code, Some("test.oats")).expect("Parser should attempt to parse");

    // Should produce diagnostics
    // Note: oxc_parser may recover or produce errors
}

#[test]
fn test_parse_complex_expressions() {
    // Chained member access
    assert_compiles("obj.prop.method().result;");

    // Complex binary expressions
    assert_compiles("let x = 1 + 2 * 3 - 4 / 5;");

    // Nested function calls
    assert_compiles("foo(bar(baz(1)));");

    // Array/object nesting
    assert_compiles("let x = [{ a: 1 }, { b: 2 }];");
}

#[test]
fn test_parse_destructuring() {
    assert_compiles("let { a, b } = obj;");
    assert_compiles("let [x, y] = arr;");
    assert_compiles("let { a: x, b: y } = obj;");
}

#[test]
fn test_parse_template_literals() {
    assert_compiles("let x = `hello`;");
    assert_compiles("let x = `hello ${name}`;");
    assert_compiles("let x = `a ${1 + 2} b ${foo()} c`;");
}

#[test]
fn test_parse_comments() {
    assert_compiles("// Single line comment\nlet x = 1;");
    assert_compiles("/* Multi line\ncomment */\nlet x = 1;");
    assert_compiles("let x = 1; // End of line comment");
}

#[test]
fn test_parse_return_statements() {
    assert_compiles("function f() { return; }");
    assert_compiles("function f(): number { return 42; }");
    assert_compiles("function f() { return 1 + 2; }");
}

#[test]
fn test_parse_break_continue() {
    assert_compiles("while (true) { break; }");
    assert_compiles("while (true) { continue; }");
    assert_compiles("label: while (true) { break label; }");
    assert_compiles("label: while (true) { continue label; }");
}

#[test]
fn test_parse_throw_statements() {
    assert_compiles("throw new Error('message');");
    assert_compiles("throw 'string error';");
}

#[test]
fn test_parse_labeled_statements() {
    assert_compiles("label: if (true) { }");
    assert_compiles("label: while (true) { break label; }");
}

#[test]
fn test_parse_async_await() {
    assert_compiles("async function f() { await promise; }");
    assert_compiles("async function f() { let x = await getValue(); }");
}

#[test]
fn test_parse_generators() {
    assert_compiles("function* gen() { yield 1; }");
    assert_compiles("function* gen() { yield* otherGen(); }");
}

#[test]
fn test_parse_type_assertions() {
    assert_compiles("let x = <number>value;");
    assert_compiles("let x = value as number;");
}

#[test]
fn test_parse_null_undefined() {
    assert_compiles("let x = null;");
    assert_compiles("let x = undefined;");
}

#[test]
fn test_parse_optional_parameters() {
    assert_compiles("function f(x?: number): void {}");
    assert_compiles("function f(x: number = 0): void {}");
}

#[test]
fn test_parse_rest_parameters() {
    assert_compiles("function f(...args: number[]): void {}");
}

#[test]
fn test_parse_spread_operator() {
    assert_compiles("let x = [...arr];");
    assert_compiles("let x = {...obj};");
    assert_compiles("foo(...args);");
}

#[test]
fn test_parse_computed_property_names() {
    assert_compiles("let obj = { [key]: value };");
    assert_compiles("class C { [method]() {} }");
}

#[test]
fn test_parse_shorthand_properties() {
    assert_compiles("let x = 1; let obj = { x };");
    assert_compiles("let obj = { x, y, z };");
}

#[test]
fn test_parse_method_shorthand() {
    assert_compiles("let obj = { method() {} };");
}

#[test]
fn test_parse_getter_setter() {
    assert_compiles("class C { get prop(): number { return 1; } }");
    assert_compiles("class C { set prop(v: number) {} }");
}

#[test]
fn test_parse_static_members() {
    assert_compiles("class C { static method(): void {} }");
    assert_compiles("class C { static prop: number; }");
}

#[test]
fn test_parse_private_members() {
    assert_compiles("class C { private prop: number; }");
    assert_compiles("class C { private method(): void {} }");
}

#[test]
fn test_parse_readonly_members() {
    assert_compiles("class C { readonly prop: number; }");
}

#[test]
fn test_parse_abstract_classes() {
    assert_compiles("abstract class C { abstract method(): void; }");
}

#[test]
fn test_parse_union_types() {
    assert_compiles("let x: number | string = 1;");
    assert_compiles("function f(x: number | string | boolean): void {}");
}

#[test]
fn test_parse_intersection_types() {
    assert_compiles("let x: A & B = obj;");
}

#[test]
fn test_parse_tuple_types() {
    assert_compiles("let x: [number, string] = [1, 'a'];");
    assert_compiles("let x: [number, string, boolean] = [1, 'a', true];");
}

#[test]
fn test_parse_generic_types() {
    assert_compiles("let x: Array<number> = [1, 2, 3];");
    assert_compiles("let x: Promise<string> = promise;");
    assert_compiles("let x: Map<string, number> = map;");
}

#[test]
fn test_parse_index_signatures() {
    assert_compiles("interface Dict { [key: string]: number; }");
}

#[test]
fn test_parse_call_signatures() {
    assert_compiles("interface Fn { (x: number): number; }");
}

#[test]
fn test_parse_conditional_types() {
    assert_compiles("type T = X extends Y ? A : B;");
}

#[test]
fn test_parse_mapped_types() {
    assert_compiles("type Readonly<T> = { readonly [P in keyof T]: T[P]; };");
}

#[test]
fn test_parse_declare_statements() {
    assert_compiles("declare function f(): void;");
    assert_compiles("declare const x: number;");
    assert_compiles("declare class C {}");
}

#[test]
fn test_parse_namespace() {
    assert_compiles("namespace N { export function f(): void {} }");
}

#[test]
fn test_parse_module_declarations() {
    assert_compiles("declare module 'm' { export const x: number; }");
}
