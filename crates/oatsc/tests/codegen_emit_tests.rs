//! Codegen emission tests for the Oats compiler.
//!
//! This module tests top-level emission including functions, classes, enums,
//! async/generator handling, and generic monomorphization.

mod test_utils;

use test_utils::*;

#[test]
fn test_emit_function() {
    let code = "function f(): void {}";
    assert_compiles(code);
}

#[test]
fn test_emit_function_with_params() {
    let code = "function f(x: number, y: string): void {}";
    assert_compiles(code);
}

#[test]
fn test_emit_function_with_return() {
    let code = "function f(): number { return 42; }";
    assert_compiles(code);
}

#[test]
fn test_emit_async_function() {
    let code = "async function f(): Promise<number> { return 1; }";
    assert_compiles(code);
}

#[test]
fn test_emit_async_function_with_await() {
    let code = "async function f(): Promise<number> { return await Promise.resolve(1); }";
    assert_compiles(code);
}

#[test]
fn test_emit_generator_function() {
    let code = "function* gen(): Generator<number> { yield 1; }";
    assert_compiles(code);
}

#[test]
fn test_emit_generator_function_multiple_yields() {
    let code = "function* gen(): Generator<number> { yield 1; yield 2; yield 3; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class() {
    let code = "class Point { x: number; y: number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_constructor() {
    let code = "class Point { constructor(public x: number, public y: number) {} }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_methods() {
    let code = "class Point { x: number; getX(): number { return this.x; } }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_inheritance() {
    let code = "class Parent { } class Child extends Parent { }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_multiple_inheritance_levels() {
    let code = "class Grandparent { } class Parent extends Grandparent { } class Child extends Parent { }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_super() {
    let code = "class Parent { method(): void {} } class Child extends Parent { method(): void { super.method(); } }";
    assert_compiles(code);
}

#[test]
fn test_emit_enum() {
    let code = "enum Color { Red, Green, Blue }";
    assert_compiles(code);
}

#[test]
fn test_emit_enum_with_values() {
    let code = "enum Direction { Up = 1, Down = 2, Left = 3, Right = 4 }";
    assert_compiles(code);
}

#[test]
fn test_emit_top_level_variable() {
    let code = "let x: number = 1;";
    assert_compiles(code);
}

#[test]
fn test_emit_top_level_const() {
    let code = "const x: number = 1;";
    assert_compiles(code);
}

#[test]
fn test_emit_arrow_function() {
    let code = "let f = (x: number) => x + 1;";
    assert_compiles(code);
}

#[test]
fn test_emit_exported_function() {
    let code = "export function f(): void {}";
    assert_compiles(code);
}

#[test]
fn test_emit_exported_class() {
    let code = "export class Point { }";
    assert_compiles(code);
}

#[test]
fn test_emit_exported_variable() {
    let code = "export const x = 1;";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_function() {
    let code = "function f<T>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_function_multiple_params() {
    let code = "function f<T, U>(x: T, y: U): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_class() {
    let code = "class Box<T> { constructor(public value: T) {} }";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_class_instantiation() {
    let code = "class Box<T> { constructor(public value: T) {} } let b = new Box<number>(1);";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_function_call() {
    let code = "function f<T>(x: T): T { return x; } let x = f<number>(1);";
    assert_compiles(code);
}

#[test]
fn test_emit_nested_generic() {
    let code = "class Box<T> { value: T; } let b = new Box<Box<number>>(null);";
    assert_compiles(code);
}

#[test]
fn test_emit_async_generator() {
    let code = "async function* gen(): AsyncGenerator<number> { yield 1; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_static_method() {
    let code = "class C { static method(): void {} }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_static_property() {
    let code = "class C { static prop: number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_private_member() {
    let code = "class C { private prop: number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_readonly_member() {
    let code = "class C { readonly prop: number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_interface() {
    let code = "interface Point { x: number; y: number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_interface_with_methods() {
    let code = "interface Shape { area(): number; }";
    assert_compiles(code);
}

#[test]
fn test_emit_interface_extends() {
    let code = "interface Base { } interface Derived extends Base { }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_implements_interface() {
    let code = "interface I { method(): void; } class C implements I { method(): void {} }";
    assert_compiles(code);
}

#[test]
fn test_emit_multiple_functions() {
    let code = "function f1(): void {} function f2(): void {} function f3(): void {}";
    assert_compiles(code);
}

#[test]
fn test_emit_multiple_classes() {
    let code = "class C1 { } class C2 { } class C3 { }";
    assert_compiles(code);
}

#[test]
fn test_emit_function_calling_function() {
    let code = "function f1(): number { return 1; } function f2(): number { return f1(); }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_using_class() {
    let code = "class Point { x: number; } class Line { start: Point; end: Point; }";
    assert_compiles(code);
}

#[test]
fn test_emit_async_with_multiple_awaits() {
    let code = "async function f(): Promise<number> { let a = await Promise.resolve(1); let b = await Promise.resolve(2); return a + b; }";
    assert_compiles(code);
}

#[test]
fn test_emit_generator_with_multiple_yields() {
    let code = "function* gen(): Generator<number> { yield 1; yield 2; yield 3; return 4; }";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_constraint() {
    let code = "interface HasLength { length: number; } function f<T extends HasLength>(x: T): number { return x.length; }";
    assert_compiles(code);
}

#[test]
fn test_emit_complex_generic() {
    let code = "class Container<T> { items: T[]; add(item: T): void { this.items.push(item); } }";
    assert_compiles(code);
}

#[test]
fn test_emit_abstract_class() {
    let code = "abstract class Base { abstract method(): void; }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_getter() {
    let code = "class C { get prop(): number { return 1; } }";
    assert_compiles(code);
}

#[test]
fn test_emit_class_with_setter() {
    let code = "class C { set prop(v: number) {} }";
    assert_compiles(code);
}

#[test]
fn test_emit_type_alias() {
    let code = "type Point = { x: number; y: number; };";
    assert_compiles(code);
}

#[test]
fn test_emit_generic_type_alias() {
    let code = "type Box<T> = { value: T; };";
    assert_compiles(code);
}

