//! Generics tests for the Oats compiler.
//!
//! This module tests generic instantiation, type parameter inference,
//! and constraint checking.

mod test_utils;

use test_utils::*;

#[test]
fn test_generic_function() {
    let code = "function f<T>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_function_call() {
    let code = "function f<T>(x: T): T { return x; } let x = f<number>(1);";
    assert_compiles(code);
}

#[test]
fn test_generic_function_inference() {
    let code = "function f<T>(x: T): T { return x; } let x = f(1);";
    assert_compiles(code);
}

#[test]
fn test_generic_class() {
    let code = "class Box<T> { constructor(public value: T) {} }";
    assert_compiles(code);
}

#[test]
fn test_generic_class_instantiation() {
    let code = "class Box<T> { constructor(public value: T) {} } let b = new Box<number>(1);";
    assert_compiles(code);
}

#[test]
fn test_generic_class_method() {
    let code = "class Box<T> { value: T; get(): T { return this.value; } }";
    assert_compiles(code);
}

#[test]
fn test_generic_multiple_params() {
    let code = "function f<T, U>(x: T, y: U): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_multiple_params_usage() {
    let code = "function f<T, U>(x: T, y: U): [T, U] { return [x, y]; }";
    assert_compiles(code);
}

#[test]
fn test_generic_nested() {
    let code = "class Outer<T> { inner: Inner<T>; } class Inner<U> { value: U; }";
    assert_compiles(code);
}

#[test]
fn test_generic_array() {
    let code = "function f<T>(arr: T[]): T { return arr[0]; }";
    assert_compiles(code);
}

#[test]
fn test_generic_union() {
    let code = "function f<T>(x: T | null): T | null { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_promise() {
    let code = "async function f<T>(): Promise<T> { return null as T; }";
    assert_compiles(code);
}

#[test]
fn test_generic_constraint() {
    let code = "interface HasLength { length: number; } function f<T extends HasLength>(x: T): number { return x.length; }";
    assert_compiles(code);
}

#[test]
fn test_generic_constraint_multiple() {
    let code = "interface A { } interface B { } function f<T extends A & B>(x: T): void { }";
    assert_compiles(code);
}

#[test]
fn test_generic_default_parameter() {
    // Note: TypeScript supports default type parameters, testing if Oats does
    let code = "function f<T = number>(x: T): T { return x; }";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_class_with_method() {
    let code = r#"
        class Container<T> {
            items: T[];
            add(item: T): void {
                this.items.push(item);
            }
            get(index: number): T {
                return this.items[index];
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_function_overload() {
    // Note: Testing if generic functions can be overloaded
    let code = "function f<T>(x: T): T; function f(x: number): number { return x; }";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_recursive() {
    let code = "class Node<T> { value: T; children: Node<T>[]; }";
    assert_compiles(code);
}

#[test]
fn test_generic_map_like() {
    let code = r#"
        class Map<K, V> {
            entries: [K, V][];
            set(key: K, value: V): void {
                this.entries.push([key, value]);
            }
            get(key: K): V | null {
                for (let [k, v] of this.entries) {
                    if (k === key) return v;
                }
                return null;
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_identity_function() {
    let code = "function identity<T>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_partial() {
    let code = r#"
        interface Point {
            x: number;
            y: number;
        }
        function createPartial<T>(obj: Partial<T>): T {
            return obj as T;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_pick() {
    let code = r#"
        interface Person {
            name: string;
            age: number;
            email: string;
        }
        function getName<T extends { name: string }>(obj: T): string {
            return obj.name;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_conditional() {
    // Note: Testing conditional types if supported
    let code = "type IsArray<T> = T extends any[] ? true : false;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_mapped() {
    // Note: Testing mapped types if supported
    let code = "type Readonly<T> = { readonly [P in keyof T]: T[P]; };";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_utility_readonly() {
    let code = r#"
        interface Point {
            x: number;
            y: number;
        }
        function makeReadonly<T>(obj: T): Readonly<T> {
            return obj as Readonly<T>;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_utility_required() {
    let code = r#"
        interface Point {
            x?: number;
            y?: number;
        }
        function requireAll<T>(obj: T): Required<T> {
            return obj as Required<T>;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_chain() {
    let code = r#"
        class Chain<T> {
            value: T;
            map<U>(fn: (x: T) => U): Chain<U> {
                return new Chain<U>();
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_optional() {
    let code = "function f<T>(x: T | null): T | null { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_tuple() {
    let code = "function f<T, U>(): [T, U] { return null as [T, U]; }";
    assert_compiles(code);
}

#[test]
fn test_generic_rest() {
    let code = "function f<T extends any[]>(...args: T): T { return args; }";
    assert_compiles(code);
}

#[test]
fn test_generic_extends_primitive() {
    let code = "function f<T extends number>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_extends_string() {
    let code = "function f<T extends string>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_extends_object() {
    let code = "function f<T extends object>(x: T): T { return x; }";
    assert_compiles(code);
}

#[test]
fn test_generic_keyof() {
    // Note: Testing keyof operator if supported
    let code = "type Keys<T> = keyof T;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_indexed_access() {
    // Note: Testing indexed access types if supported
    let code = "type Value<T, K extends keyof T> = T[K];";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_infer() {
    // Note: Testing infer keyword if supported
    let code = "type Unwrap<T> = T extends Promise<infer U> ? U : T;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_this_type() {
    // Note: Testing 'this' type in generics if supported
    let code = "class C { method(): this { return this; } }";
    assert_compiles(code);
}

#[test]
fn test_generic_variance() {
    // Testing covariance/contravariance concepts
    let code = r#"
        interface Producer<T> {
            produce(): T;
        }
        interface Consumer<T> {
            consume(x: T): void;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_utility_record() {
    let code = r#"
        type Record<K extends string, V> = { [key in K]: V };
        function createRecord<K extends string, V>(keys: K[], value: V): Record<K, V> {
            return {} as Record<K, V>;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_utility_pick() {
    let code = r#"
        interface Person {
            name: string;
            age: number;
        }
        type NameOnly = Pick<Person, 'name'>;
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_utility_omit() {
    let code = r#"
        interface Person {
            name: string;
            age: number;
        }
        type WithoutAge = Omit<Person, 'age'>;
    "#;
    assert_compiles(code);
}

#[test]
fn test_generic_exclude() {
    // Note: Testing Exclude utility type if supported
    let code = "type NonNull<T> = Exclude<T, null | undefined>;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_extract() {
    // Note: Testing Extract utility type if supported
    let code = "type StringKeys<T> = Extract<keyof T, string>;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_nonnullable() {
    // Note: Testing NonNullable utility type if supported
    let code = "type Safe<T> = NonNullable<T>;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_parameters() {
    let code = "type Params<T> = T extends (...args: infer P) => any ? P : never;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_return_type() {
    let code = "type Return<T> = T extends (...args: any[]) => infer R ? R : never;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_instance_type() {
    let code = "type Instance<T> = T extends new (...args: any[]) => infer I ? I : never;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}

#[test]
fn test_generic_constructor_parameters() {
    let code = "type ConstructorParams<T> = T extends new (...args: infer P) => any ? P : never;";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse");
}
