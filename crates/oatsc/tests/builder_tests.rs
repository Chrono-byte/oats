//! Builder tests for the Oats compiler.
//!
//! This module tests the full compilation pipeline, multi-file compilation,
//! and external dependencies.

mod test_utils;

use oatsc::CompileOptions;
use test_utils::*;

#[test]
fn test_builder_single_file() {
    let code = "export function main(): void {}";
    assert_compiles(code);
}

#[test]
fn test_builder_simple_function() {
    let code = "function add(a: number, b: number): number { return a + b; }";
    assert_compiles(code);
}

#[test]
fn test_builder_with_class() {
    let code = "class Point { constructor(public x: number, public y: number) {} }";
    assert_compiles(code);
}

#[test]
fn test_builder_with_import() {
    // Note: This would require actual module resolution
    // For now, we just test that the code parses
    let code = "import { foo } from './module';";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse import");
}

#[test]
fn test_builder_with_export() {
    let code = "export function main(): void {}";
    assert_compiles(code);
}

#[test]
fn test_builder_complex_program() {
    let code = r#"
        class Point {
            constructor(public x: number, public y: number) {}
            distance(other: Point): number {
                let dx = this.x - other.x;
                let dy = this.y - other.y;
                return Math.sqrt(dx * dx + dy * dy);
            }
        }
        function main(): void {
            let p1 = new Point(0, 0);
            let p2 = new Point(3, 4);
            let d = p1.distance(p2);
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_generics() {
    let code = r#"
        class Box<T> {
            constructor(public value: T) {}
            get(): T { return this.value; }
        }
        function main(): void {
            let b = new Box<number>(42);
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_async() {
    let code = r#"
        async function fetchData(): Promise<number> {
            return await Promise.resolve(42);
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_unions() {
    let code = r#"
        function process(id: number | string): void {
            if (typeof id === 'number') {
                // handle number
            } else {
                // handle string
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_interfaces() {
    let code = r#"
        interface Shape {
            area(): number;
        }
        class Circle implements Shape {
            constructor(public radius: number) {}
            area(): number {
                return Math.PI * this.radius * this.radius;
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_enums() {
    let code = r#"
        enum Color {
            Red,
            Green,
            Blue
        }
        function getColor(): Color {
            return Color.Red;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_tuples() {
    let code = r#"
        function getPoint(): [number, number] {
            return [1, 2];
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_arrays() {
    let code = r#"
        function processArray(arr: number[]): number {
            let sum = 0;
            for (let i = 0; i < arr.length; i++) {
                sum += arr[i];
            }
            return sum;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_control_flow() {
    let code = r#"
        function classify(x: number): string {
            if (x > 0) {
                return 'positive';
            } else if (x < 0) {
                return 'negative';
            } else {
                return 'zero';
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_loops() {
    let code = r#"
        function sum(n: number): number {
            let result = 0;
            for (let i = 0; i <= n; i++) {
                result += i;
            }
            return result;
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_with_error_handling() {
    let code = r#"
        function risky(): number {
            try {
                return 42;
            } catch (e) {
                return 0;
            }
        }
    "#;
    assert_compiles(code);
}

#[test]
fn test_builder_compile_options_new() {
    let opts = CompileOptions::new("test.oats".to_string());
    assert_eq!(opts.src_file, "test.oats");
    assert!(opts.additional_src_files.is_empty());
}

#[test]
fn test_builder_compile_options_with_modules() {
    let opts = CompileOptions::with_modules("main.oats".to_string(), vec!["util.oats".to_string()]);
    assert_eq!(opts.src_file, "main.oats");
    assert_eq!(opts.additional_src_files.len(), 1);
}

#[test]
fn test_builder_compile_options_for_package() {
    use std::path::PathBuf;
    let opts = CompileOptions::for_package(PathBuf::from("."));
    assert!(opts.package_root.is_some());
    assert!(opts.emit_object_only);
}
