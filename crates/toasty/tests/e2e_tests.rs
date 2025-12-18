use assert_cmd::Command;
use std::fs;
use tempfile::TempDir;

/// Helper to run a piece of Oats code and return its stdout.
/// Panics if compilation or execution fails.
fn run_oats_code(name: &str, code: &str) -> String {
    // Setup paths (similar to proper_tests.rs)
    let toasty_bin = assert_cmd::cargo::cargo_bin("toasty");
    let target_dir = toasty_bin.parent().expect("Failed to get target dir");
    let runtime_path = target_dir.join("libruntime.a");
    let std_path = target_dir.join("liboats_std.a");
    let oatsc_path = assert_cmd::cargo::cargo_bin("oatsc");

    if !runtime_path.exists() || !std_path.exists() {
        panic!(
            "Required libraries not found in {}.\nPlease run 'cargo build' or 'cargo test --workspace' to ensure all artifacts are built.",
            target_dir.display()
        );
    }

    // Create temp dir for this test
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let src_path = temp_dir.path().join(format!("{}.oats", name));
    let out_dir = temp_dir.path().join("out");

    // Write code to file
    fs::write(&src_path, code).expect("Failed to write source file");

    // Run toasty run
    let mut cmd = Command::cargo_bin("toasty")
        .expect("Failed to find toasty binary. Run 'cargo build' first.");
    cmd.arg("run")
        .arg("--color")
        .arg("never")
        .arg(&src_path)
        .env("OATS_OUT_DIR", &out_dir)
        .env("OATS_RUNTIME_PATH", &runtime_path)
        .env("OATS_STD_PATH", &std_path)
        .env("OATS_OATSC_PATH", &oatsc_path);

    let output = cmd.output().expect("Failed to execute toasty run");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();

    println!("Command: {:?}", cmd);
    println!("STDOUT:\n{}", stdout);
    println!("STDERR:\n{}", stderr);

    if !output.status.success() {
        panic!(
            "Test '{}' failed:\n--- STDOUT ---\n{}\n--- STDERR ---
{}",
            name, stdout, stderr
        );
    }

    strip_ansi_codes(&stdout)
}

fn strip_ansi_codes(s: &str) -> String {
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[A-Za-z]")
        .expect("Failed to compile ANSI escape code regex pattern");
    re.replace_all(s, "").to_string()
}

#[test]
fn test_basic_types() {
    let code = r#" 
        export function main(): void {
            let s: string = "hello";
            let n: number = 42;
            let b: boolean = true;
            console.log(s);
            console.log(n);
            console.log(b);
        }
    "#;
    let output = run_oats_code("basic_types", code);
    assert!(output.contains("hello"));
    assert!(output.contains("42"));
    // Boolean currently prints as 1
    assert!(output.contains("1"));
}

#[test]
fn test_control_flow() {
    let code = r#" 
        export function main(): void {
            let i: number = 0;
            while (i < 3) {
                console.log("loop " + i);
                i = i + 1;
            }
            if (true) {
                console.log("condition met");
            }
        }
    "#;
    let output = run_oats_code("control_flow", code);
    assert!(output.contains("loop 0"));
    assert!(output.contains("loop 1"));
    assert!(output.contains("loop 2"));
    assert!(output.contains("condition met"));
}

#[test]
fn test_arrays() {
    let code = r#" 
        export function main(): void {
            let arr: number[] = [1, 2, 3];
            console.log(arr[0]);
            console.log(arr[1]);
            console.log(arr[2]);
            console.log("len: " + arr.length);
        }
    "#;
    let output = run_oats_code("arrays", code);
    assert!(output.contains("1"));
    assert!(output.contains("2"));
    assert!(output.contains("3"));
    assert!(output.contains("len: 3"));
}

#[test]
fn test_functions() {
    let code = r#" 
        function add(a: number, b: number): number {
            return a + b;
        }
        export function main(): void {
            console.log(add(10, 20));
        }
    "#;
    let output = run_oats_code("functions", code);
    assert!(output.contains("30"));
}

#[test]
fn test_classes() {
    let code = r#" 
        class Greeter {
            constructor(public msg: string) {}
            greet(): void {
                console.log("Greeting: " + this.msg);
            }
        }
        export function main(): void {
            let g = new Greeter("Rust");
            g.greet();
        }
    "#;
    let output = run_oats_code("classes", code);
    assert!(output.contains("Greeting: Rust"));
}

#[test]
fn test_inheritance() {
    let code = r#" 
        class Animal {
            speak(): void { console.log("..."); }
        }
        class Dog extends Animal {
            speak(): void { console.log("Woof"); }
        }
        export function main(): void {
            let d = new Dog();
            d.speak();
        }
    "#;
    let output = run_oats_code("inheritance", code);
    assert!(output.contains("Woof"));
}

#[test]
fn test_generics() {
    let code = r#" 
        class Box<T> {
            constructor(public val: T) {}
            get(): T { return this.val; }
        }
        export function main(): void {
            let b = new Box<number>(123);
            console.log(b.get());
            let s = new Box<string>("str");
            console.log(s.get());
        }
    "#;
    let output = run_oats_code("generics", code);
    assert!(output.contains("123"));
    assert!(output.contains("str"));
}

#[test]
fn test_interfaces() {
    let code = r#" 
        interface Point {
            x: number;
            y: number;
        }
        function printPoint(p: Point): void {
            console.log("x=" + p.x + ", y=" + p.y);
        }
        export function main(): void {
            let p: Point = { x: 10, y: 20 };
            printPoint(p);
        }
    "#;
    let output = run_oats_code("interfaces", code);
    assert!(output.contains("x=10, y=20"));
}

#[test]
fn test_unions() {
    let code = r#" 
        function printId(id: number | string): void {
            console.log("ID: " + id);
        }
        export function main(): void {
            printId(101);
            printId("202");
        }
    "#;
    let output = run_oats_code("unions", code);
    assert!(output.contains("ID: 101"));
    assert!(output.contains("ID: 202"));
}
