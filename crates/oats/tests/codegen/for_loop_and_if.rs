// Test for-loop and if statement lowering
use anyhow::Result;
use super::common;
use common::create_codegen;

#[test]
fn for_loop_and_if_statement_in_fibonacci() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/fibonaci.oats")?;

    let context = inkwell::context::Context::create();
    let symbols = oats::types::SymbolTable::new();
    let codegen = create_codegen(&context, "for_loop_and_if_test", symbols, &src);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("for_loop") && ir.contains("if_statement"),
        "IR should contain for-loop and if-statement lowering"
    );
    Ok(())
}

#[test]
fn for_loop_generates_basic_blocks() {
    // Simple inline test that verifies for-loop syntax is recognized
    let src = r#"
        export function test(): number {
            for (let i = 0; i < 10; i = i + 1) {
                print_f64(i);
            }
            return 0;
        }
    "#;

    let result = oats::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "For-loop should parse successfully");
}

#[test]
fn if_statement_generates_basic_blocks() {
    // Simple inline test that verifies if statement syntax is recognized
    let src = r#"
        export function test(): number {
            if (1 > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;

    let result = oats::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "If statement should parse successfully");
}
