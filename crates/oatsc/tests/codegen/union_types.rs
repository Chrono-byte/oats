use anyhow::Result;
use super::common;
use common::create_codegen;

#[test]
fn union_local_and_param_boxing() -> Result<()> {
    let src = r#"
        export function main(): number {
            let u: number | string = 1;
            let s: number | string = "hi";
            return 0;
        }
    "#;

    let context = inkwell::context::Context::create();
    let symbols = oatsc::types::SymbolTable::new();
    let codegen = create_codegen(&context, "union_types_test", symbols, src);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("union_box_f64"),
        "IR should box numbers into union_box_f64"
    );
    Ok(())
}

#[test]
fn union_param_boxing() -> Result<()> {
    let src = r#"
        export function main(): number {
            function f(x: number | string): number { return 0; }
            f(3);
            return 0;
        }
    "#;

    let context = inkwell::context::Context::create();
    let symbols = oatsc::types::SymbolTable::new();
    let codegen = create_codegen(&context, "union_types_test", symbols, src);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("union_box_f64") || ir.contains("union_box_ptr"),
        "Param boxing should emit union_box_f64 when numbers are passed"
    );
    Ok(())
}

#[test]
fn typeof_guard_uses_discriminant() -> Result<()> {
    let src = r#"
        export function main(): number {
            let u: number | string = 1;
            if (typeof u === "number") {
                return 1;
            }
            return 0;
        }
    "#;

    let context = inkwell::context::Context::create();
    let symbols = oatsc::types::SymbolTable::new();
    let codegen = create_codegen(&context, "union_types_test", symbols, src);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("union_unbox_f64") || ir.contains("union_get_discriminant"),
        "The lowered IR should test the union discriminant or unbox number"
    );
    Ok(())
}
