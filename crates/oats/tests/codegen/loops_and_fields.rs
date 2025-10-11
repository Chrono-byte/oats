use anyhow::Result;
use oats::types::SymbolTable;
use super::common::create_codegen;

fn gen_ir_for_source(src: &str) -> Result<String> {
    let context = inkwell::context::Context::create();
    let symbols = SymbolTable::new();
    let codegen = create_codegen(&context, "loops_and_fields_test", symbols, src);

    Ok(codegen.module.print_to_string().to_string())
}

#[test]
fn labeled_break_continue_lowering_generates_ir() -> Result<()> {
    let src = r#"
export function main(): number {
  let s = 0;
  outer: for (let i = 0; i < 3; i = i + 1) {
    for (let j = 0; j < 3; j = j + 1) {
      if (i == 1) { break outer; }
      if (j == 1) { continue; }
      s = s + 1;
    }
  }
  return s;
}
"#;
    let ir = gen_ir_for_source(src)?;
    // Ensure IR was generated and contains a function definition and a return
    assert!(
        ir.contains("define") && ir.contains("ret"),
        "unexpected IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn dot_member_param_access_generates_field_load() -> Result<()> {
    let src = r#"
export class Foo { x: number }
export function main(p: Foo): number { return p.x; }
"#;
    let ir = gen_ir_for_source(src)?;
    // Field access lowering uses a named load "field_load"
    assert!(
        ir.contains("field_f64_load") || ir.contains("field_load"),
        "expected field load in IR: {}",
        ir
    );
    Ok(())
}
