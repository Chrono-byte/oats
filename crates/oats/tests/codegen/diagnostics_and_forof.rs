use anyhow::Result;

use super::common;
use common::gen_ir_for_source;
use oats::parser;

// use the shared helper

#[test]
fn parser_reports_missing_semicolon_with_hint() -> Result<()> {
    let src = "export function main(): number { let x = 1\n return x; }"; // missing semicolon after let
    let res = parser::parse_oats_module(src, None);
    assert!(
        res.is_err(),
        "expected parse to fail due to missing semicolon"
    );
    let err = match res {
        Err(e) => format!("{}", e),
        Ok(_) => panic!("expected error"),
    };
    assert!(err.contains("missing semicolon"));
    assert!(
        err.contains("add a trailing") || err.contains("hint:"),
        "err was: {}",
        err
    );
    Ok(())
}

#[test]
fn for_of_lowering_uses_array_helpers() -> Result<()> {
    let src = r#"
export function main(): number {
  let s = 0;
  for (let v of [1,2,3]) {
    s = s + v;
  }
  return s;
}
"#;
    let ir = gen_ir_for_source(src)?;
    assert!(
        ir.contains("array_get_f64") || ir.contains("array_get_ptr") || ir.contains("array_alloc"),
        "expected array helpers in IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn for_of_pointer_array_uses_ptr_helpers() -> Result<()> {
    let src = r#"
export function main(): number {
  for (let s of ["a","b"]) {
    println(s);
  }
  return 0;
}
"#;
    let ir = gen_ir_for_source(src)?;
    // pointer-based for-of should rely on array_get_ptr/array_alloc/array_set_ptr
    assert!(
        ir.contains("array_get_ptr") || ir.contains("array_alloc") || ir.contains("array_set_ptr"),
        "expected pointer-array helpers in IR: {}",
        ir
    );
    Ok(())
}
