#[path = "../common/mod.rs"]
mod common;
use anyhow::Result;
use common::gen_ir_for_source;

#[test]
fn numeric_array_loop_sum_uses_array_get_f64() -> Result<()> {
    // Example source that reads a numeric array element by index
    let src =
        "export function main(): number { let a = [1,2,3,4]; let x = a[2]; println(x); return 0; }";
    let tmp_path =
        std::env::temp_dir().join(format!("oats_test_numeric_{}.oats", std::process::id()));
    std::fs::write(&tmp_path, src)?;
    let ir = gen_ir_for_source(&std::fs::read_to_string(&tmp_path)?)?;
    let _ = std::fs::remove_file(&tmp_path);
    assert!(
        ir.contains("array_get_f64"),
        "expected array_get_f64 call in IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn pointer_array_index_and_set_uses_ptr_helpers() -> Result<()> {
    // Example source that creates a pointer array (string literals) and reads an element
    let src = "export function main(): number { let a = [\"x\", \"y\"]; let p = a[1]; return 0; }";
    let tmp_path = std::env::temp_dir().join(format!("oats_test_ptr_{}.oats", std::process::id()));
    std::fs::write(&tmp_path, src)?;
    let ir = gen_ir_for_source(&std::fs::read_to_string(&tmp_path)?)?;
    let _ = std::fs::remove_file(&tmp_path);
    // The lowering should at least declare or call array_alloc/array_set_ptr/array_get_ptr
    assert!(
        ir.contains("array_alloc") || ir.contains("array_set_ptr") || ir.contains("array_get_ptr"),
        "expected pointer-array helpers in IR: {}",
        ir
    );
    Ok(())
}
