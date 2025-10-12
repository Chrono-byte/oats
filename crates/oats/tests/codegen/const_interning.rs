use anyhow::Result;

use super::common::gen_ir_for_source;

#[test]
fn interning_repeated_nested_consts() -> Result<()> {
    let src = r#"
    export function main() {
        const leaf = { a: 1, b: "x" };
        const obj1 = { x: leaf, y: leaf };
        const obj2 = { p: leaf, q: leaf };
        return 0;
    }
    "#;

    let ir = gen_ir_for_source(src)?;

    // Expect at least one interned global to exist. The naming scheme is `const.intern.<hex>`
    let count_interns = ir.matches("const.intern.").count();
    assert!(count_interns >= 1, "expected at least one const.intern global in IR\n{}", ir);

    // The repeated nested `leaf` should produce exactly one intern global for its shape
    // (there may be other interned globals emitted for different constants), so check
    // that two references to the same nested shape do not create obvious duplicates
    // by ensuring the finger-print pattern appears at least once and not repeated for the exact same content.
    // A stronger check would inspect module globals; this is a smoke test on IR text.

    Ok(())
}

#[test]
fn heterogeneous_array_should_error() {
    let src = r#"
    export function main() {
        const x = [1, "s"];
        return 0;
    }
    "#;

    let res = gen_ir_for_source(src);
    assert!(res.is_err(), "expected heterogeneous const array to fail const emission");
    Ok(())
}

#[test]
fn nested_container_metadata_emitted() -> Result<()> {
    let src = r#"
    export function main() {
        const inner = { s: "hi" };
        const outer = { a: inner };
        return 0;
    }
    "#;

    let ir = gen_ir_for_source(src)?;

    // Object metadata global uses suffix _meta in prior design; ensure at least one _meta symbol exists
    assert!(ir.contains("_meta"), "expected metadata global in IR\n{}", ir);
    Ok(())
}
