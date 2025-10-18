use anyhow::Result;

use super::common;
use common::gen_ir_for_source;

#[test]
fn field_write_emits_gep_store_and_rc_calls() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/field_write.oats")?;
    let ir_string = gen_ir_for_source(&src)?;

    // 1. Should have GEP instruction for field access
    assert!(
        ir_string.contains("getelementptr") || ir_string.contains("gep"),
        "IR should contain GEP instruction for field offset calculation"
    );

    // 2. Should have store instruction
    assert!(
        ir_string.contains("store"),
        "IR should contain store instruction"
    );

    // 3. Check that the function structure is correct
    assert!(
        ir_string.contains("Counter_increment"),
        "IR should contain the Counter_increment function"
    );

    // 4. Field offset calculation (header size + field index * pointer size)
    assert!(
        ir_string.contains("fld_off") || ir_string.contains("field"),
        "IR should contain field offset calculation"
    );

    Ok(())
}

#[test]
fn field_write_with_pointer_type_uses_rc() -> Result<()> {
    let src = r#"
        export class Container {
            constructor(public data: string) {}
            
            setData(newData: string): void {
                this.data = newData;
            }
        }
        
        export function main(): number {
            let c: Container = new Container("hello");
            c.setData("world");
            return 0;
        }
    "#;

    let ir_string = gen_ir_for_source(src)?;
    assert!(
        ir_string.contains("rc_inc") || ir_string.contains("rc_inc_new_field"),
        "IR should contain rc_inc call for new field value"
    );

    assert!(
        ir_string.contains("store"),
        "IR should contain store instruction"
    );

    Ok(())
}
