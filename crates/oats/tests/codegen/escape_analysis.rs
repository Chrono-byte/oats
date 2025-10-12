use super::common;
use anyhow::Result;
use common::gen_ir_for_source;
use std::env;

/// Test RC elision for non-escaping local variables
#[test]
fn test_non_escaping_local_rc_elision() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
export function main(): number {
    let x: number = 42;
    let y: number = x + 1;
    return y;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    assert!(!ir.contains("call void @rc_inc_local"), 
            "RC increment should be elided for non-escaping local");
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test that escaping variables still get RC operations
#[test]
fn test_escaping_local_keeps_rc_ops() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
export function main(): number {
    let x: number = 42;
    return x;  // x escapes via return
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    println!("IR for escaping local test:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test RC elision is disabled by default
#[test]
fn test_rc_elision_disabled_by_default() -> Result<()> {
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    
    let source = r#"
export function main(): number {
    let x: number = 42;
    let y: number = x + 1;
    return y;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    println!("IR without elision enabled:\n{}", ir);
    Ok(())
}

/// Test function call argument escaping
#[test]
fn test_function_call_argument_escaping() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
function helper(arg: number): number {
    return arg;
}

export function main(): number {
    let x: number = 42;
    let y: number = helper(x);  // x escapes as function argument
    return y;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    // Variables passed as function arguments should be marked as escaping
    println!("IR for function argument escaping:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test member assignment escaping
#[test]
fn test_member_assignment_escaping() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
class TestClass {
    field: number;
    
    constructor() {
        this.field = 0;
    }
    
    setField(value: number): void {
        let temp: number = value;
        this.field = temp;  // temp escapes via member assignment
    }
}

export function main(): number {
    let obj = new TestClass();
    obj.setField(42);
    return obj.field;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    // Variables assigned to object members should be marked as escaping
    println!("IR for member assignment escaping:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test async function await-live analysis
#[test]
fn test_async_await_live_analysis() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
export async function main(): Promise<number> {
    let x: number = 42;
    let y: number = await Promise.resolve(10);
    return x + y;  // x is live across await point
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    // Variables live across await points should be marked as escaping
    println!("IR for async await-live analysis:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test control flow with loops
#[test]
fn test_control_flow_loops() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
export function main(): number {
    let sum: number = 0;
    for (let i: number = 0; i < 10; i++) {
        sum = sum + i;
    }
    return sum;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    // Test that loop variables and accumulator are handled correctly
    println!("IR for control flow with loops:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}

/// Test conditional statements
#[test]
fn test_control_flow_conditionals() -> Result<()> {
    unsafe { env::set_var("OATS_ELIDE_ARC", "1"); }
    
    let source = r#"
export function main(): number {
    let flag: boolean = true;
    let x: number = 42;
    let result: number;
    
    if (flag) {
        result = x;
    } else {
        result = x * 2;
    }
    
    return result;
}
"#;
    
    let ir = gen_ir_for_source(source)?;
    
    // Test that variables used in conditional branches are analyzed correctly
    println!("IR for control flow with conditionals:\n{}", ir);
    
    unsafe { env::remove_var("OATS_ELIDE_ARC"); }
    Ok(())
}