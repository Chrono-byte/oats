// Test to see all unary operators
use oats_parser::parse_module;

#[test]
fn dump_all_unary_ops() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
export function main(): number {
    let x: number = 5;
    let neg = -x;      // unary minus
    let pos = +x;      // unary plus
    let not = !true;   // logical NOT
    let bnot = ~x;     // bitwise NOT
    return 0;
}
"#;

    let _parsed = parse_module(source).map_err(|e| format!("Parse errors: {:?}", e))?;

    // println!("{:#?}", _parsed);
    Ok(())
}
