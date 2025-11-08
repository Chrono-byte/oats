// Test to see update expressions
use oats_parser::parse_module;

#[test]
fn dump_update_expr() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
export function main(): number {
    let x: number = 5;
    x++;
    return x;
}
"#;

    let _parsed = parse_module(source).map_err(|e| format!("Parse errors: {:?}", e))?;

    // println!("{:#?}", _parsed);
    Ok(())
}
