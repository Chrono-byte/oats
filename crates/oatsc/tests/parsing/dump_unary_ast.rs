// Test to print what AST node type we get for unary operators
use oats_parser::parse_module;

#[test]
fn dump_unary_ast() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
export function main(): number {
    let x: number = 5;
    let y = -x;
    return y;
}
"#;

    let _parsed = parse_module(source).map_err(|e| format!("Parse errors: {:?}", e))?;

    // Print the program structure
    // println!("{:#?}", _parsed);
    Ok(())
}
