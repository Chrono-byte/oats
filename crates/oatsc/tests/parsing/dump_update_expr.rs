// Test to see update expressions
use deno_ast::{MediaType, ParseParams, parse_module};

#[test]
fn dump_update_expr() -> Result<(), Box<dyn std::error::Error>> {
    let source = r#"
export function main(): number {
    let x: number = 5;
    x++;
    return x;
}
"#;

    let _parsed = parse_module(ParseParams {
        specifier: deno_ast::ModuleSpecifier::parse("file://test.ts")?,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    })?;

    // println!("{:#?}", _parsed.program());
    Ok(())
}
