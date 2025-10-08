// Test to print what AST node type we get for unary operators
use deno_ast::{MediaType, ParseParams, parse_module};

#[test]
fn dump_unary_ast() {
    let source = r#"
export function main(): number {
    let x: number = 5;
    let y: number = -x;
    return y;
}
"#;

    let parsed = parse_module(ParseParams {
        specifier: deno_ast::ModuleSpecifier::parse("file://test.ts").unwrap(),
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    })
    .unwrap();

    // Print the program structure
    println!("{:#?}", parsed.program());
}
