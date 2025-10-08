// Test to see update expressions
use deno_ast::{parse_module, MediaType, ParseParams};

#[test]
fn dump_update_expr() {
    let source = r#"
export function main(): number {
    let x: number = 5;
    x++;
    x--;
    ++x;
    --x;
    return 0;
}
"#;

    let parsed = parse_module(ParseParams {
        specifier: deno_ast::ModuleSpecifier::parse("file:///test.ts").unwrap(),
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    }).unwrap();

    println!("{:#?}", parsed.program());
}
