// Test to see all unary operators
use deno_ast::{MediaType, ParseParams, parse_module};

#[test]
fn dump_all_unary_ops() {
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

    let parsed = parse_module(ParseParams {
        specifier: deno_ast::ModuleSpecifier::parse("file:///test.ts").unwrap(),
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        scope_analysis: false,
        maybe_syntax: None,
    })
    .unwrap();

    println!("{:#?}", parsed.program());
}
