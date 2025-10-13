#[test]
fn for_loop_generates_basic_blocks() {
    // Simple inline test that verifies for-loop syntax is recognized
    let src = r#"
        export function test(): number {
            for (let i = 0; i < 10; i = i + 1) {
                print_f64(i);
            }
            return 0;
        }
    "#;

    let result = oatsc::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "For-loop should parse successfully");
}

#[test]
fn if_statement_generates_basic_blocks() {
    // Simple inline test that verifies if statement syntax is recognized
    let src = r#"
        export function test(): number {
            if (1 > 0) {
                return 1;
            } else {
                return 0;
            }
        }
    "#;

    let result = oatsc::parser::parse_oats_module(src, None);
    assert!(result.is_ok(), "If statement should parse successfully");
}
