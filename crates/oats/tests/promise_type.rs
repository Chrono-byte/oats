use anyhow::Result;
use oats::parser;
/// Unit tests for Promise type support in the type system
use oats::types::{OatsType, map_ts_type, infer_type_from_expr, infer_type};

#[test]
fn promise_type_creation() {
    // Test creating a Promise<number> type
    let promise_number = OatsType::wrap_in_promise(OatsType::Number);
    assert!(promise_number.is_promise());
    assert_eq!(
        promise_number.unwrap_promise_inner(),
        Some(&OatsType::Number)
    );
}

#[test]
fn promise_type_nested() {
    // Test Promise<Promise<number>>
    let inner_promise = OatsType::wrap_in_promise(OatsType::Number);
    let outer_promise = OatsType::wrap_in_promise(inner_promise.clone());

    assert!(outer_promise.is_promise());
    let unwrapped = outer_promise.unwrap_promise_inner();
    assert!(unwrapped.is_some());
    assert!(unwrapped.unwrap().is_promise());
}

#[test]
fn promise_type_with_string() {
    // Test Promise<string>
    let promise_string = OatsType::wrap_in_promise(OatsType::String);
    assert!(promise_string.is_promise());
    assert_eq!(
        promise_string.unwrap_promise_inner(),
        Some(&OatsType::String)
    );
}

#[test]
fn promise_type_with_array() {
    // Test Promise<number[]>
    let array_type = OatsType::Array(Box::new(OatsType::Number));
    let promise_array = OatsType::wrap_in_promise(array_type.clone());

    assert!(promise_array.is_promise());
    assert_eq!(promise_array.unwrap_promise_inner(), Some(&array_type));
}

#[test]
fn non_promise_type_checks() {
    // Verify non-promise types return false for is_promise()
    assert!(!OatsType::Number.is_promise());
    assert!(!OatsType::String.is_promise());
    assert!(!OatsType::Boolean.is_promise());
    assert!(!OatsType::Void.is_promise());

    let array_type = OatsType::Array(Box::new(OatsType::Number));
    assert!(!array_type.is_promise());

    let struct_type = OatsType::NominalStruct("Foo".to_string());
    assert!(!struct_type.is_promise());
}

#[test]
fn promise_type_unwrap_none_for_non_promise() {
    // Verify unwrap_promise_inner returns None for non-promise types
    assert_eq!(OatsType::Number.unwrap_promise_inner(), None);
    assert_eq!(OatsType::String.unwrap_promise_inner(), None);
}

#[test]
fn parse_promise_number_type() -> Result<()> {
    // Test parsing Promise<number> from TypeScript source
    let src = r#"
        async function test(): Promise<number> {
            return 42;
        }
    "#;

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the function
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Fn(fn_decl) = decl
        {
            // Check if it has a return type annotation
            if let Some(return_type) = &fn_decl.function.return_type {
                let ts_type = &return_type.type_ann;
                let oats_type = map_ts_type(ts_type);

                assert!(oats_type.is_some(), "Should parse Promise<number>");
                let oats_type = oats_type.unwrap();
                assert!(oats_type.is_promise(), "Should be a Promise type");
                assert_eq!(
                    oats_type.unwrap_promise_inner(),
                    Some(&OatsType::Number),
                    "Promise should wrap Number type"
                );
                return Ok(());
            }
        }
    }

    panic!("Should have found function with Promise return type");
}

#[test]
fn parse_promise_string_type() -> Result<()> {
    // Test parsing Promise<string> from TypeScript source
    let src = r#"
        async function fetchData(): Promise<string> {
            return "hello";
        }
    "#;

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the function
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Fn(fn_decl) = decl
            && let Some(return_type) = &fn_decl.function.return_type
        {
            let ts_type = &return_type.type_ann;
            let oats_type = map_ts_type(ts_type);

            assert!(oats_type.is_some());
            let oats_type = oats_type.unwrap();
            assert!(oats_type.is_promise());
            assert_eq!(oats_type.unwrap_promise_inner(), Some(&OatsType::String));
            return Ok(());
        }
    }

    panic!("Should have found function with Promise<string> return type");
}

#[test]
fn parse_promise_void_type() -> Result<()> {
    // Test parsing Promise<void> from TypeScript source
    let src = r#"
        async function doSomething(): Promise<void> {
            return;
        }
    "#;

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the function
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Fn(fn_decl) = decl
            && let Some(return_type) = &fn_decl.function.return_type
        {
            let ts_type = &return_type.type_ann;
            let oats_type = map_ts_type(ts_type);

            assert!(oats_type.is_some());
            let oats_type = oats_type.unwrap();
            assert!(oats_type.is_promise());
            assert_eq!(oats_type.unwrap_promise_inner(), Some(&OatsType::Void));
            return Ok(());
        }
    }

    panic!("Should have found function with Promise<void> return type");
}

#[test]
fn parse_promise_custom_type() -> Result<()> {
    // Test parsing Promise<CustomType> from TypeScript source
    let src = r#"
        class User {
            name: string;
            constructor(name: string) {
                this.name = name;
            }
        }
        
        async function fetchUser(): Promise<User> {
            return new User("Alice");
        }
    "#;

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the fetchUser function
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Fn(fn_decl) = decl
            && fn_decl.ident.sym.as_ref() == "fetchUser"
            && let Some(return_type) = &fn_decl.function.return_type
        {
            let ts_type = &return_type.type_ann;
            let oats_type = map_ts_type(ts_type);

            assert!(oats_type.is_some());
            let oats_type = oats_type.unwrap();
            assert!(oats_type.is_promise());

            let inner = oats_type.unwrap_promise_inner().unwrap();
            match inner {
                OatsType::NominalStruct(name) => {
                    assert_eq!(name, "User");
                }
                _ => panic!("Expected NominalStruct(User), got {:?}", inner),
            }
            return Ok(());
        }
    }

    panic!("Should have found fetchUser function with Promise<User> return type");
}

#[test]
fn promise_type_equality() {
    // Test that Promise types compare correctly
    let p1 = OatsType::wrap_in_promise(OatsType::Number);
    let p2 = OatsType::wrap_in_promise(OatsType::Number);
    let p3 = OatsType::wrap_in_promise(OatsType::String);

    assert_eq!(p1, p2);
    assert_ne!(p1, p3);
}

#[test]
fn infer_type_from_expr_literals() -> Result<()> {
    let source = r#"
export function main(): number {
    return 42;
}
"#;
    let parsed = parser::parse_oats_module(source, None)?;
    let program = parsed.parsed.program_ref();

    // Find the return statement with literal 42
    for item in program.body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                    if let Some(body) = &f.function.body {
                        for stmt in &body.stmts {
                            if let deno_ast::swc::ast::Stmt::Return(ret) = stmt {
                                if let Some(expr) = &ret.arg {
                                    let inferred = infer_type_from_expr(expr);
                                    assert_eq!(inferred, Some(OatsType::Number));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

#[test]
fn infer_type_from_expr_arrays() -> Result<()> {
    let source = r#"
export function main(): number {
    let arr = [1, 2, 3];
    return 0;
}
"#;
    let parsed = parser::parse_oats_module(source, None)?;
    let program = parsed.parsed.program_ref();

    // Find the array literal [1, 2, 3]
    for item in program.body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if let deno_ast::swc::ast::Stmt::Decl(decl) = stmt {
                if let deno_ast::swc::ast::Decl::Var(var_decl) = decl {
                    for decl in &var_decl.decls {
                        if let Some(init) = &decl.init {
                            if let deno_ast::swc::ast::Expr::Array(_) = &**init {
                                let inferred = infer_type_from_expr(init);
                                assert_eq!(inferred, Some(OatsType::Array(Box::new(OatsType::Number))));
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(())
}

#[test]
fn infer_type_combined() {
    // Test the combined infer_type function with TypeScript annotations
    // Create a simple number type annotation
    use deno_ast::swc::ast::{TsKeywordType, TsKeywordTypeKind, TsType};

    let ts_number = TsType::TsKeywordType(TsKeywordType {
        span: Default::default(),
        kind: TsKeywordTypeKind::TsNumberKeyword,
    });

    // Test with TypeScript type only
    let inferred = infer_type(Some(&ts_number), None);
    assert_eq!(inferred, OatsType::Number);

    // Test with expression only (should fallback to Number)
    use deno_ast::swc::ast::{Expr, Lit};
    let expr = Expr::Lit(Lit::Str(deno_ast::swc::ast::Str {
        span: Default::default(),
        value: "hello".into(),
        raw: None,
    }));

    let inferred_expr = infer_type(None, Some(&expr));
    assert_eq!(inferred_expr, OatsType::String);

    // Test fallback when both are None
    let inferred_fallback = infer_type(None, None);
    assert_eq!(inferred_fallback, OatsType::Number);
}
