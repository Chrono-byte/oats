// Minimal test to isolate the stack overflow issue
// This tests if the issue is with the basic recursive parser structure

use chumsky::prelude::*;
use chumsky::recursive::Recursive;

// Test 1: Simple recursive parser (should work)
fn test_simple_recursive() -> impl Parser<'static, &'static str, String> {
    recursive(|ty| {
        just("void").to("void".to_string())
            .or(ty.delimited_by(just("("), just(")")))
            .boxed()
    })
}

// Test 2: Recursive parser with function calls (like our structure)
fn test_with_functions() -> impl Parser<'static, &'static str, String> {
    recursive(|ty| {
        helper_parser(ty.clone()).boxed()
    })
}

fn helper_parser(ty: impl Parser<'static, &'static str, String> + Clone) -> impl Parser<'static, &'static str, String> {
    just("void").to("void".to_string())
        .or(ty.delimited_by(just("("), just(")")))
}

// Test 3: Using Recursive::declare()/define()
fn test_declare_define() -> impl Parser<'static, &'static str, String> {
    let mut ty = Recursive::declare();
    let parser = just("void").to("void".to_string())
        .or(ty.clone().delimited_by(just("("), just(")")))
        .boxed();
    ty.define(parser);
    ty
}

// Test 4: Multiple levels of function calls (like our actual structure)
fn test_multiple_levels() -> impl Parser<'static, &'static str, String> {
    recursive(|ty| {
        level1(ty.clone()).boxed()
    })
}

fn level1(ty: impl Parser<'static, &'static str, String> + Clone) -> impl Parser<'static, &'static str, String> {
    level2(ty.clone())
}

fn level2(ty: impl Parser<'static, &'static str, String> + Clone) -> impl Parser<'static, &'static str, String> {
    level3(ty.clone())
}

fn level3(ty: impl Parser<'static, &'static str, String> + Clone) -> impl Parser<'static, &'static str, String> {
    just("void").to("void".to_string())
        .or(ty.delimited_by(just("("), just(")")))
}

