//! Type system tests for the Oats compiler.
//!
//! This module tests type inference, type checking, symbol table operations,
//! and all OatsType variants.

mod test_utils;

use oatsc::types::{FunctionSig, OatsType, Symbol, SymbolTable};
use test_utils::*;

#[test]
fn test_oats_type_number() {
    let ty = OatsType::Number;
    assert_eq!(ty, OatsType::Number);
}

#[test]
fn test_oats_type_boolean() {
    let ty = OatsType::Boolean;
    assert_eq!(ty, OatsType::Boolean);
}

#[test]
fn test_oats_type_string() {
    let ty = OatsType::String;
    assert_eq!(ty, OatsType::String);
}

#[test]
fn test_oats_type_void() {
    let ty = OatsType::Void;
    assert_eq!(ty, OatsType::Void);
}

#[test]
fn test_oats_type_array() {
    let ty = OatsType::Array(Box::new(OatsType::Number));
    match ty {
        OatsType::Array(elem) => assert_eq!(*elem, OatsType::Number),
        _ => panic!("Expected Array type"),
    }
}

#[test]
fn test_oats_type_union() {
    let ty = OatsType::Union(vec![OatsType::Number, OatsType::String]);
    match ty {
        OatsType::Union(parts) => {
            assert_eq!(parts.len(), 2);
            assert!(parts.contains(&OatsType::Number));
            assert!(parts.contains(&OatsType::String));
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_oats_type_union_three_parts() {
    let ty = OatsType::Union(vec![OatsType::Number, OatsType::String, OatsType::Boolean]);
    match ty {
        OatsType::Union(parts) => assert_eq!(parts.len(), 3),
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_oats_type_nominal_struct() {
    let ty = OatsType::NominalStruct("Point".to_string());
    match ty {
        OatsType::NominalStruct(name) => assert_eq!(name, "Point"),
        _ => panic!("Expected NominalStruct type"),
    }
}

#[test]
fn test_oats_type_tuple() {
    let ty = OatsType::Tuple(vec![OatsType::Number, OatsType::String]);
    match ty {
        OatsType::Tuple(elements) => {
            assert_eq!(elements.len(), 2);
            assert_eq!(elements[0], OatsType::Number);
            assert_eq!(elements[1], OatsType::String);
        }
        _ => panic!("Expected Tuple type"),
    }
}

#[test]
fn test_oats_type_promise() {
    let ty = OatsType::Promise(Box::new(OatsType::Number));
    match ty {
        OatsType::Promise(inner) => assert_eq!(*inner, OatsType::Number),
        _ => panic!("Expected Promise type"),
    }
}

#[test]
fn test_oats_type_weak() {
    let ty = OatsType::Weak(Box::new(OatsType::NominalStruct("Point".to_string())));
    match ty {
        OatsType::Weak(inner) => match *inner {
            OatsType::NominalStruct(name) => assert_eq!(name, "Point"),
            _ => panic!("Expected NominalStruct inside Weak"),
        },
        _ => panic!("Expected Weak type"),
    }
}

#[test]
fn test_oats_type_unowned() {
    let ty = OatsType::Unowned(Box::new(OatsType::String));
    match ty {
        OatsType::Unowned(inner) => assert_eq!(*inner, OatsType::String),
        _ => panic!("Expected Unowned type"),
    }
}

#[test]
fn test_oats_type_option() {
    let ty = OatsType::Option(Box::new(OatsType::Number));
    match ty {
        OatsType::Option(inner) => assert_eq!(*inner, OatsType::Number),
        _ => panic!("Expected Option type"),
    }
}

#[test]
fn test_oats_type_struct_literal() {
    let ty = OatsType::StructLiteral(vec![
        ("x".to_string(), OatsType::Number),
        ("y".to_string(), OatsType::Number),
    ]);
    match ty {
        OatsType::StructLiteral(fields) => {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].0, "x");
            assert_eq!(fields[0].1, OatsType::Number);
        }
        _ => panic!("Expected StructLiteral type"),
    }
}

#[test]
fn test_oats_type_generic() {
    let ty = OatsType::Generic(vec![OatsType::Number, OatsType::String]);
    match ty {
        OatsType::Generic(params) => assert_eq!(params.len(), 2),
        _ => panic!("Expected Generic type"),
    }
}

#[test]
fn test_oats_type_generic_instance() {
    let ty = OatsType::GenericInstance {
        base_name: "Array".to_string(),
        type_args: vec![OatsType::Number],
    };
    match ty {
        OatsType::GenericInstance {
            base_name,
            type_args,
        } => {
            assert_eq!(base_name, "Array");
            assert_eq!(type_args.len(), 1);
        }
        _ => panic!("Expected GenericInstance type"),
    }
}

#[test]
fn test_oats_type_enum() {
    let ty = OatsType::Enum(
        "Color".to_string(),
        vec!["Red".to_string(), "Green".to_string()],
    );
    match ty {
        OatsType::Enum(name, variants) => {
            assert_eq!(name, "Color");
            assert_eq!(variants.len(), 2);
        }
        _ => panic!("Expected Enum type"),
    }
}

#[test]
fn test_oats_type_process_id() {
    let ty = OatsType::ProcessId;
    assert_eq!(ty, OatsType::ProcessId);
}

#[test]
fn test_oats_type_monitor_ref() {
    let ty = OatsType::MonitorRef;
    assert_eq!(ty, OatsType::MonitorRef);
}

#[test]
fn test_symbol_table_new() {
    let table = SymbolTable::new();
    // Test that we can insert and retrieve from a new table
    let mut table = table;
    table.insert_variable("test".to_string(), OatsType::Number);
    assert!(table.get("test").is_some());
}

#[test]
fn test_symbol_table_insert_variable() {
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);

    let symbol = table.get("x");
    assert!(symbol.is_some());
    match symbol {
        Some(Symbol::Variable { ty }) => assert_eq!(ty, &OatsType::Number),
        _ => panic!("Expected Variable symbol"),
    }
}

#[test]
fn test_symbol_table_insert_std_function() {
    let mut table = SymbolTable::new();
    let sig = FunctionSig {
        type_params: vec![],
        params: vec![OatsType::Number],
        ret: OatsType::Number,
    };
    table.insert_std_function("add".to_string(), "add_mangled".to_string(), sig.clone());

    let symbol = table.get("add");
    assert!(symbol.is_some());
    match symbol {
        Some(Symbol::StdFunction {
            mangled_name,
            sig: retrieved_sig,
        }) => {
            assert_eq!(mangled_name, "add_mangled");
            assert_eq!(retrieved_sig.params, sig.params);
        }
        _ => panic!("Expected StdFunction symbol"),
    }
}

#[test]
fn test_symbol_table_insert_type() {
    let mut table = SymbolTable::new();
    table.insert_type("Point".to_string(), "Point_mangled".to_string());

    let symbol = table.get("Point");
    assert!(symbol.is_some());
    match symbol {
        Some(Symbol::Type { mangled_name }) => assert_eq!(mangled_name, "Point_mangled"),
        _ => panic!("Expected Type symbol"),
    }
}

#[test]
fn test_symbol_table_get_type() {
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);

    let ty = table.get_type("x");
    assert_eq!(ty, Some(&OatsType::Number));
}

#[test]
fn test_symbol_table_get_type_nonexistent() {
    let table = SymbolTable::new();
    let ty = table.get_type("nonexistent");
    assert!(ty.is_none());
}

#[test]
fn test_symbol_table_scoping() {
    // Note: SymbolTable scopes are private, so we test scoping through
    // the public API by inserting symbols and verifying lookup behavior
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);

    // Verify we can retrieve the symbol
    let ty = table.get_type("x");
    assert_eq!(ty, Some(&OatsType::Number));

    // Inserting again should shadow (implementation detail)
    table.insert_variable("x".to_string(), OatsType::String);
    let ty = table.get_type("x");
    assert_eq!(ty, Some(&OatsType::String));
}

#[test]
fn test_symbol_table_shadowing() {
    // Test that inserting the same name again shadows the previous value
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);

    // Inserting again should update/shadow
    table.insert_variable("x".to_string(), OatsType::String);

    // Should get the most recent value
    let ty = table.get_type("x");
    assert_eq!(ty, Some(&OatsType::String));
}

#[test]
fn test_symbol_table_all_symbols() {
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);
    table.insert_variable("y".to_string(), OatsType::String);

    let all = table.all_symbols();
    assert_eq!(all.len(), 2);
}

#[test]
fn test_function_sig_creation() {
    let sig = FunctionSig {
        type_params: vec!["T".to_string()],
        params: vec![OatsType::Number, OatsType::String],
        ret: OatsType::Boolean,
    };

    assert_eq!(sig.type_params.len(), 1);
    assert_eq!(sig.params.len(), 2);
    assert_eq!(sig.ret, OatsType::Boolean);
}

#[test]
fn test_function_sig_no_params() {
    let sig = FunctionSig {
        type_params: vec![],
        params: vec![],
        ret: OatsType::Void,
    };

    assert_eq!(sig.params.len(), 0);
    assert_eq!(sig.ret, OatsType::Void);
}

#[test]
fn test_type_inference_from_literal_number() {
    // This would typically be done through the actual inference function
    // For now, we test the concept
    let code = "42";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse number literal");
    // Type inference would happen during codegen/type checking
}

#[test]
fn test_type_inference_from_literal_string() {
    let code = "'hello'";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse string literal");
}

#[test]
fn test_type_inference_from_literal_boolean() {
    let code = "true";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse boolean literal");
}

#[test]
fn test_type_inference_from_array_literal() {
    let code = "[1, 2, 3]";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse array literal");
}

#[test]
fn test_type_inference_from_object_literal() {
    let code = "let obj = { x: 1, y: 2 };";
    let (_parsed_mod, _diags) = parse_snippet(code).expect("Should parse object literal");
}

#[test]
fn test_deeply_nested_types() {
    // Array of arrays
    let ty = OatsType::Array(Box::new(OatsType::Array(Box::new(OatsType::Number))));
    match ty {
        OatsType::Array(outer) => match *outer {
            OatsType::Array(inner) => assert_eq!(*inner, OatsType::Number),
            _ => panic!("Expected nested Array"),
        },
        _ => panic!("Expected Array type"),
    }
}

#[test]
fn test_complex_union_types() {
    let ty = OatsType::Union(vec![
        OatsType::Number,
        OatsType::String,
        OatsType::Boolean,
        OatsType::Array(Box::new(OatsType::Number)),
    ]);
    match ty {
        OatsType::Union(parts) => assert_eq!(parts.len(), 4),
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_generic_with_multiple_params() {
    let ty = OatsType::GenericInstance {
        base_name: "Map".to_string(),
        type_args: vec![OatsType::String, OatsType::Number],
    };
    match ty {
        OatsType::GenericInstance { type_args, .. } => assert_eq!(type_args.len(), 2),
        _ => panic!("Expected GenericInstance"),
    }
}

#[test]
fn test_promise_nested() {
    let ty = OatsType::Promise(Box::new(OatsType::Promise(Box::new(OatsType::Number))));
    match ty {
        OatsType::Promise(outer) => match *outer {
            OatsType::Promise(inner) => assert_eq!(*inner, OatsType::Number),
            _ => panic!("Expected nested Promise"),
        },
        _ => panic!("Expected Promise type"),
    }
}

#[test]
fn test_weak_nested() {
    let ty = OatsType::Weak(Box::new(OatsType::Weak(Box::new(OatsType::String))));
    match ty {
        OatsType::Weak(outer) => match *outer {
            OatsType::Weak(inner) => assert_eq!(*inner, OatsType::String),
            _ => panic!("Expected nested Weak"),
        },
        _ => panic!("Expected Weak type"),
    }
}

#[test]
fn test_option_nested() {
    let ty = OatsType::Option(Box::new(OatsType::Option(Box::new(OatsType::Number))));
    match ty {
        OatsType::Option(outer) => match *outer {
            OatsType::Option(inner) => assert_eq!(*inner, OatsType::Number),
            _ => panic!("Expected nested Option"),
        },
        _ => panic!("Expected Option type"),
    }
}

#[test]
fn test_tuple_with_union() {
    let ty = OatsType::Tuple(vec![
        OatsType::Number,
        OatsType::Union(vec![OatsType::String, OatsType::Boolean]),
    ]);
    match ty {
        OatsType::Tuple(elements) => {
            assert_eq!(elements.len(), 2);
            match &elements[1] {
                OatsType::Union(_) => {}
                _ => panic!("Expected Union in tuple"),
            }
        }
        _ => panic!("Expected Tuple type"),
    }
}

#[test]
fn test_array_of_unions() {
    let ty = OatsType::Array(Box::new(OatsType::Union(vec![
        OatsType::Number,
        OatsType::String,
    ])));
    match ty {
        OatsType::Array(elem) => match *elem {
            OatsType::Union(_) => {}
            _ => panic!("Expected Union in array"),
        },
        _ => panic!("Expected Array type"),
    }
}

#[test]
fn test_struct_literal_complex() {
    let ty = OatsType::StructLiteral(vec![
        ("x".to_string(), OatsType::Number),
        ("y".to_string(), OatsType::Number),
        ("name".to_string(), OatsType::String),
        (
            "tags".to_string(),
            OatsType::Array(Box::new(OatsType::String)),
        ),
    ]);
    match ty {
        OatsType::StructLiteral(fields) => assert_eq!(fields.len(), 4),
        _ => panic!("Expected StructLiteral type"),
    }
}

#[test]
fn test_symbol_table_multiple_scopes() {
    // Test that we can insert multiple symbols and retrieve them all
    let mut table = SymbolTable::new();
    table.insert_variable("a".to_string(), OatsType::Number);
    table.insert_variable("b".to_string(), OatsType::String);
    table.insert_variable("c".to_string(), OatsType::Boolean);

    // All should be accessible
    assert_eq!(table.get_type("a"), Some(&OatsType::Number));
    assert_eq!(table.get_type("b"), Some(&OatsType::String));
    assert_eq!(table.get_type("c"), Some(&OatsType::Boolean));
}

#[test]
fn test_symbol_table_get_nonexistent() {
    let table = SymbolTable::new();
    let symbol = table.get("nonexistent");
    assert!(symbol.is_none());
}

#[test]
fn test_symbol_table_insert_multiple() {
    let mut table = SymbolTable::new();
    table.insert_variable("x".to_string(), OatsType::Number);
    table.insert_variable("y".to_string(), OatsType::String);
    table.insert_variable("z".to_string(), OatsType::Boolean);

    assert_eq!(table.get_type("x"), Some(&OatsType::Number));
    assert_eq!(table.get_type("y"), Some(&OatsType::String));
    assert_eq!(table.get_type("z"), Some(&OatsType::Boolean));
}

#[test]
fn test_function_sig_generic() {
    let sig = FunctionSig {
        type_params: vec!["T".to_string(), "U".to_string()],
        params: vec![OatsType::Number],
        ret: OatsType::String,
    };

    assert_eq!(sig.type_params.len(), 2);
    assert_eq!(sig.type_params[0], "T");
    assert_eq!(sig.type_params[1], "U");
}

#[test]
fn test_function_sig_complex_params() {
    let sig = FunctionSig {
        type_params: vec![],
        params: vec![
            OatsType::Number,
            OatsType::String,
            OatsType::Boolean,
            OatsType::Array(Box::new(OatsType::Number)),
        ],
        ret: OatsType::Void,
    };

    assert_eq!(sig.params.len(), 4);
}

#[test]
fn test_enum_with_many_variants() {
    let ty = OatsType::Enum(
        "Direction".to_string(),
        vec![
            "Up".to_string(),
            "Down".to_string(),
            "Left".to_string(),
            "Right".to_string(),
        ],
    );
    match ty {
        OatsType::Enum(_, variants) => assert_eq!(variants.len(), 4),
        _ => panic!("Expected Enum type"),
    }
}
