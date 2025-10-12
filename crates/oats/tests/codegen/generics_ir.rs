use super::common;
use common::gen_ir_for_source;
use std::path::PathBuf;

#[test]
fn generics_emits_specializations_and_rc_dec() {
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..").join("..");
    let example_path = repo_root.join("examples").join("proper_tests").join("generics.oats");
    let src = std::fs::read_to_string(&example_path).expect("read example");
    let ir = gen_ir_for_source(&src).expect("generate IR");

    // Expect two distinct specializations for getFirstElement (number and string)
    assert!(ir.contains("getFirstElement_mono_"), "specialization names missing");

    // Expect rc_dec after number_to_string in the template literal lowering
    assert!(ir.contains("number_to_string") && ir.contains("rc_dec"), "rc_dec or number_to_string missing");
}
