use oats::tests::common::gen_ir_for_source;

#[test]
fn generics_emits_specializations_and_rc_dec() {
    let src = std::fs::read_to_string("examples/proper_tests/generics.oats").expect("read example");
    let ir = gen_ir_for_source(&src).expect("generate IR");

    // Expect two distinct specializations for getFirstElement (number and string)
    assert!(ir.contains("getFirstElement_mono_"), "specialization names missing");

    // Expect rc_dec after number_to_string in the template literal lowering
    assert!(ir.contains("number_to_string") && ir.contains("rc_dec"), "rc_dec or number_to_string missing");
}
