use anyhow::Result;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use inkwell::values::BasicValue;
use oats::codegen::CodeGen;

#[test]
fn test_heap_alloc_with_ptr_fields_weak_emits_rc_weak_inc() -> Result<()> {
    let context = Context::create();
    let module = context.create_module("oats_test");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();

    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        next_str_id: std::cell::Cell::new(0),
        string_literals: std::cell::RefCell::new(std::collections::HashMap::new()),
        f64_t: context.f64_type(),
        i64_t: context.i64_type(),
        i32_t: context.i32_type(),
        bool_t: context.bool_type(),
        i8ptr_t: context.ptr_type(inkwell::AddressSpace::default()),
        fn_print_f64: std::cell::RefCell::new(None),
        fn_print_str: std::cell::RefCell::new(None),
        fn_malloc: std::cell::RefCell::new(None),
        fn_memcpy: std::cell::RefCell::new(None),
        fn_strlen: std::cell::RefCell::new(None),
        fn_free: std::cell::RefCell::new(None),
        fn_array_alloc: std::cell::RefCell::new(None),
        fn_rc_inc: std::cell::RefCell::new(None),
        fn_rc_dec: std::cell::RefCell::new(None),
        fn_number_to_string: std::cell::RefCell::new(None),
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        current_class_parent: std::cell::RefCell::new(None),
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        async_await_counter: std::cell::Cell::new(0),
        async_await_live_sets: std::cell::RefCell::new(None),
        async_cont_blocks: std::cell::RefCell::new(None),
        async_local_name_to_slot: std::cell::RefCell::new(None),
        async_param_count: std::cell::Cell::new(0),
        async_poll_function: std::cell::RefCell::new(None),
        async_resume_blocks: std::cell::RefCell::new(None),
        async_poll_locals: std::cell::RefCell::new(None),
        source: "",
    };

    // create a dummy function to act as context for builder position
    let fn_ty = codegen.f64_t.fn_type(&[], false);
    let f = codegen.module.add_function("test_fn", fn_ty, None);
    let entry = codegen.context.append_basic_block(f, "entry");
    codegen.builder.position_at_end(entry);

    // ensure malloc and rc_weak_inc declarations exist
    codegen.declare_libc();
    let null_ptr = codegen.i8ptr_t.const_null().as_basic_value_enum();
    let args = [(null_ptr, true)];
    let _ = codegen
        .heap_alloc_with_ptr_fields(&args)
        .map_err(|d| anyhow::anyhow!(d.message))?;

    let ir = codegen.module.print_to_string().to_string();
    assert!(ir.contains("rc_weak_inc"), "IR should contain rc_weak_inc");
    Ok(())
}
