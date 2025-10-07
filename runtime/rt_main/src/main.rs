// Small host object that calls a uniform entrypoint emitted by the AOT
// generator. The host does not need to know the script's function
// signature; it simply calls `oats_entry()` which the AOT module provides as
// a void, no-argument wrapper around whatever `oats_main` the user exported.

extern "C" {
    // oats_entry is emitted by the codegen when a host main is desired. It
    // has signature `void oats_entry(void)` and is a stable call target for
    // the host object.
    fn oats_entry();
}

fn main() {
    // Call the script entrypoint and exit. Keep the host code minimal and
    // agnostic about script signatures.
    unsafe { oats_entry() };
}
