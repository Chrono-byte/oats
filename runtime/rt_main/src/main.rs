// Small host object that calls a uniform entrypoint emitted by the AOT
// generator. The host does not need to know the script's function
// signature; it simply calls `oats_entry()` which the AOT module provides as
// a void, no-argument wrapper around whatever `oats_main` the user exported.

extern "C" {
    // oats_entry is emitted by the codegen when a host main is desired.
    // It now has signature `int oats_entry(void)` and returns a numeric
    // exit code when the script's main returns a Number. The host simply
    // exits with that code.
    fn oats_entry() -> i32;
}

fn main() {
    // Call the script entrypoint and exit with its returned code.
    let code = unsafe { oats_entry() };
    std::process::exit(code);
}
