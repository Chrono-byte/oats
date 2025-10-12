# Toasty CLI

Toasty is a small CLI wrapper around the Oats AOT compiler. It provides a
`build` subcommand (like `cargo build`) and a `run` subcommand (like `cargo run`).

Examples:

Build in debug mode:

```bash
cargo run -p toasty -- build examples/add.oats
```

Build in release mode:

```bash
cargo run -p toasty -- build --release examples/add.oats
```

Emit object only (skip linking):

```bash
cargo run -p toasty -- build --emit-object-only examples/add.oats
```

Run the compiled program:

```bash
cargo run -p toasty -- run examples/add.oats -- arg1 arg2
```
