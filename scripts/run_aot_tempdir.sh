#!/usr/bin/env sh
# Run the AOT pipeline in a temporary directory and optionally clean up
# Usage: ./scripts/run_aot_tempdir.sh [--keep]

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

# shellcheck source=/dev/null
. "$ROOT_DIR/scripts/setup_env.sh" || exit 1

# Use a persistent output directory in the repo root
OUTDIR="$ROOT_DIR/aot_out"
mkdir -p "$OUTDIR"

pushd "$ROOT_DIR" >/dev/null || exit 1

# ensure a clean build of runtime etc
cargo build --quiet || exit 1

# run the aot runner, providing the example source file path and setting OATS_OUT_DIR
EXAMPLE_SRC="$ROOT_DIR/examples/add.oats"
OATS_OUT_DIR="$OUTDIR" OATS_SRC_FILE="$EXAMPLE_SRC" LLVM_SYS_181_PREFIX="$LLVM_SYS_181_PREFIX" cargo run --bin aot_run -- "$EXAMPLE_SRC" || exit 1

echo "[oats] artifacts are in $OUTDIR"

popd >/dev/null || true
