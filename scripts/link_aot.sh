echo "Building runtime staticlib (${MODE})..."
echo "Linking $AOT_OBJ + $STATICLIB -> $OUT_EXE"
echo "Created $OUT_EXE"
#!/usr/bin/env bash
set -euo pipefail

# Usage: link_aot.sh <path-to-aot-object.o> <output-exe> [--release]
#
# Builds the runtime staticlib and links it with the provided AOT object to
# produce a host executable that calls oats_entry directly at link time.

if [ "$#" -lt 2 ]; then
  echo "Usage: $0 <aot_object.o> <output-exe> [--release]" >&2
  exit 2
fi

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# Runtime has been moved into the workspace under crates/runtime
RUNTIME_DIR="$SCRIPT_DIR/.."/crates/runtime

AOT_OBJ="$1"
OUT_EXE="$2"
MODE="debug"
if [ "${3-}" = "--release" ]; then
  MODE="release"
fi

if [ ! -f "$AOT_OBJ" ]; then
  echo "AOT object not found: $AOT_OBJ" >&2
  exit 2
fi

echo "[oats] Building runtime staticlib (${MODE})..."
if [ "$MODE" = "release" ]; then
  # Use workspace-aware invocation to build the runtime crate
  cargo build -p runtime --release
else
  cargo build -p runtime
fi

STATICLIB="$SCRIPT_DIR/../crates/runtime/target/$MODE/libruntime.a"
if [ ! -f "$STATICLIB" ]; then
  echo "runtime staticlib not found at $STATICLIB" >&2
  exit 3
fi

echo "[oats] Linking $AOT_OBJ + $STATICLIB -> $OUT_EXE"

# Prefer clang if available (better LLVM compatibility), fall back to cc
if command -v clang >/dev/null 2>&1; then
  CC=clang
else
  CC=cc
fi

"$CC" -o "$OUT_EXE" "$AOT_OBJ" "$STATICLIB" -lm -ldl -lpthread -lc

echo "[oats] Created $OUT_EXE"
