#!/usr/bin/env sh
# Run the AOT pipeline in a temporary directory and optionally clean up
# Usage: ./scripts/run_aot_tempdir.sh [--keep]

KEEP=0
if [ "$1" = "--keep" ]; then
  KEEP=1
fi

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

# shellcheck source=/dev/null
. "$ROOT_DIR/scripts/setup_env.sh" || exit 1

TD=$(mktemp -d -t oats.aot.XXXX)
echo "[oats] using tempdir: $TD"

pushd "$ROOT_DIR" >/dev/null || exit 1

# ensure a clean build of runtime etc
cargo build --quiet || exit 1

# run the aot runner, but set OATS_OUT_DIR so outputs go into the tempdir
OATS_OUT_DIR="$TD" LLVM_SYS_181_PREFIX="$LLVM_SYS_181_PREFIX" cargo run --bin aot_run || exit 1

echo "[oats] artifacts are in $TD"
if [ "$KEEP" -eq 0 ]; then
  echo "[oats] cleaning up $TD"
  rm -rf "$TD"
else
  echo "[oats] kept $TD"
fi

popd >/dev/null || true
