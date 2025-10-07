#!/usr/bin/env bash
set -euo pipefail
# Run the AOT pipeline using a persistent output dir in the repo root.
# Usage: ./scripts/run_aot_tempdir.sh [--keep] | [-h|--help]
#
# By default this script leaves artifacts in ./aot_out. Pass --keep to keep the
# same behaviour (keeps artifacts). This flag is present for clarity and future
# extension; the script does not delete artifacts by default.

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT_DIR=$(cd "$SCRIPT_DIR/.." && pwd)

KEEP=true
while [ "$#" -gt 0 ]; do
	case "$1" in
		--keep)
			KEEP=true; shift;;
		-h|--help)
			echo "Usage: $0 [--keep]"; exit 0;;
		*)
			echo "Unknown argument: $1" >&2; echo "Usage: $0 [--keep]"; exit 2;;
	esac
done

# Source environment helper (must exist when working in repo)
if [ -f "$ROOT_DIR/scripts/setup_env.sh" ]; then
	# shellcheck source=/dev/null
	. "$ROOT_DIR/scripts/setup_env.sh"
else
	echo "[oats] missing setup_env.sh; please run from repository root" >&2
	exit 1
fi

OUTDIR="$ROOT_DIR/aot_out"
mkdir -p "$OUTDIR"

# Run build + aot runner in a subshell so we don't need pushd/popd (POSIX
# shells often don't provide pushd/popd).  Keep output directory persistent by
# default.
(
	cd "$ROOT_DIR" || exit 1
	# Build the workspace (prefer building the oats package explicitly)
	cargo build -p oats --quiet || exit 1

	EXAMPLE_SRC="$ROOT_DIR/examples/add.oats"
	# Pass detected LLVM prefix if set, and point aot runner at the out dir
	LLVM_SYS_181_PREFIX="${LLVM_SYS_181_PREFIX:-}" \
		OATS_OUT_DIR="$OUTDIR" \
		OATS_SRC_FILE="$EXAMPLE_SRC" \
		cargo run -p oats --bin aot_run -- "$EXAMPLE_SRC" || exit 1

	echo "[oats] artifacts are in $OUTDIR"
)

if [ "$KEEP" != true ]; then
	# Placeholder for future cleanup behaviour; currently we keep artifacts.
	:
fi
