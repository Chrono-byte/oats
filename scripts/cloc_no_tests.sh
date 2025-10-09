#!/usr/bin/env bash
set -euo pipefail

# cloc_no_tests.sh
# Run cloc for the repository while excluding any directories named `tests`.
# Usage: ./scripts/cloc_no_tests.sh [--extra-args "--by-file --include-lang=Rust"]

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

EXTRA_ARGS=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --extra-args)
      shift
      EXTRA_ARGS="$1"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [--extra-args \"<additional cloc args>\"]"
      echo
      echo "Runs cloc on the repository root and excludes any directories named 'tests'."
      exit 0
      ;;
    *)
      echo "Unknown arg: $1" >&2
      exit 2
      ;;
  esac
done

if ! command -v cloc >/dev/null 2>&1; then
  echo "Error: cloc not found. Install it (e.g. 'sudo apt install cloc' or 'brew install cloc')" >&2
  exit 1
fi

echo "Running cloc on: $REPO_ROOT (excluding directories named 'tests')"
cd "$REPO_ROOT"

# Default exclude list: common non-code or build directories
DEFAULT_EXCLUDES=(tests target aot_out examples tmp .git build dist)

# Join excludes into a comma-separated list
IFS=',' read -r -a _ <<< "${DEFAULT_EXCLUDES[*]}"
EXCLUDE_LIST=$(printf ",%s" "${DEFAULT_EXCLUDES[@]}")
EXCLUDE_LIST=${EXCLUDE_LIST:1}

echo "Excluding directories: $EXCLUDE_LIST"
cloc --exclude-dir="$EXCLUDE_LIST" ${EXTRA_ARGS} .
