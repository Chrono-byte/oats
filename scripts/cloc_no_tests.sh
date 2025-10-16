#!/usr/bin/env bash
set -euo pipefail

# cloc_no_tests.sh
# Run cloc for the repository while excluding any directories named `tests` and specific files.
# Usage: ./scripts/cloc_no_tests.sh [--extra-args "--by-file --include-lang=Rust"] [--exclude-files "file1,file2"]

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"

EXTRA_ARGS=""
EXCLUDE_FILES=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --extra-args)
      shift
      EXTRA_ARGS="$1"
      shift
      ;;
    --exclude-files)
      shift
      EXCLUDE_FILES="$1"
      shift
      ;;
    -h|--help)
      echo "Usage: $0 [--extra-args \"<additional cloc args>\"] [--exclude-files \"<comma-separated file list>\"]"
      echo
      echo "Runs cloc on the repository root, excluding directories named 'tests' and specific files."
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

echo "Running cloc on: $REPO_ROOT (excluding directories named 'tests' and specified files)"
cd "$REPO_ROOT"

# Default exclude list: common non-code or build directories
DEFAULT_EXCLUDES=(tests target aot_out examples tmp .git build dist fuzz docs scripts .venv .vscode third_party)

# Join excludes into a comma-separated list
EXCLUDE_DIRS=$(IFS=','; echo "${DEFAULT_EXCLUDES[*]}")

echo "Excluding directories: $EXCLUDE_DIRS"
if [[ -n "$EXCLUDE_FILES" ]]; then
  echo "Excluding files: $EXCLUDE_FILES"
  cloc --exclude-dir="$EXCLUDE_DIRS" --exclude-list-file=<(echo "$EXCLUDE_FILES" | tr ',' '\n') ${EXTRA_ARGS} .
else
  cloc --exclude-dir="$EXCLUDE_DIRS" ${EXTRA_ARGS} .
fi
