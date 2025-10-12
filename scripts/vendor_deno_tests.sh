#!/usr/bin/env bash
set -euo pipefail

# Usage: ./scripts/vendor_deno_tests.sh <deno_commit_sha>
#
# Performs a sparse checkout of the Deno repository and copies the files listed
# in third_party/deno_tests/allowlist.txt into third_party/deno_tests/ while
# preserving their relative paths.
#
# This script assumes `git` is available and runs in a clean working tree.
# It performs the following steps:
#   1. Clones the Deno repository into a temporary directory.
#   2. Uses sparse checkout to fetch only the allow-listed test files.
#   3. Copies those files into third_party/deno_tests/ while preserving layout.
#   4. Updates the pinned commit in README.md.

if ! command -v git >/dev/null 2>&1; then
  echo "git is required to vendor Deno tests" >&2
  exit 1
fi

if [ "$#" -ne 1 ]; then
  echo "expected a Deno commit SHA" >&2
  exit 1
fi

DENO_SHA="$1"
ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
ALLOWLIST="$ROOT_DIR/third_party/deno_tests/allowlist.txt"
DEST_DIR="$ROOT_DIR/third_party/deno_tests"
TMP_DIR="$(mktemp -d)"

trap 'rm -rf "$TMP_DIR"' EXIT

echo "Preparing to vendor Deno tests from commit $DENO_SHA"

if [ ! -f "$ALLOWLIST" ]; then
  echo "missing allow-list: $ALLOWLIST" >&2
  exit 1
fi

mapfile -t ALLOWED < <(grep -Ev '^(#|\s*$)' "$ALLOWLIST")

if [ "${#ALLOWED[@]}" -eq 0 ]; then
  echo "allow-list is empty; nothing to vendor" >&2
  exit 1
fi

REPO_DIR="$TMP_DIR/deno"
echo "Cloning Deno repository..."
git clone --filter=blob:none --quiet --no-checkout https://github.com/denoland/deno.git "$REPO_DIR"

pushd "$REPO_DIR" >/dev/null
git config core.sparseCheckout true
git sparse-checkout init --cone >/dev/null

SPARSE_PATHS=("LICENSE.md")
for entry in "${ALLOWED[@]}"; do
  SPARSE_PATHS+=("tests/$entry")
done

git sparse-checkout set "${SPARSE_PATHS[@]}" >/dev/null
git fetch --quiet origin "$DENO_SHA"
git checkout --quiet "$DENO_SHA"
popd >/dev/null

echo "Cleaning destination directory..."
shopt -s dotglob
for path in "$DEST_DIR"/*; do
  name="$(basename "$path")"
  if [ "$name" = "README.md" ] || [ "$name" = "allowlist.txt" ]; then
    continue
  fi
  rm -rf "$path"
done
shopt -u dotglob

echo "Copying allow-listed files..."
for entry in "${ALLOWED[@]}"; do
  src="$REPO_DIR/tests/$entry"
  dest="$DEST_DIR/$entry"
  if [ ! -e "$src" ]; then
    echo "warning: missing $entry in commit $DENO_SHA" >&2
    continue
  fi
  mkdir -p "$(dirname "$dest")"
  cp -R "$src" "$dest"
done

LICENSE_SRC="$REPO_DIR/LICENSE.md"
if [ -f "$LICENSE_SRC" ]; then
  cp "$LICENSE_SRC" "$DEST_DIR/LICENSE.md"
fi

if [ -f "$DEST_DIR/README.md" ]; then
  if ! sed -i "s/^- Pinned commit: .*/- Pinned commit: $DENO_SHA/" "$DEST_DIR/README.md"; then
    echo "warning: unable to update pinned commit line" >&2
  fi
else
  cat <<EOF >"$DEST_DIR/README.md"
Deno tests vendored for the Oats compiler live in this directory.

- Upstream repo: https://github.com/denoland/deno
- License: MIT / Apache-2.0 (preserve headers when updating)
- Pinned commit: $DENO_SHA

Files in this directory are managed by scripts/vendor_deno_tests.sh. Aside from
license adjustments, manual edits should be avoided; update the allow-list and
rerun the script instead.
EOF
fi

echo "Vendored ${#ALLOWED[@]} entries from Deno commit $DENO_SHA"
echo "Please review the copied files and commit the changes."
