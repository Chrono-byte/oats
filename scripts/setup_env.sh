#!/usr/bin/env bash
# Bash/Zsh-compatible helper to locate LLVM 18 and export LLVM_SYS_181_PREFIX.
# This file is intended to be sourced. Example:
#   . ./scripts/setup_env.sh
#
# If the file is executed directly, it will still attempt to set/print values
# but note exported variables won't persist to the caller's environment.

# --- Sourcing Detection ---
_is_sourced=0
# Detect if script is being sourced (bash/zsh compatible check)
if [ "${BASH_SOURCE+set}" = set ] && [ "$0" != "${BASH_SOURCE[0]}" ]; then
  _is_sourced=1
fi

# --- Usage Function ---
usage() {
  cat <<'EOF'
Usage: source ./scripts/setup_env.sh

This script attempts to detect an LLVM 18 installation and set
LLVM_SYS_181_PREFIX and LD_LIBRARY_PATH. It is meant to be sourced so the
environment variables remain in your shell.
EOF
}

if [ "${1-}" = "-h" ] || [ "${1-}" = "--help" ]; then
  usage
  if [ $_is_sourced -eq 0 ]; then exit 0; else return 0; fi
fi

# --- Core Logic ---

# If the user already set LLVM_SYS_181_PREFIX, trust it and do nothing more.
if [ -n "${LLVM_SYS_181_PREFIX-}" ]; then
  echo "[oats] LLVM_SYS_181_PREFIX is already set to: ${LLVM_SYS_181_PREFIX}"
else
  # Candidate prefixes to search for LLVM 18 (as a space-separated string for portability)
  _candidates="/usr/lib64/llvm18 /usr/lib/llvm18 /usr/lib/llvm-18 /usr/lib64/llvm-18 /usr/local/opt/llvm@18 /opt/llvm18"

  _found=""
  for _p in $_candidates; do
    if [ -d "$_p" ]; then
      _found="$_p"
      break
    fi
  done

  if [ -z "$_found" ]; then
    echo "[oats] Could not auto-detect LLVM 18 installation." >&2
    echo "[oats] Please set LLVM_SYS_181_PREFIX to your LLVM 18 prefix, e.g.:" >&2
    echo "    export LLVM_SYS_181_PREFIX=/usr/lib64/llvm18" >&2
    if [ $_is_sourced -eq 0 ]; then
      exit 1
    else
      return 1
    fi
  fi

  export LLVM_SYS_181_PREFIX="$_found"
  echo "[oats] LLVM_SYS_181_PREFIX set to: ${LLVM_SYS_181_PREFIX}"
fi

# Prepend LLVM lib path to LD_LIBRARY_PATH, handling if the variable is already set or not.
if [ -n "${LD_LIBRARY_PATH-}" ]; then
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib:${LD_LIBRARY_PATH}"
else
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib"
fi
echo "[oats] LD_LIBRARY_PATH updated to include: ${LLVM_SYS_181_PREFIX}/lib"

if [ $_is_sourced -eq 0 ]; then
  # When executed directly, remind user to source instead
  echo "[oats] Note: run 'source ./scripts/setup_env.sh' to export vars into your shell"
fi