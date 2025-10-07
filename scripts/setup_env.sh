#!/usr/bin/env bash
# POSIX-compatible helper to locate LLVM 18 and export LLVM_SYS_181_PREFIX.
# This file is intended to be sourced. Example:
#   . ./scripts/setup_env.sh
#
# If the file is executed directly, it will still attempt to set/print values
# but note exported variables won't persist to the caller's environment.

# Do NOT enable 'nounset' here. This script is intended to be sourced and
# must not change the caller's shell options (enabling 'set -u' here would
# enable nounset in the user's interactive shell and can break it). Keep
# expansions in this file defensive (use "${var-}") instead.

_is_sourced=0
# Detect if script is being sourced (bash/ksh/zsh compatible check)
if [ "${BASH_SOURCE+set}" = set ] && [ "$0" != "${BASH_SOURCE[0]}" ]; then
  _is_sourced=1
fi

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

### Preserve caller 'nounset' option and enable 'set -u' locally
# When this file is sourced we want to be able to perform strict checks
# internally without leaking 'set -u' into the caller's shell. We save the
# caller's nounset state, enable nounset for the remainder of the script, and
# restore the caller's state when the script returns.
_oats__save_nounset_state() {
  case "$-" in *u*) _oats__nounset_was_set=1 ;; *) _oats__nounset_was_set=0 ;; esac
}
_oats__restore_nounset_state() {
  if [ "${_oats__nounset_was_set:-0}" -eq 0 ]; then
    set +u
  else
    set -u
  fi
  unset _oats__nounset_was_set
  trap - RETURN
}

# If sourced, arrange to restore options when this file returns
_oats__save_nounset_state
if [ $_is_sourced -eq 1 ]; then
  trap _oats__restore_nounset_state RETURN
fi

# Enable strict mode for internal checks only
set -u

# If the user already set LLVM_SYS_181_PREFIX, trust it
if [ -n "${LLVM_SYS_181_PREFIX-}" ]; then
  _found="${LLVM_SYS_181_PREFIX}"
else
  # Candidate prefixes to search for LLVM 18
  _candidates=(/usr/lib64/llvm18 /usr/lib/llvm18 /usr/lib/llvm-18 /usr/lib64/llvm-18 /usr/local/opt/llvm@18 /opt/llvm18)

  _found=""
  for _p in "${_candidates[@]}"; do
    if [ -d "$_p" ]; then
      _found="$_p"
      break
    fi
  done

  if [ -z "$_found" ]; then
    echo "[oats] Could not auto-detect LLVM 18 installation." 1>&2
    echo "[oats] Please set LLVM_SYS_181_PREFIX to your LLVM 18 prefix, e.g.:" 1>&2
    echo "    export LLVM_SYS_181_PREFIX=/usr/lib64/llvm18" 1>&2
    if [ $_is_sourced -eq 0 ]; then
      exit 1
    else
      return 1
    fi
  fi

  export LLVM_SYS_181_PREFIX="$_found"
fi

if [ -n "${LD_LIBRARY_PATH-}" ]; then
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib:${LD_LIBRARY_PATH}"
else
  export LD_LIBRARY_PATH="${LLVM_SYS_181_PREFIX}/lib"
fi

echo "[oats] LLVM_SYS_181_PREFIX set to: ${LLVM_SYS_181_PREFIX}"
echo "[oats] LD_LIBRARY_PATH updated to include: ${LLVM_SYS_181_PREFIX}/lib"

if [ $_is_sourced -eq 0 ]; then
  # When executed directly, remind user to source instead
  echo "[oats] Note: run 'source ./scripts/setup_env.sh' to export vars into your shell"
fi
