#!/bin/sh
# Apply the local fixes to the haskell-buck2 submodule (buck2/) kept in
# project-buck2/patches/. Run after `git submodule update --init buck2`.
# Idempotent: already-applied patches are skipped.
set -eu
cd "$(dirname "$0")/.."
for patch in project-buck2/patches/*.patch; do
    if git -C buck2 apply --check --reverse "../$patch" 2>/dev/null; then
        echo "already applied: $patch"
    else
        git -C buck2 apply "../$patch"
        echo "applied: $patch"
    fi
done
