# Build-mode aliases for the root PACKAGE file.
#
# `buck2 build //... -m opt` resolves `opt` through BUILD_MODE_ALIASES to the
# constraint value declared in buck2/constraints/BUCK. `dev` is the default.

_CONSTRAINTS = "root//buck2/constraints:"

BUILD_MODE_ALIASES = struct(
    dev = _CONSTRAINTS + "dev",
    opt = _CONSTRAINTS + "opt",
    prof = _CONSTRAINTS + "prof",
    asan = _CONSTRAINTS + "asan",
)

DEFAULT_MODIFIERS = [
    _CONSTRAINTS + "dev",
]
