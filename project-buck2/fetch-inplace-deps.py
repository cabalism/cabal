#!/usr/bin/env python3
"""Copy the sources of Hackage packages that cabal builds in-place into
project-buck2/vendor/<package>/src, so that the BUCK file kept there can
build them.

A Hackage package ends up built in-place (rather than in the cabal store)
when it depends on a package from this repository: currently that is
hackage-security, which depends on Cabal-syntax. buck2/gen-haskell-prebuilt.py
can only reference store packages, so these are built from source instead.

Run after `cabal build all --only-dependencies --enable-tests`, which
unpacks the sources under dist-newstyle/src/.
"""

import json
import os
import shutil
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PLAN = os.path.join(ROOT, "dist-newstyle", "cache", "plan.json")
VENDOR = os.path.join(ROOT, "project-buck2", "vendor")


def main():
    if not os.path.exists(PLAN):
        print(f"ERROR: {PLAN} not found - run "
              "'cabal build all --only-dependencies --enable-tests' first",
              file=sys.stderr)
        sys.exit(1)
    with open(PLAN) as f:
        plan = json.load(f)

    inplace = {p["pkg-name"]: p for p in plan["install-plan"]
               if p.get("style") == "inplace"}
    if not inplace:
        print("No in-place packages in the cabal plan.")
        return

    failed = False
    for name, pkg in sorted(inplace.items()):
        pkg_id = f"{name}-{pkg['pkg-version']}"
        src = os.path.join(ROOT, "dist-newstyle", "src", pkg_id)
        dest_dir = os.path.join(VENDOR, name)
        if not os.path.isdir(os.path.join(dest_dir)) or \
                not os.path.exists(os.path.join(dest_dir, "BUCK")):
            print(f"WARNING: {pkg_id} is built in-place by cabal but "
                  f"{dest_dir}/BUCK does not exist - add one", file=sys.stderr)
            failed = True
            continue
        if not os.path.isdir(src):
            print(f"ERROR: {src} not found - run "
                  "'cabal build all --only-dependencies --enable-tests' first",
                  file=sys.stderr)
            failed = True
            continue
        dest = os.path.join(dest_dir, "src")
        if os.path.exists(dest):
            shutil.rmtree(dest)
        shutil.copytree(os.path.join(src, "src"), dest)
        print(f"  {pkg_id}: copied {src}/src -> {dest}")

    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
