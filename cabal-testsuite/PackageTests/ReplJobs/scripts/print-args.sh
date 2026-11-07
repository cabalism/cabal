#!/bin/sh
# Stands in for GHCi via --with-repl: prints each argument it was given on a
# line of its own, prefixed with "repl-arg:", expanding any @response-file so
# the flags inside it are visible. Used by the ReplJobs test to check that -j
# reaches the interpreter.
for a in "$@"; do
  case $a in
    @*) sed 's/^/repl-arg: /' "${a#@}" ;;
    *) printf 'repl-arg: %s\n' "$a" ;;
  esac
done
