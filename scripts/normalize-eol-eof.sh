#!/bin/bash
# SEE: https://unix.stackexchange.com/questions/31947/how-to-add-a-newline-to-the-end-of-a-file
for f in $(git grep -Il '' -- "$1"); do
    echo "$f";
    tail -c1 < "$f" | read -r _ || echo >> "$f";
done
