#!/bin/sh
# use sed to convert the input file to Prolog
set -eu

input="${1:-input.txt}"
tempfile=$(mktemp)

sed -E '1i \
:- use_module(library(clpfd)).\
\
solve(A, B) :-
s/NOT/\\/g
s/AND/\/\\/g
s/OR/\\\//g
s/LSHIFT/<</g
s/RSHIFT/>>/g
/-> b$/{h;d}
s/(.*) -> (.*)/    \U\1\E mod 65536 #= \U\2\E,/g
$s/,/.\n/
$p
$x
$s/(.*) -> b/:- solve(PartOne, \1),/
$a \
format("Part One: ~d~n", PartOne),\
solve(PartTwo, PartOne),\
format("Part Two: ~d~n", PartTwo),\
halt.
' "$input" >> "$tempfile"

swipl -s "$tempfile" 2>/dev/null
