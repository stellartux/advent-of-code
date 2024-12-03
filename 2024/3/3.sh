#!/bin/sh
# usage: sh 3.sh FILENAME
set -eu

if [ $# -ne 1 ]; then
    echo "usage: sh 3.sh FILENAME"
    exit 1
elif [ ! -r "$1" ]; then
    echo "File not found."
    exit 1
fi

solve() {
    grep -Po "$1" "$2" |\
        tr ',()' '   ' |\
        awk '
            BEGIN { d = 1 }
            /^do/ { d = 1 }
            /^don'\''t/ { d = 0 }
            /^mul/ { total += $2 * $3 * d }
            END { print +total }'
}

solve 'mul\(\d{1,3},\d{1,3}\)' "$1"
solve 'mul\(\d{1,3},\d{1,3}\)|do(n'\''t)?\(\)' "$1"
