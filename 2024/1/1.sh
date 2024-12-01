#!/bin/sh
# usage: sh 1.sh FILENAME
set -u

if [ $# -ne 1 ]; then
    echo "usage: sh 1.sh FILENAME"
    exit 1
elif [ ! -r "$1" ]; then
    echo "File not found."
    exit 1
fi

left=$(mktemp -q)
right=$(mktemp -q)

awk '{print $1}' "$1" | sort -n >"$left"
awk '{print $2}' "$1" | sort -n >"$right"

paste "$left" "$right" | awk '
    { s += $1 > $2 ? $1 - $2 : $2 - $1 }
    END { print +s }
'

awk "
    $(uniq -c "$right" | awk '
        BEGIN { print "BEGIN {" }
        { print "counts["$2"]="$1 }
        END { print "}" }
    ')
    { s += counts[\$1] * \$1 }
    END { print +s }
" "$left"

rm "$left" "$right"
