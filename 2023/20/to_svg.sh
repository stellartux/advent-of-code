#!/bin/sh

set -eu
filename="${1%.*}"

awk '
BEGIN { print "digraph {" }

/[%&]/ {
    type = substr($1, 0, 1) == "%" ? "flipflop" : "conjunction"
    gsub(/[%&]/, "", $0)
    gsub(/[%&]/, "", $1)
    print $1, "[class=" type "]", "[xlabel=" type "]"
}

{ print }

END { print "}\n" }
' "$1" | dot -Tsvg >"$filename.svg"
