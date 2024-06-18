#!/bin/sh

set -eu
filename="${1%.*}"

awk '
BEGIN { print "graph {" }

{
  $1 = substr($1, 0, 3)
  for (i = 2; i <= NF; ++i) {
    # l = ($1 < $2) ? ($1 "/" $2) : ($2 "/" $1)
    print "\t" $1, "--", $i #, "[label=\"" l "\"]"
  }
}

END { print "}" }
' "$1" | dot -Tsvg >"$filename.svg"
