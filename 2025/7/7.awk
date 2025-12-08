#!/usr/bin/env gawk

BEGIN {
  FS = ""
}

NR == 1 {
  for (i = 1; i <= NF; ++i) {
    if ($i == "S") prev[i] = 1
  }
}

{
  for (i = 1; i <= NF; ++i) {
    if ($i == "^") {
      if (prev[i]) partOne++
      curr[i-1] += prev[i]
      curr[i+1] += prev[i]
    } else {
      curr[i] += prev[i]
    }
  }
  for (i = 1; i <= NF; ++i) {
    prev[i] = curr[i]
    curr[i] = 0
  }
}

END {
  print +partOne
  for (k in prev) partTwo += prev[k]
  print +partTwo
}
