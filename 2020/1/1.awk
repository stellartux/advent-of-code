#!/bin/awk

t[2020 - $1] {
  print $1 * (2020 - $1)
}

{ t[$1] = 1 }

END {
  for (k1 in t) for (k2 in t) {
    if (t[2020 - k1 - k2]) {
      print k1 * k2 * (2020 - k1 - k2)
      exit
    }
  }
}
