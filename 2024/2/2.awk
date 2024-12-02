#!/bin/awk
# usage: awk -f 2.awk FILENAME

function setDirection(low, high) {
  if ($low > $high) {
    min = 1
    max = 3
  } else {
    min = -3
    max = -1
  }
}

{
  setDirection(1, 2)
  score = 1
  for (i = 1; i < NF; ++i) {
    x = $i - $(i+1)
    if (x < min || x > max) {
      score = 0
      break
    }
  }
  partOne += score

  if (score == 0) {
    for (skip = 1; skip <= NF; ++skip) {
      setDirection(skip == 1 ? 2 : 1, skip <= 2 ? 3 : 2)
      score = 1
      for (i = 1; i < NF; ++i) {
        if (i != skip) {
          j = i + 1
          if (j == skip) ++j
          if (j > NF) break
          x = $i - $j
          if (x < min || x > max) {
            score = 0
            break
          }
        }
      }
      if (score) break
    }
  }
  partTwo += score
}

END {
  print +partOne
  print +partTwo
}
