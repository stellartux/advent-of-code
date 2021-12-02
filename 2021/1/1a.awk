NR == 1 { x = $1 }
NR != 1 {
  if ($1 > x) {
    sum++
  }
  x = $1
}
END { print sum }
