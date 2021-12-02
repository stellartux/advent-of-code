NR <= 3 {
  window += $1
  a = b
  b = c
  c = $1
}
NR > 3 {
  prevwindow = window
  window -= a
  window += $1
  a = b
  b = c
  c = $1
  if (window > prevwindow) {
    sum++
  }
}
END { print sum }
