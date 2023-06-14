$1 ~ "forward" {
  hpos += $2
  vpos2 += vpos * hpos
}
$1 ~ "down" {
  vpos += $2
}
$1 ~ "up" {
  vpos -= $2
}
END {
  print vpos * hpos
  print vpos2 * hpos
}
