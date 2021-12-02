$1 ~ "forward" {
  hpos += $2
}
$1 ~ "down" {
  vpos += $2
}
$1 ~ "up" {
  vpos -= $2
}
END {
  print vpos * hpos
}
