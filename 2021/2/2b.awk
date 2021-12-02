$1 ~ "forward" {
  hpos += $2
  vpos += aim * $2
}
$1 ~ "down" {
  aim += $2
}
$1 ~ "up" {
  aim -= $2
}
END {
  print vpos * hpos
}
