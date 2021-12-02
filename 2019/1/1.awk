{ 
  part1 += fuel($1)
  for(f = fuel($1); f > 0; f = fuel(f)) {
    part2 += f
  }
}
END { 
  print "Part 1: ", part1
  print "Part 2: ", part2
}

function fuel(x) {
  return int(x / 3) - 2
}
