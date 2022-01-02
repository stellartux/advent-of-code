# part 1
{ sum += istriangle($1, $2, $3) }

# part 2
NR % 3 == 1 { a[1] = $1; a[2] = $2; a[3] = $3 }
NR % 3 == 2 { b[1] = $1; b[2] = $2; b[3] = $3 }
NR % 3 == 0 {
  sum2 += istriangle(a[1], b[1], $1) + \
    istriangle(a[2], b[2], $2) + \
    istriangle(a[3], b[3], $3)
}

END { 
  print "Part 1: " sum
  print "Part 2: " sum2
}

function istriangle(a, b, c) {
  return a + b > c && a + c > b && b + c > a
}
