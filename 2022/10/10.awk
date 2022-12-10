BEGIN { RS = "[ \n]+"; x = 1 }
{ i++ }
(i - 20) % 40 == 0 { total += i * x }
/-?[[:digit:]]+/ { x += $1 }
END { print total }
