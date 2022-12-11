BEGIN { ORS = ""; RS = "[[:space:]]+"; x = 1 }
{ print((d = x - ((NR - 1) % 40)) < 2 && d > -2 ? "#" : ".") }
NR % 40 == 0 { print "\n" }
NR % 40 == 20 { total += NR * x }
/-?[[:digit:]]+/ { x += $0 }
END { print total "\n" }
