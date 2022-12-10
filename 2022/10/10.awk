BEGIN { ORS = ""; RS = "[[:space:]]+"; x = 1 }
{ print((d = x - (i++ % 40)) < 2 && d > -2 ? "#" : ".") }
i % 40 == 0 { print "\n" }
i % 40 == 20 { total += i * x }
/-?[[:digit:]]+/ { x += $0 }
END { print total "\n" }
