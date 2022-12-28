BEGIN { ORS = ""; RS = "[[:space:]]+"; x = 1 }
NR % 40 == 20 { total += NR * x }
{ print(abs(x - ((NR - 1) % 40)) < 2 ? "#" : "."); x += $0 }
NR % 40 == 0 { print "\n" }
END { print total "\n" }
function abs(d) { return d < 0 ? -d : d }
