BEGIN { i = 0 }
{ a[i] += $1 }
/^$/ {
    i++
    next
}
END {
    asort(a)
    print a[length(a)]
    print a[length(a)] + a[length(a) - 1] + a[length(a) - 2]
}