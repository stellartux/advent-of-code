#!/usr/bin/gawk -f

{ a[i] += $1 }
/^$/ { i++ }
END {
    len = asort(a)
    print a[len]
    print a[len] + a[len - 1] + a[len - 2]
}