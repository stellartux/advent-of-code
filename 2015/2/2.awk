BEGIN { FS = "x" }

{
    areasum += area($1, $2, $3)
    lengthsum += ribbonlength($1, $2, $3)
}

END { 
    print "Part 1: ", areasum
    print "Part 2: ", lengthsum
}

function area(x, y, z) {
    sides[0] = x * y
    sides[1] = y * z
    sides[2] = x * z
    return 2 * sum(sides) + minimum(sides) 
}

function ribbonlength(x, y, z) {
    dims[0] = x
    dims[1] = y
    dims[2] = z
    return 2 * (sum(dims) - maximum(dims)) + prod(dims)
}

function maximum(xs) {
    max = xs[0]
    for (i in xs) if (xs[i] > max) max = xs[i]
    return max
}

function minimum(xs) {
    min = xs[0]
    for (i in xs) if (xs[i] < min) min = xs[i]
    return min
}

function prod(xs) {
    product = 1
    for (i in xs) product *= xs[i]
    return product
}

function sum(xs) {
    total = 0
    for (i in xs) total += xs[i]
    return total
}
