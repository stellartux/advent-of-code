input = 240920:789857

part1() = count(validnum, input)

function validnum(n)
    ds = digits(n)
    ns = zip(ds[begin:end-1], ds[begin+1:end])
    all(a >= b for (a, b) in ns) && any(a == b for (a, b) in ns)
end

function validnum2(n)
    ds = digits(n)
    ns = zip(ds[begin:end-1], ds[begin+1:end])
    all(a >= b for (a, b) in ns) && any(count(==(d), ds) == 2 for d in unique(ds))
end

part2() = count(validnum2, input)
