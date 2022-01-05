if basename(pwd()) == "aoc"
    cd("2018/2")
end
using StatsBase: countmap

function part1(filename::AbstractString)
    hastwo = 0
    hasthree = 0
    for line in eachline(filename)
        counts = values(countmap(line))
        hastwo += any(==(2), values(counts))
        hasthree += any(==(3), values(counts))
    end
    hastwo * hasthree
end
@assert part1("example.txt") == 12
part1("input.txt")

function part2(filename::AbstractString)
    lines = readlines(filename)
    pairs = [(l, r) for (l, r) in Iterators.product(lines, lines) if l != r]
    l, r = pairs[findmin(hammingdistance, pairs)[2]]
    join(a for (a, b) in zip(l, r) if a == b)
end

hammingdistance((l, r)) = count(a != b for (a, b) in zip(l, r))
