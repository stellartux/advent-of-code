if basename(pwd()) == "aoc"
    cd("2016/6")
end
using StatsBase: countmap

function loadcs(filename)
    reshape(
        [c for c in readchomp(filename) if !isspace(c)],
        length(readline(filename)),
        :
    )
end

function part1(filename::AbstractString)
    join(findmax(countmap(row))[2] for row in eachrow(loadcs(filename)))
end

@assert part1("example.txt") == "easter"
part1("input.txt")

function part2(filename::AbstractString)
    join(findmin(countmap(row))[2] for row in eachrow(loadcs(filename)))
end

@assert part2("example.txt") == "advent"
part2("input.txt")
