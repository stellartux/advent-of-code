if basename(pwd()) == "aoc"
    cd("2021/1")
end

function part1(filename::AbstractString)
    count(x > 0 for x in diff(parse.(Int, eachline(filename))))
end

@assert part1("example.txt") == 7

function part2(filename::AbstractString)
    xs = parse.(Int, eachline(filename))
    count(x > 0 for x in xs[begin+3:end] .- xs[begin:end-3])
end

@assert part2("example.txt") == 5
