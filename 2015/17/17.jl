if basename(pwd()) == "aoc"
    cd("2015/17")
end
using Combinatorics

function part1(filename::AbstractString, target::Integer)
    count(==(target) ∘ sum, combinations(parse.(Int, eachline(filename))))
end

@assert part1("example.txt", 25) == 4
part1("input.txt", 150)

function part2(filename::AbstractString, target::Integer)
    lengths = map(length,
        Iterators.filter(
            ==(target) ∘ sum,
            combinations(parse.(Int, eachline(filename)))))
    count(==(first(lengths)), lengths)
end

@assert part2("example.txt", 25) == 3
part2("input.txt", 150)
