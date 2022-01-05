if basename(pwd()) == "aoc"
    cd("2018/1")
end

part1(filename::AbstractString) = sum(parse.(Int, eachline(filename)))
@assert part1("example.txt") == 3
part1("input.txt")

part2(filename::AbstractString) = part2(parse.(Int, eachline(filename)))

function part2(freqs::AbstractVector{Int})
    reached = Set{Int}(0)
    currentfreq = 0
    for freq in Iterators.cycle(freqs)
        currentfreq += freq
        if currentfreq in reached
            return currentfreq
        else
            push!(reached, currentfreq)
        end
    end
end

@assert part2("example.txt") == 2
part2("input.txt")
