if basename(pwd()) == "aoc"
    cd("2016/15")
end

function loaddiscs(filename::AbstractString)
    [
        begin
            a, b, c = parse.(Int,
                [match.(
                    r"Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)\.",
                    line)...]
            )
            [(a + c) % b, b]
        end
        for line in eachline(filename)
    ]
end

part1(filename::AbstractString) = capsuledrop(loaddiscs(filename))
function capsuledrop(discs)
    for starttime in Iterators.countfrom(0)
        if all(iszero, (starttime + a) % b for (a, b) in discs)
            return starttime
        end
    end
end

@assert part1("example.txt") == 5
part1("input.txt")

part2(filename::AbstractString) = capsuledrop(push!(loaddiscs(filename), [7, 11]))
part2("input.txt")
