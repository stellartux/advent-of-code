if basename(pwd()) == "aoc"
    cd("2016/20")
end

function loadranges(filename::AbstractString)
    a, ranges... = sort!(
        [range.(parse.(Int, split(line, '-'))...) for line in eachline(filename)],
        by = first
    )
    len = -1
    while len != length(ranges)
        seen = UnitRange[a]
        len = length(ranges)
        for b in ranges
            if last(a) + 1 >= first(b)
                a = first(a):max(last(a), last(b))
            else
                push!(seen, a)
                a = b
            end
        end
        push!(seen, a)
        a, ranges... = seen
    end
    ranges
end

part1(filename::AbstractString) = last(first(loadranges(filename))) + 1

@assert part1("example.txt") == 3
part1("input.txt")

function part2(filename::AbstractString, limit = Int(typemax(UInt32)) + 1)
    limit - sum(length.(loadranges(filename)))
end

@assert part2("example.txt", 10) == 2
part2("input.txt")
