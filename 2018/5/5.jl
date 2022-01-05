if basename(pwd()) == "aoc"
    cd("2018/5")
end

part1(filename::AbstractString) = reaction(readchomp(filename))

function reaction(str::AbstractString)
    len = -1
    while length(str) != len
        len = length(str)
        str = replace(
            str,
            Iterators.flatten(
                (c * C => "", C * c => "")
                for (c, C) in zip('a':'z', 'A':'Z')
            )...
        )
    end
    len
end

@assert reaction("aA") == 0
@assert reaction("abBA") == 0
@assert reaction("abAB") == 4
@assert reaction("aabAAB") == 6
@assert part1("example.txt") == 10
part1("input.txt")

function part2(filename::AbstractString)
    str = readchomp(filename)
    mini = minimum(uppercase(str))
    maxi = maximum(lowercase(str))
    ranges = zip(lowercase(mini):maxi, mini:uppercase(maxi))
    minimum(reaction, replace(str, c => "", C => "") for (c, C) in ranges)
end

@assert part2("example.txt") == 4
part2("input.txt")
