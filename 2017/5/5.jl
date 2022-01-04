if basename(pwd()) == "aoc"
    cd("2017/5")
end

function part1(filename::AbstractString)
    xs = parse.(Int, eachline(filename))
    i = 1
    total = 0
    while checkbounds(Bool, xs, i)
        x = xs[i]
        xs[i] += 1
        i += x
        total += 1
    end
    total
end

@assert part1("example.txt") == 5
part1("input.txt")

function part2(filename::AbstractString)
    xs = parse.(Int, eachline(filename))
    i = 1
    total = 0
    while checkbounds(Bool, xs, i)
        x = xs[i]
        if x >= 3
            xs[i] -= 1
        else
            xs[i] += 1
        end
        i += x
        total += 1
    end
    total
end

@assert part2("example.txt") == 10
part2("input.txt")
