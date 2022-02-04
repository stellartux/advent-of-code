if basename(pwd()) == "aoc"
    cd("2017/13")
end

using OffsetArrays

function loadfirewall(filename::AbstractString)
    data = collect(parse.(Int, split(line, ": ")) for line in eachline(filename))
    firewall = OffsetArray(zeros(Int, first(last(data)) + 1), -1)
    for (i, v) in data
        firewall[i] = v
    end
    firewall
end

function part1(firewall, start = 0)
    total = 0
    for (depth, range) in zip(Iterators.countfrom(start), firewall)
        if !iszero(range) && iszero(scannerposition(range, depth))
            total += depth * range
        end
    end
    total
end

function scannerposition(range, step)
    if range < 2
        return 0
    end
    roundrange = 2 * range - 2
    roundstep = step % roundrange
    if roundstep < range
        roundstep
    else
        roundrange - roundstep
    end
end

@assert collect(scannerposition(5, x) for x in 0:10) == [0:4..., 3:-1:0..., 1:2...]
@assert part1(loadfirewall("example.txt")) == 24
part1(loadfirewall("input.txt"))

function part2(filename::AbstractString)
    firewall = loadfirewall(filename)
    for start in Iterators.countfrom(1)
        if all(
            iszero(range) || !iszero(scannerposition(range, depth))
            for (depth, range) in zip(Iterators.countfrom(start), firewall)
        )
            return start
        end
    end
end

@assert part2("example.txt") == 10
part2("input.txt")
