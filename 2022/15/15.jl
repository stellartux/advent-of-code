if basename(pwd()) != "15"
    cd("2022/15")
end

manhattandistance(a, b) = sum(abs.(a .- b))

function loadfile(filename)
    (parse.(Int, match(
        r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)",
        line).captures)
     for line in eachline(filename))
end

function partone(filename::AbstractString, y=10)
    l = Dict()
    for (sx, sy, bx, by) in loadfile(filename)
        if sy == y
            l[sx] = 'S'
        end
        if by == y
            l[bx] = 'B'
        end
        m = manhattandistance((sx, sy), (bx, by))
        d = m - abs(sy - y)
        for x in (sx-d):(sx+d)
            if !haskey(l, x)
                l[x] = '#'
            end
        end
    end
    count(==('#'), values(l))
end

@assert partone("example.txt", 10) == 26
println(partone("input.txt", 2000000))

function parttwo(filename::AbstractString, len::Int)
    lines = sort!(collect(loadfile(filename)), by=x -> x[2])
    for y in (len÷4):len
        b = BitSet(0:len)
        for (sx, sy, bx, by) in lines
            d = manhattandistance((sx, sy), (bx, by)) - abs(sy - y)
            for v in max(0, sx - d):min(len, sx + d)
                delete!(b, v)
            end
        end
        if !isempty(b)
            return only(b) * 4000000 + y
        end
    end
end

@assert parttwo("example.txt", 20) == 56000011
@time println(parttwo("input.txt", 4000000))
# 11482462818989
# 21306.558980 seconds (7.28 M allocations: 847.494 GiB, 0.25% gc time)
# Coincidentally, the length of time it took me to think of the proper way to do it
