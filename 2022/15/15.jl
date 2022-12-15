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

