if basename(pwd()) == "aoc"
    cd("2015/14")
end

function loadreindeer(filename::AbstractString)
    map(eachmatch(r"(.*) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.", readchomp(filename))) do m
        m[1], (parse(Int, m[i]) for i in 2:4)...
    end
end

function position(reindeer, time::Integer)
    _, speed, on, off = reindeer
    fulls, final = divrem(time, on + off)
    (fulls * on + min(final, on)) * speed
end
position(time::Integer) = Base.Fix2(position, time)

part1(filename::AbstractString, n) = part1(loadreindeer(filename), n)
part1(reindeer, n = 2503) = maximum(position(n), reindeer)

part2(filename::AbstractString, n = 2503) = part2(loadreindeer(filename), n)

function part2(reindeer, n = 2503)
    scores = Dict(r[1] => 0 for r in reindeer)
    for i in 1:n
        top = maximum(position(i), reindeer)
        for r in reindeer
            if position(r, i) == top
                scores[r[1]] += 1
            end
        end
    end
    maximum(values(scores))
end
