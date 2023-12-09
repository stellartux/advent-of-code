#!/usr/bin/env julia
# usage: julia 8.jl [FILENAME]

module AOC2023Day8

hash(key) = 1 + foldl((a, c) -> 26a + (c - 'A'), key; init=0)

function load(file)
    lines = eachline(file)
    leftrights = [c == 'L' ? 1 : 2 for c in first(lines)]
    first(lines)
    locations = zeros(Int, (26^3, 2))
    for line in lines
        k, l, r = hash.(match(r"(...) = \((...), (...)\)", line))
        locations[k, 1] = l
        locations[k, 2] = r
    end
    leftrights, locations
end

function partone(leftrights, maps)
    steps = 0
    current = 1
    for lr in Iterators.cycle(leftrights)
        if current == 26 * 26 * 25 + 26 * 25 + 26
            break
        end
        current = maps[current, lr]
        steps += 1
    end
    steps
end


if abspath(PROGRAM_FILE) == @__FILE__
    leftrights, maps = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(leftrights, maps))
end

end # module
