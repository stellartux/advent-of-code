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
        if current == hash("ZZZ")
            return steps
        end
        current = maps[current, lr]
        steps += 1
    end
end

function parttwo(leftrights, maps)
    positions = [key for key in 1:size(maps, 1) if !iszero(maps[key, 1]) && isone(key % 26)]
    cyclestartfound = falses(length(positions))
    cycleendfound = falses(length(positions))
    steps = zeros(Int, length(positions))
    for lr in Iterators.cycle(leftrights)
        if all(cycleendfound)
            return lcm(steps)
        end
        positions .= maps[positions, lr]
        isendposition = @. iszero(positions % 26)
        @. cycleendfound |= cyclestartfound && isendposition
        @. cyclestartfound |= isendposition
        @. steps += (cyclestartfound && !cycleendfound)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    leftrights, maps = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(leftrights, maps))
    println(parttwo(leftrights, maps))
end

end # module
