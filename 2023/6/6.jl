#!/usr/bin/env julia
# usage: julia 6.jl [FILENAME]

module AOC2023Day6

function race((time, distance))
    li = fld(time, 2)
    times = [t * (time - t) for t in 1:li]
    n = searchsortedlast(times, distance) + 1
    2 * (li - n + 1) - iseven(time)
end

partone(lines) =
    prod(race, splat(zip)(parse.(Int, split(line)[2:end]) for line in lines))

parttwo(lines) =
    race(parse.(Int, filter.(isdigit, lines)))

if abspath(PROGRAM_FILE) == @__FILE__
    lines = readlines(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(lines))
    println(parttwo(lines))
end

end # module
