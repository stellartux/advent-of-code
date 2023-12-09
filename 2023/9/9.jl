#!/usr/bin/env julia
# usage: julia 9.jl [FILENAME]

module AOC2023Day9

load(file) = [parse.(Int, split(line)) for line in eachline(file)]

extrapolate(xs) = allequal(xs) ? xs[end] : last(xs) + extrapolate(diff(xs))
partone(input) = sum(extrapolate, input)

extrapolatefirst(xs) = allequal(xs) ? xs[1] : first(xs) - extrapolatefirst(diff(xs))
parttwo(input) = sum(extrapolatefirst, input)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
