#!/usr/bin/env julia
# usage: julia 2.jl [FILENAME]

module AOC2024Day2

function safe(row)::Bool
    rowdiff = diff(row)
    all(in(1:3), rowdiff) || all(in(-3:-1), rowdiff)
end

load(file) = [parse.(Int, split(line)) for line in eachline(file)]

selectout(list) = ([list[begin:i-1]; list[i+1:end]] for i in eachindex(list))

partone(input) = count(safe, input)
parttwo(input) = count(row -> any(safe, selectout(row)), input)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
