#!/usr/bin/env julia
# usage: julia 9.jl [FILENAME]
# https://adventofcode.com/2025/day/9

module AOC2025Day9

load(file::AbstractString) =
    [parse.(Int, split(line, ",")) for line in eachline(file)]

function partone(tiles)
    maximum(
        prod(abs.(tiles[i] .- tiles[j]) .+ 1)
        for i in eachindex(tiles)
        for j in i+1:lastindex(tiles)
    )
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day9)
end

end # module
