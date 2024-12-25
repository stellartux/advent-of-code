#!/usr/bin/env julia
# usage: julia 25.jl [FILENAME]

module AOC2024Day25

function load(file::AbstractString)
    locks, keys = Vector{Int}[], Vector{Int}[]
    blocks = split(readchomp(file), "\n\n")
    for block in blocks
        push!(block[1] == '#' ? locks : keys, [
            foldl((a, b) -> 2a + Int(b == '#'), row; init=0)
            for row in eachrow(hcat(collect.(split(block, "\n"))...))
        ])
    end
    locks, keys
end

partone((locks, keys)) =
    count(all(iszero, key .& lock) for key in keys for lock in locks)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
end

end # module
