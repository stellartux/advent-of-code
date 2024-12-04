#!/usr/bin/env julia
# usage: julia 4.jl [FILENAME]

module AOC2024Day4

using LinearAlgebra: diag

load(file::AbstractString) = hcat(collect.(eachline(file))...)

function partone(input::Matrix{Char})
    total = 0
    for index in CartesianIndices(input)
        if input[index] == 'X'
            for offsets in (
                CartesianIndices((0:0, 0:3)),
                CartesianIndices((0:3, 0:0)),
                CartesianIndices((0:0, 0:-1:-3)),
                CartesianIndices((0:-1:-3, 0:0)),
                diag(CartesianIndices((0:-1:-3, 0:-1:-3))),
                diag(CartesianIndices((0:-1:-3, 0:3))),
                diag(CartesianIndices((0:3, 0:-1:-3))),
                diag(CartesianIndices((0:3, 0:3)))
            )
                indices = index .+ offsets

                if all(checkbounds(Bool, input, index) for index in indices) &&
                   all([input[indices]...] .== ["XMAS"...])
                    total += 1
                end
            end
        end
    end
    total
end

function parttwo(input)
    total = 0
    offsets = [
        CartesianIndex(-1, -1),
        CartesianIndex(-1, 1),
        CartesianIndex(1, -1),
        CartesianIndex(1, 1),
    ]
    for index in CartesianIndices(input)
        if input[index] == 'A'
            indices = index .+ offsets
            if all(checkbounds(Bool, input, index) for index in indices)
                if join([input[indices]...]) in ("MMSS", "SSMM", "MSMS", "SMSM")
                    total += 1
                end
            end
        end
    end
    total
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
