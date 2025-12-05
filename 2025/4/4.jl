#!/usr/bin/env julia
# usage: julia 4.jl [FILENAME]
# https://adventofcode.com/2025/day/4

module AOC2025Day4

load(file::AbstractString) =
    stack([x == '@' for x in line] for line in eachline(file))

neighbours(here, indices) = (
    here + step
    for step in CartesianIndices((-1:1, -1:1))
    if !iszero(step) && step + here in indices
)

function removablepoints(paper)
    indices = CartesianIndices(paper)
    [i for i in indices if paper[i] && count(paper[neighbours(i, indices)]) < 4]
end

partone = length ∘ removablepoints

function parttwo(paper)
    total = 0
    removable = removablepoints(paper)
    while !isempty(removable)
        total += length(removable)
        paper[removable] .= false
        removable = removablepoints(paper)
    end
    total
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day4)
end

end # module
