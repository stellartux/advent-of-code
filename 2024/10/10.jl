#!/usr/bin/env julia
# usage: julia 10.jl [FILENAME]

module AOC2024Day10

function load(file::AbstractString)
    hcat((parse.(Int, collect(line)) for line in eachline(file))...)
end

function trailheads(grid::Matrix{Int}, pos, val=0)
    if grid[pos] == val
        if val == 9
            (pos,)
        elseif grid[pos] == val
            Iterators.flatten(
                trailheads(grid, pos + offset, val + 1)
                for offset in CartesianIndex.(((0, -1), (0, 1), (-1, 0), (1, 0)))
                if checkbounds(Bool, grid, pos + offset)
            )
        else
            ()
        end
    else
        ()
    end
end

partone(grid) =
    sum(length(Set(trailheads(grid, pos))) for pos in CartesianIndices(grid))

function counttrails(grid::Matrix{Int}, pos, val=0)::Int
    if val == 9
        grid[pos] == 9 ? 1 : 0
    elseif grid[pos] == val
        sum(
            counttrails(grid, pos + offset, val + 1)
            for offset in CartesianIndex.(((0, -1), (0, 1), (-1, 0), (1, 0)))
            if checkbounds(Bool, grid, pos + offset)
        )
    else
        0
    end
end

parttwo(grid) = sum(counttrails(grid, pos) for pos in CartesianIndices(grid))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
