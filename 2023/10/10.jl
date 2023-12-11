#!/usr/bin/env julia
# usage: julia 10.jl [FILENAME]

module AOC2023Day10

load(file) = permutedims(splat(hcat)(collect.(eachline(file))))

function neighbours(grid, visited, here)
    result = []
    this = grid[here]
    if occursin(this, "S-FL") # can go right
        there = here + CartesianIndex(0, 1)
        if occursin(get(grid, there, '.'), "-J7") && there ∉ visited
            push!(result, there)
        end
    end
    if occursin(this, "S-J7") # can go left
        there = here + CartesianIndex(0, -1)
        if occursin(get(grid, there, '.'), "-FL") && there ∉ visited
            push!(result, there)
        end
    end
    if occursin(this, "S|F7") # can go down
        there = here + CartesianIndex(1, 0)
        if occursin(get(grid, there, '.'), "|JL") && there ∉ visited
            push!(result, there)
        end
    end
    if occursin(this, "S|JL") # can do up
        there = here + CartesianIndex(-1, 0)
        if occursin(get(grid, there, '.'), "|F7") && there ∉ visited
            push!(result, there)
        end
    end
    result
end

function traverseloop(grid)
    start = findfirst(==('S'), grid)
    visited = Set([start])
    a, b = neighbours(grid, visited, start)
    push!(visited, a, b)
    while a != b
        a = only(neighbours(grid, visited, a))
        b = only(neighbours(grid, visited, b))
        push!(visited, a, b)
    end
    visited
end

if abspath(PROGRAM_FILE) == @__FILE__
    grid = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    loop = traverseloop(grid)
    println(length(loop) ÷ 2)
end

end # module
