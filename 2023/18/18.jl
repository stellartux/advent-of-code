#!/usr/bin/env julia
# usage: julia 18.jl [FILENAME]

module AOC2023Day18

const up = CartesianIndex(-1, 0)
const down = CartesianIndex(1, 0)
const left = CartesianIndex(0, -1)
const right = CartesianIndex(0, 1)

direction(s::AbstractString) = direction(first(s))
function direction(c::AbstractChar)
    if c == 'U'
        up
    elseif c == 'D'
        down
    elseif c == 'L'
        left
    elseif c == 'R'
        right
    end
end

function load(file=stdin)
    [
        (direction(dir), parse(Int, len), parse(UInt32, color[3:end-1], base=16))
        for (dir, len, color) in split.(eachline(file))
    ]
end

function diggrid(digplan)::Matrix{Char}
    here = CartesianIndex(0, 0)
    mini, maxi = CartesianIndex(0, 0), CartesianIndex(0, 0)
    for (dir, len) in digplan
        here += dir * len
        mini = min(mini, here)
        maxi = max(maxi, here)
    end
    here = CartesianIndex(1, 1) - mini
    grid = fill('.', Tuple(here + maxi))
    for (dir, len) in digplan
        there = here + dir * len
        grid[range(sort([here, there])...)] .= '#'
        here = there
    end
    grid
end

function floodfill!(grid::AbstractMatrix{<:AbstractChar})
    for i in eachindex(grid[1, :])
        if grid[1, i] == '#' && checkbounds(Bool, grid[1, :], i + 1) && grid[1, i+1] == '#' && grid[2, i] == '#'
            return floodfill!(grid, [CartesianIndex(2, i + 1)])
        end
    end
    error("Top left corner not found.")
end

function floodfill!(grid::AbstractMatrix{<:AbstractChar}, indices::AbstractVector{CartesianIndex{2}})
    while !isempty(indices)
        index = popfirst!(indices)
        if checkbounds(Bool, grid, index) && grid[index] == '.'
            grid[index] = '#'
            push!(indices, index + up, index + left, index + right, index + down)
        end
    end
    grid
end

partone(filename::AbstractString) = partone(load(filename))
partone(digplan) = count(==('#'), floodfill!(diggrid(digplan)))

function show(grid)
    for row in eachrow(grid)
        print.(row)
        println()
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    grid = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(grid))
end

end # module
