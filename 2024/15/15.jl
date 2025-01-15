#!/usr/bin/env julia
# usage: julia 15.jl [FILENAME]

module AOC2024Day15

function load(file::AbstractString)
    a, b = split(readchomp(file), "\n\n"; limit=2)
    hcat(collect.(split(a, "\n"))...), filter(!isspace, b)
end

gps(coord::CartesianIndex{2}) = 100(coord[2] - 1) + coord[1] - 1

const direction = Dict{Char,CartesianIndex{2}}(
    '^' => CartesianIndex(0, -1),
    'v' => CartesianIndex(0, 1),
    '<' => CartesianIndex(-1, 0),
    '>' => CartesianIndex(1, 0))

function move!(grid, dir, pos=findfirst(==('@'), grid))
    newpos = pos + dir
    if grid[newpos] == 'O'
        move!(grid, dir, newpos)
    end
    if grid[newpos] == '.'
        grid[pos], grid[newpos] = grid[newpos], grid[pos]
    end
    grid
end

function partone((grid, path))
    grid = copy(grid)
    for step in path
        move!(grid, direction[step])
    end
    sum(gps, findall(==('O'), grid))
end

# function parttwo((grid, path))

# end

if abspath(PROGRAM_FILE) == @__FILE__
    input = get(ARGS, 1, joinpath(@__DIR__, "input.txt"))
    println(partone(input))
    # println(parttwo(input))
end

end # module
