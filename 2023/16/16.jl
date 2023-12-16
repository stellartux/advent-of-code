#!/usr/bin/env julia
# usage: julia 16.jl [FILENAME]

module AOC2023Day16

load(file) = permutedims(splat(hcat)(collect.(eachline(file))))

up = CartesianIndex(-1, 0)
down = CartesianIndex(1, 0)
left = CartesianIndex(0, -1)
right = CartesianIndex(0, 1)

turnright(dir) =
    if dir == up
        right
    elseif dir == right
        down
    elseif dir == down
        left
    elseif dir == left
        up
    end

turnleft(dir) =
    if dir == up
        left
    elseif dir == left
        down
    elseif dir == down
        right
    elseif dir == right
        up
    end

function partone(grid, init=(CartesianIndex(1, 0), right))
    energised = falses(size(grid))
    visited = Set{NTuple{2,CartesianIndex{2}}}()
    beams = [init]
    while !isempty(beams)
        position, direction = popfirst!(beams)
        position += direction
        while checkbounds(Bool, grid, position) && (position, direction) ∉ visited
            push!(visited, (position, direction))
            energised[position] = true
            tile = grid[position]
            if tile == '/'
                if direction[1] == 0
                    direction = turnleft(direction)
                else
                    direction = turnright(direction)
                end
            elseif tile == '\\'
                if direction[1] == 0
                    direction = turnright(direction)
                else
                    direction = turnleft(direction)
                end
            elseif tile == '|' && direction[1] == 0 || tile == '-' && direction[2] == 0
                push!(beams, (position, turnright(direction)))
                direction = turnleft(direction)
            end
            position += direction
        end
    end
    count(energised)
end

function parttwo(grid)
    height, width = size(grid)
    beams = []
    for x = 1:width
        push!(beams,
            (CartesianIndex(0, x), down),
            (CartesianIndex(height + 1, x), up))
    end
    for y = 1:height
        push!(beams,
        (CartesianIndex(y, 0), right),
        (CartesianIndex(y, width + 1), left))
    end
    maximum(partone(grid, beam) for beam in beams)
end

if abspath(PROGRAM_FILE) == @__FILE__
    grid = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(grid))
    println(parttwo(grid))
end

end # module
