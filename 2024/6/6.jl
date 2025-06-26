#!/usr/bin/env julia
# usage: julia 6.jl [FILENAME]

module AOC2024Day6

const up = CartesianIndex(-1, 0)
const down = CartesianIndex(1, 0)
const left = CartesianIndex(0, -1)
const right = CartesianIndex(0, 1)

mutable struct Guard
    position::CartesianIndex{2}
    direction::CartesianIndex{2}
end
Guard(grid::AbstractMatrix{<:AbstractChar}) = Guard(findfirst(==('^'), grid), up)
Base.:(==)(g1::Guard, g2::Guard)= g1.position == g2.position && g1.direction == g2.direction

load(file::AbstractString)::Matrix{Char} = stack(eachline(file); dims=1)

isobstructed(grid, guard::Guard, c) = isobstructed(grid, guard.position + guard.direction, c)
isobstructed(grid, coord::CartesianIndex{2}, c) = c == coord || get(grid, coord, nothing) == '#'

function turnright(dir::CartesianIndex{2})::CartesianIndex{2}
    if dir == up
        right
    elseif dir == right
        down
    elseif dir == down
        left
    else
        up
    end
end

turnright!(guard::Guard) = (guard.direction = turnright(guard.direction); guard)
move!(guard::Guard) = (guard.position += guard.direction; guard)

function step!(guard::Guard, grid, obstruction=nothing)
    if isobstructed(grid, guard, obstruction)
        turnright!(guard)
    else
        move!(guard)
    end
end

function partone(grid)
    guard = Guard(grid)
    visited = Set{CartesianIndex{2}}()
    while checkbounds(Bool, grid, guard.position)
        push!(visited, guard.position)
        step!(guard, grid)
    end
    length(visited)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2024Day6)
end

end # module
