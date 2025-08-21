#!/usr/bin/env julia
# usage: julia 17.jl [FILENAME]
# https://adventofcode.com/2023/day/17

module AOC2023Day17
include("../../helpers/heaps.jl")
using .Heaps: BinaryHeap, SortHeap

load(file::AbstractString) =
    stack(parse.(Int, split(line, "")) for line in eachline(file))

turnleft(dir) = CartesianIndex(dir[2], -dir[1])
turnright(dir) = CartesianIndex(-dir[2], dir[1])

function nextstep(grid, here, dir, section, range)
    result = Tuple{CartesianIndex{2},Int}[]
    if section < last(range) && checkbounds(Bool, grid, here + dir)
        push!(result, (here + dir, section + 1))
    end
    if section >= first(range)
        if checkbounds(Bool, grid, here + turnleft(dir))
            push!(result, (here + turnleft(dir), 1))
        end
        if checkbounds(Bool, grid, here + turnright(dir))
            push!(result, (here + turnright(dir), 1))
        end
    end
    result
end

function search(grid::Matrix{Int}, range)
    visited = Dict{Tuple{CartesianIndex{2},CartesianIndex{2},Int},Int}()
    queue = BinaryHeap((
        (grid[CartesianIndex(2, 1)], CartesianIndex(2, 1), CartesianIndex(1, 0), 1),
        (grid[CartesianIndex(1, 2)], CartesianIndex(1, 2), CartesianIndex(0, 1), 1),
    ); by=first)
    stop = last(keys(grid))
    while !isempty(queue)
        path, here, dir, section = popfirst!(queue)
        if here == stop && section in range
            return path
        end
        push!(visited, (here, dir, section) => path )
        for (there, sec) in nextstep(grid, here, dir, section, range)
            newpath = path + grid[there]
            if newpath < get(visited, (here, there, sec), typemax(Int))
                visited[(here, there, sec)] = newpath
                push!(queue, (newpath, there, there - here, sec))
            end
        end
    end
end

partone(grid) = search(grid, 0:3)
parttwo(grid) = search(grid, 4:10)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2023Day17)
end

end # module
