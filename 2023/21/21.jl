#!/usr/bin/env julia
# usage: julia 21.jl [FILENAME]

module AOC2023Day21

function load(file)
    points = Set{CartesianIndex{2}}()
    start = nothing
    for (j, line) in enumerate(eachline(file))
        for (i, c) in enumerate(line)
            if c != '#'
                push!(points, CartesianIndex(i, j))
            end
            if c == 'S'
                start = CartesianIndex(i, j)
            end
        end
    end
    grid = Matrix{Union{Int,Missing}}(missing, Tuple(maximum(points))...)
    stack = [(start, 0)]
    while !isempty(stack)
        point, steps = popfirst!(stack)
        if point in points
            grid[point] = steps
            for neighbour in neighbours(point)
                push!(stack, (neighbour, steps + 1))
            end
            delete!(points, point)
        end
    end
    grid
end

function partone(grid, dist = 64)
    count(iseven(x) == iseven(dist) && x <= dist for x in grid if !ismissing(x))
end

function neighbours(pos)
    (CartesianIndex(pos[1] + x, pos[2] + y) for (x, y) in [(-1, 0), (1, 0), (0, -1), (0, 1)])
end

if abspath(PROGRAM_FILE) == @__FILE__
    grid = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(grid))
end

end # module
