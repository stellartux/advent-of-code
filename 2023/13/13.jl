#!/usr/bin/env julia
# usage: julia 13.jl [FILENAME]

module AOC2023Day13

function load(file)
    groups = Matrix{Char}[]
    lines = Iterators.Stateful(eachline(file))
    while !Base.isdone(lines)
        push!(groups, permutedims(splat(hcat)(collect.(Iterators.takewhile(!isempty, lines)))))
    end
    groups
end

function reflectionscore(grid::Matrix, fn=isequal)
    height, width = size(grid)
    # vertical reflection
    for x = 1:width-1
        len = min(x, width - x)
        left = grid[:, max(1, x - len + 1):x]
        right = reverse(grid[:, x+1:min(width, x + len)], dims=2)
        if fn(left, right)
            return x
        end
    end
    # horizontal reflection
    for y = 1:height-1
        len = min(y, height - y)
        left = grid[max(1, y - len + 1):y, :]
        right = reverse(grid[y+1:min(height, y + len), :], dims=1)
        if fn(left, right)
            return 100y
        end
    end
    # no reflection
    nothing
end
reflectionscore(fn::Function) = Base.Fix2(reflectionscore, fn)

partone(grids) = sum(reflectionscore, grids)

almostequal(left, right) = isone(count(splat(!=), zip(left, right)))
parttwo(grids) = sum(reflectionscore(almostequal), grids)

if abspath(PROGRAM_FILE) == @__FILE__
    grids = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(grids))
    println(parttwo(grids))
end

end # module
