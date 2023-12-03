#!/usr/bin/env julia
# usage: julia 3.jl [FILENAME]

module AOC2023Day3

load(file=stdin) = hcat(collect.(eachline(file))...)

partnumber(grid, range) = foldl((a, c) -> 10a + (c - '0'), grid[range]; init=0)
partnumber(grid) = Base.Fix1(partnumber, grid)

function partnumberlocations(grid::Matrix{Char})
    partnumberlocations = Set()
    for index in CartesianIndices(grid)
        if isdigit(grid[index]) && (isone(index[1]) || !isdigit(grid[index-CartesianIndex(1, 0)]))
            start = i = index
            topleft = CartesianIndex(max(1, i[1] - 1), max(1, i[2] - 1))
            while checkbounds(Bool, grid, i) && isdigit(grid[i])
                i += CartesianIndex(1, 0)
            end
            stop = i - CartesianIndex(1, 0)
            bottomright = CartesianIndex(min(size(grid, 1), i[1]), min(size(grid, 2), i[2] + 1))
            if !all(char -> isdigit(char) || char == '.', grid[topleft:bottomright])
                push!(partnumberlocations, start:stop)
            end
        end
    end
    partnumberlocations
end

partone(grid, partlocations) = sum(partnumber(grid), partlocations)

overlapswith(left, right) = !isempty(intersect(left, right))
overlapswith(left) = Base.Fix1(overlapswith, left)

function parttwo(grid, partlocations)
    total = 0
    for asterisk in findall(==('*'), grid)
        surroudings = asterisk-CartesianIndex(1, 1):asterisk+CartesianIndex(1, 1)
        adjacentlocations = filter(overlapswith(surroudings), partlocations)
        if length(adjacentlocations) == 2
            total += prod(partnumber(grid), adjacentlocations)
        end
    end
    total
end

if abspath(PROGRAM_FILE) == @__FILE__
    grid = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    parts = partnumberlocations(grid)
    println(partone(grid, parts))
    println(parttwo(grid, parts))
end

end # module
