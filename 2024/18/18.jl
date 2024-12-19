#!/usr/bin/env julia
# usage: julia 18.jl [FILENAME]

module AOC2024Day18

function load(file::AbstractString)
    coords = [splat(CartesianIndex)(parse.(Int, coord) .+ 1) for coord in split.(eachline(file), ',')]
    size, range = 71, 1024
    if occursin("example", file)
        size, range = 7, 12
    end
    coords[1:range], coords[range+1:end], size
end

const up = CartesianIndex(0, -1)
const down = CartesianIndex(0, 1)
const left = CartesianIndex(-1, 0)
const right = CartesianIndex(1, 0)

function disp(grid, blocked)
    for (y, row) in enumerate(eachrow(grid))
        for (x, val) in enumerate(row)
            p = CartesianIndex(x, y)
            print(p ∈ blocked ? '#' : val < typemax(Int) ? 'O' : '.')
        end
        println()
    end
end

function search(blocked, size)::Union{Int,Nothing}
    grid = fill(typemax(Int), size, size)
    routes = [(CartesianIndex(1, 1), 0)]
    target = CartesianIndex(size, size)
    while !isempty(routes)
        here, steps = popfirst!(routes)
        if here == target
            return steps
        elseif grid[here] > steps
            grid[here] = steps
            push!(routes, (
                (position, steps + 1)
                for position in here .+ (up, down, left, right)
                if checkbounds(Bool, grid, position) && position ∉ blocked
            )...)
        end
    end
    nothing
end

partone((input, _, size)) = search(Set(input), size)

# function parttwo(input)

# end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    # println(parttwo(input))
end

end # module
