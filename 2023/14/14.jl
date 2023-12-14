#!/usr/bin/env julia
# usage: julia 14.jl [FILENAME]

module AOC2023Day14

north = CartesianIndex(-1, 0)
south = CartesianIndex(1, 0)
east = CartesianIndex(0, 1)
west = CartesianIndex(0, -1)

struct Dish
    grid::Matrix{Char}
end

Dish(str::AbstractString) = Dish(permutedims(splat(hcat)(collect.(split(str)))))

Base.copy(dish::Dish) = Dish(copy(dish.grid))

function Base.show(io::IO, dish::Dish)
    println(io, "Dish(\"")
    for row in eachrow(dish.grid)
        for char in row
            printstyled(io, char, color=if char == 'O'
                :yellow
            elseif char == '.'
                :black
            else
                :normal
            end)
        end
        println(io)
    end
    print(io, "\")")
end

load(file) = Dish(readchomp(file))

function totalload(dish::Dish, rocks=findall(==('O'), dish.grid))
    offset = size(dish.grid, 1) + 1
    sum(offset - rock[1] for rock in rocks)
end

function shift!(dish, rocks=findall(==('O', dish.grid)), directions=(north, west, south, east))
    for direction in directions
        dish.grid[rocks] .= '.'
        if direction == north || direction == south
            sort!(rocks, by=x -> x[1], rev=direction == south)
        elseif direction == west || direction == east
            sort!(rocks, by=x -> x[2], rev=direction == east)
        end
        for i in eachindex(rocks)
            newposition = rocks[i] + direction
            while checkbounds(Bool, dish.grid, newposition) && dish.grid[newposition] == '.'
                rocks[i] = newposition
                newposition = rocks[i] + direction
            end
            dish.grid[rocks[i]] = 'O'
        end
    end
    dish
end

partone(dish) = totalload(shift!(copy(dish), findall(==('O'), dish.grid), (north,)))

function parttwo(dish)
    dish = copy(dish)
    rocks = findall(==('O'), dish.grid)
    i = 1
    visited = Set{Matrix{Char}}()
    while dish.grid ∉ visited
        push!(visited, copy(dish.grid))
        shift!(dish, rocks)
        @debug if i <= 3
            println("After $i cycle$(i > 1 ? "s" : ""):")
            show(dish)
        end
        i += 1
    end
    loopgrid = copy(dish.grid)
    loopstart = i
    shift!(dish, rocks)
    i += 1
    while dish.grid != loopgrid
        shift!(dish, rocks)
        i += 1
    end
    for _ = 0:(1000000000-i)%(i-loopstart)
        shift!(dish, rocks)
    end
    totalload(dish, rocks)
end

if abspath(PROGRAM_FILE) == @__FILE__
    dish = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(dish))
    println(parttwo(dish))
end

end # module
