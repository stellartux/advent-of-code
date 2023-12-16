function makeneighbourhood(dims::Int)::Array{CartesianIndex}
    filter(c -> !all(==(0), c[i] for i in eachindex(c)), CartesianIndices(tuple(repeat([-1:1], dims)...)))
end

mutable struct CellularAutomaton
    grid::Set{CartesianIndex}
end

function rules(grid::Set{CartesianIndex}, location::CartesianIndex, neighbourhood=makeneighbourhood(3))::Bool
    neighbourcount = count(neighbour in grid for neighbour in [location] .+ neighbourhood)
    neighbourcount == 3 || neighbourcount == 2 && location in grid
end

function CellularAutomaton(initstate::String, active::Char; rowseparator='\n', size=3)
    grid = Set{CartesianIndex}()

    for (j, row) in Iterators.enumerate(split(initstate, rowseparator))
        for (i, cell) in Iterators.enumerate(row)
            if cell == active
                push!(grid, CartesianIndex((i, j, repeat([0], size - 2)...)))
            end
        end
    end

    CellularAutomaton(grid)
end

import Base.iterate
function iterate(cellularautomaton::CellularAutomaton, state=cellularautomaton; neighbourhood=makeneighbourhood(3))::Tuple{Set{CartesianIndex},CellularAutomaton}
    grid = Set{CartesianIndex}()
    unitvector = CartesianIndex(tuple(repeat([1], length(first(cellularautomaton.grid)))...))
    for location in (minimum(state.grid) - unitvector:maximum(state.grid) + unitvector)
        if rules(state.grid, location, neighbourhood)
            push!(grid, location)
        end
    end
    state.grid = grid
    (grid, state)
end

function solve(init::String, neighbourhood::Array{CartesianIndex})::Int64
    auto = CellularAutomaton(init, '#', size=length(first(neighbourhood)))
    for _ in 1:6
        iterate(auto; neighbourhood=neighbourhood)
    end
    length(auto.grid)
end

using Test

exampleinput = ".#.\n..#\n###"
input = string(strip(read("input.txt", String)))

moore3 = makeneighbourhood(3)
moore4 = makeneighbourhood(4)

println("Part one")
@test solve(exampleinput, moore3) == 112
@show solve(input, moore3)

println("Part two")
@test solve(exampleinput, moore4) == 848
@show solve(input, moore4)
