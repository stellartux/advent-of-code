#!/usr/bin/env julia
# usage: julia 20.jl [FILENAME]
# https://adventofcode.com/19/day/20

module AOC2019Day20

function load(file::AbstractString)
    grid = stack(eachline(file))
    nodes = Dict{Symbol,Vector{CartesianIndex{2}}}()
    allowed = Set{CartesianIndex{2}}(i for i in CartesianIndices(grid) if grid[i] == '.')
    for coord in CartesianIndices(grid)
        if isuppercase(grid[coord])
            for offset in (CartesianIndex(1, 0), CartesianIndex(0, 1))
                next = coord + offset
                if isuppercase(get(grid, next, ' '))
                    key = Symbol(grid[coord], grid[next])
                    value = get(grid, next + offset, ' ') == '.' ? next : coord
                    push!(get!(nodes, key, CartesianIndex{2}[]), value)
                    push!(allowed, value)
                    break
                end
            end
        end
    end
    start = pop!(nodes, :AA)[]
    stop = pop!(nodes, :ZZ)[]
    delete!(allowed, start)
    portals = Dict(Iterators.flatten(
        (left => right, right => left) for (left, right) in values(nodes)
    ))
    start, stop, portals, allowed
end

nextto(here) = here .+ (
    CartesianIndex(0, 1), CartesianIndex(0, -1),
    CartesianIndex(1, 0), CartesianIndex(-1, 0)
)

function partone((start, stop, portals, allowed))
    allowed = copy(allowed)
    portals = copy(portals)
    queue = [start => 0]
    while !isempty(queue)
        here, steps = popfirst!(queue)
        if here == stop
            return steps - 2
        end
        steps += 1
        if haskey(portals, here)
            delete!(allowed, here)
            here = portals[here]
            delete!(portals, here)
            for next in nextto(here)
                if next ∈ allowed
                    here = next
                    break
                end
            end
        end
        for next in nextto(here)
            if next ∈ allowed
                push!(queue, next => steps)
                delete!(allowed, next)
            end
        end
    end
    @error "Expected to find stop"
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2019Day20)
end

end # module
