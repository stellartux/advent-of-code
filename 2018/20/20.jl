#!/usr/bin/env julia
# usage: julia 20.jl [FILENAME]

module AOC2018Day20

const south = CartesianIndex(0, 1)
const north = CartesianIndex(0, -1)
const east = CartesianIndex(1, 0)
const west = CartesianIndex(-1, 0)

struct World
    range::CartesianIndices{2}
    easts::Set{CartesianIndex{2}}
    souths::Set{CartesianIndex{2}}
end

origin(::World) = CartesianIndex(0, 0)

Base.eachrow(w::World) = eachrow(w.range)
Base.eachcol(w::World) = eachcol(w.range)
Base.eachindex(w::World) = w.range
Base.firstindex(w::World) = first(w.range)
Base.lastindex(w::World) = last(w.range)
Base.size(w::World, xs...) = size(w.range, xs...)

canmovesouth(w::World, c::CartesianIndex{2}) = c ∈ w.souths
canmoveeast(w::World, c::CartesianIndex{2}) = c ∈ w.easts
canmovenorth(w::World, c::CartesianIndex{2}) = c + north ∈ w.souths
canmovewest(w::World, c::CartesianIndex{2}) = c + west ∈ w.easts

function Base.show(io::IO, w::World)::Nothing
    println(io, "World(\"\"\"")
    border = repeat('#', 2 * size(w, 1) + 1)
    println(io, border)
    for col in eachcol(w)
        print(io, '#')
        for c in col
            print(io, c == origin(w) ? 'X' : '.')
            print(io, canmoveeast(w, c) ? '|' : '#')
        end
        print(io, "\n#")
        for c in col
            print(io, canmovesouth(w, c) ? "-#" : "##")
        end
        println(io)
    end
    println(io, "\"\"\")")
    nothing
end

function load(input::AbstractString)
    x, y, minx, maxx, miny, maxy = 0, 0, 0, 0, 0, 0
    easts, souths = Set{CartesianIndex{2}}(), Set{CartesianIndex{2}}()
    stack = NTuple{2,Int}[]
    for c in input
        if c == '('
            push!(stack, (x, y))
        elseif c == '|'
            x, y = last(stack)
        elseif c == ')'
            pop!(stack)
        elseif c == 'N'
            y -= 1
            push!(souths, CartesianIndex(x, y))
            miny = min(y, miny)
        elseif c == 'E'
            push!(easts, CartesianIndex(x, y))
            x += 1
            maxx = max(x, maxx)
        elseif c == 'W'
            x -= 1
            push!(easts, CartesianIndex(x, y))
            minx = min(x, minx)
        elseif c == 'S'
            push!(souths, CartesianIndex(x, y))
            y += 1
            maxy = max(y, maxy)
        end
    end
    World(CartesianIndices((minx:maxx, miny:maxy)), easts, souths)
end

function bfs(w::World)
    offset = origin(w) - firstindex(w) + CartesianIndex(1, 1)
    distances = fill(typemax(Int), size(w))
    distances[offset] = 0
    state = [(0, origin(w))]
    function move(steps, pos)
        if distances[pos+offset] > steps
            distances[pos+offset] = steps
            push!(state, (steps, pos))
        end
    end
    while !isempty(state)
        steps, position = popfirst!(state)
        if canmovenorth(w, position)
            move(steps + 1, position + north)
        end
        if canmoveeast(w, position)
            move(steps + 1, position + east)
        end
        if canmovesouth(w, position)
            move(steps + 1, position + south)
        end
        if canmovewest(w, position)
            move(steps + 1, position + west)
        end
    end
    distances
end

partone(s::AbstractString) = partone(load(s))
partone(w::World) = maximum(bfs(w))

parttwo(s::AbstractString) = parttwo(load(s))
parttwo(w::World) = count(>=(1000), bfs(w))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(readchomp(get(ARGS, 1, joinpath(@__DIR__, "input.txt"))))
    println(partone(input))
    println(parttwo(input))
end

end # module
