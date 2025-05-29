#!/usr/bin/env julia
# usage: julia 22.jl [FILENAME]

module AOC2018Day22
export Cave, erosionlevel, geologicalindex, risklevel, regiontype, showview

struct Cave
    depth::Int
    target::CartesianIndex{2}
    erosionlevels::Dict{CartesianIndex{2},Int}
    Cave(depth::Int, target::CartesianIndex{2}) =
        new(depth, target, Dict(target => 0, CartesianIndex(0, 0) => 0))
end

function Cave(s::AbstractString)
    m = match(r"depth\s*:\s*(\d+)\s*target\s*:\s*(\d+)\s*,\s*(\d+)", s)
    if isnothing(m)
        @error "Couldn't parse \"$s\""
    end
    Cave(parse(Int, m[1]), CartesianIndex(parse(Int, m[3]), parse(Int, m[2])))
end

Base.show(io::IO, cave::Cave) =
    print(io, "Cave(", cave.depth, ", ", cave.target, ")")

function erosionlevel(cave::Cave, coord::CartesianIndex{2})::Int
    get!(cave.erosionlevels, coord) do
        mod(geologicalindex(cave, coord) + cave.depth, 20183)
    end
end

const left = CartesianIndex(0, -1)
const right = CartesianIndex(0, 1)
const up = CartesianIndex(-1, 0)
const down = CartesianIndex(1, 0)

function geologicalindex(cave::Cave, coord::CartesianIndex{2})::Int
    if coord == cave.target
        0
    elseif iszero(coord[1])
        coord[2] * 16807
    elseif iszero(coord[2])
        coord[1] * 48271
    else
        erosionlevel(cave, coord + left) * erosionlevel(cave, coord + up)
    end
end

risklevel(cave::Cave, coord::CartesianIndex{2}) = mod(erosionlevel(cave, coord), 3)
risklevel(cave::Cave) = (coord) -> risklevel(cave, coord)

const rocky = '.'
const wet = '='
const narrow = '|'
regiontype(cave::Cave, coord::CartesianIndex{2}) =
    (rocky, wet, narrow)[1+risklevel(cave, coord)]

showview(cave, range) = showview(stdout, cave, range)
function showview(io::IO, cave::Cave, range::CartesianIndices{2})
    for row in eachrow(range)
        for coord in row
            print(io, iszero(coord) ? 'M' : coord == cave.target ? 'T' : regiontype(cave, coord))
        end
        println(io)
    end
end

load = Cave ∘ readchomp

partone(cave::Cave) =
    sum(risklevel(cave), CartesianIndices((0:cave.target[1], 0:cave.target[2])))

Base.checkbounds(::Type{Bool}, ::Cave, coord::CartesianIndex{2}) = coord[1] >= 0 && coord[2] >= 0

function swaptool(cave::Cave, coord::CartesianIndex{2}, tool::Symbol)
    type = regiontype(cave, coord)
    if type == rocky
        tool == :gear ? :torch : :gear
    elseif type == wet
        tool == :gear ? :neither : :gear
    else
        tool == :torch ? :neither : :torch
    end
end

function istoolallowed(cave::Cave, coord::CartesianIndex{2}, tool::Symbol)
    type = regiontype(cave, coord)
    if type == rocky
        tool == :gear || tool == :torch
    elseif type == wet
        tool == :gear || tool == :neither
    else
        tool == :torch || tool == :neither
    end
end

function parttwo(cave::Cave)
    q1 = [(0, :torch, CartesianIndex(0, 0))]
    q2 = empty(q1)
    visited = ( torch = Set{CartesianIndex{2}}(), gear = Set{CartesianIndex{2}}(), neither = Set{CartesianIndex{2}}() )
    while true
        steps, tool, pos = popfirst!(!isempty(q1) && isempty(q2) || q1[1][1] <= q2[1][1] ? q1 : q2)
        if tool == :torch && pos == cave.target
            return steps
        elseif pos ∈ visited[tool]
            continue
        end
        push!(visited[tool], pos)
        for p in pos .+ (up, down, left, right)
            if checkbounds(Bool, cave, p) && istoolallowed(cave, p, tool)
                push!(q1, (steps + 1, tool, p))
            end
        end
        t = swaptool(cave, pos, tool)
        if istoolallowed(cave, pos, t)
            push!(q2, (steps + 7, t, pos))
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    cave = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(cave))
    println(parttwo(cave))
end

end # module
