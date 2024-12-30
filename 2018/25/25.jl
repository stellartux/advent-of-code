#!/usr/bin/env julia
# usage: julia 25.jl [FILENAME]

module AOC2018Day25

manhattandistance(c1::CartesianIndex, c2::CartesianIndex) =
    sum(abs.(Tuple(c1) .- Tuple(c2)))

"Load the points, sorted by Manhattan distance from the origin."
load(file::AbstractString) =
    [CartesianIndex(parse.(Int, split(line, ","))...) for line in eachline(file)]

struct UnionFind{T}
    parent::Dict{T,T}
end
unionfind(xs) = UnionFind(Dict(x => x for x in xs))

function find!(uf::UnionFind{T}, x::T) where {T}
    p = uf.parent[x]
    if p != x
        uf.parent[x] = find!(uf, p)
    else
        x
    end
end

function unite!(uf::UnionFind{T}, x::T, y::T) where {T}
    x = find!(uf, x)
    y = find!(uf, y)
    if x ≠ y
        if x > y
            x, y = y, x
        end
        uf.parent[y] = x
    end
    uf
end

groupcount(uf::UnionFind) = count(splat(==), pairs(uf.parent))

function partone(points)
    uf = unionfind(points)
    for i in eachindex(points)
        for j in i+1:lastindex(points)
            if manhattandistance(points[i], points[j]) <= 3
                unite!(uf, points[i], points[j])
            end
        end
    end
    groupcount(uf)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
end

end # module
