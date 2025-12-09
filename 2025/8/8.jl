#!/usr/bin/env julia
# usage: julia 8.jl [FILENAME]
# https://adventofcode.com/2025/day/8

module AOC2025Day8

function load(file::AbstractString)
    boxes = [parse.(Int, split(line, ",")) for line in eachline(file)]
    boxcount = length(boxes)
    edges = first.(sort!([((x, y), sum((boxes[x] .- boxes[y]) .^ 2)) for x in 1:boxcount-1 for y in x+1:boxcount], by=last))
    boxes, edges
end

struct UnionFind{T}
    vals::Dict{T,T}
end
UnionFind{T}() where {T} = UnionFind(Dict{T,T}())

function Base.getindex(d::UnionFind{T}, x::T)::T where {T}
    if !haskey(d.vals, x)
        x
    else
        d.vals[x] = d[d.vals[x]]
    end
end

function Base.setindex!(d::UnionFind{T}, v::T, k::T) where {T}
    if k == v
        delete!(d.vals, k)
    else
        d.vals[k] = v
    end
    d
end

function Base.union!(d::UnionFind{T}, x::T, y::T)::UnionFind{T} where {T}
    xp = d[x]
    yp = d[y]
    d[x] = d[y] = d[xp] = d[yp] = min(x, y, xp, yp)
    d
end

function groupsizes(d::UnionFind{T}) where {T}
    result = Dict{T,Int}()
    for k in keys(d.vals)
        x = d[k]
        result[x] = get!(result, x, 0) + 1
    end
    result
end

function isfullyconnected(d::UnionFind)
    k, ks = Iterators.peel(keys(d.vals))
    v = d[k]
    for k in ks
        if d[k] != v
            return false
        end
    end
    true
end

function partone((boxes, edges))
    uf = UnionFind{Int}()
    for (x, y) in Iterators.take(edges, length(boxes) == 20 ? 10 : 1000)
        union!(uf, x, y)
    end
    prod(sort!(collect(values(groupsizes(uf))), rev=true)[1:3] .+ 1)
end

function parttwo((boxes, edges))
    boxcount = length(boxes)
    uf = UnionFind{Int}()
    for (x, y) in edges
        union!(uf, x, y)
        if isfullyconnected(uf) && length(keys(uf.vals)) == boxcount - 1
            return boxes[x][1] * boxes[y][1]
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day8)
end

end # module
