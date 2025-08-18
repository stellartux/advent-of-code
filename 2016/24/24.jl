#!/usr/bin/env julia
# usage: julia 24.jl [FILENAME]
# https://adventofcode.com/2016/day/24

module AOC2016Day24

neighbours(coord::CartesianIndex{2}) =
    coord .+ (
        CartesianIndex(-1, 0),
        CartesianIndex(1, 0),
        CartesianIndex(0, -1),
        CartesianIndex(0, 1))

"Converts the grid of characters to an adjancency matrix of nodes."
function gridtoadjacencies(grid::Matrix{Char})::Matrix{Int}
    nodes = sort!(findall(isdigit, grid), by=(p) -> grid[p])
    len = length(nodes)
    adjs = fill(typemax(Int), (len, len))
    for i in 1:len
        adjs[i, i] = 0
    end
    visited = falses(size(grid))
    for (i, node) in enumerate(nodes)
        queue = [(node, 0)]
        visited .= false
        while !isempty(queue)
            here, steps = popfirst!(queue)
            visited[here] && continue
            visited[here] = true
            steps += 1
            for there in neighbours(here)
                if grid[there] != '#'
                    push!(queue, (there, steps))
                end
                if isdigit(grid[there])
                    j = findfirst(==(there), nodes)
                    if adjs[i, j] > steps
                        adjs[i, j] = adjs[j, i] = steps
                    end
                end
            end
        end
    end
    adjs
end

load = gridtoadjacencies ∘ stack ∘ eachline

"Heap's algorithm"
struct Permutation{T}
    A::Vector{T}
    c::Vector{Int}
    n::Int
end
Permutation(values::Vector{T}) where {T} =
    Permutation{T}(values, ones(Int, length(values)), length(values))

function Base.iterate(p::Permutation)
    p.c .= 1
    copy(p.A), 2
end

function Base.iterate(p::Permutation, i::Int)
    while i <= p.n
        if p.c[i] < i
            if isodd(i)
                p.A[1], p.A[i] = p.A[i], p.A[1]
            else
                p.A[p.c[i]], p.A[i] = p.A[i], p.A[p.c[i]]
            end
            p.c[i] += 1
            return copy(p.A), 2
        else
            p.c[i] = 1
            i += 1
        end
    end
end

Base.length(p::Permutation) = factorial(p.n)
Base.isdone(::Permutation) = false
Base.isdone(p::Permutation, ::Int) = p.n < 2 || p.c[i] >= i
Base.eltype(::Permutation{T}) where {T} = Vector{T}

eachpermutation(values) = Permutation([values...])

"Uses brute-force search to find the shortest path visiting all nodes starting at 1,1"
bruteforcetravellingsalesman(pathlength::Function, adjs::Matrix{Int}) =
    minimum(pathlength, eachpermutation(2:size(adjs, 1)))

pathlength1(adjs, path) = sum(adjs[a, b] for (a, b) in zip([1; path], path))

partone(adjs::Matrix{Int}) =
    bruteforcetravellingsalesman(Base.Fix1(pathlength1, adjs), adjs)

pathlength2(adjs, path) = sum(adjs[a, b] for (a, b) in zip([1; path], [path; 1]))

parttwo(adjs::Matrix{Int}) =
    bruteforcetravellingsalesman(Base.Fix1(pathlength2, adjs), adjs)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2016Day24)
end

end # module
