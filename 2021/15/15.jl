if basename(pwd()) == "aoc"
    cd("2021/15")
end
using Pkg
Pkg.activate(".")
using Base.Iterators
using Graphs, SimpleWeightedGraphs

function loadfile(filename::AbstractString)
    transpose(reshape(
        [parse(Int, c) for line in eachline(filename) for c in line],
        length(readline(filename)),
        :
    ))
end

function loadbig(filename::AbstractString)
    A = loadfile(filename)
    B = hcat((A .+ n for n = 0:4)...)
    C = vcat((B .+ n for n = 0:4)...)
    (C .- 1) .% 9 .+ 1
end

function neighbourhood(A::AbstractMatrix, i::CartesianIndex)
    (
        n + i
        for n in CartesianIndex.([(1, 0), (0, 1), (-1, 0), (0, -1)])
        if n + i in CartesianIndices(A)
    )
end

tolinear(A, i::CartesianIndex) = size(A, 2) * (i[1] - 1) + i[2]

function todigraph(A::AbstractMatrix{Int})::SimpleWeightedDiGraph
    g = SimpleWeightedDiGraph{Int,Int}(length(A))
    for i in CartesianIndices(A)
        for n in neighbourhood(A, i)
            @assert add_edge!(g, tolinear(A, i), tolinear(A, n), A[n])
        end
    end
    g
end

minpath(g::AbstractGraph) = last(dijkstra_shortest_paths(g, 1).dists)
minpath(A::AbstractMatrix) = minpath(todigraph(A))

part1(filename) = minpath(loadfile(filename))
part2(filename) = minpath(loadbig(filename))
