#!/usr/bin/env julia
# usage: julia 16.jl [FILENAME]

module AOC2024Day16

load(file::AbstractString) = stack(eachline(file); dims=2)

function debug(scores, visited=Set{CartesianIndex{2}}())
    for (x, col) in enumerate(eachcol(scores))
        for (y, v) in enumerate(col)
            c = CartesianIndex(y, x)
            print(v == typemax(Int) ? "\033[97m#" : c ∈ visited ? "\033[93mO" : "\033[90m.")
        end
        println()
    end
    println("\033[0m")
end

leftright(dir) = (CartesianIndex(dir[2], dir[1]), CartesianIndex(-dir[2], -dir[1]))

function solve(grid::Matrix{Char})
    stop = findfirst(==('E'), grid)
    start = findfirst(==('S'), grid)
    queue = [(start, CartesianIndex(1, 0), 0, Set{CartesianIndex{2}}())]
    visited = Set{CartesianIndex{2}}()
    scores = fill(typemax(Int), size(grid))
    minscore = typemax(Int)
    while !isempty(queue)
        here, dir, score, path = popfirst!(queue)
        while score <= minscore
            if score > scores[here] && get(scores, here + dir, typemax(Int)) < score + 1
                break
            elseif score == scores[here] && here ∈ visited
                union!(visited, path)
                break
            end
            scores[here] = score
            push!(path, here)
            if here == stop
                if score < minscore
                    minscore = score
                    visited = path
                elseif score == minscore
                    union!(visited, path)
                end
                break
            end
            for d in leftright(dir)
                if grid[here+d] != '#' && scores[here+d] > score + 1001
                    push!(queue, (here + d, d, score + 1001, copy(path)))
                end
            end
            if grid[here+dir] == '#'
                break
            end
            here += dir
            score += 1
        end
    end
    minscore, length(visited)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    # solve both parts in one pass
    println.(solve(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2024Day16)
end

end # module
