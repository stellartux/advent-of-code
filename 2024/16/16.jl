#!/usr/bin/env julia
# usage: julia 16.jl [FILENAME]

module AOC2024Day16

load(file::AbstractString) = hcat(collect.(eachline(file))...)

const up = CartesianIndex(0, -1)
const down = CartesianIndex(0, 1)
const left = CartesianIndex(-1, 0)
const right = CartesianIndex(1, 0)

function turnright(dir)
    if dir == up
        right
    elseif dir == right
        down
    elseif dir == down
        left
    else
        up
    end
end

function turnleft(dir)
    if dir == up
        left
    elseif dir == right
        up
    elseif dir == down
        right
    else
        down
    end
end

function partone(grid)
    stop = findfirst(==('E'), grid)
    start = findfirst(==('S'), grid)
    reindeer = [(start, right, 0)]
    scores = fill(typemax(Int), size(grid))
    while !isempty(reindeer)
        pos, dir, score = popfirst!(reindeer)
        while score <= scores[pos]
            scores[pos] = score
            for d in (turnright(dir), turnleft(dir))
                if grid[pos+d] != '#' && scores[pos+d] >= score + 1000
                    push!(reindeer, (pos + d, d, score + 1001))
                end
            end
            if grid[pos+dir] == '#'
                break
            end
            pos += dir
            score += 1
        end
    end
    scores[stop]
end

# function parttwo(input)

# end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    # println(parttwo(input))
end

end # module
