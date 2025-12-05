#!/usr/bin/env julia
# usage: julia 5.jl [FILENAME]
# https://adventofcode.com/2025/day/5

module AOC2025Day5

function load(file::AbstractString)
    lines = eachline(file)
    ranges = [range(parse.(Int, split(line, '-'))...) for line in Iterators.takewhile(!isempty, lines)]
    ranges, parse.(Int, lines)
end

function partone((ranges, ids))
    sort!(ranges, by=last)
    count(ids) do id
        index = searchsortedfirst(ranges, id, by=last)
        while index in keys(ranges) && id <= last(ranges[index])
            if id in ranges[index]
                return true
            end
            index += 1
        end
        false
    end
end

function combineranges(ranges)
    sort!(ranges, by=first)
    result = []
    combined = popfirst!(ranges)
    for range in ranges
        if first(range) <= last(combined)
            combined = first(combined):max(last(combined), last(range))
        else
            push!(result, combined)
            combined = range
        end
    end
    push!(result, combined)
    result
end

parttwo = Base.Fix1(sum, length) ∘ combineranges ∘ first

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day5)
end

end # module
