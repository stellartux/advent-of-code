#!/usr/bin/env julia
# usage: julia 10.jl [FILENAME]
# https://adventofcode.com/2020/day/10

module AOC2020Day10
using Base.Iterators: dropwhile, takewhile, Stateful

function load(file::AbstractString)
    chargers = sort(parse.(Int, eachline(file)))
    diff([0; chargers; last(chargers) + 3])
end

partone(differences) = count(==(1), differences) * count(==(3), differences)

"""Takes a sublist of diffs that are less than 3 and calculates the possible permutations of the subbranch"""
calculatesubbranch(diffs) = (1, 2, 4, 7)[length(diffs)]

"""Splits a full list of differences into sublists for easier calculation"""
function parttwo(diffs)
    iter = Stateful(diffs)
    result = BigInt(1)
    while !isempty(iter)
        result *= (1, 1, 2, 4, 7)[count(_ -> true, takewhile(<(3), iter)) + 1]
        dropwhile(==(3), iter)
    end
    result
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2020Day10)
end

end # module
