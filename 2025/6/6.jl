#!/usr/bin/env julia
# usage: julia 6.jl [FILENAME]
# https://adventofcode.com/2025/day/6

module AOC2025Day6

load = readlines

partone(lines) = sum(
        (row[end] == "*" ? prod : sum)(parse.(Int, row[1:end-1]))
        for row in eachrow(stack(split.(lines))))

function chunkby(pred::Function, iter)
    group = Vector{eltype(iter)}()
    result = Vector{typeof(group)}()
    for val in iter
        if pred(val)
            push!(group, val)
        else
            push!(result, group)
            group = []
        end
    end
    if !isempty(group)
        push!(result, group)
    end
    result
end

cephalopodmaths(grid::AbstractMatrix{<:AbstractChar})::Int =
    ('*' ∈ grid ? prod : sum)(
        parse.(Int, join.(filter(!isempty, filter.(isdigit, eachcol(grid))))))

partwo(lines) = sum(cephalopodmaths,
    stack.(chunkby(row -> !all(isspace, row), eachrow(stack(lines)))))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day6)
end

end # module
