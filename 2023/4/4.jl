#!/usr/bin/env julia
# usage: julia 4.jl [FILENAME]

module AOC2023Day4

function load(file)
    [
        begin
            id, x = split(line, ':')
            left, right = split(x, " | ")
            parse(Int, id[6:end]), parse.(Int, split(left)), parse.(Int, split(right))
        end for line in eachline(file)
    ]
end

function partone(numbers)
    sum(1 << (count(in(left), right) - 1) for (_, left, right) in numbers)
end

function parttwo(numbers)
    counts = ones(Int, length(numbers))
    for (id, left, right) in numbers
        counts[id+1:id+count(in(left), right)] .+= counts[id]
    end
    sum(counts)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
