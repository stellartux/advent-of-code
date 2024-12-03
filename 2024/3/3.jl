#!/usr/bin/env julia
# usage: julia 3.jl [FILENAME]

module AOC2024Day3

partone(input) =
    sum(prod(parse.(Int, m)) for m in eachmatch(r"mul\((\d{1,3}),(\d{1,3})\)", input))

parttwo(input) =
    partone(replace(input, r"don't\(\)(.|\s)*?(do\(\)|$)"=>""))

if abspath(PROGRAM_FILE) == @__FILE__
    input = readchomp(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
