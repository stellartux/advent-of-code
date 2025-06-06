#!/usr/bin/env julia
# usage: julia 7.jl [FILENAME]

module AOC2021Day7
using Statistics: median

load(file::AbstractString) =  parse.(Int, split(readchomp(file), ','))

partone(crabs) = sum(abs.(crabs .- median(crabs)))

triangle(n) = n * (n + 1) ÷ 2
offset(a, b) = triangle(abs(a - b))

parttwo(crabs) =
    minimum(sum(offset(x, n) for x in crabs) for n in range(extrema(crabs)...))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2021Day7)
end

end # module
