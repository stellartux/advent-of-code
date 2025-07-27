#!/usr/bin/env julia
# usage: julia 6.jl [FILENAME]
# https://adventofcode.com/2020/day/6

module AOC2020Day6

load(file::AbstractString) =
    [Set.(split(group, "\n")) for group in split(readchomp(file), "\n\n")]

partone = Base.Fix1(sum, length ∘ splat(union))
parttwo = Base.Fix1(sum, length ∘ splat(intersect))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2020Day6)
end

end # module
