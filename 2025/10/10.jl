#!/usr/bin/env julia
# usage: julia 10.jl [FILENAME]
# https://adventofcode.com/2025/day/10

module AOC2025Day10

struct Machine
    indicators::Int
    buttons::Vector{Int}
    joltages::Vector{Int}
end

function Machine(str::AbstractString)
    lights, buttons..., joltages = split(str, " ")
    Machine(
        foldr((c, a) -> 2a + Int(c == '#'), collect(lights[2:end-1]), init=0),
        [foldl((a, c) -> a | (1 << c), parse.(Int, split(b[2:end-1], ",")), init=0) for b in buttons],
        parse.(Int, split(joltages[2:end-1], ","))
    )
end

load(file::AbstractString) = Machine.(eachline(file))

function buttonpresses(m::Machine)::Int
    seen = Set{Int}()
    queue = [0 => 0]
    while !isempty(queue)
        state, steps = popfirst!(queue)
        if state == m.indicators
            return steps
        elseif state ∉ seen
            push!(seen, state)
            push!(queue, (state .⊻ m.buttons .=> steps + 1)...)
        end
    end
    show(m)
end

partone(input) = sum(buttonpresses, input)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day10)
end

end # module
