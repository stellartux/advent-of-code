#!/usr/bin/env julia
# usage: julia 1.jl [FILENAME]
# https://adventofcode.com/2025/day/1

module AOC2025Day1

load(file::AbstractString) = [parse(Int, replace(l, 'L' => '-', 'R' => "")) for l in eachline(file)]

function partone(turns)
    total = 0
    dial = 50
    for turn in turns
        dial = mod(dial + turn, 100)
        if iszero(dial)
            total += 1
        end
    end
    total
end

function parttwo(turns)
    total = 0
    dial = 50
    for turn in turns
        while turn > 0
            dial = mod(dial + 1, 100)
            turn -= 1
            total += Int(iszero(dial))
        end
        while turn < 0
            dial = mod(dial - 1, 100)
            turn += 1
            total += Int(iszero(dial))
        end
    end
    total
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day1)
end

end # module
