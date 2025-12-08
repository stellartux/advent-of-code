#!/usr/bin/env julia
# usage: julia 7.jl [FILENAME]
# https://adventofcode.com/2025/day/7

module AOC2025Day7

load = eachline
function bothparts(lines)
    splits = 0
    previous = [Int(c == 'S') for c in first(lines)]
    current = copy(previous)
    for line in lines
        for (i, c) in enumerate(line)
            if c == '^' && !iszero(previous[i])
                current[i] = 0
                splits += 1
                current[i-1] += previous[i]
                current[i+1] += previous[i]
            end
        end
        copy!(previous, current)
    end
    splits, sum(previous)
end

if abspath(PROGRAM_FILE) == @__FILE__
    println.(bothparts(eachline(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day7)
end

end # module
