#!/usr/bin/env julia
# usage: julia 3.jl [FILENAME]
# https://adventofcode.com/2025/day/3

module AOC2025Day3

function load(file::AbstractString)
    stack(parse.(Int, split(line, "")) for line in eachline(file))
end

maxjoltage(batterycount::Integer) = Base.Fix1(maxjoltage, batterycount)
function maxjoltage(batterycount::Integer, bank::AbstractVector{<:Integer})
    result = 0
    index = 0
    for offset in batterycount-1:-1:0
        digit, i = findmax(bank[index+1:end-offset])
        result = 10result + digit
        index += i
    end
    result
end

partone(grid) = sum(maxjoltage(2), eachcol(grid))
parttwo(grid) = sum(maxjoltage(12), eachcol(grid))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2025Day3)
end

end # module
