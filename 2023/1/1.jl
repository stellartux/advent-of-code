#!/usr/bin/env julia
# usage: julia 1.jl [FILENAME]

module AOC2023Day1

partone(lines) =
    sum(parse(Int, line[[findfirst(isdigit, line), findlast(isdigit, line)]]) for line in lines)

digits = Dict(
    "zero" => "0",
    "one" => "1",
    "two" => "2",
    "three" => "3",
    "four" => "4",
    "five" => "5",
    "six" => "6",
    "seven" => "7",
    "eight" => "8",
    "nine" => "9")

parttwo(lines) =
    sum(begin
        first = match(r"[0-9]|zero|one|two|three|four|five|six|seven|eight|nine", line).match
        last = match(r"^.*([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)", line)[1]
        parse(Int, get(digits, first, first) * get(digits, last, last))
    end for line in lines)


if abspath(PROGRAM_FILE) == @__FILE__
    input = readlines(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
