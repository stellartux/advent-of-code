#!/usr/bin/env julia
# usage: julia 6.jl [FILENAME]

module AOC2021Day6

load(file::AbstractString)::Vector{Int} = parse.(Int, split(readchomp(file), ','))

descendants = let
    cache = Dict{Int,Int}()
    function (age)
        get!(cache, age) do
            kids = age-9:-7:0
            sum(descendants, kids; init=length(kids))
        end
    end
end

calculate(fish, days) = length(fish) + sum(descendants, days + 8 .- fish)

function calculatematrix(fish, age)
    F = [count(==(n), fish) for n in 0:8]
    P = [
        0 1 0 0 0 0 0 0 0
        0 0 1 0 0 0 0 0 0
        0 0 0 1 0 0 0 0 0
        0 0 0 0 1 0 0 0 0
        0 0 0 0 0 1 0 0 0
        0 0 0 0 0 0 1 0 0
        1 0 0 0 0 0 0 1 0
        0 0 0 0 0 0 0 0 1
        1 0 0 0 0 0 0 0 0
    ]
    sum(P^age * F)
end

partone(fish) = calculate(fish, 80)
parttwo(fish) = calculate(fish, 256)

if abspath(PROGRAM_FILE) == @__FILE__
    fish = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(fish))
    println(parttwo(fish))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2021Day6)
end

end # module
