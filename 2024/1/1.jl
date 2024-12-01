#!/usr/bin/env julia
# usage: julia 1.jl [FILENAME]

module AOC2024Day1

function load(file)
    nums = parse.(Int, split(readchomp(file)))
    sort(nums[1:2:end]), sort(nums[2:2:end])
end

partone((a, b)) = sum(abs.(a .- b))

function parttwo((a, b))
    counts = Dict{Int,Int}()
    for x in b
        counts[x] = get(counts, x, 0) + 1
    end
    sum(x * get(counts, x, 0) for x in a)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
