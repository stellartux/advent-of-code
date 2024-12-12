#!/usr/bin/env julia
# usage: julia 11.jl [FILENAME]

module AOC2024Day11
using Base.Iterators: flatmap

load(file::AbstractString) = parse.(Int, split(readline(file)))

fromdigits(ds::Vector{Int})::Int = foldr((b, a) -> 10a + b, ds)

cache1 = Dict{Int,Vector{Int}}(0 => [1])
function blink(stone::Int)::Vector{Int}
    get!(cache1, stone) do
        ds = digits(stone)
        l = length(ds)
        if iseven(l)
            [fromdigits(ds[1:l÷2]), fromdigits(ds[l÷2+1:end])]
        else
            [stone * 2024]
        end
    end
end

function ntimes(fn::Function, n::Int, init)
    for _ = 1:n
        init = flatmap(fn, init)
    end
    init
end

cache5 = Dict{Int,Vector{Int}}()

function blink5(stone::Int)::Vector{Int}
    get!(cache5, stone) do
        [ntimes(blink, 5, stone)...]
    end
end

cache25 = Dict{Int,Vector{Int}}()

function blink25(stone::Int)::Vector{Int}
    get!(cache25, stone) do
        [ntimes(blink5, 5, stone)...]
    end
end

blink75(stones::Vector{Int}) = ntimes(blink25, 3, stones)

partone(stones) = sum(length, blink25.(stones))
parttwo(stones) = sum(length, blink75(stones))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
