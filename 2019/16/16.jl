#!/usr/bin/env julia
# usage: julia 16.jl [FILENAME]

module AOC2019Day16
using Base.Iterators: cycle, drop, flatten, repeated, zip

load(file::AbstractString) = parse.(Int, collect(readchomp(file)))

function basesignal(n::Int)
    drop(cycle(flatten((
            repeated(0, n),
            repeated(1, n),
            repeated(0, n),
            repeated(-1, n),
        ))), 1)
end

function process(input; phases=100)
    src::Vector{Int} = collect(input)
    dest = copy(src)
    for _ = 1:phases
        for j in eachindex(src)
            dest[j] = abs(rem(sum(prod, zip(src, basesignal(j))), 10))
        end
        src, dest = dest, src
    end
    src
end

partone(input::AbstractString; kwargs...) = partone(parse.(Int, collect(input)); kwargs...)
partone(input; phases=100) = join(process(input; phases)[1:8])


if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2019Day16)
end

end # module
