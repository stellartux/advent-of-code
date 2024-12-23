#!/usr/bin/env julia
# usage: julia 22.jl [FILENAME]

module AOC2024Day22
using Base.Iterators

load(file::AbstractString) = parse.(Int, eachline(file))

function process(s::Int)::Int
    s = xor(s, s << 6) & 0xffffff
    s = xor(s, s >> 5) & 0xffffff
    xor(s, s << 11) & 0xffffff
end

struct Buyer
    init::Int
end
Base.IteratorSize(::Type{Buyer}) = Base.IsInfinite()
Base.eltype(::Type{Buyer}) = Int
Base.iterate(b::Buyer, init=b.init) = init, process(init)

Base.getindex(b::Buyer, i::Int) = first(drop(b, i - 1))
Base.getindex(b::Buyer, r::UnitRange) = collect(take(drop(b, first(r) - 1), length(r)))

partone(secrets) = sum(Buyer(secret)[2000] for secret in secrets)

function p2(secret::Int, target::NTuple{4,Int}; count=2000)::Int
    a, b, c, d, es... = (mod(x, 10) for x in take(Buyer(secret), count+1))
    for e in es
        if (b - a, c - b, d - c, e - d) == target
            return e
        end
        a, b, c, d = b, c, d, e
    end
    0
end

parttwo(secrets; kwargs...) =
    maximum(sum(p2(secret, target; kwargs...) for secret in secrets)
            for target in product(-9:9, -9:9, -9:9, -9:9)
            if all(in(-9:9), diff(collect(target))))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input)) # 845s
end

end # module
