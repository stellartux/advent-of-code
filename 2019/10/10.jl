#!/usr/bin/env julia
# usage: julia 10.jl [FILENAME]

module AOC2019Day10

function load(file::AbstractString)
    asteroids = CartesianIndex{2}[]
    for (y, line) in enumerate(eachline(file))
        for (x, char) in enumerate(line)
            if char == '#'
                push!(asteroids, CartesianIndex(x - 1, y - 1))
            end
        end
    end
    asteroids
end

pairwiseproduct(coll) =
    ((coll[i], coll[j]) for i in eachindex(coll) for j in i+step(eachindex(coll)):lastindex(coll))

function haslineofsight(asteroids, left::CartesianIndex{2}, right::CartesianIndex{2})
    x, y = Tuple(right - left)
    step =
        if x == 0 || y == 0
            CartesianIndex(sign(x), sign(y))
        else
            factor = gcd(x, y)
            CartesianIndex(x ÷ factor, y ÷ factor)
        end
    here = left + step
    while here != right
        if here ∈ asteroids
            return false
        end
        here += step
    end
    true
end

function bestvantagepoint(asteroids::Vector{CartesianIndex{2}})
    counts = Dict{CartesianIndex{2},Int}(asteroids .=> 0)
    for (left, right) in pairwiseproduct(asteroids)
        if haslineofsight(keys(counts), left, right)
            counts[left] += 1
            counts[right] += 1
        end
    end
    argmax(last, counts)
end

partone(asteroids::Vector{CartesianIndex{2}}) = bestvantagepoint(asteroids)[2]

function filtersplit(fn::Function, coll)
    ins = eltype(coll)[]
    outs = eltype(coll)[]
    for el in coll
        push!(fn(el) ? ins : outs, el)
    end
    ins, outs
end

anglefrom(here::CartesianIndex{2}) = Base.Fix1(anglefrom, here)
function anglefrom(here::CartesianIndex{2}, there::CartesianIndex{2})
    x1, y1 = Tuple(here)
    x2, y2 = Tuple(there)
    mod2pi(atan(x2 - x1, y1 - y2))
end

function destroy(asteroids, vantagepoint, target::Int)
    destroyed = 0
    detected, remaining = filtersplit(
        a -> haslineofsight(asteroids, vantagepoint, a), asteroids)
    while length(detected) + destroyed < target
        destroyed += length(detected)
        if isempty(remaining)
            return nothing
        end
        detected, remaining = filtersplit(
            a -> haslineofsight(remaining, vantagepoint, a), remaining)
    end
    sort!(detected; by=anglefrom(vantagepoint))[target-destroyed]
end

function parttwo(asteroids, target=200)
    vantagepoint = bestvantagepoint(asteroids)[1]
    deleteat!(asteroids, findfirst(==(vantagepoint), asteroids))
    x, y = Tuple(destroy(asteroids, vantagepoint, target))
    100x + y
end

if abspath(PROGRAM_FILE) == @__FILE__
    asteroids = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(asteroids))
    println(parttwo(asteroids))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2019Day10)
end

end # module
