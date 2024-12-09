#!/usr/bin/env julia
# usage: julia 8.jl [FILENAME]

module AOC2024Day8

function load(file::AbstractString)
    y = 1
    result = Dict{Char,Vector{CartesianIndex{2}}}()
    for line in eachline(file)
        for x in eachindex(line)
            if line[x] != '.'
                push!(get!(result, line[x], CartesianIndex{2}[]), CartesianIndex(x, y))
            end
        end
        y += 1
    end
    result, CartesianIndices((1:length(readline(file)), 1:y-1))
end

function partone((data, range))
    locations = Set{CartesianIndex{2}}()
    for antennae in values(data)
        for i = eachindex(antennae)
            for j = i+1:lastindex(antennae)
                a, b = antennae[[i, j]]
                for location in (a - (b - a), b + (b - a))
                    if location in range
                        push!(locations, location)
                    end
                end
            end
        end
    end
    length(locations)
end

function parttwo((data, range))
    locations = Set{CartesianIndex{2}}()
    for antennae in values(data)
        for i = eachindex(antennae)
            for j = i+1:lastindex(antennae)
                a, b = antennae[[i, j]]
                d = b - a
                while a in range
                    push!(locations, a)
                    a -= d
                end
                while b in range
                    push!(locations, b)
                    b += d
                end
            end
        end
    end
    length(locations)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
