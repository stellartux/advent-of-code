#!/usr/bin/env julia
# usage: julia 9.jl [FILENAME]

module AOC2024Day9
using Base.Iterators

function load(file::AbstractString)
    input = parse.(Int, collect(readline(file)))
    i = 1
    j = 0
    disk = []
    for (k, v) in zip(input, flatten(zip(countfrom(0), repeated(-1))))
        j += k
        push!(disk, i:j => v)
        i += k
    end
    disk
end

struct DiskMap
    map::Vector{Int}
end

function Base.show(io::IO, dm::DiskMap)
    for c in dm.map
        print(io, c == -1 ? '.' : c)
    end
end

function diskmap(layout)::DiskMap
    dm = fill(-1, last(first(last(layout))))
    for (i, v) in layout
        dm[i] .= v
    end
    DiskMap(dm)
end

function compact!(dm::DiskMap)::DiskMap
    map = dm.map
    i = findfirst(==(-1), map)
    j = findlast(>(-1), map)
    while !isnothing(i) && !isnothing(j) && i < j
        map[i], map[j] = map[j], map[i]
        i = findnext(==(-1), map, i)
        j = findprev(>(-1), map, j)
    end
    dm
end

function checksum(dm::DiskMap)::Int
    sum((i - 1) * x for (i, x) in enumerate(dm.map) if x > -1)
end

partone = checksum ∘ compact! ∘ diskmap

mutable struct DiskFile
    length::Int
    value::Int
end

function showfiles(files)
    for file in files
        print((file.value == -1 ? "." : string(file.value))^file.length)
    end
    println()
end

diskfiles(layout) = [DiskFile(length(k), v) for (k, v) in layout if length(k) > 0]

function mergeadjacent!(files, i)
    for j in i:-1:i-1
        if checkbounds(Bool, files, j:j+1)
            if files[j].value == files[j+1].value
                files[j].length += files[j+1].length
                deleteat!(files, j + 1)
            end
        end
    end
end

function compact!(files::Vector{DiskFile})::Vector{DiskFile}
    for value in maximum(f -> f.value, files):-1:0
        i = findfirst(files) do f
            f.value == value
        end
        j = findfirst(files) do f
            f.value == -1 && f.length >= files[i].length
        end
        if !isnothing(j) && j < i
            if files[i].length == files[j].length
                files[i], files[j] = files[j], files[i]
            else
                len = files[i].length
                files[j].length -= len
                insert!(files, j, popat!(files, i))
                insert!(files, i += 1, DiskFile(len, -1))
            end
            mergeadjacent!(files, i)
        end
    end
    files
end

function checksum(files::Vector{DiskFile})::Int
    total = 0
    i = 0
    for file in files
        if file.value > -1
            total += sum(i:(i+file.length-1)) * file.value
        end
        i += file.length
    end
    total
end

parttwo = checksum ∘ compact! ∘ diskfiles

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
