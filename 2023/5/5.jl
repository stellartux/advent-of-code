#!/usr/bin/env julia
# usage: julia 5.jl [FILENAME]

module AOC2023Day5

function load(file)
    lines = eachline(file)
    seeds = parse.(Int, split(first(lines))[2:end])
    first(lines)
    maps = []
    while !isempty(lines)
        map = Pair{UnitRange{Int},Int}[]
        for line in Iterators.drop(Iterators.takewhile(!isempty, lines), 1)
            dst, src, len = parse.(Int, split(line))
            push!(map, src:src+len-1 => dst - src)
        end
        push!(maps, sort!(map,by=last∘first))
    end
    seeds, maps
end

function partone(seeds, maps)
    minimum(foldl(maps; init=seeds) do state, map
        [
            begin
                index = searchsortedfirst(map, value; by=last∘first)
                if checkbounds(Bool, map, index) && value in map[index][1]
                    value + map[index][2]
                else
                    value
                end
            end for value in state
        ]
    end)
end

function parttwo(seeds, maps)
    minimum(partone(start:start+len-1, maps) for (start, len) in Iterators.partition(seeds, 2))
end

if abspath(PROGRAM_FILE) == @__FILE__
    seeds, maps = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(seeds, maps))
    println(parttwo(seeds, maps))
end

# RL: took 111,943,596µs wall time
# RL: ballooned to 9,994,532kb in size

end # module
