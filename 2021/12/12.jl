#!/usr/bin/env julia
# usage: julia 12.jl [FILENAME]

module AOC202021Day12

function load(file::AbstractString)
    links = Dict{SubString, Vector{SubString}}()
    function addlink(l, r)
        if haskey(links, l)
            push!(links[l], r)
        else
            links[l] = [r]
        end
    end
    for line in readlines(file)
        l, r = split(line, '-')
        addlink(l, r)
        addlink(r, l)
    end
    links
end

isbigcave(cave::AbstractString) = first(cave) < 'a'

partone(caves) = length(takepath(caves, "start"))

function takepath(links::Dict, here::AbstractString, history = [])
    push!(history, here)
    if here == "end"
        [history]
    else
        options = filter(links[here]) do cave
            isbigcave(cave) || !(cave in history)
        end
        vcat((takepath(links, options, copy(history)) for options in options)...)
    end
end

parttwo(caves) = length(takepath2(caves, "start"))

function takepath2(links::Dict, here::AbstractString, history = [])
    push!(history, here)
    if here == "end"
        [history]
    else
        options = filter(links[here]) do cave
            cave != "start" && (
                isbigcave(cave) ||
                !(cave in history) ||
                allunique(filter(!isbigcave, history))
            )
        end
        vcat((takepath2(links, option, copy(history)) for option in options)...)
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC202021Day12)
end

end # module
