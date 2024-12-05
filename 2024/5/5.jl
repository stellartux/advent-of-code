#!/usr/bin/env julia
# usage: julia 5.jl [FILENAME]

module AOC2024Day5

function load(file::AbstractString)
    rules = Dict{Int,Vector{Int}}()
    updates = Vector{Int}[]
    lines = eachline(file)
    for line in lines
        if isempty(line)
            break
        end
        k, v = parse.(Int, split(line, '|'))
        push!(get!(rules, k, []), v)
    end
    sort!.(values(rules))
    for line in lines
        push!(updates, parse.(Int, split(line, ",")))
    end
    rules, updates
end

summiddles(xss) = sum(xs[length(xs)÷2+1] for xs in xss; init=0)

function issortedby(rules::Dict{Int,Vector{Int}})
    function (update::Vector{Int})
        for i in eachindex(update), j in i+1:lastindex(update)
            if insorted(update[i], get(rules, update[j], []))
                return false
            end
        end
        true
    end
end

partone((rules, updates)) = summiddles(filter(issortedby(rules), updates))

function weirdsort!(rules, update)
    sorted = false
    while !sorted
        sorted = true
        for i in eachindex(update)
            for j in i+1:lastindex(update)
                if update[i] in rules[update[j]]
                    update[i], update[j] = update[j], update[i]
                    sorted = false
                end
            end
        end
    end
    update
end

parttwo((rules, updates)) =
    summiddles(weirdsort!(rules, update) for update in filter(!issortedby(rules), updates))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
