#!/usr/bin/env julia
# usage: julia 15.jl [FILENAME]

module AOC2023Day15

load(file) = split(readline(file), ',')

hash(str) =
    foldl(str; init=UInt8(0)) do current, char
        (current + UInt8(char)) * UInt8(17)
    end
@assert hash("HASH") == 52

partone(seq) = sum(hash, seq; init=0)

function parttwo(seq)
    boxes = [Tuple{AbstractString,Int}[] for _ = 1:256]
    for step in seq
        label = match(r"^[a-z]+", step).match
        box = boxes[hash(label)+1]
        index = findfirst(((x,),) -> x == label, box)
        if occursin("=", step)
            focallength = parse(Int, step[end])
            if isnothing(index)
                push!(box, (label, focallength))
            else
                box[index] = (label, focallength)
            end
        elseif !isnothing(index)
            deleteat!(box, index)
        end
    end
    sum(n * sum(i * v for (i, (_, v)) in pairs(box); init=0) for (n, box) in pairs(boxes); init=0)
end

if abspath(PROGRAM_FILE) == @__FILE__
    seq = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(seq))
    println(parttwo(seq))
end

end # module
