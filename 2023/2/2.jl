#!/usr/bin/env julia
# usage: julia 2.jl [FILENAME]

module AOC2023Day2

struct Bag
    red::Int
    green::Int
    blue::Int
end

"""    Bag("1 red, 2 green, 3 blue") == Bag(1, 2, 3)"""
function Bag(description::AbstractString)
    red, green, blue = 0, 0, 0
    for (amount, color) in eachmatch(r"(\d+) (red|green|blue)", description)
        n = parse(Int, amount)
        if color == "red"
            red += n
        elseif color == "green"
            green += n
        elseif color == "blue"
            blue += n
        end
    end
    Bag(red, green, blue)
end

function Base.show(io::IO, bag::Bag)
    print(io, "Bag(\"")
    sep = ""
    for color in fieldnames(Bag)
        if getfield(bag, color) > 0
            print(io, sep, getfield(bag, color), ' ', color)
            sep = ", "
        end
    end
    print(io, "\")")
end

load(filename::AbstractString) =
    [Bag.(split(game, "; ")) for game in eachline(filename)]

Base.:<=(left::Bag, right::Bag) =
    left.red <= right.red && left.green <= right.green && left.blue <= right.blue

partone(games) =
    sum(id for (id, bags) in pairs(games) if all(<=(Bag(12, 13, 14)), bags))

power(bag) =
    bag.red * bag.green * bag.blue

fewestcubespossible(bags) =
    Bag((maximum.(getfield.(bags, field) for field in fieldnames(Bag)))...)

parttwo(games) =
    sum(power ∘ fewestcubespossible, games)

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
