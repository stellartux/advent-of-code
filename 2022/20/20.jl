#!/usr/bin/env julia
# usage: julia 20.jl [FILENAME]

module AOC2022Day20

mutable struct ListNode{T}
    value::T
    next::ListNode{T}
    previous::ListNode{T}
    ListNode{T}(x::T) where {T} = new{T}(x)
end

struct CircularList{T}
    nodes::Vector{ListNode{T}}
    function CircularList{T}(values::Vector{T}) where {T}
        nodes = ListNode{T}.(values)
        for (left, right) in zip(nodes[begin:end-1], nodes[begin+1:end])
            left.next = right
            right.previous = left
        end
        nodes[begin].previous = nodes[end]
        nodes[end].next = nodes[begin]
        new{T}(nodes)
    end
end

function Base.show(io::IO, list::CircularList{T}) where {T}
    print(io, "CircularList{", T, "}([")
    if !isempty(list.nodes)
        node = list.nodes[1]
        print(io, node.value)
        while node.next !== list.nodes[1]
            node = node.next
            print(io, ", ", node.value)
        end
    end
    print(io, "])")
end

function mix!(list::CircularList)
    for node in list.nodes
        steps = mod(node.value, length(list.nodes) - 1)
        if iszero(steps)
            continue
        end
        node.next.previous, node.previous.next = node.previous, node.next
        target = node
        for _ in 1:steps
            target = target.next
        end
        node.previous = target
        node.next = target.next
        node.next.previous = node
        node.previous.next = node
    end
    list
end

function sumgrovecoordinates(list::CircularList)
    result = 0
    node = list.nodes[findfirst((n) -> iszero(n.value), list.nodes)]
    for _ = 1:3
        for _ = 1:1000
            node = node.next
        end
        result += node.value
    end
    result
end

load(file::AbstractString) = parse.(Int, eachline(file))

partone(values::Vector{Int}) = sumgrovecoordinates(mix!(CircularList{Int}(values)))

function parttwo(values::Vector{Int})
    list = CircularList{Int}(811589153 .* values)
    for _ = 1:10
        mix!(list)
    end
    sumgrovecoordinates(list)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2022Day20)
end

end # module
