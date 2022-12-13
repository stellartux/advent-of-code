using Base.Iterators
using JSON

compare(left::Integer, right::Integer) = cmp(left, right)
compare(left::AbstractArray, right::Integer) = compare(left, [right])
compare(left::Integer, right::AbstractArray) = compare([left], right)

function compare(left::AbstractArray, right::AbstractArray)::Int
    for (l, r) in zip(left, right)
        c = compare(l, r)
        if !iszero(c)
            return c
        end
    end
    cmp(length(left), length(right))
end

function partone(filename::AbstractString)
    total = 0
    for (i, (l, r)) in enumerate(partition(eachline(filename), 3))
        if compare(JSON.parse(l), JSON.parse(r)) < 1
            total += i
        end
    end
    total
end

function parttwo(filename::AbstractString)
    packets = collect(JSON.parse.(Iterators.filter(!isempty, eachline(filename))))
    push!(packets, [[2]], [[6]])
    sort!(packets, lt=(l, r) -> compare(l, r) < 0)
    findfirst(==([[2]]), packets) * findfirst(==([[6]]), packets)
end

function print13(packets)
    for packet in JSON.json.(packets)
        printstyled(packet, '\n'; bold=packet == "[[2]]" || packet == "[[6]]")
    end
    nothing
end
