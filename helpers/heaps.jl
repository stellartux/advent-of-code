module Heaps
export AbstractHeap, BinaryHeap

"""
AbstractHeaps implement `popfirst!`, `push!`, and the stateful iteration protocol.

AbstractHeap constructors take an iterable of values, and `by`, `lt` and `rev` keywords
for custom Ordering of the heap. See https://docs.julialang.org/en/v1/base/sort/#Alternate-Orderings
"""
abstract type AbstractHeap{T} end

Base.cmp(heap::AbstractHeap{T}, a::T, b::T) where {T} = cmp(heap.order, a, b)
Base.lt(heap::AbstractHeap{T}, a::T, b::T) where {T} = Base.lt(heap.order, a, b)

Base.eltype(::AbstractHeap{T}) where {T} = T
Base.isdone(h::AbstractHeap, heap::AbstractHeap=h) = isempty(heap)
function Base.iterate(h::AbstractHeap, heap::AbstractHeap=h)
    if !Base.isdone(heap, heap)
        popfirst!(heap), heap
    end
end

"A heap backed by a vector of values as an implicit binary tree."
struct BinaryHeap{T} <: AbstractHeap{T}
    values::Vector{T}
    order::Base.Order.Ordering
    function BinaryHeap(iterable; lt=isless, by=identity, rev=false)
        order = Base.ord(lt, by, rev)
        values = sort!([iterable...]; order=order)
        bubbledown!(new{eltype(values)}(values, order))
    end
end

Base.empty!(heap::BinaryHeap) = (empty!(heap.values); heap)
Base.isempty(heap::BinaryHeap) = isempty(heap.values)
Base.length(heap::BinaryHeap) = length(heap.values)

function Base.push!(heap::BinaryHeap{T}, values::Vararg{T})::BinaryHeap{T} where {T}
    for value in values
        push!(heap.values, value)
        bubbleup!(heap)
    end
    heap
end

function Base.popfirst!(heap::BinaryHeap{T})::T where {T}
    if length(heap.values) == 1
        pop!(heap.values)
    else
        result = first(heap.values)
        heap.values[1] = pop!(heap.values)
        bubbledown!(heap)
        result
    end
end

function bubbledown!(heap::BinaryHeap, n::Int=1)::BinaryHeap
    m = n
    if checkbounds(Bool, heap.values, 2n) && @inbounds Base.lt(heap, heap.values[2n], heap.values[m])
        m = 2n
    end
    if checkbounds(Bool, heap.values, 2n + 1) && @inbounds Base.lt(heap, heap.values[2n+1], heap.values[m])
        m = 2n + 1
    end
    if m != n
        @inbounds heap.values[n], heap.values[m] = heap.values[m], heap.values[n]
        bubbledown!(heap, m)
    else
        heap
    end
end

function bubbleup!(heap::BinaryHeap, n::Int=lastindex(heap.values))::BinaryHeap
    m = n ÷ 2
    if n > 1 && @inbounds Base.lt(heap, heap.values[n], heap.values[m])
        @inbounds heap.values[n], heap.values[m] = heap.values[m], heap.values[n]
        bubbleup!(heap, m)
    else
        heap
    end
end

end # module
