module Heaps
export AbstractHeap, BinaryHeap
using Base.Order

"""
AbstractHeaps implement `popfirst!`, `push!`, and the stateful iteration protocol.

AbstractHeap constructors take an iterable of values, and `by`, `lt` and `rev` keywords
for custom Ordering of the heap. See https://docs.julialang.org/en/v1/base/sort/#Alternate-Orderings
"""
abstract type AbstractHeap{T} end

Base.cmp(heap::AbstractHeap{T}, a::T, b::T) where {T} = Base.cmp(heap.order, a, b)
Base.Order.lt(heap::AbstractHeap{T}, a::T, b::T) where {T} = Base.Order.lt(heap.order, a, b)

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
    order::Ordering
    function BinaryHeap(iterable; lt=isless, by=identity, rev=false, order=Base.ord(lt, by, rev))
        values = sort!([iterable...]; order=order)
        new{eltype(values)}(values, order)
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
    if checkbounds(Bool, heap.values, 2n) && @inbounds Base.Order.lt(heap, heap.values[2n], heap.values[m])
        m = 2n
    end
    if checkbounds(Bool, heap.values, 2n + 1) && @inbounds Base.Order.lt(heap, heap.values[2n+1], heap.values[m])
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
    if n > 1 && @inbounds Base.Order.lt(heap, heap.values[n], heap.values[m])
        @inbounds heap.values[n], heap.values[m] = heap.values[m], heap.values[n]
        bubbleup!(heap, m)
    else
        heap
    end
end

mutable struct SkewTree{T}
    value::T
    left::Union{SkewTree{T},Nothing}
    right::Union{SkewTree{T},Nothing}
end
SkewTree(value::T, left=nothing, right=nothing) where {T} =
    SkewTree{T}(value, left, right)

function skewtree(values; lt=isless, by=identity, rev=false, order=Base.ord(lt, by, rev))
    tree = nothing
    for value in values
        tree = skewmerge(order, tree, SkewTree(value))
    end
    tree
end

skewmerge(::Ordering, ::Nothing, ::Nothing) = nothing
skewmerge(::Ordering, ::Nothing, tree::SkewTree) = tree
skewmerge(::Ordering, tree::SkewTree, ::Nothing) = tree

function skewmerge(order::Ordering, tree1::SkewTree{T}, tree2::SkewTree{T}) where {T}
    if !Base.Order.lt(order, tree1.value, tree2.value)
        tree2, tree1 = tree1, tree2
    end
    tree1.right, tree1.left = tree1.left, skewmerge(order, tree2, tree1.right)
    tree1
end

"A heap backed by a skew tree."
mutable struct SkewHeap{T} <: AbstractHeap{T}
    root::Union{SkewTree{T},Nothing}
    const order::Ordering
    function SkewHeap(values; lt=isless, by=identity, rev=false, order=Base.ord(lt, by, rev))
        new{eltype(values)}(skewtree(values; order), order)
    end
end

function Base.push!(heap::SkewHeap{T}, values::Vararg{T}) where {T}
    heap.root = skewmerge(heap.order, heap.root, skewtree(values; order=heap.order))
end

function Base.popfirst!(heap::SkewHeap{T})::T where {T}
    isnothing(heap.root) && throw(ArgumentError("heap must be non-empty"))
    value = heap.root.value
    heap.root = skewmerge(heap.order, heap.root.left, heap.root.right)
    value
end

Base.empty!(heap::SkewHeap) = (heap.root = nothing; heap)
Base.isempty(heap::SkewHeap) = isnothing(heap.root)

Base.length(heap::SkewHeap) = length_(heap.root, 0)

length_(::Nothing, len::Int) = len
length_(tree::SkewTree, len::Int) = length_(tree.right, length_(tree.left, len + 1))

end # module
