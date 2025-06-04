#!/usr/bin/env julia
# usage: julia 18.jl [FILENAME]

module AOC2021Day18

abstract type Snailfish end

Snailfish(n::Int) = SFNumber(n)
Snailfish(p::Vector) = SFPair(Snailfish(p[1]), Snailfish(p[2]))

root(p::Snailfish) = isnothing(p.parent) ? p : root(p.parent)

Base.:(==)(::Snailfish, ::Snailfish) = false
function Base.parse(::Type{Snailfish}, s::AbstractString)::Snailfish
    m = match(r"(\d+)|\[((?R)),((?R))\]", s)
    if !isnothing(m[1])
        SFNumber(parse(Int, m[1]))
    elseif !isnothing(m[2]) && !isnothing(m[3])
        SFPair(parse(Snailfish, m[2]), parse(Snailfish, m[3]))
    else
        @error "Couldn't parse \"$(s)\" as a Snailfish number."
    end
end
Base.values(sf::Snailfish) = (n.value for n in sf)

mutable struct SFNumber <: Snailfish
    value::Int
    parent::Union{Snailfish,Nothing}
end
SFNumber(n::Int) = SFNumber(n, nothing)

Base.:(==)(n1::SFNumber, n2::SFNumber) = n1.value == n2.value
Base.iterate(n::SFNumber) = n, nothing
Base.iterate(::SFNumber, ::Nothing) = nothing
Base.length(::SFNumber) = 1
Base.eltype(T::Type{SFNumber}) = T
Base.first(n::SFNumber) = n
Base.last(n::SFNumber) = n
Base.iterate(n::Iterators.Reverse{SFNumber}) = n, nothing
Base.iterate(::Iterators.Reverse{SFNumber}, ::Nothing) = nothing
Base.show(io::IO, n::SFNumber) = print(io, Snailfish, "(", n.value, ")")
prettyprint(io::IO, sf::SFNumber) = print(io, sf.value)
Base.deepcopy(n::SFNumber) = SFNumber(n.value)

mutable struct SFPair <: Snailfish
    left::Snailfish
    right::Snailfish
    parent::Union{Snailfish,Nothing}
    function SFPair(left::Snailfish, right::Snailfish, parent::Union{Snailfish,Nothing}=nothing)
        self = new(left, right, parent)
        left.parent = self
        right.parent = self
        self
    end
end

function prettyprint(io::IO, sf::SFPair)
    print(io, "[")
    prettyprint(io, sf.left)
    print(io, ",")
    prettyprint(io, sf.right)
    print(io, "]")
end

function Base.show(io::IO, p::SFPair)
    print(io, Snailfish, "(")
    prettyprint(io, p)
    print(io, ")")
end

Base.:(==)(p1::SFPair, p2::SFPair) = p1.left == p2.left && p1.right == p2.right

Base.deepcopy(p::SFPair) = SFPair(deepcopy(p.left), deepcopy(p.right))

Base.first(p::SFPair) = first(p.left)
Base.last(p::SFPair) = last(p.right)

Base.iterate(p::SFPair, n::SFNumber=first(p)) = n, next(n)
Base.iterate(::SFPair, ::Nothing) = nothing
Base.length(p::SFPair) = length(p.left) + length(p.right)
Base.eltype(::SFPair) = SFNumber

Base.first(p::Iterators.Reverse{SFPair}) = last(p.itr)
Base.iterate(p::Iterators.Reverse{SFPair}, n=first(p)) = n, prev(n)
Base.iterate(::Iterators.Reverse{SFPair}, ::Nothing) = nothing

function prev(sf::SFNumber)::Union{SFNumber,Nothing}
    if isnothing(sf.parent)
        nothing
    elseif sf === sf.parent.left
        prev(sf.parent)
    else
        last(sf.parent.left)
    end
end

function prev(sf::SFPair)::Union{SFNumber,Nothing}
    if isnothing(sf.parent)
        nothing
    elseif sf === sf.parent.right
        last(sf.parent.left)
    else
        prev(sf.parent)
    end
end

function next(sf::SFNumber)::Union{SFNumber,Nothing}
    if isnothing(sf.parent)
        nothing
    elseif sf === sf.parent.left
        first(sf.parent.right)
    else
        next(sf.parent)
    end
end

function next(sf::SFPair)::Union{SFNumber,Nothing}
    if isnothing(sf.parent)
        nothing
    elseif sf === sf.parent.left
        first(sf.parent.right)
    else
        next(sf.parent)
    end
end

load(file::AbstractString) = parse.(Snailfish, eachline(file))

function reduce!(sf::Snailfish)::Snailfish
    n1 = explodepoint(sf)
    if !isnothing(n1)
        return reduce!(explode!(n1))
    end
    n2 = splitpoint(sf)
    if !isnothing(n2)
        return reduce!(split!(n2))
    end
    sf
end

function explodepoint(p::SFPair, d=0)
    if d == 4
        p
    else
        @something explodepoint(p.left, d + 1) Some(explodepoint(p.right, d + 1))
    end
end
explodepoint(::SFNumber, _=0) = nothing

function explode!(sf::SFPair)
    p = prev(sf)
    if !isnothing(p)
        p.value += sf.left.value
    end
    n = next(sf)
    if !isnothing(n)
        n.value += sf.right.value
    end
    parent = sf.parent
    if sf === parent.left
        parent.left = SFNumber(0, parent)
    else
        parent.right = SFNumber(0, parent)
    end
    root(parent)
end

function splitpoint(p::SFPair)::Union{SFNumber,Nothing}
    for n in p
        if n.value >= 10
            return n
        end
    end
end
splitpoint(::SFNumber) = nothing

function split!(sf::SFNumber)::SFPair
    p = SFPair(SFNumber(fld(sf.value, 2)), SFNumber(cld(sf.value, 2)), sf.parent)
    if isnothing(sf.parent)
        # do nothing
    elseif sf === sf.parent.left
        sf.parent.left = p
    else
        sf.parent.right = p
    end
    root(p)
end

add!(n1::SFNumber, n2::SFNumber) = SFPair(n1, n2)
add!(sf1::Snailfish, sf2::Snailfish) = reduce!(SFPair(sf1, sf2))
add(sf1::Snailfish, sf2::Snailfish) = add!(deepcopy(sf1), deepcopy(sf2))

Base.sum(sfs::AbstractVector{SFPair}) = mapfoldl(deepcopy, add!, sfs)

magnitude(n::SFNumber) = n.value
magnitude(p::SFPair) = 3magnitude(p.left) + 2magnitude(p.right)

partone(sfs) = magnitude(sum(sfs))
parttwo(sfs) = maximum(magnitude, (add(sf1, sf2) for sf1 in sfs, sf2 in sfs))

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo(input))
end

end # module
