#!/usr/bin/env julia
# usage: julia 24.jl [FILENAME]

module AOC2024Day24

abstract type Gate end

struct AndGate <: Gate
    left::Symbol
    right::Symbol
end
apply(::AndGate, a::Bool, b::Bool) = a & b
Base.show(io::IO, g::AndGate) = print(io, g.left, " AND ", g.right)

struct OrGate <: Gate
    left::Symbol
    right::Symbol
end
apply(::OrGate, a::Bool, b::Bool) = a | b
Base.show(io::IO, g::OrGate) = print(io, g.left, " OR ", g.right)

struct XorGate <: Gate
    left::Symbol
    right::Symbol
end
apply(::XorGate, a::Bool, b::Bool) = a ⊻ b
Base.show(io::IO, g::XorGate) = print(io, g.left, " XOR ", g.right)

struct Circuit
    inits::Dict{Symbol,Bool}
    connections::Dict{Symbol,Gate}
end

function Base.show(io::IO, c::Circuit)
    for (k, v) in pairs(c.inits)
        println(io, k, ": ", v ? "1" : "0")
    end
    println(io)
    for (k, v) in pairs(c.connections)
        println(io, v, " -> ", k)
    end
end

function Base.getindex(c::Circuit, s::Symbol)
    if haskey(c.inits, s)
        c.inits[s]
    elseif haskey(c.connections, s)
        cnx = c.connections[s]
        c.inits[s] = apply(cnx, c[cnx.left], c[cnx.right])
    else
        throw(KeyError(s))
    end
end

Base.keys(c::Circuit) = unique!(sort!([keys(c.inits)..., keys(c.connections)...]))
Base.pairs(c::Circuit) = ((k, c[k]) for k in keys(c))

function load(file::AbstractString)
    lines = eachline(file)
    inits = Dict{Symbol,Bool}()
    connections = Dict{Symbol,Gate}()
    for line in lines
        if isempty(line)
            break
        end
        name, val = match(r"(...): (0|1)", line)
        inits[Symbol(name)] = val == "1"
    end
    for line in lines
        x, op, y, z = match(r"(...) (AND|X?OR) (...) -> (...)", line)
        gate = op == "AND" ? AndGate : op == "OR" ? OrGate : XorGate
        connections[Symbol(z)] = gate(Symbol(x), Symbol(y))
    end
    Circuit(inits, connections)
end

function partone(circuit::Circuit)
    foldr((a, b) -> 2b + a, (v for (k, v) in pairs(circuit) if startswith(string(k), 'z')); init=0)
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
end

end # module
