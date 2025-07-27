#!/usr/bin/env julia
# usage: julia 8.jl [FILENAME]
# https://adventofcode.com/2020/day/8

module AOC2020Day8

load(file::AbstractString) =
    [
        begin
            s, n = split(line)
            (Symbol(s), parse(Int, n))
        end
        for line in eachline(file)
    ]

function partone(instrs)
    acc = 0
    ip = 1
    visited = Set{Int}()

    while checkbounds(Bool, instrs, ip)
        if ip in visited
            return (acc, false)
        end

        push!(visited, ip)
        instr = instrs[ip]

        if instr[1] == :acc
            acc += instr[2]
        end

        ip += instr[1] == :jmp ? instr[2] : 1
    end

    (acc, true)
end

function parttwo(instrs)
    for i in eachindex(instrs)
        instr = instrs[i]
        if instr[1] == :acc
            continue
        end
        instrs[i] = instr[1] == :jmp ? (:nop, instr[2]) : (:jmp, instr[2])
        result = partone(instrs)
        if result[2]
            return result[1]
        end
        instrs[i] = instr
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input)[1])
    println(parttwo(input))
elseif isinteractive()
    using REPL
    REPL.activate(AOC2020Day8)
end

end # module
