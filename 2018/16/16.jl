if basename(pwd()) != "16"
    cd("2018/16")
end
using OffsetArrays, StaticArrays

Registers(a=0, b=0, c=0, d=0) = OffsetArray(MVector(a, b, c, d), -1)

examplesample = """Before: [3, 2, 1, 1]
    9 2 1 2
    After:  [3, 2, 2, 1]

    """

function loadfile(filename::AbstractString)
    samples, testprogram = split(readchomp(filename), r"\r?\n\r?\n\r?\n\r?\n")
    split(samples, r"\r?\n\r?\n"), [parse.(Int, split(row)) for row in split(testprogram, r"\r?\n")]
end

function part1(filename::AbstractString)
    samples, _ = loadfile(filename)
    count(length(possibilities(sample)) >= 3 for sample in samples)
end
# 705 is too high

function possibilities(sample::AbstractString)
    possibilities((
        Registers(parse.(Int, collect(match))...)
        for match in eachmatch(r"(\d+),? (\d+),? (\d+),? (\d+)", sample)
    )...)
end

function possibilities(before, instr, after)
    filter(chronofns) do fn
        after == fn(copy(before), instr[begin+1:end]...)
    end
end

"""
addr (add register) stores into register C the result of adding register A and register B.
"""
function addr(registers, a, b, c)
    registers[c] = registers[a] + registers[b]
    registers
end

"""
addi (add immediate) stores into register C the result of adding register A and value B.
"""
function addi(registers, a, b, c)
    registers[c] = registers[a] + b
    registers
end

"""
mulr (multiply register) stores into register C the result of multiplying register A and register B.
"""
function mulr(registers, a, b, c)
    registers[c] = registers[a] * registers[b]
    registers
end

"""
muli (multiply immediate) stores into register C the result of multiplying register A and value B.
"""
function muli(registers, a, b, c)
    registers[c] = registers[a] * b
    registers
end

"""
banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
"""
function banr(registers, a, b, c)
    registers[c] = registers[a] & registers[b]
    registers
end

"""
bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
"""
function bani(registers, a, b, c)
    registers[c] = registers[a] & b
    registers
end

"""
borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
"""
function borr(registers, a, b, c)
    registers[c] = registers[a] | registers[b]
    registers
end

"""
bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
"""
function bori(registers, a, b, c)
    registers[c] = registers[a] | b
    registers
end

"""
setr (set register) copies the contents of register A into register C. (Input B is ignored.)
"""
function setr(registers, a, _, c)
    registers[c] = registers[a]
    registers
end

"""
seti (set immediate) stores value A into register C. (Input B is ignored.)
"""
function seti(registers, a, _, c)
    registers[c] = a
    registers
end

"""
gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
"""
function gtir(registers, a, b, c)
    registers[c] = Int(a > registers[b])
    registers
end


"""
gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
"""
function gtri(registers, a, b, c)
    registers[c] = Int(registers[a] > b)
    registers
end


"""
gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
"""
function gtrr(registers, a, b, c)
    registers[c] = Int(registers[a] > registers[b])
    registers
end


"""
eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
"""
function eqir(registers, a, b, c)
    registers[c] = Int(a == registers[b])
    registers
end

"""
eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
"""
function eqri(registers, a, b, c)
    registers[c] = Int(registers[a] == b)
    registers
end


"""
eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
"""
function eqrr(registers, a, b, c)
    registers[c] = Int(registers[a] == registers[b])
    registers
end

chronofns = [addi, addr, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

@assert length(possibilities(examplesample)) == 3
part1("input.txt")

function sampletoopcode(sample::AbstractString)::Int
    parse(Int, match(r"\]\n(\d+)", sample)[1])
end

function part2()
    samples, testprogram = loadfile("input.txt")
    opcodes = Dict{Int,Function}()
    poss = possibilities.(samples)
    while !all(i in keys(opcodes) for i in 0:15)
        i = findfirst(isone ∘ length, poss)
        fn = only(poss[i])
        opcodes[sampletoopcode(samples[i])] = fn
        filter!.(!=(fn), poss)
    end
    registers = Registers()
    for (op, a, b, c) in testprogram
        opcodes[op](registers, a, b, c)
    end
    registers[0]
end
