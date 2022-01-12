module IntCodeMachine
export IntCode, execute!, reset!

struct IntCode
    memory::Vector{Int}
    IntCode(memory::Vector{Int}) = new(copy(memory))
end
IntCode(str::AbstractString) = IntCode(parse.(Int, split(str, ',')))

function Base.rpad(mem::IntCode, n::Integer, p::Int = 0)
    append!(mem.memory, repeat([p], max(0, n - length(mem.memory))))
    mem
end

Base.checkbounds(::Type{Bool}, c::IntCode, i::Integer) = checkbounds(Bool, c.memory, i + 1)

Base.getindex(mem::IntCode, i::Integer) = rpad(mem, i).memory[i+1]
function Base.getindex(mem::IntCode, i::Integer, mode::Mode)
    val = mem[i]
    if mode == Position
        val
    elseif mode == Immediate
        mem[val]
    elseif mode == Relative
        @error "UNFIJNASDOFHSA"
    end
end
Base.getindex(mem::IntCode, r::UnitRange{Int}) = view(mem.memory, r .+ 1)
function Base.getindex(mem::IntCode, r::UnitRange{Int}, ms::Vector{Mode})
    [mem[index, mode] for (index, immediate) in zip(r, ms)]
end

Base.setindex!(mem::IntCode, value, i::Integer) = rpad(mem, i).memory[i+1] = value

reset!(mem::IntCode, str::AbstractString) = reset!(mem, parse.(Int, split(str, ",")))
function reset!(mem::IntCode, xs::Vector{Int})
    append!(empty!(mem.memory), copy(xs))
    mem
end

@enum Mode Position Immediate Relative

function Base.iterate(mem::IntCode, pc::Int = 0)
    if !checkbounds(Bool, mem, pc)
        return nothing
    end

    opcode = mem[pc]
    E, D, A, B, C = digits(opcode, pad = 5)

    # The instruction the opcode represents
    instr = 10D + E

    # The access modes, `0` == position , `1` == immediate
    modes = Mode[A, B, C]
    A, B, C = modes

    if instr in 1:2
        a, b = mem[pc+1:pc+2, modes]
        c = mem[pc+3]
        if instr == 1 # ADD
            @debug "$(pc) $(opcode) ADD: mem[$c] = $a + $b"
            mem[c] = a + b
        elseif instr == 2 # MUL
            @debug "$(pc) $(opcode) MUL: mem[$c] = $a * $b"
            mem[c] = a * b
        end
        (mem, pc + 4)

    elseif instr in 3:4
        if instr == 3 # INPUT
            a = mem[pc+1]
            input = parse(Int, readline())
            @debug "$(pc) $(opcode) INPUT: mem[$a] = $(input)"
            mem[a] = input
        elseif instr == 4 # OUTPUT
            a = mem[pc+1, A]
            @debug "$(pc) $(opcode) OUTPUT: $a"
            println(a)
        end
        (mem, pc + 2)

    elseif instr in 5:6
        a, b = mem[pc+1:pc+2, modes]
        @debug if instr == 5
            "$(pc) $(opcode) JUMP-IF-TRUE: if !iszero($(a)) pc = $(b) end"
        elseif instr == 6
            "$(pc) $(opcode) JUMP-IF-FALSE: if iszero($(a)) pc = $(b) end"
        end
        (mem, (instr == 5 && !iszero(a)) || (instr == 6 && iszero(a)) ? b : pc + 3)

    elseif instr in 7:8
        a, b = mem[pc+1:pc+2, modes]
        mem[mem[pc+3]] = (instr == 7 && a < b) || (instr == 8 && a == b)
        (mem, pc + 4)

    elseif instr == 99
        @debug "$(pc) $(opcode) END"
        nothing

    else
        throw("Unimplemented instruction: $(instr) at $(pc)")
    end
end

Base.IteratorSize(::Type{IntCode}) = Base.SizeUnknown()

function execute!(code::IntCode)
    foreach(code) do _
    end
    code
end

load(filename::AbstractString) = IntCode(readchomp(filename))

end # module
