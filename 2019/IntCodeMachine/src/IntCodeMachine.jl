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
function Base.getindex(mem::IntCode, i::Integer, immediate::Bool)
    val = mem[i]
    if immediate
        val
    else
        mem[val]
    end
end
Base.getindex(mem::IntCode, r::UnitRange{Int}) = view(mem.memory, r .+ 1)
function Base.getindex(mem::IntCode, r::UnitRange{Int}, ms::Vector{Bool})
    [mem[index, immediate] for (index, immediate) in zip(r, ms)]
end

Base.setindex!(mem::IntCode, value, i::Integer) = rpad(mem, i).memory[i+1] = value

reset!(mem::IntCode, str::AbstractString) = reset!(mem, parse.(Int, split(str, ",")))
function reset!(mem::IntCode, xs::Vector{Int})
    append!(empty!(mem.memory), copy(xs))
    mem
end

function Base.iterate(mem::IntCode, pc::Int = 0)
    if !checkbounds(Bool, mem, pc)
        return nothing
    end

    opcode = mem[pc]
    E, D, A, B, C = digits(opcode, pad = 5)

    # The instruction the opcode represents
    instr = 10D + E

    # The access modes, `0` == position , `1` == immediate
    modes = Bool[A, B, C]

    if instr in 1:2
        a, b = mem[pc+1:pc+2, modes]
        c = mem[pc+3]
        if instr == 1 # ADD
            @debug "$(opcode) ADD: mem[$c] = " *
                   (Bool(A) ? repr(a) : "mem[$a]") * " + " *
                   (Bool(B) ? repr(b) : "mem[$b]")
            mem[c] = a + b
        elseif instr == 2 # MUL
            @debug "$(opcode) MUL: mem[$c] = " *
                   (Bool(A) ? repr(a) : "mem[$a]") * " * " *
                   (Bool(B) ? repr(b) : "mem[$b]")
            mem[c] = a * b
        end
        (mem, pc + 4)

    elseif instr in 3:4
        if instr == 3 # INPUT
            a = mem[pc+1]
            input = parse(Int, readline())
            @debug "$(opcode) INPUT: mem[$a] = $(input)"
            mem[a] = input
        elseif instr == 4 # OUTPUT
            a = mem[pc+1, Bool(A)]
            @debug "$(opcode) OUTPUT: $a"
            println(a)
        end
        (mem, pc + 2)

    elseif instr == 99
        @debug "$(opcode) END"
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
