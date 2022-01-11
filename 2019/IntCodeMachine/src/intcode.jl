struct IntCode
    memory::Vector{Int}
    IntCode(memory::Vector{Int}) = new(copy(memory))
end
IntCode(str::AbstractString) = IntCode(parse.(Int, split(str, ',')))

function Base.rpad(c::IntCode, n::Integer, p::Int = 0)
    append!(c.memory, repeat([p], max(0, n - length(c.memory))))
    c
end

Base.checkbounds(::Type{Bool}, c::IntCode, i::Integer) = checkbounds(Bool, c.memory, i + 1)
Base.getindex(c::IntCode, i::Integer) = rpad(c, i).memory[i+1]
Base.setindex!(c::IntCode, value, i::Integer) = rpad(c, i).memory[i+1] = value

reset!(code::IntCode, str::AbstractString) = reset!(code, parse.(Int, split(str, ",")))
function reset!(code::IntCode, xs::Vector{Int})
    append!(empty!(code.memory), copy(xs))
    code
end

function Base.iterate(mem::IntCode, pc::Int = 1)
    if !checkbounds(Bool, mem, pc)
        return nothing
    end

    instr, a, b, c = view(mem.memory, pc:pc+3)

    if instr == 1 # ADD
        @debug "ADD: mem[$c] = mem[$a] + mem[$b]"
        mem[c] = mem[a] + mem[b]

    elseif instr == 2 # MUL
        @debug "MUL: mem[$c] = mem[$a] * mem[$b]"
        mem[c] = mem[a] * mem[b]

    elseif instr == 99
        @debug "END"
        return nothing

    end

    (mem, pc + 4)
end
Base.IteratorSize(::Type{IntCode}) = Base.SizeUnknown()

function execute!(code::IntCode)
    foreach(code) do _
    end
    code
end
