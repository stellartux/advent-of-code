mutable struct IntCode
    str::String
    memory::Vector{Int}
    pc::Int
    isdone::Bool
end
IntCode(str::AbstractString) = IntCode(str, parse.(Int, split(str, ',')), 0, false)

function Base.rpad(c::IntCode, n::Integer, p::Int = 0)
    while n >= length(c.memory)
        push!(c.memory, p)
    end
    c
end

Base.getindex(c::IntCode, i::Integer) = rpad(c, i).memory[i + 1]
Base.setindex!(c::IntCode, value, i::Integer) = rpad(c, i).memory[i + 1] = value

function step!(c::IntCode)
    if c.isdone || c[c.pc] == 99
        c.isdone = true
    elseif c[c.pc] == 1
        c[c[c.pc + 3]] = c[c[c.pc + 1]] + c[c[c.pc + 2]]
        c.pc += 4
    elseif c[c.pc] == 2
        c[c[c.pc + 3]] = c[c[c.pc + 1]] * c[c[c.pc + 2]]
        c.pc += 4
    end
    c
end

execute!(c::IntCode) = execute!(identity, c)
function execute!(fn::Function, c::IntCode)
    while c.pc < length(c.memory) && !c.isdone
        fn(step!(c))
    end
    c
end

function reset!(c::IntCode)
    c.memory = parse.(Int, split(c.str, ","))
    c.pc = 0
    c.isdone = false
    c
end

function s(c)
    println("Instruction: ", join(c[c.pc:c.pc+3], ' '))
end

function part1()
    c = IntCode(readchomp("input.txt"))
    c[1] = 12
    c[2] = 2
    print(execute!(c)[0])
end

function part2()
    c = IntCode(readchomp("input.txt"))
    for x in 0:99, y in 0:99
        reset!(c)
        c[1] = x
        c[2] = y
        if execute!(c)[0] == 19690720
            println(c[1], " ", c[2])
            return c
        end
    end
end
