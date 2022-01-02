if basename(pwd()) == "aoc"
    cd("2015/23")
end

mutable struct Registers
    a::BigInt
    b::BigInt
end
Registers() = Registers(0, 0)

function interpret(filename, rs = Registers())
    program = readlines(filename)
    pc = 1
    while checkbounds(Bool, program, pc)
        line = program[pc]
        if startswith(line, "j")
            if startswith(line, "jmp")
                pc += parse(Int, line[4:end])
            else
                cond = startswith(line, "jie") ? iseven : isone
                x = parse(Int, line[7:end])
                pc += cond(getfield(rs, Symbol(line[5]))) ? x : 1
            end
        else
            r = Symbol(line[5])
            if startswith(line, "hlf")
                setfield!(rs, r, getfield(rs, r) ÷ 2)
            elseif startswith(line, "tpl")
                setfield!(rs, r, getfield(rs, r) * 3)
            elseif startswith(line, "inc")
                setfield!(rs, r, getfield(rs, r) + 1)
            end
            pc += 1
        end
    end
    rs
end

@assert interpret("example.txt").a == 2
interpret("input.txt").b
interpret("input.txt", Registers(1, 0)).b
