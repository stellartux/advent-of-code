if basename(pwd()) == "aoc"
    cd("2017/18")
end

function part1(filename::AbstractString)
    lines = split.(readlines(filename))
    pc = 1
    registers = Dict{Char,Int}(c => 0 for c in 'a':'z')
    fromtoken(token) = isdigit(last(token)) ? parse(Int, token) : registers[token[1]]
    snd = nothing
    rcv = nothing
    while checkbounds(Bool, lines, pc)
        line = lines[pc]
        pc += 1
        instr = line[1]
        if instr == "snd"
            snd = fromtoken(line[2])
        elseif instr == "set"
            registers[line[2][1]] = fromtoken(line[3])
        elseif instr == "add"
            registers[line[2][1]] += fromtoken(line[3])
        elseif instr == "mul"
            registers[line[2][1]] *= fromtoken(line[3])
        elseif instr == "mod"
            registers[line[2][1]] %= fromtoken(line[3])
        elseif instr == "rcv"
            if !iszero(fromtoken(line[2]))
                return rcv = snd
            end
        elseif instr == "jgz"
            if fromtoken(line[2]) > 0
                pc += fromtoken(line[3]) - 1
            end
        end
    end
end

part1("example.txt")

function part2(filename::AbstractString)
    lines = split.(readlines(filename))
    atob = Channel{Int}()
    btoa = Channel{Int}()
    @sync begin
        a = @spawn machine(btoa, atob, lines, 0)
        b = machine(atob, btoa, lines, 1)
    end
end

function machine(input, output, lines, p = 0)
    pc = 1
    registers = Dict{Char,Int}(c => 0 for c in 'a':'z')
    registers['p'] = p
    fromtoken(token) = isdigit(last(token)) ? parse(Int, token) : registers[token[1]]
    while checkbounds(Bool, lines, pc)
        line = lines[pc]
        pc += 1
        instr = line[1]
        if instr == "snd"
            put!(output, fromtoken(line[2]))
        elseif instr == "set"
            registers[line[2][1]] = fromtoken(line[3])
        elseif instr == "add"
            registers[line[2][1]] += fromtoken(line[3])
        elseif instr == "mul"
            registers[line[2][1]] *= fromtoken(line[3])
        elseif instr == "mod"
            registers[line[2][1]] %= fromtoken(line[3])
        elseif instr == "rcv"
            registers[line[2]] = take!(input)
        elseif instr == "jgz"
            if fromtoken(line[2]) > 0
                pc += fromtoken(line[3]) - 1
            end
        end
    end
end
