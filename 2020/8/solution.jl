instrs = split(strip(read("input.txt", String)), "\n")

function interpret(instrs)
    acc = 0
    ip = 1
    visited = Set()

    while ip <= length(instrs)
        if ip in visited
            return (acc, false)
        end

        push!(visited, ip)
        instr = instrs[ip]

        if startswith(instr, "acc")
            acc += parse(Int, instr[5:end])
        end

        if startswith(instr, "jmp")
            ip += parse(Int, instr[5:end])
        else
            ip += 1
        end
    end

    (acc, true)
end

interpret(instrs)[1]

function correct(instrs)
    for i in eachindex(instrs)
        instr = instrs[i]
        if startswith(instr, "acc")
            continue
        end
        newinstr = (startswith(instr, "jmp") ? "nop" : "jmp") * instr[4:end]
        newinstrs = deepcopy(instrs)
        newinstrs[i] = newinstr
        result = interpret(newinstrs)
        if result[2]
            return result[1]
        end
    end
end

correct(instrs)
