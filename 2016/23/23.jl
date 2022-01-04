if basename(pwd()) == "aoc"
    cd("2016/23")
end
include("../12/12.jl")

expectedlength(line) = expectedlength(first(line))
function expectedlength(instr::AbstractString)
    if insorted(instr, ["dec", "inc", "tgl"])
        2
    elseif insorted(instr, ["add", "cpy", "jnz", "mul"])
        3
    else
        throw(ArgumentError("Unrecognized instruction: '$(instr)'"))
    end
end

function assembunny(filename::AbstractString; vm = AssembunnyVM(), pc = 1, debug = false, debugrun = false)
    lines = split.(readlines(filename))
    while checkbounds(Bool, lines, pc)
        line = lines[pc]
        pc += 1

        # step through each instruction
        if debug
            printstyled("pc: ", pc - 1, '\t', color = :blue)
            for sym in (:a, :b, :c, :d)
                printstyled(sym, ": "; color = :green)
                print(getfield(vm, sym), '\t')
            end
            printstyled(join(line, " "); color = :yellow)
            if debugrun
                println()
            else
                input = readline()
                if input == "stop"
                    return vm
                elseif input == "run"
                    debugrun = true
                end
            end
        end

        try
            if isempty(line) || length(line) != expectedlength(line)
                # do nothing

            elseif line[1] == "cpy"
                vm[line[3]] = line[2]

            elseif line[1] == "inc"
                if checkbounds(Bool, lines, pc) &&
                   checkbounds(Bool, lines, pc + 1) &&
                   lines[pc][1] == "dec" &&
                   lines[pc+1][1] == "jnz" &&
                   lines[pc][2] == lines[pc+1][2] &&
                   lines[pc+1][3] == "-2" &&
                   lines[pc][2] == lines[pc+1][2]

                    if checkbounds(Bool, lines, pc + 2) &&
                       checkbounds(Bool, lines, pc + 3) &&
                       lines[pc+2][1] == "dec" &&
                       lines[pc+3][1] == "jnz" &&
                       lines[pc+2][2] == lines[pc+3][2] &&
                       lines[pc+3][3] == "-5"

                        if debug
                            printstyled("OPTIMISED INSTR:\tmul ",
                                lines[pc+1][2], ' ',
                                lines[pc+3][2], ' ',
                                line[2], '\n';
                                color = :red)
                        end
                        vm[line[2]] += vm[lines[pc+1][2]] * vm[lines[pc+3][2]]
                        vm[lines[pc+1][2]] = 0
                        vm[lines[pc+3][2]] = 0
                        pc += 4

                    else
                        if debug
                            printstyled("OPTIMISED INSTR:\tadd ",
                                lines[pc+1][2], ' ', line[2], '\n'; color = :red)
                        end
                        vm[line[2]] += vm[lines[pc+1][2]]
                        vm[lines[pc+1][2]] = 0
                        pc += 2
                    end
                else
                    vm[line[2]] += 1
                end

            elseif line[1] == "dec"
                if lines[pc][1] == "dec" &&
                   lines[pc+1][1] == "jnz" &&
                   lines[pc+1][3] == "-2" &&
                   lines[pc][2] == lines[pc+1][2]

                    if debug
                        printstyled("OPTIMISED INSTR:\tSUB ", lines[pc+1][2], ' ', lines[pc][2], '\n'; color = :red)
                    end
                    vm[line[2]] -= vm[lines[pc][2]]
                    vm[lines[pc+1][2]] = 0
                    pc += 2
                else
                    vm[line[2]] -= 1
                end

            elseif line[1] == "jnz"
                if !iszero(vm[line[2]])
                    pc += vm[line[3]] - 1
                end

            elseif line[1] == "tgl"
                i = pc + vm[line[2]] - 1
                if checkbounds(Bool, lines, i)
                    instr = lines[i][1]
                    lines[i][1] =
                        if instr == "inc"
                            "dec"
                        elseif instr == "jnz"
                            "cpy"
                        elseif expectedlength(instr) == 2
                            "inc"
                        else
                            "jnz"
                        end
                end

            elseif line[1] == "add"
                vm[line[3]] += vm[line[2]]

            elseif line[1] == "sub"
                vm[line[3]] -= vm[line[2]]

            elseif line[1] == "mul"
                vm[line[3]] *= vm[line[2]]

            else
                throw(ArgumentError("Unrecognized instruction: '$(line[1])'"))

            end
        catch err
            println("Crashing on line ", pc - 1, ": ", join(line, ' '), '\t', (vm[x] for x in line[2:end])...)
            rethrow(err)
        end
    end
    if debug
        show(vm)
    end
    vm
end

function part1()
    @assert assembunny("example.txt").a == 3
    assembunny("input.txt"; vm = AssembunnyVM(7, 0, 0, 0)).a # 10365
end

function part2()
    assembunny("input.txt"; vm = AssembunnyVM(12, 0, 0, 0)).a
end
