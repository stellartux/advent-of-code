if basename(pwd()) == "aoc"
    cd("2017/8")
end

function part1(filename::AbstractString)
    registers = Dict{AbstractString,Int}()
    for line in eachline(filename)
        target, instr, val, _, lc, cond, rc = split(line)
        if eval(Meta.parse(cond))(get(registers, lc, 0), parse(Int, rc))
            registers[target] = (instr == "inc" ? (+) : (-))(get(registers, target, 0), parse(Int, val))
        end
    end
    maximum(values(registers))
end

@assert part1("example.txt") == 1
part1("input.txt")

function part2(filename::AbstractString)
    registers = Dict{AbstractString,Int}()
    biggest = 0
    for line in eachline(filename)
        target, instr, val, _, lc, cond, rc = split(line)
        if eval(Meta.parse(cond))(get(registers, lc, 0), parse(Int, rc))
            registers[target] = (instr == "inc" ? (+) : (-))(get(registers, target, 0), parse(Int, val))
            biggest = max(registers[target], biggest)
        end
    end
    biggest
end

@assert part2("example.txt") == 10
part2("input.txt")
