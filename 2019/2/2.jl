if basename(pwd()) == "aoc"
    cd("2019/2")
end
include("../IntCodeMachine/src/IntCodeMachine.jl")
using .IntCodeMachine

function part1()
    c = IntCode(readchomp("input.txt"))
    c[1] = 12
    c[2] = 2
    print(execute!(c)[0])
end

function part2()
    c = IntCode(readchomp("input.txt"))
    initmem = copy(c.memory)
    for x in 0:99, y in 0:99
        reset!(c, initmem)
        c[1] = x
        c[2] = y
        if execute!(c)[0] == 19690720
            return c[1] * 100 + c[2]
        end
    end
end
