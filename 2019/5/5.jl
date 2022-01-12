if basename(pwd()) == "aoc"
    cd("2019/5")
end
include("../IntCodeMachine/src/IntCodeMachine.jl")

part1(filename::AbstractString) = IntCodeMachine.execute!(IntCodeMachine.load(filename))
part1("input.txt")
# type 1 for part 1, 5 for part 2
