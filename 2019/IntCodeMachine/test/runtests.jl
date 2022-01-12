using Test
include("../src/IntCodeMachine.jl")
using .IntCodeMachine

@testset "Day 2 Example - Add, Mul, End" begin
    program = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]
    expected = [3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50]
    @test IntCodeMachine.execute!(IntCodeMachine.IntCode(program)).memory == expected
end

@testset "Day 5 Example - Immediate/Position mode" begin
    program = [1002, 4, 3, 4, 33]
    expected = [1002, 4, 3, 4, 99]
    @test IntCodeMachine.execute!(IntCodeMachine.IntCode(program)).memory == expected
end

# "Day 5 Example - I/O" begin
# program = [3, 0, 4, 0, 99]
# IntCodeMachine.execute!(IntCodeMachine.IntCode(program))
