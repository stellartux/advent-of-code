loadfish(filename)::Vector{Int} = parse.(Int, split(readchomp(filename), ','))
examplefish = loadfish("example.txt")
inputfish = loadfish("input.txt")

descendants = let
    cache = Dict{Int,Int}()
    function(age)
        get!(cache, age) do
            kids = age - 9:-7:0
            if length(kids) == 0
                0
            else
                length(kids) + sum(descendants.(kids))
            end
        end
    end
end

part2(fish, days) = length(fish) + sum(descendants.(days + 8 .- fish))

using Test
@testset begin
    @test part2(examplefish, 18) == 26
    @test part2(examplefish, 80) == 5934
    @test part2(examplefish, 256) == 26984457539
    println("The answer to part 1 is: ", part2(inputfish, 80))
    println("The answer to part 2 is: ", part2(inputfish, 256))
end
