function loadleds(filename::AbstractString)
    (split.(split(line, " | ")) for line in eachline(filename))
end

function part1(data)
    sum(count(length(d) in (2, 3, 4, 7) for (_, ds) in data for d in ds))
end

using Test

@test part1(loadleds("example.txt")) == 26
println("The answer to part 1 is: ", part1(loadleds("input.txt")))
