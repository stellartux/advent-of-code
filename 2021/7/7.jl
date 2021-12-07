loadcrabs(filename) = parse.(Int, split(readchomp(filename), ','))

if basename(pwd()) == "aoc"
    cd("2021/7")
end
examplecrabs = loadcrabs("example.txt")
inputcrabs = loadcrabs("input.txt")

using Statistics
part1(crabs) = sum(abs.(crabs .- median(crabs)))
part1(inputcrabs) # 331067

triangle(n) = n * (n + 1) ÷ 2
offset(a, b) = triangle(abs(a - b))

function part2(crabs)
    minimum(sum(offset(x, n) for x in crabs) for n in range(extrema(crabs)...))
end
