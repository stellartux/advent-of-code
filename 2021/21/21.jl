if basename(pwd()) == "aoc"
    cd("2021/20")
end
using Base.Iterators: cycle, partition, Stateful

function loadstartingpositions(filename::AbstractString)
    [parse(Int, last(line)) for line in eachline(filename)]
end

part1(filename::AbstractString) = part1(loadstartingpositions(filename))

examplepositions = [4, 8]
inputpositions = [6, 7]

function part1(positions::Vector{Int})
    positions = copy(positions)
    scores = [0, 0]
    rolls = 0
    index = 0
    dice = Stateful(sum(p) for p in partition(cycle(1:100), 3))
    while all(<(1000), scores)
        index = offsetmod(index + 1, 2)
        positions[index] = offsetmod(positions[index] + popfirst!(dice), 10)
        scores[index] += positions[index]
        rolls += 3
    end
    rolls * minimum(scores)
end

offsetmod(x, n) = ((x - 1) % n) + 1
