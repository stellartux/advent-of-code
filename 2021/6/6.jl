loadfish(filename)::Vector{Int} = parse.(Int, split(readchomp(filename), ','))

function oneday(fish)
    newfish = count(==(0), fish)
    map!(f -> iszero(f) ? 6 : f - 1, fish, fish)
    append!(fish, repeat([8], newfish))
end

function ndays(fish, n)
    for _ in 1:n
        oneday(fish)
    end
    fish
end

part1(fish::Vector{Int}) = length(ndays(fish, 80))

if basename(pwd()) == "aoc"
    cd("2021/6")
end
examplefish = loadfish("example.txt")
inputfish = loadfish("input.txt")

println("Part 1 answer: ", part1(inputfish))

