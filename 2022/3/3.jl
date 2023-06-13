if pwd() != "3"
    cd("2022/3")
end

priority(c::AbstractChar) = Int(c) - (isuppercase(c) ? 38 : 96)
sumpriorities(xs) = sum(priority(only(intersect(x...))) for x in xs)

function splitinhalf(xs)
    len = length(xs) ÷ 2
    xs[1:len], xs[len+1:end]
end

part1(rucksacks) = sumpriorities(splitinhalf.(rucksacks))
part2(rucksacks) = sumpriorities(Iterators.partition(rucksacks, 3))

if abspath(PROGRAM_FILE) == @__FILE__
    rucksacks = readlines(get(ARGS, 1, "example.txt"))
    println(part1(rucksacks))
    println(part2(rucksacks))
end
