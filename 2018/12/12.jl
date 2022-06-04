if basename(pwd()) == "aoc"
    cd("2018/12")
end

function loadfile(filename::AbstractString)
    init, lines = Iterators.peel(eachline(filename))
    state = init[16:end]
    rules = Dict(l => r for (l, r) in split.(Iterators.drop(lines, 1), " => "))
    state, rules
end

part1(filename::AbstractString, limit=20) = part1(loadfile(filename)..., limit)

function eachslice(str, len=5)
    (join(get(str, j, '.') for j in i:i+len-1) for i in firstindex(str)-len+1:lastindex(str))
end

function nextgeneration(state, rules)
    join(get(rules, slice, '.') for slice in eachslice(state))
end

function part1(state, rules, limit=20)
    firstidx = 0
    for _ in 1:limit
        state = nextgeneration(state, rules)
        firstidx += findfirst('#', state) - 3
        state = strip(state, '.')
    end
    sum(i for (i, c) in zip(Iterators.countfrom(firstidx), state) if c == '#')
end

@assert part1("example.txt") == 325
