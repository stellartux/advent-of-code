if basename(pwd()) == "aoc"
    cd("2021/14")
end
using Pkg
Pkg.activate(".")
using StatsBase

function loadfile(filename::AbstractString)
    file = readchomp(filename)
    target, ps = split(file, r"\r?\n\r?\n")
    rules = Dict(a => first(b) for (a, b) in split.(split(ps, r"\r?\n"), " -> "))
    target, rules
end

function doloop(target, rules)
    result = ""
    for (a, b) in zip(target[1:end-1], target[2:end])
        result *= a
        result *= rules[a * b]
    end
    result *= target[end]
    result
end

function part1(filename::AbstractString)
    target, rules = loadfile(filename)
    for _ in 1:10
        target = doloop(target, rules)
    end
    cm = values(countmap(target))
    maximum(cm) - minimum(cm)
end
