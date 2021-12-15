if basename(pwd()) == "aoc"
    cd("2021/14")
end
using Pkg
Pkg.activate(".")
using StatsBase: countmap
using Base.Iterators

function loadfile(filename::AbstractString)
    file = readchomp(filename)
    target, ps = split(file, r"\r?\n\r?\n")
    rules = Dict(a => first(b) for (a, b) in split.(split(ps, r"\r?\n"), " -> "))
    target, rules
end

"Returns an iterator of each overlapping pair of characters in a string."
charpairs(str::AbstractString) = (view(str, i-1:i) for i in drop(eachindex(str), 1))

function part1(filename::AbstractString)
    target, rules = loadfile(filename)
    for _ = 1:10
        result = ""
        for (a, b) in charpairs(target)
            result *= a
            result *= rules[a*b]
        end
        result *= target[end]
        target = result
    end
    cm = values(countmap(target))
    maximum(cm) - minimum(cm)
end

function part2(filename::AbstractString, n::Integer)
    target, oldrules = loadfile(filename)
    rules = Dict{String,Tuple{String,String}}(
        k => (first(k) * v, v * last(k)) for (k, v) in pairs(oldrules)
    )
    counts = Dict{String,Int}()
    for cs in charpairs(target)
        counts[cs] = get(counts, cs, 0) + 1
    end
    for _ = 1:n
        oldcounts = counts
        counts = empty(counts)
        for (k, v) in pairs(oldcounts)
            if k in keys(rules) && !iszero(v)
                for o in rules[k]
                    counts[o] = get(counts, o, 0) + v
                end
            end
        end
    end
    charcounts = Dict{Char,Int}((
        first(target) => 1,
        last(target) => 1
    ))
    for (lr, v) in pairs(counts)
        l, r = lr
        charcounts[l] = get(charcounts, l, 0) + v
        charcounts[r] = get(charcounts, r, 0) + v
    end
    map!(v -> v ÷ 2, values(charcounts))
    maximum(values(charcounts)) - minimum(values(charcounts))
end
