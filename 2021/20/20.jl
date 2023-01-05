if basename(pwd()) != "20"
    cd("2021/20")
end
using OffsetArrays

function load20(filename::AbstractString)
    lines = readlines(filename)
    rule = BitSet(i - 1 for (i, c) in enumerate(popfirst!(lines)) if c == '#')
    popfirst!(lines)
    newsize = (length(lines), length(first(lines)))
    rule, OffsetArrays.centered(transpose(reshape([c == '#' for line in lines for c in line], newsize)))
end

function coordhash(A, c, default = 0)
    h = 0
    for y = -1:1
        for x = -1:1
            h <<= 1
            h += get(A, c + CartesianIndex(y, x), default)
        end
    end
    h
end

function next(A::AbstractMatrix, rule::BitSet, default = 0)
    B = OffsetArrays.centered(zeros(Bool, size(A) .+ (2, 2)))
    for c in CartesianIndices(B)
        B[c] = coordhash(A, c, default) in rule
    end
    B
end

function run(filename::AbstractString, times = 2)
    rule, A = load20(filename)
    for i = 1:times
        A = next(A, rule, iseven(i) && (0 in rule))
    end
    count(A)
end
