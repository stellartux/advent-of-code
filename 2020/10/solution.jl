chargers = sort(parse.(Int, split(strip(read("input.txt", String)), "\n")))

diffs(chargers) = map(t -> t[2] - t[1], zip(chargers[1:end - 1], chargers[2:end]))

joltages = [0; chargers; chargers[end] + 3]
differences = diffs(joltages)

count(==(1), differences) * count(==(3), differences)

examplechargers = [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]
examplediffs = [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3]
exampleanswer = 8

otherexample = [0, 1, 2, 3, 4, 7, 8, 9, 10, 11, 14, 17, 18, 19, 20, 23, 24, 25, 28, 31,
32, 33, 34, 35, 38, 39, 42, 45, 46, 47, 48, 49, 52]
otherdiffs = [1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 3, 1, 1, 1, 3, 1, 1, 3, 3, 1, 1, 1, 1, 3, 1, 3, 3, 1, 1, 1, 1, 3]
otheranswer = 19208

"""Splits a full list of differences into sublists for easier calculation"""
function calculatebranches(diffs)
    iter = Iterators.Stateful(diffs)
    arglist = []
    while !isempty(iter)
        args = [Iterators.takewhile(<(3), iter)...]
        push!(arglist, args)
        Iterators.takewhile(==(3), iter)
    end
    mapreduce(calculatesubbranch, *, filter(!isempty, arglist))
end

"""Takes a sublist of diffs that are less than 3 and calculates the possible permutations of the subbranch"""
function calculatesubbranch(diffs)
    if length(diffs) == 1
        return 1
    elseif length(diffs) == 2
        return 2
    elseif length(diffs) == 3
        return 4
    elseif length(diffs) == 4
        return 7
    end
end

@assert calculatebranches(examplediffs) == exampleanswer
@assert calculatebranches(otherdiffs) == otheranswer
calculatebranches(differences) # holy shit it actually works
