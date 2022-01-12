if basename(pwd()) == "aoc"
    cd("2017/21")
end

struct RuleDict
    rules2to3::Dict{BitMatrix,BitMatrix}
    rules3to4::Dict{BitMatrix,BitMatrix}
end
RuleDict() = RuleDict(Dict{BitMatrix,BitMatrix}(), Dict{BitMatrix,BitMatrix}())

function Base.getindex(rules::RuleDict, index::BitMatrix)
    dims = size(index)
    if dims == (2, 2)
        rules.rules2to3[index]
    elseif dims == (3, 3)
        rules.rules3to4[index]
    else
        throw(KeyError(index))
    end
end

Base.isempty(rd::RuleDict) = isempty(rd.rules2to3) && isempty(rd.rules3to4)
Base.length(rd::RuleDict) = length(rd.rules2to3) + length(rd.rules3to4)

function Base.setindex!(rules::RuleDict, value::BitMatrix, index::BitMatrix)
    push!(
        if size(index) == (2, 2) && size(value) == (3, 3)
            rules.rules2to3
        elseif size(index) == (3, 3) && size(value) == (4, 4)
            rules.rules3to4
        else
            throw(ArgumentError("Can't set a pair of $(summary(index)) => $(summary(value))"))
        end,
        (
            f(g(index)) => value
            for f in (identity, rotr90, rot180, rotl90),
            g in (identity, x -> reverse(x; dims = 1))
        )...
    )
    value
end

function Base.iterate(rd::RuleDict, iter = Iterators.Stateful(Iterators.flatten((rd.rules2to3, rd.rules3to4))))
    (popfirst!(iter), iter)
end
Base.eltype(::Type{RuleDict}) = Pair{BitMatrix,BitMatrix}

function parsebitmatrix(str::AbstractString)
    BitMatrix(permutedims(hcat(([c == '#' ? true : false for c in s] for s in split(str, '/'))...)))
end

glider() = BitMatrix([0 1 0; 0 0 1; 1 1 1])

@assert parsebitmatrix("../.#") == BitMatrix([0 0; 0 1])
@assert parsebitmatrix(".#./..#/###") == glider()
@assert parsebitmatrix("#..#/..../#..#/.##.") == [
    1 0 0 1
    0 0 0 0
    1 0 0 1
    0 1 1 0
]

function loadrules(filename::AbstractString)
    rules = RuleDict()
    for line in eachline(filename)
        left, right = split(line, " => ")
        rules[parsebitmatrix(left)] = parsebitmatrix(right)
    end
    rules
end

function hashdot(bits::BitMatrix)
    '\n' * join((join(bit ? '#' : '.' for bit in row) for row in eachrow(bits)), '\n')
end

function advance(rules::RuleDict, bits::BitMatrix)
    offset = 1 + isodd(size(bits, 1))
    inds = [x:x+offset for x in firstindex(bits):(offset+1):size(bits, 1)]
    hcat(([(rules[bits[ys, xs]] for xs in inds)...;] for ys in inds)...)
end

part1(filename::AbstractString, iters = 5) = part1(loadrules(filename), iters)
function part1(rules::RuleDict, iters = 5)
    bits = glider()
    @debug hashdot(bits)
    for _ in 1:iters
        bits = advance(rules, bits)
        @debug hashdot(bits)
    end
    count(bits)
end

@assert part1("example.txt", 2) == 12
part1("input.txt", 5)
