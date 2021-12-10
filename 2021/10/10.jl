function part1(filename::AbstractString)
    sum((scoreline(line) for line in eachline(filename)), init = 0)
end

scoreline(line::AbstractString) = scoreline(lastunmatched(line))
scoreline(_) = 0
scoreline(c::Char) = if c in ')'
    3
elseif c == ']'
    57
elseif c == '}'
    1197
elseif c == '>'
    25137
else
    0
end

partnerof(c::Char) = ")(][}{><"[findfirst(c, "()[]{}<>")]

"""
Returns one of:
- `c::Char` an unmatched right bracket
- `cs::Vector{Char}` the unmatched left brackets
- `nothing` brackets are matched
"""
function lastunmatched(line::AbstractString)
    stack = Char[]
    for c in line
        if c in "{[(<"
            push!(stack, c)
        elseif c in "}])>"
            if pop!(stack) != partnerof(c)
                return c
            end
        end
    end
    if !isempty(stack)
        stack
    end
end

if basename(pwd()) == "aoc"
    cd("2021/10")
end
@assert part1("example.txt") == 26397
part1("input.txt")

function part2(filename::AbstractString)
    scores = autocompletescores.(
        filter!(
            Base.Fix2(isa, Vector{Char}),
            lastunmatched.(readlines(filename))
        )
    )
    sort!(scores)[1 + length(scores) ÷ 2]
end

function autocompletescores(chars)
    mapfoldr(c -> findfirst(c, "([{<"), (a, b) -> a + 5b, chars, init=0)
end
