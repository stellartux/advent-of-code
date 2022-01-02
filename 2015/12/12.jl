if basename(pwd()) == "aoc"
    cd("2015/12")
end

function part1(filename::AbstractString)
    sum(parse(Int, m.match) for m in eachmatch(r"-?\d+", readchomp(filename)))
end

using JSON
part2(filename::AbstractString) = stripred(JSON.parse(readchomp(filename)))

function stripred(d::AbstractDict)
    xs = values(d)
    if "red" in xs
        0
    else
        stripred(xs)
    end
end
stripred(xs::Union{AbstractArray,Base.ValueIterator}) = sum(stripred(x) for x in xs)
stripred(x::Integer) = x
stripred(::Any) = 0

@assert stripred(JSON.parse("[1,2,3]")) == 6
@assert stripred(JSON.parse("""[1,{"c":"red","b":2},3]""")) == 4
@assert stripred(JSON.parse("""{"d":"red","e":[1,2,3,4],"f":5}""")) == 0
@assert stripred(JSON.parse("""[1,"red",5]""")) == 6
