if basename(pwd()) == "aoc"
    cd("2017/2")
end

function part1(filename::AbstractString)
    sum(-(reverse(extrema(parse.(Int, split(line))))...) for line in eachline(filename))
end

@assert part1("example.txt") == 18
part1("input.txt")

findround(str::AbstractString) = findround(parse.(Int, split(str)))

function findround(xs::Vector{Int})
    for i in firstindex(xs):lastindex(xs)-1
        for j in i+1:lastindex(xs)
            lo, hi = minmax(xs[i], xs[j])
            if isinteger(hi / lo)
                return hi ÷ lo
            end
        end
    end
end

part2(filename::AbstractString) = sum(findround.(eachline(filename)))

@assert part2("example2.txt") == 9
part2("input.txt")
