if basename(pwd()) == "aoc"
    cd("2017/4")
end

part1(filename::AbstractString) = count(isvalidpassphrase.(eachline(filename)))

function isvalidpassphrase(line::AbstractString)
    words = split(line)
    length(words) == length(unique!(sort!(words)))
end

@assert part1("example.txt") == 2
part1("input.txt")

part2(filename::AbstractString) = count(isvalidpassphrase2.(eachline(filename)))

function isvalidpassphrase2(line::AbstractString)
    words = split(line)
    length(words) == length(unique!(sort!([join(sort!(split(word, ""))) for word in words])))
end

part2("input.txt")
