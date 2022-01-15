if basename(pwd()) == "aoc"
    cd("2016/7")
end

regex = r"\[([^\]]+)\]"

function supportstls(line::AbstractString)
    all(!hasabba(m.match) for m in eachmatch(regex, line)) && hasabba(line)
end

function hasabba(str::AbstractString)
    m = match(r"(.)(.)\2\1", str)
    !isnothing(m) && m[1] != m[2]
end

part1(filename::AbstractString) = count(supportstls, eachline(filename))
@assert part1("example.txt") == 2
part1("input.txt")

function supportssl(line::AbstractString)
    str = replace(line, regex => "")
    any(
        occursin(m[2] * m[1] * m[2], str)
        for section in eachmatch(regex, line)
        for m in eachmatch(r"(.)(.)\1", first(section), overlap = true)
        if m[1] != m[2]
    )
end

part2(filename::AbstractString) = count(supportssl, eachline(filename))
@assert part2("example2.txt") == 3
