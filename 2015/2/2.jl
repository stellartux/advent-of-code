part1(filename::AbstractString) = sum(area(line) for line in eachline(filename))

function area(line::AbstractString)
    x, y, z = parse.(Int, split(line, 'x'))
    sides = [x * y, y * z, x * z]
    2 * sum(sides) + minimum(sides)
end

@assert area("2x3x4") == 58
@assert area("1x1x10") == 43

part2(filename::AbstractString) = sum(ribbonlength(line) for line in eachline(filename))

function ribbonlength(line::AbstractString)
    dims = parse.(Int, split(line, 'x'))
    2 * sum(dims) - 2 * maximum(dims) + prod(dims)
end

@assert ribbonlength("2x3x4") == 34
@assert ribbonlength("1x1x10") == 14
