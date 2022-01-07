if basename(pwd()) == "aoc"
    cd("2018/10")
end
using OffsetArrays

mutable struct LightPoint
    position::CartesianIndex{2}
    velocity::CartesianIndex{2}
end

struct LightField
    points
    step::Int
end

function Base.size(lightfield::LightField)
    Tuple(-(reverse(extrema(getfield.(lightfield.points, :position)))...))
end

Base.length(lightfield::LightField) = prod(size(lightfield))

function Base.in(position::CartesianIndex, lightfield::LightField)
    any(point.position == position for point in lightfield.points)
end

function loadpoints(filename::AbstractString)
    LightField([
            begin
                a, b, c, d = parse.(Int, m.match for m in eachmatch(r"-?\d+", line))
                LightPoint(CartesianIndex(a, b), CartesianIndex(c, d))
            end
            for line in eachline(filename)
        ], 0)
end
lightfield = loadpoints("example.txt")

function step!(lightfield::LightField; rev = false)
    foreach(rev ? point -> point.position -= point.velocity : point -> point.position += point.velocity, lightfield.points)
    lightfield
end

function Base.show(io::IO, lightfield::LightField)
    println("LightField with ", length(lightfield.points), " points:")
    for col in eachcol(range(extrema(getfield.(lightfield.points, :position))...))
        println(io, "  ", (p in lightfield ? '#' : ' ' for p in col)...)
    end
end

part1(filename::AbstractString) = part1(loadpoints(filename))
function part1(lightfield::LightField)
    len = length(lightfield)
    minlen = len + 1
    steps = -1
    while minlen > len
        minlen = len
        len = length(step!(lightfield))
        steps += 1
    end
    step!(lightfield, rev = true), steps
end
