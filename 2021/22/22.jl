if basename(pwd()) == "aoc"
    cd("2021/22")
end
using Pkg
Pkg.activate(".")
using OffsetArrays

function loadreactor(filename::AbstractString)
    A = OffsetArray(zeros(Bool, (101, 101, 101)), -50:50, -50:50, -50:50)
    rng(a, b) = max(-50, a):min(50, b)
    for line in eachline(filename)
        state, ns... = match(r"(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)", line)
        x₁, x₂, y₁, y₂, z₁, z₂ = parse.(Int, ns)
        @inbounds A[rng(z₁, z₂), rng(y₁, y₂), rng(x₁, x₂)] .= state == "on"
    end
    A
end

part1(filename::AbstractString) = count(loadreactor(filename))

@assert part1("example.txt") == 39
@assert part1("example2.txt") == 590784
