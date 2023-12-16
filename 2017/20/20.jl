if basename(pwd()) == "aoc"
    cd("2017/20")
end

using StaticArrays: MVector
using StatsBase: countmap

struct Particle
    id::Int
    position::MVector{3}
    velocity::MVector{3}
    acceleration::MVector{3}
end
@accessors Particle

Particle(id::Integer, xs::Vararg{Vector}) = Particle(id, MVector{3}.(xs)...)
function Particle(id::Integer, str::AbstractString)
    Particle(id, (parse.(Int, m) for m in eachmatch(r"(-?\d+),(-?\d+),(-?\d+)", str))...)
end

function advance!(particle::Particle)
    particle.velocity .+= particle.acceleration
    particle.position .+= particle.velocity
    particle
end

manhattan(xs) = sum(abs.(xs))

function sorter(left::Particle, right::Particle)
    lₐ = manhattan(left.acceleration)
    rₐ = manhattan(right.acceleration)
    if lₐ == rₐ
        lᵥ = manhattan(left.velocity)
        rᵥ = manhattan(right.velocity)
        if lᵥ == rᵥ
            manhattan(left.position) <= manhattan(right.position)
        else
            lᵥ < rᵥ
        end
    else
        lₐ < rₐ
    end
end

function loadparticles(filename::AbstractString)
    [Particle(id - 1, line) for (id, line) in enumerate(eachline(filename))]
end

part1(filename::AbstractString) = part1(loadparticles(filename))
part1(particles) = id(first(sort(particles, lt = sorter)))

@assert part1("example.txt") == 0

part2(filename::AbstractString) = part2(loadparticles(filename))

function part2(particles, limit = 25)
    len = length(particles)
    lastchange = 0
    while lastchange <= limit
        foreach(advance!, particles)
        sort!(particles, by = position)
        collisions = sort!(collect(keys(filter!(kv -> kv[2] > 1, countmap(position.(particles))))))
        if isempty(collisions)
            lastchange += 1
        else
            filter!(particle -> !insorted(position(particle), collisions), particles)
            diff = len - length(particles)
            len -= diff
            println(diff, " particle", isone(diff) ? "" : "s", " destroyed. ", len, " particle", isone(len) ? "" : "s", " remain", isone(len) ? "s" : "", ".")
            lastchange = 0
        end
    end
end
