if basename(pwd()) == "aoc"
    cd("2019/12")
end

struct Moon
    position::Vector{Int}
    velocity::Vector{Int}
end

function Moon(str::AbstractString)
    halfmatch = r"<x\s*=\s*(-?\d+)\s*,\s*y\s*=\s*(-?\d+)\s*,\s*z\s*=\s*(-?\d+)\s*>"
    fullmatch = r"pos\s*=\s*" * halfmatch * r",\s*vel\s*=\s*" * halfmatch
    if occursin(fullmatch, str)
        Moon(collect.(Iterators.partition(parse.(Int, match(fullmatch, str)), 3)))
    elseif occursin(halfmatch, str)
        Moon(parse.(Int, match(halfmatch, str)), [0, 0, 0])
    else
        throw(ArgumentError("Could not parse string as a Moon: $(str)"))
    end
end

potentialenergy(moon::Moon) = sum(abs.(moon.position))
kineticenergy(moon::Moon) = sum(abs.(moon.velocity))
energy(moon::Moon) = potentialenergy(moon) * kineticenergy(moon)

function prettyprint(moon::Moon)
    println(
        "pos=<x=", lpad(moon.position[1], 3),
        ", y=", lpad(moon.position[2], 3),
        ", z=", lpad(moon.position[3], 3),
        ">, vel=<x=", lpad(moon.velocity[1], 3),
        ", y=", lpad(moon.velocity[2], 3),
        ", z=", lpad(moon.velocity[3], 3),
        ">"
    )
end

struct MoonSystem
    moons::Vector{Moon}
end

Base.iterate(system::MoonSystem, _ = nothing) = (velocity!(gravity!(system)), nothing)

function loadsystem(filename::AbstractString)
    MoonSystem([Moon(line) for line in eachline(filename)])
end

function prettyprint(system::MoonSystem)
    prettyprint.(system.moons)
    nothing
end

example = loadsystem("example.txt")

function gravity!(system::MoonSystem)
    for (left, right) in Iterators.product(system.moons, system.moons)
        if left != right
            left.velocity .+= sign.(right.position .- left.position)
        end
    end
    system
end

function velocity!(system::MoonSystem)
    velocity!.(system.moons)
    system
end

function velocity!(moon::Moon)
    moon.position .+= moon.velocity
    moon
end

energy(system::MoonSystem) = sum(energy.(system.moons))

function part1(filename::AbstractString, n; verbose = false)
    system = loadsystem(filename)
    if verbose
        println("After 0 steps:")
        prettyprint(system)
    end
    for (i, system) in Iterators.zip(1:n, system)
        if verbose
            println("\nAfter ", i, " steps:")
            prettyprint(system)
        end
    end
    energy(system)
end

@assert part1("example.txt", 10) == 179
@assert part1("example2.txt", 100) == 1940
part1("input.txt", 1000)
