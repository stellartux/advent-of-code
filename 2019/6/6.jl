if basename(pwd()) == "aoc"
    cd("2019/6")
end

struct Planet
    name
    parent
    children
end
Planet(name) = Planet(name, nothing, Planet[])

function Base.getindex(planet::Planet, name::AbstractString)
    if planet.name == name
        planet
    else
        for child in planet.children
            result = child[name]
            if !isnothing(result)
                return result
            end
        end
    end
end

Base.haskey(planet::Planet, name::AbstractString) = !isnothing(planet[name])

function loadsystem(filename::AbstractString)
    system = Planet("COM")
    lines = readlines(filename)
    while length(lines) > 0
        line = popfirst!(lines)
        parent, child = split(line, ')')
        if haskey(system, parent)
            push!(system[parent].children, Planet(child, system[parent], Planet[]))
        else
            push!(lines, line)
        end
    end
    system
end

function orbits(planet::Planet)
    depth(planet) * length(planet.children) + sum(orbits.(planet.children), init = 0)
end

depth(::Nothing) = 0
depth(planet::Planet) = 1 + depth(planet.parent)

function parents(planet::Planet)
    if isnothing(planet.parent)
        [planet]
    else
        [planet, parents(planet.parent)...]
    end
end

part1(filename::AbstractString) = orbits(loadsystem(filename))
@assert part1("example.txt") == 42

function part2(filename::AbstractString)
    system = loadsystem(filename)
    you = system["YOU"].parent
    santa = system["SAN"].parent
    common = first(intersect(parents(you), parents(santa)))
    depth(you) + depth(santa) - 2 * depth(common)
end

@assert part2("example2.txt") == 4
part2("input.txt")
