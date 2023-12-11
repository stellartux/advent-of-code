#!/usr/bin/env julia
# usage: julia 11.jl [--factor=FACTOR] [FILENAME]

module AOC2023Day11

function load(file)
    image = permutedims(splat(hcat)(collect.(eachline(file))))
    image, findall(==('#'), image)
end

function galaxydistances(image, galaxies, factor=2)
    emptycols = [all(==('.'), col) for col in eachcol(image)]
    emptyrows = [all(==('.'), row) for row in eachrow(image)]
    result = 0
    for i = eachindex(galaxies), j = i+1:lastindex(galaxies)
        left = galaxies[i]
        right = galaxies[j]
        minx, maxx = extrema([left[1], right[1]])
        miny, maxy = extrema([left[2], right[2]])
        result += maxx - minx + maxy - miny +
                  (factor - 1) * (count(emptycols[miny:maxy]) + count(emptyrows[minx:maxx]))
    end
    result
end

if abspath(PROGRAM_FILE) == @__FILE__
    factor = 1000000
    if startswith(get(ARGS, 1, ""), "--factor=")
        factor = parse(Int, popfirst!(ARGS)[10:end])
    end
    image, galaxies = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(galaxydistances(image, galaxies))
    println(galaxydistances(image, galaxies, factor))
end

end # module
