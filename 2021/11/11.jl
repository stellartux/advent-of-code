function loadoctopodes(filename::AbstractString)
    lines = readlines(filename)
    transpose(reshape(
        [parse(Int, c) for line in lines for c in line],
        (length(lines), length(first(lines)))
    ))
end

if basename(pwd()) == "aoc"
    cd("2021/11")
end

example = loadoctopodes("example.txt")
input = loadoctopodes("input.txt")

function neighbourcoords(A, c)
    (
        c + off
        for off in (CartesianIndices((-1:1, -1:1)))
        if !iszero(off) && (c + off) in CartesianIndices(A)
    )
end

function part1(octos, n::Integer = 1)
    octos = copy(octos)
    flashes = 0
    while n > 0
        # First, the energy level of each octopus increases by 1.
        octos .+= 1

        # Then, any octopus with an energy level greater than 9 flashes.
        # This increases the energy level of all adjacent octopuses by 1,
        # including octopuses that are diagonally adjacent. If this causes
        # an octopus to have an energy level greater than 9, it also flashes.
        # This process continues as long as new octopuses keep having their
        # energy level increased beyond 9. (An octopus can only flash at most
        # once per step.)
        canflash = ones(Bool, size(octos))
        while any(>(9), octos[canflash])
            coord = findfirst(i -> canflash[i] && octos[i] > 9, CartesianIndices(octos))
            octos[collect(neighbourcoords(octos, coord))] .+= 1
            canflash[coord] = false
            flashes += 1
        end

        # Finally, any octopus that flashed during this step has its energy
        # level set to 0, as it used all of its energy to flash.
        octos .*= canflash

        n -= 1
    end

    octos, flashes
end


function part2(octos)
    octos = copy(octos)
    n = 0
    while true
        n += 1

        # First, the energy level of each octopus increases by 1.
        octos .+= 1

        # Then, any octopus with an energy level greater than 9 flashes.
        # This increases the energy level of all adjacent octopuses by 1,
        # including octopuses that are diagonally adjacent. If this causes
        # an octopus to have an energy level greater than 9, it also flashes.
        # This process continues as long as new octopuses keep having their
        # energy level increased beyond 9. (An octopus can only flash at most
        # once per step.)
        canflash = ones(Bool, size(octos))
        while any(>(9), octos[canflash])
            coord = findfirst(i -> canflash[i] && octos[i] > 9, CartesianIndices(octos))
            octos[collect(neighbourcoords(octos, coord))] .+= 1
            canflash[coord] = false
        end

        # Finally, any octopus that flashed during this step has its energy
        # level set to 0, as it used all of its energy to flash.
        octos .*= canflash

        if !any(canflash)
            return n
        end
    end
end

println("Part 1: ", part1(input, 100))
println("Part 2: ", part2(input))
