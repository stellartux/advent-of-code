if basename(pwd()) == "aoc"
    cd("2017/22")
end

function loadgrid(filename::AbstractString)
    lines = readlines(filename)
    offset = CartesianIndex(cld.(length.((first(lines), lines)), 2)...)
    Dict{CartesianIndex{2},Char}(
        CartesianIndex(x, y) - offset => char
        for (y, line) in enumerate(lines)
        for (x, char) in enumerate(line)
        if char != '.'
    )
end

directions = (
    up = CartesianIndex(0, -1),
    right = CartesianIndex(1, 0),
    down = CartesianIndex(0, 1),
    left = CartesianIndex(-1, 0)
)

mutable struct VirusCarrier
    position::CartesianIndex
    direction::CartesianIndex
end
"The virus carrier begins in the middle of the map facing up."
VirusCarrier() = VirusCarrier(CartesianIndex(0, 0), directions.up)

function turnleft!(carrier::VirusCarrier)
    carrier.direction =
        if carrier.direction == directions.up
            directions.left
        elseif carrier.direction == directions.right
            directions.up
        elseif carrier.direction == directions.down
            directions.right
        else
            directions.down
        end
    carrier
end

function turnright!(carrier::VirusCarrier)
    carrier.direction =
        if carrier.direction == directions.up
            directions.right
        elseif carrier.direction == directions.right
            directions.down
        elseif carrier.direction == directions.down
            directions.left
        else
            directions.up
        end
    carrier
end

forward!(carrier::VirusCarrier) = carrier.position += carrier.direction

function advance!(grid, virus::VirusCarrier)
    if haskey(grid, virus.position)
        turnright!(virus)
        delete!(grid, virus.position)
        forward!(virus)
        false
    else
        turnleft!(virus)
        grid[virus.position] = '#'
        forward!(virus)
        true
    end
end

part1(filename::AbstractString, len = 10000) = part1(loadgrid(filename), len)
function part1(grid::Dict{CartesianIndex{2},Char}, len = 10000)
    virus = VirusCarrier()
    count(advance!(grid, virus) for _ in 1:len)
end

@assert part1("example.txt", 70) == 41
@assert part1("example.txt", 10000) == 5587
part1("input.txt", 10000)

function turnaround!(carrier::VirusCarrier)
    carrier.direction =
        if carrier.direction == directions.up
            directions.down
        elseif carrier.direction == directions.right
            directions.left
        elseif carrier.direction == directions.down
            directions.up
        else
            directions.right
        end
    carrier
end

function advanceevolved!(grid, virus::VirusCarrier)
    if !haskey(grid, virus.position)
        turnleft!(virus)
        grid[virus.position] = 'W'
        forward!(virus)
        false
    elseif grid[virus.position] == 'W'
        grid[virus.position] = '#'
        forward!(virus)
        true
    elseif grid[virus.position] == '#'
        turnright!(virus)
        grid[virus.position] = 'F'
        forward!(virus)
        false
    elseif grid[virus.position] == 'F'
        turnaround!(virus)
        delete!(grid, virus.position)
        forward!(virus)
        false
    end
end

part2(filename::AbstractString, len = 10000000) = part2(loadgrid(filename), len)
function part2(grid::Dict{CartesianIndex{2},Char}, len = 10000000)
    virus = VirusCarrier()
    count(advanceevolved!(grid, virus) for _ in 1:len)
end

@assert part2("example.txt", 100) == 26
@assert part2("example.txt") == 2511944
@time part2("input.txt")
