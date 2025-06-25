#!/usr/bin/env julia
# usage: julia 15.jl [FILENAME]

module AOC2024Day15

function load(file::AbstractString)
    lines = eachline(file)
    stack(Iterators.takewhile(!isempty, lines); dims=1), join(lines)
end

gps(coord::CartesianIndex{2}) = 100(coord[1] - 1) + coord[2] - 1

const direction = Dict{Char,CartesianIndex{2}}(
    '^' => CartesianIndex(-1, 0),
    'v' => CartesianIndex(1, 0),
    '<' => CartesianIndex(0, -1),
    '>' => CartesianIndex(0, 1))

"For checking pushing boxes vertically where pushing may ripple"
function canpushboxvertical(grid, here, dir)
    if grid[here] == '['
        canpushboxvertical(grid, here + dir, dir) && canpushboxvertical(grid, here + dir + direction['>'], dir)
    elseif grid[here] == ']'
        canpushboxvertical(grid, here + dir, dir) && canpushboxvertical(grid, here + dir + direction['<'], dir)
    else
        grid[here] == '.'
    end
end

function pushbox!(grid, here, dir)
    if grid[here] == ']'
        here = here + direction['<']
    end
    there = here + dir
    if grid[here] == 'O'
        pushbox!(grid, there, dir)
        if grid[there] == '.'
            grid[here], grid[there] = grid[there], grid[here]
        end
    elseif grid[here] == '['
        if dir == direction['<']
            yonder = here + direction['>']
            pushbox!(grid, there, dir)
            if grid[there] == '.'
                grid[there], grid[here], grid[yonder] = '[', ']', '.'
            end
        elseif dir == direction['>']
            yonder = there + dir
            pushbox!(grid, yonder, dir)
            if grid[yonder] == '.'
                grid[here], grid[there], grid[yonder] = '.', '[', ']'
            end
        elseif canpushboxvertical(grid, here, dir)
            tail = here + direction['>']
            yonder = there + direction['>']
            pushbox!(grid, there, dir)
            pushbox!(grid, yonder, dir)
            if grid[there] == '.' && grid[yonder] == '.'
                grid[there], grid[yonder] = '[', ']'
                grid[here], grid[tail] = '.', '.'
            end
        end
    end
    grid
end

function debug(grid, instruction=nothing)
    """$(isnothing(instruction) ? "Initial state" : "Move $(instruction)"):
    $(join(join.(eachrow(grid)), '\n'))
    """
end

function walk!(grid::AbstractMatrix, path::AbstractString)
    @debug debug(grid)
    here = findfirst(==('@'), grid)
    for instruction in path
        dir = direction[instruction]
        there = here + dir
        pushbox!(grid, there, dir)
        if grid[there] == '.'
            grid[here], grid[there] = grid[there], grid[here]
            here = there
        end
        @debug debug(grid, instruction)
    end
    grid
end

function transformgrid(grid::AbstractMatrix)
    stack(join(Iterators.flatten(
            if c == '#'
                "##"
            elseif c == '.'
                ".."
            elseif c == 'O'
                "[]"
            elseif c == '@'
                "@."
            end
            for c in row
        )) for row in eachrow(grid); dims=1)
end

partone((grid, path)) = sum(gps, findall(in("O["), walk!(grid, path)))
parttwo((grid, path)) = partone((transformgrid(grid), path))

if abspath(PROGRAM_FILE) == @__FILE__
    input = get(ARGS, 1, joinpath(@__DIR__, "input.txt"))
    println(partone(input))
    println(parttwo(input))
elseif isinteractive()
    using REPL
    using Logging
    REPL.activate(AOC2024Day15)
    debug_logger = ConsoleLogger(stderr, Logging.Debug)
end

end # module
