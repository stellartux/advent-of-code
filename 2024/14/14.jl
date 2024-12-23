#!/usr/bin/env julia
# usage: julia 14.jl [FILENAME]

module AOC2024Day14

mutable struct Robot
    xpos::Int
    ypos::Int
    xvel::Int
    yvel::Int
end
function Robot(str::AbstractString)
    a, b, c, d = parse.(Int, match(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)", str))
    Robot(a, b, c, d)
end

function load(file::AbstractString)
    width, height = occursin("example", file) ? (11, 7) : (101, 103)
    Robot.(eachline(file)), width, height
end

function step!(robot::Robot, n::Int, width::Int, height::Int)
    robot.xpos = mod(robot.xpos + n * robot.xvel, width)
    robot.ypos = mod(robot.ypos + n * robot.yvel, height)
    robot
end

function safetyfactor(robots::Vector{Robot}, width::Int, height::Int)
    hw = width ÷ 2
    hh = height ÷ 2
    tl = 0
    tr = 0
    bl = 0
    br = 0
    for robot in robots
        if robot.xpos < hw
            if robot.ypos < hh
                tl += 1
            elseif robot.ypos > hh
                bl += 1
            end
        elseif robot.xpos > hw
            if robot.ypos < hh
                tr += 1
            elseif robot.ypos > hh
                br += 1
            end
        end
    end
    tl * tr * bl * br
end

partone((robots, width, height)) =
    safetyfactor(step!.(robots, 100, width, height), width, height)

struct Robullet
    xvel::Int
    yvel::Int
end
Robullet(r::Robot) = Robullet(r.xvel, r.yvel)

struct World
    grid::Matrix{Vector{Robullet}}
end
function World(robots, width::Int, height::Int)
    grid = [Robullet[] for _ in 1:height, __ in 1:width]
    for robot in robots
        push!(grid[robot.ypos+1, robot.xpos+1], Robullet(robot))
    end
    World(grid)
end
Base.size(world::World) = size(world.grid)

function Base.show(io::IO, world::World)
    for row in eachrow(world.grid)
        for pos in row
            len = length(pos)
            print(len == 0 ? '.' : len > 9 ? 'X' : len)
        end
        println(io)
    end
end

function step(world::World, n::Int=1)::World
    height, width = size(world)
    grid = [Robullet[] for _ in 1:height, __ in 1:width]
    for pos in CartesianIndices(size(world))
        for robot in world.grid[pos]
            y = mod1(pos[1] + n * robot.yvel, height)
            x = mod1(pos[2] + n * robot.xvel, width)
            push!(grid[y, x], robot)
        end
    end
    World(grid)
end

function safetyfactor(world::World)
    h, w = size(world)
    hh, hw = h ÷ 2, w ÷ 2
    prod(sum(length, world.grid[CartesianIndices(r)]) for r in (
        (1:hh, 1:hw),
        (hh+2:h, 1:hw),
        (1:hh, hw+2:w),
        (hh+2:h, hw+2:w),
    ))
end

function explore(world::World; skip=0, jump=1)
    if skip > 0
        world = step(world, skip)
    end
    for i in Iterators.countfrom(skip + jump, jump)
        world = step(world, jump)
        println("Step $i:")
        println(world)
        readline()
    end
end

function parttwo()
    # stripes in 99:101:inf
    # stripes in 58:103:inf
    for i in Iterators.countfrom(99, 101)
        if mod(i, 103) == 58
            return i
        end
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    input = load(get(ARGS, 1, joinpath(@__DIR__, "input.txt")))
    println(partone(input))
    println(parttwo())
end

end # module
