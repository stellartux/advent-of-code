commands = ["F10", "N3", "F7", "R90", "F11"]
commands = split(strip(read("input.txt", String)), "\n")

mutable struct Position
    x::Int64
    y::Int64
    bearing::Int64
end

pos = Position(0, 0, 90)
commandmap = Dict(
    'N' => v -> pos.y += v,
    'S' => v -> pos.y -= v,
    'W' => v -> pos.x -= v,
    'E' => v -> pos.x += v,
    'R' => v -> pos.bearing = (pos.bearing + v) % 360,
    'L' => v -> pos.bearing = (360 + pos.bearing - v) % 360,
    'F' => v -> commandmap["NESW"[(pos.bearing ÷ 90) + 1]](v))

for command in commands
    commandmap[command[1]](parse(Int, command[2:end]))
    println(pos)
end

print(abs(pos.x) + abs(pos.y)) # 521

ship = Position(0, 0, 0)
waypoint = Position(10, 1, 0)

function rotate(n)
    prev = Position(waypoint.x, waypoint.y, 0)
    if n == 90
        waypoint.x = prev.y
        waypoint.y = -prev.x
    elseif n == 180
        waypoint.x = -waypoint.x
        waypoint.y = -waypoint.y
    elseif n == 270
        waypoint.x = -prev.y
        waypoint.y = prev.x
    end
end

commanddict = Dict(
    'N' => v -> waypoint.y += v,
    'S' => v -> waypoint.y -= v,
    'W' => v -> waypoint.x -= v,
    'E' => v -> waypoint.x += v,
    'R' => rotate,
    'L' => v -> rotate(360 - v),
    'F' => function (v)
    diff = (x = waypoint.x - ship.x,)
    ship.x += waypoint.x * v
    ship.y += waypoint.y * v
end)

for command in commands
    commanddict[command[1]](parse(Int, command[2:end]))
end

print(abs(ship.x) + abs(ship.y))
