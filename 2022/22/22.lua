#!/usr/bin/env lua
-- usage: 22.lua filename

---@enum Facing
local Facing = {
    [0] = ">",
    [1] = "v",
    [2] = "<",
    [3] = "^",
    [">"] = 0,
    ["v"] = 1,
    ["<"] = 2,
    ["^"] = 3
}

-- load the monkey map

local fp = assert(io.open(arg[1], "r"))

---@type string[]
local monkeymap = {}
for line in fp:lines() do
    if #line == 0 then break end
    table.insert(monkeymap, line)
end

local instructions = coroutine.wrap(function()
    while true do
        local x = fp:read("n")
        if x then coroutine.yield(x) else break end
        x = fp:read(1)
        if x == "L" or x == "R" then coroutine.yield(x) else break end
    end
end)

---@type Facing
local facing = Facing[">"]

---@alias Position { [1]: integer, [2]: integer }
---@type Position
local position = { monkeymap[1]:find("%."), 1 }

---@param x integer
---@param y integer
local function mod1(x, y) return (x + y - 1) % y + 1 end

---@param n integer?
local function step(n)
    n = math.tointeger(n) or 1
    local direction = ({ [0] = { 1, 0 }, { 0, 1 }, { -1, 0 }, { 0, -1 } })[facing]
    local newx, newy = table.unpack(position)
    while n > 0 do
        newx = mod1(newx + direction[1], #monkeymap[1])
        newy = mod1(newy + direction[2], #monkeymap)
        local tile = monkeymap[newy]:byte(newx, newx)
        if tile == 35 --[[#]] then
            return
        elseif tile == 46 --[[.]] then
            n = n - 1
            position[1] = newx
            position[2] = newy
        end
    end
end

for i in instructions do
    if i == "L" then
        facing = (facing + 3) % 4
    elseif i == "R" then
        facing = (facing + 1) % 4
    elseif math.tointeger(i) then
        step(i --[[@as integer]])
    end
end

print(1000 * position[2] + 4 * position[1] + facing)
