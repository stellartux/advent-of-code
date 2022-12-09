#!/usr/bin/env lua
-- usage: lua 9.lua --length=N filename
local count, filename = ...
count = math.tointeger(count:match("%d+"))
local rope = {}
for i = 1, count do rope[i] = { 0, 0 } end
local visited = { ["0,0"] = true }
local function sign(x) return x == 0 and 0 or x > 0 and 1 or -1 end

for line in io.lines(filename) do
    local dir, steps = line:match("([ULRD]) (%d+)")
    for _ = 1, math.tointeger(steps) do
        if dir == "U" then
            rope[1][2] = rope[1][2] - 1
        elseif dir == "L" then
            rope[1][1] = rope[1][1] - 1
        elseif dir == "R" then
            rope[1][1] = rope[1][1] + 1
        else
            rope[1][2] = rope[1][2] + 1
        end
        for i = 2, #rope do
            local dx = rope[i][1] - rope[i - 1][1]
            local dy = rope[i][2] - rope[i - 1][2]
            if math.abs(dx) == 2 or math.abs(dy) == 2 then
                rope[i][1] = rope[i][1] - sign(dx)
                rope[i][2] = rope[i][2] - sign(dy)
                if i == count then
                    visited[table.concat(rope[count], ",")] = true
                end
            else
                break
            end
        end
    end
end
local total = 0
for _ in pairs(visited) do
    total = total + 1
end
print(total)
