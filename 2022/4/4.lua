#!/usr/bin/env lua

local partone, parttwo = 0, 0
for line in io.lines(...) do
    local a, b, x, y = line:match("(%d+)-(%d+),(%d+)-(%d+)")
    if not a then break end
    a = math.tointeger(a) --[[@as integer]]
    b = math.tointeger(b) --[[@as integer]]
    x = math.tointeger(x) --[[@as integer]]
    y = math.tointeger(y) --[[@as integer]]
    if a <= x and b >= y or x <= a and y >= b then
        partone = partone + 1
    end
    if a <= x and b >= x or x <= a and y >= a then
        parttwo = parttwo + 1
    end
end
print(partone, parttwo)
