#!/usr/bin/env lua

local function forever(fn, t, k)
    local i = k
    return function()
        local v
        k, v = fn(t, k)
        if not k then
            k, v = fn(t, i)
        end
        return k, v
    end
end

---@class Coord17: integer[]
---@operator call(integer[]?): Coord17
---@operator add(Coord17): Coord17
local Coord17 = setmetatable({
    __name = "Coord"
}, {
    __call = function(self, t)
        return setmetatable(t, self)
    end
})
Coord17.__index = Coord17

function Coord17:__add(other)
    local t = {}
    for i = 1, #self do
        t[i] = self[i] + other[i]
    end
    return Coord17(t)
end

local function rangenext(n, i)
    i = i + 1
    if i <= n then
        return i, i
    end
end

--- Count from 1 to n.
---@param n integer
---@returns function, integer, integer
local function range(n)
    return rangenext, n, 0
end

local nextrocktype = forever(range(5))

---@class Rock: Coord
---@field height integer
---@field offsets Coord[]
---@operator call(): Rock
local Rock = setmetatable({
    __name = "Rock",
    offsets = {
        { Coord { 0, 0 }, Coord { 1, 0 }, Coord { 2, 0 }, Coord { 3, 0 } },
        { Coord { 1, 0 }, Coord { 0, -1 }, Coord { 1, -1 }, Coord { 2, -1 }, Coord { 1, -2 } },
        { Coord { 2, 0 }, Coord { 2, -1 }, Coord { 0, -2 }, Coord { 1, -2 }, Coord { 2, -2 } },
        { Coord { 0, 0 }, Coord { 0, -1 }, Coord { 0, -2 }, Coord { 0, -3 } },
        { Coord { 0, 0 }, Coord { 1, 0 }, Coord { 0, -1 }, Coord { 1, -1 } }
    },
    height = { 1, 3, 3, 4, 2 }
}, {
    __call = function(self, t)
        local rock = t or { 4, math.huge }
        local type = nextrocktype()
        rock.offsets = self.offsets[type]
        rock.height = self.height[type]
        rock[2] = rock[2] + rock.height
        return setmetatable(rock, self)
    end,
    __index = Coord
})
Rock.__index = Rock

---@param rocks Rocks
---@param position Coord?
function Rock:iscolliding(rocks, position)
    position = position or self
    for _, offset in ipairs(self.offsets) do
        local coord = position + offset
        if coord[1] < 1 or coord[1] > 7 or coord[2] < 1 or
            rocks[coord[2]][coord[1]] ~= "." then
            return true
        end
    end
    return false
end

---@param rocks Rocks
function Rock:lock(rocks)
    for _, offset in ipairs(self.offsets) do
        local coord = offset + self
        rocks[coord[2]][coord[1]] = '#'
    end
    rocks.height = math.max(rocks.height, self[2])
    return self
end

---@class Rocks: { [integer]: { [integer]: any } }
---@operator call(table?): Rocks
---@field height integer
---@field width integer
---@field nextrock fun(): string
---@field nextjet fun(): integer
local Rocks = setmetatable({
    __name = "Rocks"
}, {
    __call = function(self, s)
        return setmetatable({
            height = 0,
            width = 7,
            nextjet = forever(utf8.codes(s))
        }, self)
    end
})

function Rocks:__index(k)
    if math.tointeger(k) and k > 0 then
        local t = { '.', '.', '.', '.', '.', '.', '.' }
        rawset(self, k, t)
        return t
    else
        return Rocks[k]
    end
end

function Rocks:has(coord)
    return coord[1] > 0 and coord[1] <= self.width and coord[2] > 0
end

function Rocks:droprock()
    local rock = Rock { 3, self.height + 4 }
    local left, right, down = Coord { -1, 0 }, Coord { 1, 0 }, Coord { 0, -1 }
    repeat
        rock[2] = rock[2] - 1
        local lr = select(2, self.nextjet()) == 60 and left or right
        if not rock:iscolliding(self, lr + rock) then
            rock[1] = rock[1] + lr[1]
        end
    until rock:iscolliding(self, down + rock)
    rock:lock(self)
    return self
end

function Rocks:__tostring()
    local result = {}
    for y = self.height + 4, 1, -1 do
        io.write("|")
        io.write(table.concat(self[y]))
        print("|")
    end
    table.insert(result, "+-------+")
    return table.concat(result, '\n')
end

function Rocks:iterate(start, stop)
    for _ = start, stop do self:droprock() end
    return self.height
end

---@param ... string
local function main(...)
    local filename = select(-1, ...)
    local rocks = Rocks(io.lines(filename)())
    print(rocks:iterate(1, 2022))
    -- print(rocks:iterate(2033, 1000000000000))
end

if pcall(debug.getlocal, 4, 1) then
    return Rocks
else
    main(...)
end
