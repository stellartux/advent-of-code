#!/usr/bin/env lua
-- usage: lua 12.lua [--verbose] filename

---@class Heightmap: { [integer]: { [integer]: { height: integer, steps: integer, rsteps: integer } }}
---@field start integer[]
---@field stop integer[]
local Heightmap = setmetatable({}, {
    __name = "Heightmap",
    ---@param s string|function string literal or lines iterator
    ---@return Heightmap
    __call = function(self, s, x, y)
        local heightmap = {}
        x, y = 0, 0
        for line in type(s) == "string" and s:gmatch("%S+") or s do
            y = y + 1
            local row = {}
            for c in line:gmatch(".") do
                x = x + 1
                local height = c:byte() - 96
                if c == "S" then
                    height = 1
                    heightmap.start = { x, y }
                elseif c == "E" then
                    height = 26
                    heightmap.stop = { x, y }
                end
                table.insert(row, {
                    height = height,
                    steps = math.huge,
                    rsteps = math.huge
                })
            end
            x = 0
            table.insert(heightmap, row)
        end
        return setmetatable(heightmap, self)
    end
})
Heightmap.__index = Heightmap

function Heightmap:load(filename)
    return self(io.lines(filename))
end

function Heightmap:neighbours(x, y)
    local width = #self[y]
    local height = #self
    return function(t)
        local ix, iy = 0, 0
        while ix == 0 or iy == 0 or ix > width or iy > height do
            if #t == 0 then return end
            ix, iy = table.unpack(table.remove(t))
        end
        return ix, iy
    end, { { x - 1, y }, { x + 1, y }, { x, y - 1 }, { x, y + 1 } }
end

function Heightmap:flood(x, y, steps)
    x = x or self.start[1]
    y = y or self.start[2]
    steps = steps or 0
    local here = self[y][x]
    if steps < here.steps then
        here.steps = steps
        for nx, ny in self:neighbours(x, y) do
            if here.height + 1 >= self[ny][nx].height then
                self:flood(nx, ny, steps + 1)
            end
        end
    end
end

function Heightmap:rflood(x, y, steps)
    x = x or self.stop[1]
    y = y or self.stop[2]
    steps = steps or 0
    local here = self[y][x]
    if steps < here.rsteps then
        here.rsteps = steps
        for nx, ny in self:neighbours(x, y) do
            if here.height ~= 1 and self[ny][nx].height + 1 >= here.height then
                self:rflood(nx, ny, steps + 1)
            end
        end
    end
end

function Heightmap:maxsteps()
    return self[self.stop[2]][self.stop[1]].steps
end

function Heightmap:minrsteps()
    local min, i, j = math.huge, -1, -1
    for y, row in ipairs(self) do
        for x, hill in ipairs(row) do
            if hill.height == 1 and hill.rsteps < min then
                min = hill.rsteps
                i = x
                j = y
            end
        end
    end
    return min, i, j
end

function Heightmap:show()
    local heights, steps, rsteps = {}, {}, {}
    print("\nHeightmap\n")
    for y, row in ipairs(self) do
        local heightsrow, stepsrow, rstepsrow = {}, {}, {}
        for x, hill in ipairs(row) do
            table.insert(heightsrow,
                x == self.start[1] and y == self.start[2] and "S"
                or x == self.stop[1] and y == self.stop[2] and "E"
                or string.char(hill.height + 96))
            table.insert(stepsrow, #stepsrow + 1, hill.steps)
            table.insert(rstepsrow, #rstepsrow + 1, hill.rsteps)
        end
        table.insert(heights, table.concat(heightsrow))
        table.insert(steps, table.concat(stepsrow, "\t"))
        table.insert(rsteps, table.concat(rstepsrow, "\t"))
    end
    print(table.concat(heights, "\n"))
    print()
    print(table.concat(steps, "\n"))
    print()
    print(table.concat(rsteps, "\n"))
    print()
end

---@param filename string
local function main(verbose, filename)
    if not filename then
        filename = verbose
    end
    local heightmap = Heightmap:load(filename)
    heightmap:flood()
    heightmap:rflood()
    heightmap:show()
    print(heightmap:maxsteps())
    print(heightmap:minrsteps())
end

if pcall(debug.getlocal, 4, 1) then
    return Heightmap
else
    main(...)
end
