#!/usr/bin/env lua
-- usage: lua 14.lua [--time=0.1 [--columns=$COLUMNS] [--lines=$LINES]] filename

string.gsplit = require("ji/string").gsplit
math.sign = require("ji/math").sign

---@class SandReservoir: { [integer]: { [integer]: string } }
---@field min integer[]
---@field max integer[]
---@field activesand integer[]
---@field viewport table
local SandReservoir = setmetatable({
    __index = function(self, y)
        local row = getmetatable(self)[y]
        if row then return row end
        row = {}
        rawset(self, y, row)
        return row
    end
}, {
    __name = "SandReservoir",
    __call = function(self, columns, lines)
        columns = columns or 132
        lines = lines or 40
        return setmetatable({
            min = { math.huge, math.huge },
            max = { -math.huge, -math.huge },
            activesand = { 500, 0 },
            viewport = {
                500 - columns // 2,
                0,
                width = columns,
                height = lines
            },
            done = false
        }, self)
    end
})

local function Floor()
    return setmetatable({}, {
        __index = function() return '#' end
    })
end

local function Coord(s)
    if not s then return end
    local x, y = s:match("(%d+),(%d+)")
    return math.tointeger(x)--[[@as integer]] , math.tointeger(y)
end

local function cmp(l, r)
    return l > r and 1 or l < r and -1 or 0
end

---@param x integer
---@param y integer
---@param v string?
function SandReservoir:push(x, y, v)
    x = assert(math.tointeger(x))
    y = assert(math.tointeger(y))
    if v then
        self.min[1] = math.min(self.min[1], x)
        self.min[2] = math.min(self.min[2], y)
        self.max[1] = math.max(self.max[1], x)
        self.max[2] = math.max(self.max[2], y)
    end
    self[y][x] = v
end

---@param filename string
---@return SandReservoir
function SandReservoir:load(filename, ...)
    local reservoir = SandReservoir(...)
    reservoir:push(500, 0, "+")
    for line in io.lines(filename) do
        local coords = line:gsplit(" -> ")
        local prevx, prevy = Coord(coords())
        local nextx, nexty = Coord(coords())
        while nextx do
            if prevx == nextx then
                for y = prevy, nexty, cmp(nexty, prevy) do
                    reservoir:push(prevx, y, "#")
                end
            else
                for x = prevx, nextx, cmp(nextx, prevx) do
                    reservoir:push(x, prevy, "#")
                end
            end
            prevx, prevy, nextx, nexty = nextx, nexty, Coord(coords())
        end
    end
    return reservoir
end

function SandReservoir:step()
    local x, y = table.unpack(self.activesand)
    local nextx, nexty = x, y + 1
    if self[nexty][nextx] then
        nextx = x - 1
        if self[nexty][nextx] then
            nextx = x + 1
            if self[nexty][nextx] then
                if x == 500 and y == 0 then
                    self[y][x] = "o"
                    self.done = true
                    return "blocked"
                end
                self.activesand[1] = 500
                self.activesand[2] = 0
                return "at rest"
            end
        end
    end
    self[y][x] = self[y][x] == "+" and "+" or nil
    if nexty > self.max[2] + 2 then
        self.activesand[1] = 500
        self.activesand[2] = 0
        return "fell off"
    end
    self[nexty][nextx] = "o"
    self.activesand[1] = nextx
    self.activesand[2] = nexty
    return "falling"
end

function SandReservoir:play(time)
    io.write("\27[s\27[?1049h\27[?25l")
    repeat
        if time then
            self:show()
            os.execute("sleep " .. time)
        end
        local s = self:step()
    until s == "fell off" or s == "blocked"
    io.write("\27[?1049l\27[u\27[?25h")
end

function SandReservoir:show()
    local x, y = table.unpack(self.activesand)
    local vx, vy = table.unpack(self.viewport)
    local width = self.viewport.width
    local height = self.viewport.height
    local d = math.abs(vx - x)
    if d > 4 * width // 5 then
        vx = x - width // 2
        self.viewport[1] = vx
    end
    d = math.abs(vy - y)
    if d > 4 * height // 5 then
        vy = math.max(0, y - height // 4)
        self.viewport[2] = vy
    end
    io.write("\27[H")
    for j = vy, vy + height - 2 do
        for i = vx, vx + width - 2 do
            if i == 500 and j == 0 then
                io.write("\27[33m+")
            elseif i == self.activesand[1] and j == self.activesand[2] then
                io.write("\27[1\27[93mo\27[0m")
            else
                local c = self[j][i]
                if c == "#" then
                    io.write("\27[38;2;200;80;40m#")
                elseif c == "o" then
                    io.write("\27[38;2;150;140;20mo")
                else
                    io.write("\27[30m.")
                end
            end
        end
        io.write("\n")
    end
    io.write("\27[0m\n")
end

function SandReservoir:countsand()
    local total = 0
    for y, row in ipairs(self), self, -1 do
        if y > self.max[2] + 1 then break end
        for _, x in pairs(row) do
            if x == "o" then
                total = total + 1
            end
        end
    end
    return total
end

---@overload fun(filename:string)
local function main(...)
    local time, filename, columns, lines
    for i = 1, select("#", ...) do
        local arg = select(i, ...)
        local flag, value = arg:match("([^=-]+)=(.+)")
        if flag == "columns" then
            columns = assert(math.tointeger(value))
        elseif flag == "lines" then
            lines = assert(math.tointeger(value))
        elseif flag == "time" then
            time = assert(tonumber(value))
        else
            filename = arg
        end
    end
    local r = SandReservoir:load(filename, columns - 4, lines - 4)
    r:play(time)
    print(r:countsand())
    r[r.max[2] + 2] = Floor()
    r:play(time)
    print(r:countsand())
end

if pcall(debug.getlocal, 4, 1) then
    return SandReservoir
else
    main(...)
end
