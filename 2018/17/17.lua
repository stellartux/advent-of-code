-- usage: lua 17.lua [$COLUMNS $LINES [fps]]

---@class Material: { string: Material }
---@field private _s string
---@field private _i integer
local Material = { __name = "Material" }
Material.__index = Material
local Material_mt = {}

---@param s string
---@param i integer
---@return Material
function Material_mt:__call(s, i)
    return setmetatable({
        _s = assert(tostring(s)),
        _i = assert(math.tointeger(i))
    }, Material)
end

setmetatable(Material, Material_mt)

function Material:__tostring()
    return self._s
end

function Material:tointeger()
    return self._i
end

Material.clay = Material("#", 37)
Material.sand = Material(".", 30)
Material.spring = Material("+", 96)
Material.water = Material("~", 36)
Material.wetsand = Material("|", 34)

---@class Reservoir: Material[][]
---@field min Coord17
---@field max Coord17
---@field columns integer?
---@field lines integer?
---@field view Coord17?
---@field frametime integer?
local Reservoir = { __name = "Reservoir" }
Reservoir.__index = Reservoir

local row_mt = {
    __index = function(self, key)
        return rawget(self, key) or math.tointeger(key) and Material.sand
    end
}
---@param t? { [integer]: Material }
function Row(t)
    return setmetatable(t or {}, row_mt)
end

local mt = {
    __index = function(self, key)
        local t = rawget(self, key) or rawget(getmetatable(self), key)
        if not t and type(key) == "number" then
            t = setmetatable({}, row_mt)
            self[key] = t
        end
        return t
    end
}

---@param filename string
---@param columns integer?
---@param lines integer?
---@param fps integer? defaults to as fast as possible
---@return Reservoir
function mt:__call(filename, columns, lines, fps)
    fps = tonumber(fps)
    local reservoir = setmetatable({
        min = { math.huge, math.huge },
        max = { -math.huge, -math.huge },
        columns = math.tointeger(columns),
        lines = math.tointeger(lines),
        view = columns and lines and { 500, 0 },
        frametime = fps and fps > 0 and 1 / fps > 0.001 and 1 / fps
    }, Reservoir)
    if reservoir.view then
        reservoir.columns = 74 * reservoir.columns // 80
        reservoir.lines = 20 * reservoir.lines // 24
        reservoir.view[1] = reservoir.view[1] - reservoir.columns // 2
        reservoir.view[2] = reservoir.view[2] - reservoir.lines // 2
    end
    reservoir[0][500] = Material.spring
    for line in io.lines(filename) do
        local xy, a, b, c = line:match("([xy])=(%d+), [xy]=(%d+)%.%.(%d+)")
        if xy then
            a = math.tointeger(a) --[[@as integer]]
            b = math.tointeger(b) --[[@as integer]]
            c = math.tointeger(c) --[[@as integer]]
            if xy == "x" then
                for y = b, c do
                    reservoir[y] = reservoir[y] or {}
                    reservoir[y][a] = Material.clay
                end
                reservoir.min[1] = math.min(reservoir.min[1], a)
                reservoir.min[2] = math.min(reservoir.min[2], b)
                reservoir.max[1] = math.max(reservoir.max[1], a)
                reservoir.max[2] = math.max(reservoir.max[2], c)
            else -- if xy == "y" then
                reservoir[a] = reservoir[a] or {}
                for x = b, c do
                    reservoir[a][x] = Material.clay
                end
                reservoir.min[1] = math.min(reservoir.min[1], b)
                reservoir.min[2] = math.min(reservoir.min[2], a)
                reservoir.max[1] = math.max(reservoir.max[1], c)
                reservoir.max[2] = math.max(reservoir.max[2], a)
            end
        end
    end
    return reservoir
end

---@param x integer
---@param y integer
---@return string
function Reservoir:__tostring(x, y)
    if x < self.view[1] + self.columns // 4 then
        self.view[1] = x - self.columns // 2
    elseif x > self.view[1] + 3 * self.columns // 4 then
        self.view[1] = x + self.columns // 2
    end
    if y < self.view[2] + self.lines // 5 then
        self.view[2] = y - self.lines // 2
    elseif y > self.view[2] + 4 * self.lines // 5 then
        self.view[2] = y - self.lines // 3
    end
    local rows = { "\27[H" }
    for j = self.view[2], self.view[2] + self.lines do
        local row = {}
        for i = self.view[1], self.view[1] + self.columns do
            local c = self[j][i]
            table.insert(row, ("\27[%dm%s"):format(c:tointeger(), tostring(c)))
        end
        table.insert(rows, table.concat(row))
    end
    table.insert(rows, "\27[39m\n")
    return table.concat(rows, "\n")
end

setmetatable(Reservoir, mt)

function Reservoir:__call()
    ---@type integer?, integer?
    local x, y = self.min[1] - 1, self.min[2]
    return function()
        if not x then return end
        x = x + 1
        if x > self.max[1] then
            x = 1
            y = y + 1
            if y > self.max[2] then
                x = nil
                y = nil
                return nil
            end
        end
        return self[y][x]
    end
end

---@param x integer
---@param y integer
function Reservoir:findflowpoint(x, y)
    repeat
        if self[y - 1][x] == Material.wetsand then
            return x, y - 1
        end
        x = x - 1
    until self[y][x] ~= Material.wetsand and self[y][x] ~= Material.water
    while self[y][x + 1] ~= Material.clay do
        x = x + 1
        if self[y - 1][x] == Material.wetsand then
            return x, y - 1
        end
    end
end

---@param x integer
---@param y integer
---@param d integer
function Reservoir:flood(x, y, d)
    while self[y][x] ~= Material.clay do
        self[y][x] = Material.water
        self:show(x, y)
        x = x + d
    end
end

---@param x integer
---@param y integer
function Reservoir:flowdown(x, y)
    while self[y + 1][x] == Material.sand do
        y = y + 1
        self[y][x] = Material.wetsand
        self:show(x, y)
        if y > self.max[2] then return end
    end
    if self[y + 1][x] == Material.clay or self[y + 1][x] == Material.water then
        local lx, ly = self:flowsideways(x, y, -1)
        local rx, ry = self:flowsideways(x, y, 1)
        while not lx and not rx do
            self:flood(x, y, -1)
            self:flood(x + 1, y, 1)
            x, y = self:findflowpoint(x, y)
            if not x then return end
            lx, ly = self:flowsideways(x, y, -1)
            rx, ry = self:flowsideways(x, y, 1)
        end
        if lx and ly then
            self:flowdown(lx, ly)
        end
        if rx and ry then
            self:flowdown(rx, ry)
        end
    end
end

---@param x integer
---@param y integer
---@param d integer
---@return integer? x, integer? y point to flow down from, if any
function Reservoir:flowsideways(x, y, d)
    while self[y][x + d] ~= Material.clay do
        x = x + d
        self[y][x] = Material.wetsand
        self:show(x, y)
        if self[y + 1][x] == Material.sand then
            return x, y
        end
    end
end

function Reservoir:show(x, y)
    if self.view then
        print(self:__tostring(x, y))
        if self.frametime then
            os.execute("sleep " .. self.frametime)
        end
    end
end

function Reservoir:partone()
    local water = 0
    for y = self.min[2], self.max[2] do
        for x = self.min[1] - 1, self.max[1] + 1 do
            local c = self[y][x]
            if c == Material.water or c == Material.wetsand then
                water = water + 1
            end
        end
    end
    return water
end

function Reservoir:parttwo()
    local water = 0
    for y = self.min[2], self.max[2] do
        for x = self.min[1] - 1, self.max[1] + 1 do
            local c = self[y][x]
            if c == Material.water then
                water = water + 1
            end
        end
    end
    return water
end

local function main(filename, columns, lines, fps)
    print("\27[?1049h\27[s\27[?25l")
    local reservoir = Reservoir(filename, columns, lines, fps)
    reservoir:flowdown(500, 0)
    print("\27[?1049l\27u\27[?25h\27[0m" .. reservoir:partone(),
        reservoir:parttwo())
    return 0
end

if pcall(debug.getlocal, 4, 1) then
    return main
else
    main(table.unpack(arg))
end
