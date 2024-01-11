local activecoords = setmetatable({}, { __mode = "v" })

---@alias BareCoord { [1]: integer, [2]: integer }

---@class Coord: { [1]: integer, [2]: integer }
---@operator call(BareCoord): Coord
local Coord
Coord = setmetatable({
    __name = "Coord"
}, {
    ---@param coord BareCoord|string?
    ---@return Coord
    __call = function(self, coord)
        if type(coord) == "string" then
            if coord == "^" then
                coord = { 0, -1 }
            elseif coord == ">" then
                coord = { 1, 0 }
            elseif coord == "<" then
                coord = { -1, 0 }
            elseif coord == "v" then
                coord = { 0, 1 }
            else
                error("Couldn't make a Coord from \"" .. coord .. "\"")
            end
        end
        return self:new(coord)
    end
})
Coord.__index = Coord

---@param coord BareCoord
---@return Coord
function Coord:new(coord)
    local key = table.concat(coord, ',')
    if not activecoords[key] then
        activecoords[key] = setmetatable(table.pack(table.unpack(coord)), self)
    end
    return activecoords[key]
end

---@param other BareCoord
---@return Coord
function Coord:__add(other)
    local t = {}
    for i = 1, #self do
        t[i] = self[i] + other[i]
    end
    return Coord(t)
end

---@param other BareCoord
---@return Coord
function Coord:__sub(other)
    local t = {}
    for i = 1, #self do
        t[i] = self[i] - other[i]
    end
    return Coord(t)
end

---@param other BareCoord
function Coord:__eq(other)
    if #self ~= #other then return false end
    for i = 1, #self do
        if self[i] ~= other[i] then return false end
    end
    return true
end

---@return Coord
function Coord:__unm()
    local t = {}
    for i = 1, #self do
        t[i] = -self[i]
    end
    return Coord(t)
end

--- Finds the bounding rectangle of an iterator of Coords.
---@param fn fun(...): Coord?
---@return Coord, Coord
function Coord.extrema(fn, ...)
    local top, left, right, bottom =
        math.maxinteger, math.maxinteger, math.mininteger, math.mininteger
    for _, coord in fn, ... do
        local x, y = coord[1], coord[2]
        if x < left then left = x end
        if x > right then right = x end
        if y < top then top = y end
        if y > bottom then bottom = y end
    end
    return Coord { left, top }, Coord { right, bottom }
end

---@return Coord[]
function Coord:neighbours4()
    return {
        Coord { 0, -1 } + self,
        Coord { -1, 0 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, 0 } + self
    }
end

---@return Coord[]
function Coord:neighbours5()
    return {
        Coord { 0, -1 } + self,
        Coord { -1, 0 } + self,
        Coord { 0, 0 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, 0 } + self
    }
end

---@return Coord[]
function Coord:neighbours8()
    return {
        Coord { -1, -1 } + self,
        Coord { 0, -1 } + self,
        Coord { 1, -1 } + self,
        Coord { -1, 0 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, -1 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, 1 } + self
    }
end

---@return Coord[]
function Coord:neighbours9()
    return {
        Coord { -1, -1 } + self,
        Coord { 0, -1 } + self,
        Coord { 1, -1 } + self,
        Coord { -1, 0 } + self,
        Coord { 0, 0 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, -1 } + self,
        Coord { 1, 0 } + self,
        Coord { 1, 1 } + self
    }
end

---@param other BareCoord
---@return fun(): Coord?
function Coord:range(other)
    local x, xmin, xmax, y, ymax
    xmin = math.min(self[1], other[1])
    x = xmin
    xmax = math.max(self[1], other[1])
    y = math.min(self[2], other[2])
    ymax = math.max(self[2], other[2])
    return function()
        if x > xmax then
            x = xmin
            y = y + 1
        end
        if y <= ymax then
            ---@type Coord
            local coord = Coord { x, y }
            x = x + 1
            return coord
        end
    end
end

function Coord:__tostring()
    return "Coord { " .. table.concat(self, ", ") .. " }"
end

return Coord
