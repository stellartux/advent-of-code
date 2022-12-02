---@alias Coord { [1]: integer, [2]: integer }

---@param filepath string
---@return Coord[] coords, Coord min, Coord max
function LoadCoords(filepath)
    local coords = {}
    local minx, miny, maxx, maxy = math.huge, math.huge, -math.huge, -math.huge
    for s in io.lines(filepath) do
        local x, y = s:match("(%d+), (%d+)")
        x = math.tointeger(x)
        y = math.tointeger(y)
        table.insert(coords, { x, y })
        minx = math.min(minx, x)
        maxx = math.max(maxx, x)
        miny = math.min(miny, y)
        maxy = math.max(maxy, y)
    end
    return coords, { minx, miny }, { maxx, maxy }
end

local function ManhattanDistance(left, right)
    return math.abs(left[1] - right[1]) + math.abs(left[2] - right[2])
end

---@param coord Coord
---@param coords Coord[]
local function TotalDistance(coord, coords)
    local total = 0
    for _, other in ipairs(coords) do
        total = total + ManhattanDistance(coord, other)
    end
    return total
end

---@param coords Coord[]
---@param min Coord
---@param max Coord
---@param size integer
local function PartTwo(size, coords, min, max)
    local count = 0
    for y = min[2], max[2] do
        for x = min[1], max[1] do
            local coord = { x, y }
            local total = TotalDistance(coord, coords)
            if total < size then
                count = count + 1
            end
        end
    end
    return count
end

---@param filepath string
---@param size string
local function main(filepath, size)
    print(PartTwo(
        assert(math.tointeger(size)),
        LoadCoords(assert(filepath, "Expected a filepath"))
    ))
end

main(table.unpack(arg))
