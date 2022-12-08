local Forest = {}
Forest.__name = "Forest"
Forest.__index = Forest

function Forest:load(filename)
    local forest = {}
    for line in io.lines(filename) do
        local row = {}
        for d in line:gmatch("%d") do
            table.insert(row, math.tointeger(d))
        end
        if #row > 0 then
            table.insert(forest, row)
        end
    end
    return setmetatable(forest, self)
end

---@alias Direction
---| 0 Edge
---| 1 Left
---| 2 Right
---| 3 Up
---| 4 Down
---@return Direction? -- the direction that the tree is visible from, if any
function Forest:isvisible(x, y)
    if x == 1 or x == #self[1] or y == 1 or y == #self then
        return 0
    end
    local tree = self[y][x]
    local blocked = false
    for i = 1, x - 1 do
        if self[y][i] >= tree then
            blocked = true
            break
        end
    end
    if not blocked then return 1 end
    blocked = false
    for i = x + 1, #self[y] do
        if self[y][i] >= tree then
            blocked = true
            break
        end
    end
    if not blocked then return 2 end
    blocked = false
    for j = 1, y - 1 do
        if self[j][x] >= tree then
            blocked = true
            break
        end
    end
    if not blocked then return 3 end
    blocked = false
    for j = y + 1, #self do
        if self[j][x] >= tree then
            blocked = true
            break
        end
    end
    if not blocked then return 4 end
end

---@return integer
function Forest:visible()
    local total = 0
    for y, row in ipairs(self) do
        for x in ipairs(row) do
            if self:isvisible(x, y) then
                total = total + 1
            end
        end
    end
    return total
end

function Forest:__tostring(nocolor)
    if #self[1] < (math.tointeger(os.getenv("COLUMNS")) or 40) then
        local result = {}
        for y, row in ipairs(self) do
            local rowstr = {}
            for x, tree in ipairs(row) do
                if not nocolor then
                    table.insert(rowstr,
                        "\27[" .. (self:isvisible(x, y) and 32 or 30) .. "m")
                end
                table.insert(rowstr, tree)
            end
            table.insert(rowstr, "\n")
            table.insert(result, table.concat(rowstr))
        end
        if not nocolor then table.insert(result, "\27[39m") end
        return table.concat(result)
    else
        return ("Forest (%dx%d) with %d trees visible\n"):format(
            #self[1], #self, self:visible())
    end
end

---@param x integer
---@param y integer
---@return integer
function Forest:scenicscore(x, y)
    local score = 1
    local tree = self[y][x]
    for i = x - 1, 1, -1 do
        if self[y][i] >= tree or i == 1 then
            score = score * (x - i)
            break
        end
    end
    for i = x + 1, #self[y] do
        if self[y][i] >= tree or i == #self[y] then
            score = score * (i - x)
            break
        end
    end
    for j = y - 1, 1, -1 do
        if self[j][x] >= tree or j == 1 then
            score = score * (y - j)
            break
        end
    end
    for j = y + 1, #self do
        if self[j][x] >= tree or j == #self then
            score = score * (j - y)
            break
        end
    end
    return score
end

function Forest:mostscenic()
    local max, maxx, maxy = -math.huge, 0, 0
    for y, row in ipairs(self) do
        if not (y == 1 or y == #self) then
            for x in ipairs(row) do
                if not (x == 1 or x == #self[y]) then
                    local score = self:scenicscore(x, y)
                    if score > max then
                        max, maxx, maxy = score, x, y
                    end
                end
            end
        end
    end
    return max, maxx, maxy
end

---@param filename string
local function main(filename)
    local forest = Forest:load(filename)
    print(forest)
    print(forest:visible())
    print(forest:mostscenic())
end

if pcall(debug.getlocal, 4, 1) then
    return Forest
else
    main(...)
end
