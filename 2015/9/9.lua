#!/usr/bin/env lua

local function cmp(a, b)
    if a < b then
        return -1
    elseif a > b then
        return 1
    else
        return 0
    end
end

--- Iterate over each permutation of the given list.
--- See https://en.wikipedia.org/wiki/Steinhaus%E2%80%93Johnson%E2%80%93Trotter_algorithm#Even's_speedup
---@generic T
---@param list T[]
---@return fun(): permutation: T[]?
local function permutations(list)
    assert(type(list) == "table" and #list > 0)

    -- Initially, the direction of the number 1 is zero,
    -- and all other elements have a negative direction:
    local p = { { 1, 0 } }
    for i = 2, #list do
        p[i] = { i, -1 }
    end
    local i

    return function()
        if not i then
            i = #p
            local result = {}
            for j = 1, i do
                result[j] = list[p[j][1]]
            end
            return result
        elseif i == 0 then
            return
        end

        -- At each step, the algorithm finds the greatest element with
        -- a nonzero direction, and swaps it in the indicated direction:
        local t = table.remove(p, i)
        i = i + t[2]
        table.insert(p, i, t)

        -- If this causes the chosen element to reach the first or last
        -- position within the permutation, or if the next element in
        -- the same direction is greater than the chosen element, the
        -- direction of the chosen element is set to zero:
        if i == 1 or i == #p or p[i + t[2]][1] > t[1] then
            t[2] = 0
        end

        -- After each step, all elements greater than the chosen element
        -- which previously had direction zero have their directions
        -- set to indicate motion toward the chosen element. That is,
        -- positive for all elements between the start of the permutation
        -- and the chosen element, and negative for elements toward the
        -- end:
        local result = {}
        local maxv, maxi = -math.huge, 0
        for j, pair in ipairs(p) do
            result[j] = list[pair[1]]
            if pair[1] > t[1] then
                pair[2] = cmp(i, j)
            end
            if pair[1] > maxv and pair[2] ~= 0 then
                maxv = pair[1]
                maxi = j
            end
        end
        i = maxi
        return result
    end
end

---@class SantaMap: { [string]: { [string]: integer } }
---@operator call(string): SantaMap
local SantaMap = setmetatable({
        __name = "SantaMap"
    }, {
        ---@param s string
        __call = function(self, s)
            local t = {}
            for l, r, n in s:gmatch("(%w+) to (%w+) = (%d+)") do
                t[l] = t[l] or {}
                t[r] = t[r] or {}
                n = math.tointeger(n)
                t[l][r] = n
                t[r][l] = n
            end
            return setmetatable(t, self)
        end
    })
SantaMap.__index = SantaMap

function SantaMap:__tostring()
    local result = {}
    for l, r, v in pairs(self) do
        table.insert(result, ("%s to %s = %d"):format(l, r, v))
    end
    return table.concat(result, "\n")
end

function SantaMap:__pairs()
    local keys = self:keys()
    local i, j, len = 1, 1, #keys
    return function()
        while true do
            j = j + 1
            if j > len then
                i = i + 1
                j = i + 1
            end
            if i > len then return end
            local l, r = keys[i], keys[j]
            local v = self[l][r]
            if v then
                return l, r, v
            end
        end
    end
end

function SantaMap:keys()
    local keys = {}
    local key = next(self)
    while key do
        table.insert(keys, key)
        key = next(self, key)
    end
    table.sort(keys)
    return keys
end

---@param ... string
local function main(...)
    local f = assert(io.open(... or "example.txt"))
    local s = f:read("a")
    f:close()
    local sm = SantaMap(s)
    local min = math.huge
    local max = 0
    for perm in permutations(sm:keys()) do
        local city, total = perm[1], 0
        -- local route = { city }
        for i = 2, #perm do
            total = total + sm[city][perm[i]]
            city = perm[i]
            -- table.insert(route, city)
        end
        if total < min then min = total end
        if total > max then max = total end
        -- print(table.concat(route, " -> ") .. " = " .. total)
    end
    print("Minimum = " .. min)
    print("Maximum = " .. max)
    return sm
end

if pcall(debug.getlocal, 4, 1) then
    return SantaMap
else
    main(...)
end
