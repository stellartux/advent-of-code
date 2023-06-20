#!/usr/bin/env lua

---@class Domino
---@field [1] integer
---@field [2] integer
local Domino = setmetatable({}, {
    ---@param a integer
    ---@param b integer
    __call = function(self, a, b)
        return setmetatable({ a, b }, self)
    end
})
Domino.__index = Domino

function Domino:__tostring()
    return ("%d/%d"):format(table.unpack(self))
end

---@param n integer
function Domino:find(n)
    return self[1] == n and 1 or self[2] == n and 2 or nil
end

function Domino:strength()
    return self[1] + self[2]
end

---@param filename string
local function load24(filename)
    local result = {}
    for line in io.lines(filename) do
        local a, b = line:match("(%d+)/(%d+)")
        if a then
            table.insert(result, Domino(math.tointeger(a), math.tointeger(b)))
        end
    end
    return result
end

---@generic T
---@param xs T[]
---@param i integer
---@return T[] copy of `xs` with the value at `i` removed
local function without(xs, i)
    local ys = table.pack(table.unpack(xs))
    table.remove(ys, i)
    return ys
end

local function part1(dominoes, n)
    n = n or 0
    local strength = 0
    for i, domino in ipairs(dominoes) do
        local j = domino:find(n)
        if j then
            strength = math.max(strength,
                domino:strength() + part1(without(dominoes, i), domino[3 - j]))
        end
    end
    return strength
end

local function part2(dominoes, n, length)
    length = length or 1
    n = n or 0
    local maxstrength = 0
    local maxlength = 0
    for i, domino in ipairs(dominoes) do
        local j = domino:find(n)
        if j then
            local s, l = part2(without(dominoes, i), domino[3 - j], length + 1)
            if l + 1 > maxlength then
                maxlength = l + 1
                maxstrength = s + domino:strength()
            elseif l + 1 == maxlength then
                maxstrength = math.max(maxstrength, s + domino:strength())
            end
        end
    end
    return maxstrength, maxlength
end

---@param filename string
local function main(filename)
    local dominoes = load24(filename)
    print(part1(dominoes))
    print((part2(dominoes)))
end
main(#arg > 0 and arg[#arg] or "example.txt")
