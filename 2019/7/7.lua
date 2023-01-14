#!/usr/bin/env lua
local master = require("intcode"):load(...)

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
            return table.pack(table.unpack(list))
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
        -- and the chosen element, and negative for elements toward the end:
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

local function input2(a, b)
    return function()
        local c = a or b
        if a then
            a = nil
        else
            b = nil
        end
        return c
    end
end

local maxsignal = -math.huge
for permutation in permutations({ 0, 1, 2, 3, 4 }) do
    local m = master:copy({ input = input2(permutation[1], 0) })
    for i = 2, 5 do
        m = master:copy({ input = input2(permutation[i], m()) })
    end
    local signal = m()
    if signal > maxsignal then
        maxsignal = signal
    end
end
print(maxsignal)
