#!/usr/bin/env lua
local _ = #_G --[[ code here is only visible to Julia

then = true
and = *

parseline(line) = Tuple(parse(Int, m.match) for m in eachmatch(r"\d+", line))

#= code here is only visible to Lua ]]

local ARGS = arg
local eachline = io.lines
local function foreach(fn, ...) for value in ... do fn(value) end end
local function get(t, k, d) return t[k] or d end
local println = print

local function parseline(line)
    local a, b, x, y = line:match("(%d+)-(%d+),(%d+)-(%d+)")
    return math.tointeger(a), math.tointeger(b), math.tointeger(x), math.tointeger(y)
end

local -- code here is visible to Julia and Lua =#

function main(filename)
    local partone, parttwo = 0, 0
    foreach(function(line)
        local a, b, x, y = parseline(line)
        if (a <= x)and(b >= y) then
            partone = partone + 1
        elseif (x <= a)and(y >= b) then
            partone = partone + 1
        end
        if (a <= x)and(b >= x) then
            parttwo = parttwo + 1
        elseif (x <= a)and(y >= a) then
            parttwo = parttwo + 1
        end
    end, eachline(filename))
    println(partone)
    println(parttwo)
end
main(get(ARGS, 1, "example.txt"))
