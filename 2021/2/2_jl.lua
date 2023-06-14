#!/usr/bin/env lua
local _ = #_G --[[
# code here is only visible to Julia

then = true
tonumber = Base.Fix1(parse, Int)

#= code here is only visible to Lua ]]

local ARGS = arg
local eachline = io.lines
local function foreach(fn, ...)
    for x in ... do fn(x) end
end
local function get(t, k, d)
    return t[k] or d
end
local println = print
local function split(s)
    return s:match("(%S+) (%d+)")
end
local
-- code here is visible to Julia and Lua =#

function main(filename)
    local x, y1, y2 = 0, 0, 0
    foreach(function(line)
        local op, n = split(line)
        local i = tonumber(n)
        if op == "forward" then
            x = x + i
            y2 = y2 + y1 * i
        elseif op == "up" then
            y1 = y1 - i
        elseif op == "down" then
            y1 = y1 + i
        end
    end, eachline(filename))
    println(x * y1)
    println(x * y2)
end

main(get(ARGS, 1, "example.txt"))
