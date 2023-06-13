#!/usr/bin/env lua
_ = #_G --[[
0 # code here is only visible to Julia
if basename(pwd()) != "3"
    cd("2022/3")
end
using Base.Iterators: partition
using Base: Fix2
arg = ARGS
then = true
sub(s, b) = s[b:end]
sub(s, b, e) = s[b:e]

#= code here is only visible to Lua ]]
---@diagnostic disable: lowercase-global

local function div(x, y) return x // y end
local function get(xs, k, d) return xs[k] or d end
local function length(xs) return #xs end
local Int = string.byte
local function intersect(a, ...)
    local keep, maybe = {}, nil
    for _, v in ipairs({ a:byte(1, #a) }) do keep[v] = true end
    for i = 1, select("#", ...) do
        keep, maybe = {}, keep
        local b = select(i, ...)
        for _, v in ipairs({ b:byte(1, #b) }) do
            if maybe[v] then keep[v] = true end
        end
    end
    local c = {}
    for v in pairs(keep) do table.insert(c, string.char(v)) end
    return c
end
local function map(fn, t)
    local result = {}
    for i, v in ipairs(t) do
        result[i] = fn(v)
    end
    return result
end
local function only(xs)
    return assert(#xs == 1, tostring(#xs) .. ":" .. table.concat(xs)) and xs[1]
end
local function partition(xs, n)
    local result = {}
    local t = {}
    for i = 1, #xs do
        table.insert(t, xs[i])
        if i % n == 0 then
            table.insert(result, t)
            t = {}
        end
    end
    if #t > 0 then table.insert(result, t) end
    return result
end
local println = print
local function readlines(filename)
    local result = {}
    for line in io.lines(filename) do table.insert(result, line) end
    return result
end
local function splat(fn)
    return function(xs) return fn(table.unpack(xs)) end
end
local sub = string.sub
local function sum(t)
    local result = 0
    for _, v in ipairs(t) do result = result + v end
    return result
end
local tuple = table.pack

-- code here is visible to Julia and Lua =#

function priority(item)
    local c = Int(item)
    if c < 96 then
        return c - 38
    else
        return c - 96
    end
end

function splitinhalf(s)
    local len = div(length(s), 2)
    return tuple(sub(s, 1, len), sub(s, len + 1))
end

function sumpriorities(xs)
    return sum(map(function(ys)
        return priority(only(splat(intersect)(ys)))
    end, xs))
end

function part1(rucksacks)
    return sumpriorities(map(splitinhalf, rucksacks))
end

function part2(rucksacks)
    return sumpriorities(partition(rucksacks, 3))
end

rucksacks = readlines(get(arg, 1, "example.txt"))
println(part1(rucksacks))
println(part2(rucksacks))
