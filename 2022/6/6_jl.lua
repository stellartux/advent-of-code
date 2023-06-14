#!/usr/bin/env lua
_ = #{} --[[

eachview(s, len::Integer, step::Integer) =
    (view(s, n:n+len-1) for n in 1:step:length(s)-len+1)

#= ]]
---@diagnostic disable: lowercase-global

---@param t any[]
local function allunique(t)
    local r = {}
    for _, v in ipairs(t) do
        if r[v] then return false end
        r[v] = true
    end
    return true
end

local ARGS = arg

---@param s string
---@param len integer
---@param step integer?
function eachview(s, len, step)
    step = step or len
    return function(_, k)
        k = k + step
        if k <= #s then
            return k, table.pack(string.byte(s, k, k + len - 1))
        end
    end, nil, 1 - step
end

---@param fn function
local function findfirst(fn, ...)
    for k, v in ... do
        if fn(v) then return k, v end
    end
end

---@param t table
local function get(t, k, d) return t[k] or d end

local println = print

---@param filename string
---@return string
local function readchomp(filename)
    local f = assert(io.open(filename, "r"))
    local s = f:read("a"):gsub("%s+$", "")
    f:close()
    return s
end
local
-- =#

function findstartofpacket(s, n)
    return findfirst(allunique, eachview(s, n, 1)) + n - 1
end

function main(filename)
    local s = readchomp(filename)
    println(findstartofpacket(s, 4))
    println(findstartofpacket(s, 14))
end

main(get(ARGS, 1, "example.txt"))
