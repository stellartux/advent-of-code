#!/usr/bin/env lua
_ = #{} --[[ Code only visible to Julia
if basename(pwd()) != "1"
    cd("2015/1")
end
and = *
rest(s::AbstractString) = SubString(s, 2)
then = true
debug = (getlocal = function(_...)
    !isempty(ARGS) && abspath(PROGRAM_FILE) != @__FILE__
end,)
pcall(f, xs...) =
    try
        f(xs)
    catch err
        nothing, err.msg
    end

#= Code visible to Lua ]]
---@diagnostic disable: lowercase-global

local ARGS = arg

local function first(s) return s:sub(1, 1) end

local function get(t, k, d) return t[k] or d end

local function ifelse(p, c, a) return p and c or a end

local println = print

local function readchomp(filename)
    local f = assert(io.open(filename), "Could not open " .. filename)
    local s = f:read("a")
    f:close()
    return s
end

local function rest(s) return s:sub(2) end

-- Code visible to Julia and Lua =#

function loop(s, p1, p2, step)
    if s == "" then
        return p1, p2
    end
    local c = first(s)
    s = rest(s)
    if c == '(' then
        return loop(s, p1 + 1, p2, step + 1)
    elseif c == ')' then
        return loop(s, p1 - 1, ifelse((p2 == 0)and(p1 < 0), step, p2), step + 1)
    else
        return loop(s, p1, p2, step + 1)
    end
end

function main(filename)
    local s = readchomp(filename)
    local partone, parttwo = loop(s, 0, 0, 0)
    println(partone)
    println(parttwo)
end

if pcall(debug.getlocal, 4, 1) then
    return main
else
    main(get(ARGS, 1, "input.txt"))
end
