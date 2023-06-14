_ = #_G --[[
0 # code here is only visible to Julia

if basename(pwd()) != "1"
    cd("2021/1")
end

tonumber = Base.Fix1(parse, Int)

offsetdiff(xs, n) = xs[begin+n:end] .- xs[begin:end-n]

#= code here is only visible to Lua ]]
---@diagnostic disable: lowercase-global

local ARGS = arg

local function count(pred, xs)
    local result = 0
    for _, x in ipairs(xs) do
        if pred(x) then
            result = result + 1
        end
    end
    return result
end

local function offsetdiff(xs, size)
    size = size or 1
    local result = {}
    for i = 1, #xs - size do
        table.insert(result, xs[i + size] - xs[i])
    end
    return result
end

local function get(t, k, d) return t[k] or d end

local function map(fn, xs)
    for i, v in ipairs(xs) do
        xs[i] = fn(v)
    end
    return xs
end

local println = print

local function readlines(filename)
    local result = {}
    for line in assert(io.open(filename, "r"), filename):lines() do
        table.insert(result, line)
    end
    return result
end

-- code here is visible to Julia and Lua =#

function ispositive(n) return n > 0 end

function calculate(xs, len)
    return count(ispositive, offsetdiff(xs, len))
end

function main(filename)
    local xs = map(tonumber, readlines(filename))
    println(calculate(xs, 1))
    println(calculate(xs, 3))
end

main(get(ARGS, 1, "example.txt"))
