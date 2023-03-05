_ = #_G --[[
0 # code here is only visible to Julia
begin

if basename(pwd()) != "1"
    cd("2022/1")
end
local then = true
local table = (
    insert = push!,
    remove = (xs, _) -> popfirst!(xs)
)
local arg = ARGS

#= code here is only visible to Lua ]]

local exit = os.exit
local Int = "integer"
local function isempty(xs) return #xs == 0 end
local function lastindex(xs) return #xs end
local function parse(_, x) return tonumber(x) end
local println = print

local function readlines(filename)
    local lines = {}
    for line in io.open(filename, "r"):lines() do
        table.insert(lines, line)
    end
    return lines
end

local function reverse(xs)
    local result = {}
    for _, v in ipairs(xs) do
        table.insert(result, 1, v)
    end
    return result
end

local function sort(xs)
    table.sort(xs)
    return xs
end

local function zeros(_, _) return { 0 } end

-- code here is visible to Julia and Lua =#

if isempty(arg) then
    println("Usage: (julia|lua) 1.jl.lua FILENAME")
    exit(1)
end

local elves = zeros(Int, 1)

local function foreachline(lines)
    if isempty(lines) then
        return elves
    end
    local line = table.remove(lines, 1)
    if isempty(line) then
        table.insert(elves, 0)
    else
        local i = lastindex(elves)
        elves[i] = elves[i] + parse(Int, line)
    end
    return foreachline(lines)
end

foreachline(readlines(arg[1]))
local sorted = reverse(sort(elves))
println(sorted[1])
println(sorted[1] + sorted[2] + sorted[3])

_ = #{} --[[
0 ; end # ]]
