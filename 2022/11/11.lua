#!/usr/bin/env lua
-- usage: lua 11.lua --verbosity=[0-2] filename.txt

local Iterators = require("ji/iterators")
local map, collect, keys = Iterators.map, Iterators.collect, Iterators.keys

local operations = {
    ---@type fun(x: integer?): fun(y: integer): integer
    ["+"] = function(x) return function(y) return y + (x or y) end end,
    ---@type fun(x: integer?): fun(y: integer): integer
    ["*"] = function(x) return function(y) return y * (x or y) end end,
}

---@param s string
---@param p string
---@return integer
local function matchinteger(s, p)
    return assert(math.tointeger(s:match(p)))
end

---@class Monkey: integer[]
---@field id integer
---@field testvalue integer
---@field ontrue integer
---@field onfalse integer
---@field op fun(x: integer): integer
---@field opstring string
---@field opvalue integer?
---@field inspections integer
local Monkey = setmetatable({}, {
    __name = "Monkey",
    ---@param s string
    ---@return Monkey
    __call = function(self, s)
        local monkey = collect(map(
            math.tointeger,
            keys(s:match("Starting items: ([^\n]+)"):gmatch("%d+"))
        ))
        monkey.id = matchinteger(s, "Monkey (%d+):")
        monkey.inspections = 0
        local op, arg = s:match("new = old ([+*]) (%w+)")
        monkey.opstring = op == "*" and "is multiplied" or "increases"
        monkey.opvalue = math.tointeger(arg)
        monkey.op = operations[op](monkey.opvalue)
        monkey.testvalue = matchinteger(s, "Test: divisible by (%d+)")
        monkey.ontrue = matchinteger(s, "If true: throw to monkey (%d+)")
        monkey.onfalse = matchinteger(s, "If false: throw to monkey (%d+)")
        return setmetatable(monkey, self)
    end
})
Monkey.__index = Monkey

function Monkey:__tostring()
    return ("Monkey %d: %s"):format(self.id, table.concat(self, ", "))
end

---@param monkeys Monkeys
function Monkey:takeround(monkeys, verbosity)
    verbosity = verbosity or 0
    if verbosity > 1 then print(("Monkey %d:"):format(self.id)) end
    self.inspections = self.inspections + #self
    while #self > 0 do
        local worry = table.remove(self, 1)
        if verbosity > 1 then
            print(("  Monkey inspects an item with a worry level of %d.")
                :format(worry))
        end
        worry = self.op(worry)
        if verbosity > 1 then
            print(("    Worry level is %s by %s to %d.")
                :format(self.opstring, self.opvalue or "itself", worry))
        end
        worry = worry // 3
        if verbosity > 1 then
            print(("    Monkey gets bored with item. " ..
                "Worry level is divided by 3 to %d.")
                :format(worry))
        end
        local decision = self:test(worry)
        local nextmonkey = decision and self.ontrue or self.onfalse
        table.insert(monkeys[nextmonkey], worry)
        if verbosity > 1 then
            print(("    Current worry level is %sdivisible by %d.")
                :format(decision and "" or "not ", self.testvalue))
            print(("    Item with worry level %d is thrown to monkey %d.")
                :format(worry, nextmonkey))
        end
    end
end

---@param item integer
function Monkey:test(item)
    return item % self.testvalue == 0
end

---@class Monkeys: Monkey[]
---@field [0] Monkey
local Monkeys = setmetatable({}, {
    __name = "Monkeys",
    ---@param filename string
    ---@return Monkeys
    __call = function(self, filename)
        local monkeys = { round = 0 }
        for description in io.open(filename):read("a"):gmatch("M[^M]+") do
            local monkey = Monkey(description)
            monkeys[monkey.id] = monkey
        end
        return setmetatable(monkeys, self)
    end
})
Monkeys.__index = Monkeys

---@return fun(self: Monkeys, i: integer): (integer, Monkey), self, integer
function Monkeys:__pairs()
    return ipairs(self), self, -1
end

function Monkeys:__tostring()
    return table.concat(collect(map(tostring, pairs(self))), "\n") .. "\n"
end

function Monkeys:monkeybusiness(verbosity)
    verbosity = verbosity or 0
    local most, secondmost, mosti, secondmosti = -math.huge, -math.huge, -1, -1
    for i, monkey in pairs(self) do
        if monkey.inspections > most then
            secondmost, secondmosti = most, mosti
            most, mosti = monkey.inspections, i
        elseif monkey.inspections > secondmost then
            secondmost, secondmosti = monkey.inspections, i
        end
    end
    if verbosity > 0 then
        for i, monkey in pairs(self) do
            print(("\27[%dmMonkey %d inspected items %d times."):format(
                (i == mosti or i == secondmosti) and 1 or 0,
                monkey.id,
                monkey.inspections))
        end
    end
    return most * secondmost
end

---@param verbosity integer?
function Monkeys:takeround(verbosity)
    verbosity = verbosity or 0
    for _, monkey in pairs(self) do
        monkey:takeround(self, verbosity)
    end
end

---@param monkeys Monkeys
---@param verbosity integer
---@return integer
local function PartOne(monkeys, verbosity)
    verbosity = verbosity or 0
    if verbosity > 0 then
        print(monkeys)
    end
    for round = 1, 20 do
        monkeys:takeround(verbosity)
        if verbosity > 0 then
            print("After round " .. round ..
                ", the monkeys are holding items with these worry levels:")
            print(monkeys)
        end
    end
    return monkeys:monkeybusiness(verbosity)
end

---@param filename string
local function main(verbosity, filename)
    if filename then
        verbosity = assert(math.tointeger(verbosity:match("%d")))
    else
        filename = verbosity
        verbosity = 0
    end
    local monkeys = Monkeys(filename)
    print(PartOne(monkeys, verbosity))
    -- print(PartTwo(monkeys, verbosity))
end

if pcall(debug.getlocal, 4, 1) then
    return Monkeys
else
    main(...)
end
