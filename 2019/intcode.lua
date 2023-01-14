#!/usr/bin/env lua

local opnames = {
    "ADD", "MUL", "IN", "OUT", "JNZ", "JZ", "LT", "EQ", "RBO",
    [99] = "HALT"
}

---@param mode integer in the layout ABC
---@return integer C, integer B, integer A
local function parsemode(mode)
    return mode % 10, (mode // 10) % 10, mode // 100 % 10
end

---@param fn fun(a: integer, b: integer): integer
---@return fun(self: Intcode, mode?: integer): integer?
local function op4(fn)
    return function(self, mode)
        local C, B, A = parsemode(mode)
        local result = fn(self:get(self.ip + 1, C), self:get(self.ip + 2, B))
        self:set(self.ip + 3, A, result)
        self.ip = self.ip + 4
    end
end

local function add(a, b) return a + b end

local function multiply(a, b) return a * b end

local function lessthan(a, b) return a < b and 1 or 0 end

local function equals(a, b) return a == b and 1 or 0 end

---@param predicate fun(a: integer): boolean
local function opjmp(predicate)
    ---@param self Intcode
    ---@param mode integer
    return function(self, mode)
        local C, B = parsemode(mode)
        if predicate(self:get(self.ip + 1, C)) then
            self.ip = self:get(self.ip + 2, B)
        else
            self.ip = self.ip + 3
        end
    end
end

local function isnotzero(a) return a ~= 0 end

local function iszero(a) return a == 0 end

---@class Intcode: { [integer]: integer }
---@operator call(table?): Intcode
---@field done boolean true if the intcode machine has halted
---@field ip integer the current instruction pointer
---@field rbo integer the relative base offset for relative mode memory access
---@field input function
---@field output function
local Intcode = setmetatable({
    __name = "Intcode",
    [1] = op4(add),
    [2] = op4(multiply),
    -- Input
    ---@param self Intcode
    ---@param mode 0|2
    [3] = function(self, mode)
        self:set(self.ip + 1, mode, self:input())
        self.ip = self.ip + 2
    end,
    -- Output
    ---@param self Intcode
    ---@param mode 0|1|2
    [4] = function(self, mode)
        self.ip = self.ip + 2
        return self:output(self:get(self.ip - 1, mode))
    end,
    [5] = opjmp(isnotzero),
    [6] = opjmp(iszero),
    [7] = op4(lessthan),
    [8] = op4(equals),
    ---@param self Intcode
    ---@param mode 0|1|2
    [9] = function(self, mode)
        self.rbo = self.rbo + self:get(self.ip + 1, mode)
        self.ip = self.ip + 2
    end,
    --- Halt
    ---@param self Intcode
    [99] = function(self)
        self.done = true
    end
}, {
    __call = function(self, t)
        if type(t) == "string" then
            local s = t
            t = {}
            for d in s:gmatch("[-%d]+") do
                table.insert(t, math.tointeger(d))
            end
        end
        t = t or {}
        t[0] = t[0] or table.remove(t, 1)
        t.rbo = 0
        t.ip = 0
        return setmetatable(t, self)
    end,
})

---@param k integer
---@return integer?
---@overload fun(self, k: string): function|string?
function Intcode:__index(k)
    if type(k) == "number" and math.tointeger(k) then
        return k >= 0 and 0 or nil
    else
        return rawget(Intcode, k)
    end
end

function Intcode:copy(copy)
    copy = copy or {}
    for key, value in pairs(self) do
        copy[key] = copy[key] or value
    end
    return setmetatable(copy, getmetatable(self))
end

---@param i integer
---@param mode 0|1|2?
---@return integer
function Intcode:get(i, mode)
    mode = mode and mode % 10 or 0
    i = self[i]
    if mode == 2 then
        i = i + self.rbo
    elseif mode == 1 then
        return i
    end
    return self[i]
end

--- Fallback input method
function Intcode.input()
    return io.read("n")
end

---@param filename string?
function Intcode:load(filename)
    local t = {}
    local f = io.stdin
    if type(filename) == "string" then
        f = assert(io.open(filename))
    end
    local i = 0
    for d in f:read("a"):gmatch("[-%d]+") do
        t[i] = math.tointeger(d)
        i = i + 1
    end
    f:close()
    return self(t)
end

---@param x integer
function Intcode:output(x)
    return x
end

---@param fn fun(output: integer)
function Intcode:run(fn)
    fn = fn or function() end
    for output in self do
        fn(output)
    end
end

function Intcode:set(i, mode, v)
    assert(mode == 0 or mode == 2, "Expected mode to equal 0 or 2, got " .. (mode or "nil"))
    if mode == 2 then
        i = i + self.rbo
    end
    self[self[i]] = v
end

function Intcode:__call()
    local result
    while not result and not self.done do
        local opcode = self[self.ip]
        result = Intcode[opcode % 100](self, opcode // 100)
    end
    return result
end

function Intcode:__eq(other)
    if type(other) == "table" and #self == #other and self[0] == other[0] then
        for i = 1, #self do
            if self[i] ~= other[i] then return false end
        end
        return true
    end
    return false
end

function Intcode:__tostring()
    return self[0] .. "," .. table.concat(self, ",")
end

---@param filename string
local function main(filename)
    local machine = Intcode:load(filename)
    machine:run()
    print("machine[0] = " .. (machine[0] or "nil"))
end

if pcall(debug.getlocal, 4, 1) then
    return Intcode
else
    main(...)
end
