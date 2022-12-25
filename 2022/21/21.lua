#!/usr/bin/env lua

local ops = {
    ["+"] = function(a, b) return a + b end,
    ["-"] = function(a, b) return a - b end,
    ["*"] = function(a, b) return a * b end,
    ["/"] = function(a, b) return a // b end
}

local t = {}
for line in io.lines(..., "l") do
    local name, num = line:match("(....): (%d+)")
    if name then
        t[name] = math.tointeger(num)
    else
        local l, op, r
        name, l, op, r = line:match("(....): (....) (.) (....)")
        t[name] = { ops[op], l, r }
    end
end

local function getter(k)
    local v = t[k]
    if math.tointeger(v) then
        return v
    else
        return v[1](getter(v[2]), getter(v[3]))
    end
end

print(getter("root"))
