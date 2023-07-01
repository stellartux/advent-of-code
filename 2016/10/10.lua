#!/usr/bin/env lua

local function loadbots(filename)
    local bots = {}
    local outputs = {}
    local ports = { bot = bots, output = outputs }
    for line in io.lines(filename) do
        local value, botnumber = line:match("value (%d+) goes to bot (%d+)")
        if value then
            local i = assert(math.tointeger(botnumber))
            bots[i] = bots[i] or {}
            table.insert(bots[i], math.tointeger(value))
        else
            local ltype, lvalue, rtype, rvalue
            botnumber, ltype, lvalue, rtype, rvalue = line:match(
                "bot (%d+) gives low to (%w+) (%d+) and high to (%w+) (%d+)")
            lvalue = assert(math.tointeger(lvalue))
            rvalue = assert(math.tointeger(rvalue))
            local i = assert(math.tointeger(botnumber))
            bots[i] = bots[i] or {}
            bots[i].types = { ltype, rtype }
            bots[i].values = { lvalue, rvalue }
            if ltype == "output" and not outputs[lvalue] then
                outputs[lvalue] = {}
            elseif ltype == "bot" and not bots[lvalue] then
                bots[lvalue] = {}
            end
            if rtype == "output" and not outputs[rvalue] then
                outputs[rvalue] = {}
            elseif rtype == "bot" and not bots[rvalue] then
                bots[rvalue] = {}
            end
        end
    end
    return ports
end

local function any(fn, xs)
    for i = 0, #xs do if xs[i] and fn(xs[i]) then return true end end
    return false
end

local function haslength2(xs) return #xs == 2 end

local function findbot(ports, breaker)
    local bots = ports.bot
    while any(haslength2, bots) do
        for i = 0, #bots do
            local bot = bots[i]
            if haslength2(bot) then
                table.sort(bot)
                local x = breaker(ports, i)
                if x then return x end
                table.insert(ports[bot.types[2]][bot.values[2]], table.remove(bot))
                table.insert(ports[bot.types[1]][bot.values[1]], table.remove(bot))
            end
        end
    end
end

local filename = arg[1] or "input.txt"
local lowtarget = math.tointeger(arg[2]) or 17
local hightarget = math.tointeger(arg[3]) or 61
print(findbot(loadbots(filename), function(ports, i)
    local bot = ports.bot[i]
    if bot[1] == lowtarget and bot[2] == hightarget then return i end
end))
print(findbot(loadbots(filename), function(ports)
    local outputs = ports.output
    if #outputs[0] > 0 and #outputs[1] > 0 and #outputs[2] > 0 then
        return outputs[0][1] * outputs[1][1] * outputs[2][1]
    end
end))
