#!/usr/bin/env lua
local usage = [[
usage: lua 5.jl [--flags] filename.txt

flags:
    -h      --help          show this message
    -i      --input         read, parse, stringify then print the input file
    -v      --verbose       print each transformation
    -a TIME --animate=TIME  animated print, TIME is frame delta in seconds
]]

local function Load(filename)
    local stacklines = {}
    local procedure = {}
    local stacks = {}
    local lines = io.lines(filename)
    local line = lines()
    while line ~= "" do
        table.insert(stacklines, line)
        line = lines()
    end
    line = table.remove(stacklines)
    for _ = 1, math.tointeger(line:match("(%d)%s*$")) do
        table.insert(stacks, {})
    end
    for i = 1, #stacks do
        local j = 4 * i - 2
        for _, line in ipairs(stacklines) do
            local c = line:sub(j, j)
            if c ~= " " then
                table.insert(stacks[i], 1, c)
            end
        end
    end
    for line in lines do
        local a, b, c = line:match("move (%d+) from (%d+) to (%d+)")
        if not a then break end
        table.insert(procedure, {
            math.tointeger(a), math.tointeger(b), math.tointeger(c)
        })
    end
    return stacks, procedure
end

local function Wait(seconds)
    os.execute("sleep " .. (seconds or 0.5))
    print("\27[2J")
end

local function PrintStacks(stacks, highlight)
    local max = 0
    for _, stack in ipairs(stacks) do
        if #stack > max then max = #stack end
    end
    for i = max, 1, -1 do
        local line = {}
        for _, stack in ipairs(stacks) do
            local c = stack[i]
            if c then
                if highlight and i == #stack then
                    table.insert(line, "\27[1m\27[92m")
                end
                table.insert(line, "[")
                table.insert(line, c)
                table.insert(line, "] ")
                if highlight and i == #stack then
                    table.insert(line, "\27[0m")
                end
            else
                table.insert(line, "    ")
            end
        end
        print(table.concat(line))
    end
    local line = {}
    for i = 1, #stacks do
        table.insert(line, " ")
        table.insert(line, i)
        table.insert(line, "  ")
    end
    table.insert(line, "\n")
    print(table.concat(line))
end

local function PrintProc(proc)
    print(("move %d from %d to %d"):format(table.unpack(proc)))
end

local function ReadResult(stacks)
    local result = {}
    for i = 1, #stacks do
        table.insert(result, stacks[i][#stacks[i]])
    end
    return table.concat(result)
end

local function PartN(name, fn, stacks, procedure, verbose, animate)
    if verbose then
        if not animate then
            print(name)
        else
            print("\27[2J")
            PrintStacks(stacks)
            print(name)
            Wait(animate)
        end
    end
    for _, proc in ipairs(procedure) do
        local move, from, to = table.unpack(proc)
        if verbose then
            PrintStacks(stacks)
            PrintProc(proc)
            if animate then Wait(animate) end
        end
        from = stacks[from]
        to = stacks[to]
        fn(move, from, to)
    end
    if verbose then
        PrintStacks(stacks, true)
        if animate then
            print(name)
            Wait(math.max(0.5, 3 * animate))
        end
    end
    return ReadResult(stacks)
end

local function PartOne(move, from, to)
    for _ = 1, move do
        table.insert(to, table.remove(from))
    end
end

local function PartTwo(move, from, to)
    table.move(from, #from - move + 1, #from, #to + 1, to)
    for _ = 1, move do
        table.remove(from)
    end
end

---@param ... string
local function main(...)
    local stacks, procedure = Load(select(-1, ...))
    local verbose, animate
    for i = 1, #arg - 1 do
        local a = arg[i]
        if a == "-v" or a == "--verbose" then
            verbose = true
        elseif a == "-a" or a:find("^--animate=") then
            verbose = true
            animate = tonumber(a:sub(11))
            if not animate and a == "-a" and tonumber(arg[i + 1]) then
                i = i + 1
                animate = tonumber(arg[i])
            end
            animate = animate or 0.5
        elseif a == "-i" or a == "--input" then
            PrintStacks(stacks)
            for _, proc in ipairs(procedure) do
                PrintProc(proc)
            end
            return
        elseif a == "-h" or a == "--help" then
            print(usage)
            return
        end
    end
    if animate then
        -- alternate buffer on, hide cursor
        print("\27[?1049h\27[?25l")
    end
    local partone = PartN("part 1", PartOne, stacks, procedure, verbose, animate)
    stacks, procedure = Load(select(-1, ...))
    if verbose and not animate then print() end
    local parttwo = PartN("part 2", PartTwo, stacks, procedure, verbose, animate)
    -- alternate buffer off, show cursor
    print((animate and "\27[?25h\27[?1049l" or "") .. partone, parttwo)
end

if pcall(debug.getlocal, 4, 1) then
    return main
else
    main(table.unpack(arg))
end
