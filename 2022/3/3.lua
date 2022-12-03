---@param c string
local function score(c)
    return c:byte() - (c == c:lower() and 96 or 38)
end

---@param filename string
local function PartOne(filename)
    local total = 0
    for rucksack in io.lines(filename) do
        local l = #rucksack / 2
        for i = 1, l do
            local c = rucksack:sub(i, i)
            if rucksack:find(c, l + 1, true) then
                total = total + score(c)
                break
            end
        end
    end
    return total
end

local function PartTwo(filename)
    local total = 0
    local f = assert(io.open(filename, "r"),
        ("Could not open '%s'"):format(filename))
    local a, b, c = f:read(), f:read(), f:read()
    while a do
        for ch in a:gmatch("%w") do
            if b:find(ch) and c:find(ch) then
                total = total + score(ch)
                break
            end
        end
        a, b, c = f:read(), f:read(), f:read()
    end
    io.close(f)
    return total
end

if pcall(debug.getlocal, 4, 1) then
    return {
        PartOne = PartOne,
        PartTwo = PartTwo
    }
else
    print(PartOne(table.unpack(arg)))
    print(PartTwo(table.unpack(arg)))
end
