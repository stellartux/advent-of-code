#!/usr/bin/lua

local function AllUnique(t)
    local r = {}
    for _, v in ipairs(t) do
        if r[v] then return false end
        r[v] = true
    end
    return true
end

local function FindResult(s, n)
    local t = {}
    for i = 1, n do
        table.insert(t, s:byte(i))
    end
    local i = n
    while i <= #s do
        if AllUnique(t) then return i end
        i = i + 1
        table.remove(t, 1)
        table.insert(t, s:byte(i))
    end
end

---@param ... string
local function main(...)
    local s = io.open(..., "r"):read()
    print(FindResult(s, 4), FindResult(s, 14))
end

if pcall(debug.getlocal, 4, 1) then
    return main
else
    main(table.unpack(arg))
end
