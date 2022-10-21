#!/usr/bin/lua

---@param steps integer
function Spin(steps)
    local t = { 0 }
    local i = 1
    for v = 1, 2017 do
        i = (i + steps - 1) % #t + 2
        table.insert(t, i, v)
    end
    return t[i + 1]
end
