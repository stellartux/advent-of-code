#!/usr/bin/env lua
-- usage: 2/2.lua [filename]
local master = require("intcode"):load(...)
local m = master:copy()
m[1] = 12
m[2] = 2
m:run()
print(m[0])

for noun = 0, 99 do
    for verb = 0, 99 do
        m = master:copy()
        m[1] = noun
        m[2] = verb
        m:run()
        if m[0] == 19690720 then
            print(100 * noun + verb)
            return
        end
    end
end
