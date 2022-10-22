---@class Coprocessor
---@field a integer
---@field b integer
---@field c integer
---@field d integer
---@field e integer
---@field f integer
---@field g integer
---@field h integer
---@field pc integer
---@field src { name: string, x: string, y: string }[]
local Coprocessor = { __name = "Coprocessor" }

function Coprocessor:new(src)
    if type(src) == "string" then
        local t = {}
        for line in io.lines(src) do
            local name, x, y = line:match("(...) (.) (.+)")
            table.insert(t, { name = name, x = x, y = y })
        end
        src = t
    end
    return setmetatable({ pc = 1, src = src }, self)
end

function Coprocessor:__index(key)
    return rawget(self, key) or Coprocessor[key] or tonumber(key) or 0
end

function Coprocessor:__tostring()
    return string.format("{ pc = %d, a = %d, b = %d, c = %d, d = %d, e = %d, f = %d, g = %d, h = %d }",
        self.pc, self.a, self.b, self.c, self.d, self.e, self.f, self.g, self.h)
end

function Coprocessor:set(x, y)
    self[x] = self[y]
    return self
end

function Coprocessor:sub(x, y)
    self[x] = self[x] - self[y]
    return self
end

function Coprocessor:mul(x, y)
    self[x] = self[x] * self[y]
    self.mulcount = self.mulcount + 1
    return self
end

function Coprocessor:jnz(x, y)
    if self[x] ~= 0 then
        self.pc = self.pc + self[y] - 1
    end
    return self
end

function Coprocessor:step()
    local instr = self.src[self.pc]
    self.pc = self.pc + 1
    if instr then
        return self[instr.name](self, instr.x, instr.y)
    end
end

function Coprocessor:run()
    return self.step, self
end

if select('#', ...) == 0 then
    local c = Coprocessor:new("input.txt")
    for _ in c:run() do end
    print(c.mulcount)
else
    return Coprocessor
end
