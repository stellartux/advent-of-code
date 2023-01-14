local master = require("intcode"):load(...)
master:copy({
    input = function() return 1 end,
    output = function(_, x) if x ~= 0 then return x end end
}):run(print)
master:copy({ input = function() return 5 end }):run(print)
