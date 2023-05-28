_ = #_G --[[
# code here is only visible to Julia

then = true
struct ChronalCalibrator
    seen::BitSet
end
ChronalCalibrator() = ChronalCalibrator(BitSet(0))
Base.getindex(cc::ChronalCalibrator, value) = value in cc.seen
Base.setindex!(cc::ChronalCalibrator, set::Bool, index) =
    (set ? push! : delete!)(cc.seen, index)

#= code here is only visible to Lua ]]
---@diagnostic disable: lowercase-global

ARGS = arg
Base = {
    Fix1 = function(...) return tonumber end
}

parse, Int = nil, nil
eachline = io.lines
println = print
function length(xs) return #xs end

function mod1(x, y) return (x - 1) % y + 1 end

function collect(...)
    local result = {}
    for key, value in ... do
        table.insert(result, value or key)
    end
    return result
end

function map(fn, iter)
    return function()
        local v = iter()
        if v ~= nil then
            return fn(v)
        end
    end
end

function sum(xs)
    local result = 0
    for _, x in ipairs(xs) do
        result = result + x
    end
    return result
end

function ChronalCalibrator()
    return { [0] = true }
end

-- code here is visible to Julia and Lua =#

frequencies = collect(map(Base.Fix1(parse, Int), eachline(ARGS[1])))

println(sum(frequencies))

function parttwo(offsets, seen, frequency, i)
    i = mod1(i + 1, length(offsets))
    frequency = frequency + offsets[i]
    if seen[frequency] then
        return frequency
    else
        seen[frequency] = true
        return parttwo(offsets, seen, frequency, i)
    end
end

println(parttwo(frequencies, ChronalCalibrator(), 0, 0))
