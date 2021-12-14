loaddict(filename::AbstractString) = Dict(lval => rval for (rval, lval) in split.(eachline(filename), " -> "))
function prettyprint(dict)
    cache = Dict{AbstractString, UInt16}()
    foreach(sort!(collect(keys(dict)))) do key
        println(key, ": ", bobbycalculate(dict, key, cache))
    end
end

bobbycalculate(filename::AbstractString, var) = bobbycalculate(loaddict(filename), var)
function bobbycalculate(dict::Dict, var::AbstractString, cache = Dict{AbstractString, UInt16}())
    get!(cache, var) do
        str = dict[var]
        if all(isdigit, str)
            parse(UInt16, str)
        elseif startswith(str, "NOT")
            ~bobbycalculate(dict, str[5:end], cache)
        elseif str in keys(dict)
            bobbycalculate(dict, str, cache)
        else
            l, i, r = match(r"(.+) (AND|OR|[LR]SHIFT) (.+)", str)
            lval = all(isdigit, l) ? parse(UInt16, l) : bobbycalculate(dict, l, cache)
            instr = if i == "AND"
                (&)
            elseif i == "OR"
                (|)
            elseif i == "LSHIFT"
                (<<)
            elseif i == "RSHIFT"
                (>>)
            end
            rval = all(isdigit, r) ? parse(UInt16, r) : bobbycalculate(dict, r, cache)
            instr(lval, rval)
        end
    end
end

if basename(pwd()) == "aoc"
    cd("2015/7")
end
part1() = bobbycalculate(loaddict("input.txt"), "a")
part2() = bobbycalculate(loaddict("input.txt"), "a", Dict("b"=>0xb3f1))
