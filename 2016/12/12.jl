if basename(pwd()) == "aoc"
    cd("2016/12")
end

mutable struct AssembunnyVM
    a::Int
    b::Int
    c::Int
    d::Int
end
AssembunnyVM() = AssembunnyVM(0, 0, 0, 0)

function Base.getindex(vm::AssembunnyVM, i::AbstractString)
    if isdigit(last(i))
        parse(Int, i)
    else
        getfield(vm, Symbol(i))
    end
end
function Base.setindex!(vm::AssembunnyVM, val::AbstractString, i::AbstractString)
    setfield!(vm, Symbol(i), vm[val])
end
function Base.setindex!(vm::AssembunnyVM, val::Integer, i::AbstractString)
    setfield!(vm, Symbol(i), val)
end

function assembunny(filename::AbstractString; vm = AssembunnyVM())
    lines = split.(readlines(filename))
    pc = 1
    while firstindex(lines) <= pc <= lastindex(lines)
        line = lines[pc]
        try
            if line[1] == "cpy"
                vm[line[3]] = line[2]
            elseif line[1] == "inc"
                vm[line[2]] += 1
            elseif line[1] == "dec"
                vm[line[2]] -= 1
            elseif line[1] == "jnz"
                if !iszero(vm[line[2]])
                    pc += vm[line[3]]
                    continue
                end
            end
        catch err
            println("Crashing on line ", pc, ": ", join(line))
            rethrow(err)
        end
        pc += 1
    end
    vm
end

function part1()
    @assert assembunny("example.txt").a == 42
    assembunny("input.txt").a
end
part2() = assembunny("input.txt"; vm = AssembunnyVM(0, 0, 1, 0)).a
