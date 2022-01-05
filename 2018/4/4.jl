if basename(pwd()) == "aoc"
    cd("2018/4")
end

# presorted input data in shell
function loadshifts(filename::AbstractString)
    guards = Dict{Int,Dict{AbstractString,Vector{UnitRange{Int}}}}()
    activeguard = 0
    fellasleep = 0
    for line in eachline(filename)
        date, minutes, instr = match(r"\[(\S+) \d+:(\d+)\] (.*)", line)
        if startswith(instr, "Guard")
            activeguard = parse(Int, match(r"#(\d+)", line)[1])
        elseif instr == "falls asleep"
            fellasleep = parse(Int, minutes)
        else
            if !haskey(guards, activeguard)
                guards[activeguard] = Dict{AbstractString,Vector{UnitRange{Int}}}()
            end
            if !haskey(guards[activeguard], date)
                guards[activeguard][date] = UnitRange{Int}[]
            end
            wokeup = parse(Int, minutes) - 1
            push!(guards[activeguard][date], fellasleep:wokeup)
        end
    end
    guards
end

part1(filename::AbstractString) = part1(loadshifts(filename))
function part1(guards::Dict)
    _, sleepiestguardid = findmax(guards) do guard
        sum(sum.(broadcast.(length, values(guard))))
    end
    sleepyhead = guards[sleepiestguardid]
    _, sleepiestminute = findmax(0:59) do minute
        count(in.(minute, Iterators.flatten(values(sleepyhead))))
    end
    sleepiestguardid * (sleepiestminute - 1)
end
@assert part1("example.txt") == 240
part1("input.txt")

part2(filename::AbstractString) = part2(loadshifts(filename))
function part2(guards::Dict)
    ((daysasleep, atminute), sleepyid) = findmax(guards) do guard
        sleeps = vcat(values(guard)...)
        findmax(0:59) do minute
            count(minute in sleep for sleep in sleeps)
        end .- (0, 1)
    end
    sleepyid * atminute
end
@assert part2("example.txt") == 4455
part2("input.txt")
