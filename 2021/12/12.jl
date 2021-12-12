function loadcavelist(filename::AbstractString)
    links = Dict{SubString, Vector{SubString}}()
    function addlink(l, r)
        if haskey(links, l)
            push!(links[l], r)
        else
            links[l] = [r]
        end
    end
    for line in readlines(filename)
        l, r = split(line, '-')
        addlink(l, r)
        addlink(r, l)
    end
    links
end

isbigcave(cave::AbstractString) = first(cave) < 'a'

function part1(filename::AbstractString)
    length(takepath(loadcavelist(filename), "start"))
end

function takepath(links::Dict, here::AbstractString, history = [])
    push!(history, here)
    if here == "end"
        [history]
    else
        options = filter(links[here]) do cave
            isbigcave(cave) || !(cave in history)
        end
        vcat((takepath(links, options, copy(history)) for options in options)...)
    end
end

if basename(pwd()) == "aoc"
    cd("2021/12")
end

@assert part1("example.txt") == 10
@assert part1("example2.txt") == 19
@assert part1("example3.txt") == 226
# clipboard(@show part1("input.txt"))

function part2(filename::AbstractString)
    length(takepath2(loadcavelist(filename), "start"))
end

function takepath2(links::Dict, here::AbstractString, history = [])
    push!(history, here)
    if here == "end"
        [history]
    else
        options = filter(links[here]) do cave
            cave != "start" && (
                isbigcave(cave) ||
                !(cave in history) ||
                allunique(filter(!isbigcave, history))
            )
        end
        vcat((takepath2(links, option, copy(history)) for option in options)...)
    end
end

@assert part2("example.txt") == 36
@assert part2("example2.txt") == 103
@assert part2("example3.txt") == 3509
clipboard(@show part2("input.txt"))
