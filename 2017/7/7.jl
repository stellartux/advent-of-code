if basename(pwd()) == "aoc"
    cd("2017/7")
end

function mermaid(filename::AbstractString)
    open(replace(filename, ".txt" => ".mmd"), "w") do f
        write(f, "graph TD\r\n")
        for line in eachline(filename; keep = true)
            if occursin(r"->", line)
                write(f, replace(line, ", " => " & ", "->" => "-->", r"\(\d+\)" => ""))
            end
        end
    end
end

struct Disc
    value::Int
    towers::Vector
end
Disc(value) = Disc(value, Disc[])

function loaddata(filename::AbstractString)
    lines = readlines(filename)
    discs = Dict{String,Disc}()
    for line in lines
        name, num = match(r"^(\S+) \((\d+)\)", line)
        push!(discs, name => Disc(parse(Int, num)))
    end
    for line in lines
        if occursin(" -> ", line)
            name, rhs = split(line, r" \(\d+\) -> ")
            othernames = split(rhs, ", ")
            push!(discs[name].towers, (discs[n] for n in othernames)...)
        end
    end
    discs
end

function findroot(discs)
    only(setdiff(values(discs), (tower for disc in values(discs) for tower in disc.towers)))
end

part1(filename::AbstractString) = part1(loaddata(filename))
function part1(discs)
    root = findroot(discs)
    for (k, v) in pairs(discs)
        if v == root
            return k
        end
    end
end

part2(filename::AbstractString) = part2(loaddata(filename))
function part2(discs)
    disc = findunbalanced(findroot(discs))
    weights = weight.(disc.towers)
    if weights[1] == weights[2]
        weights[2]
    else
        weights[3]
    end
end

function isbalanced(discs)
    weights = weight.(discs.towers)
    weights[1] == weights[2] == weights[3]
end

function findunbalanced(disc)
    if !isbalanced(disc)
        disc
    elseif !isisempty(disc.towers)
        something((findunbalanced(d) for d in disc.towers)..., Some(nothing))
    end
end

function weight(disc)
    disc.value + sum(weight(tower) for tower in disc.towers; init = 0)
end
