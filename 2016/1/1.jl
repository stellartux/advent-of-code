if basename(pwd()) == "aoc"
    cd("2016/1")
end

function part1(filename::AbstractString)
    position = CartesianIndex(0, 0)
    dirs = (CartesianIndex(1, 0), CartesianIndex(0, 1), CartesianIndex(-1, 0), CartesianIndex(0, -1))
    i = 1
    for m in eachmatch(r"[LR]|\d+", readchomp(filename))
        if m.match == "L"
            i = isone(i) ? 4 : i - 1
        elseif m.match == "R"
            i = i % 4 + 1
        else
            position += dirs[i] * parse(Int, m.match)
        end
    end
    abs(position[1]) + abs(position[2])
end

@assert part1("example.txt") == 12
part1("input.txt")

function part2(filename::AbstractString)
    position = CartesianIndex(0, 0)
    visited = Set((position,))
    dirs = (CartesianIndex(1, 0), CartesianIndex(0, 1), CartesianIndex(-1, 0), CartesianIndex(0, -1))
    i = 1
    for m in eachmatch(r"[LR]|\d+", readchomp(filename))
        if m.match == "L"
            i = isone(i) ? 4 : i - 1
        elseif m.match == "R"
            i = i % 4 + 1
        else
            for _ in 1:parse(Int, m.match)
                position += dirs[i]
                if position ∈ visited
                    return abs(position[1]) + abs(position[2])
                else
                    push!(visited, position)
                end
            end
        end
    end
    throw(ArgumentError("Didn't loop"))
end

@assert part2("example2.txt") == 4
part2("input.txt")
