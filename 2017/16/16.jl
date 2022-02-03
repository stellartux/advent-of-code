if basename(pwd()) == "aoc"
    cd("2017/16")
end

function part1(filename::AbstractString, lastchar = 'p')
    chars = ['a':lastchar...]
    lines = split(readchomp(filename), ',')
    for line in lines
        if startswith(line, 's')
            chars = circshift(chars, parse(Int, line[2:end]))
        elseif startswith(line, 'x')
            i, j = parse.(Int, split(line[2:end], '/')) .+ 1
            chars[i], chars[j] = chars[j], chars[i]
        elseif startswith(line, 'p')
            replace!(chars, line[2] => line[4], line[4] => line[2])
        end
    end
    join(chars)
end

part1("example.txt", 'e')
part1("input.txt")
