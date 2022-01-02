if basename(pwd()) == "aoc"
    cd("2016/2")
end

offset(c::Char) =
    if c == 'U'
        CartesianIndex(-1, 0)
    elseif c == 'D'
        CartesianIndex(1, 0)
    elseif c == 'L'
        CartesianIndex(0, -1)
    elseif c == 'R'
        CartesianIndex(0, 1)
    end

function part1(filename::AbstractString)
    keypad = [1 2 3; 4 5 6; 7 8 9]
    position = CartesianIndex(2, 2)
    result = Int[]
    for line in eachline(filename)
        for c in line
            newposition = position + offset(c)
            if checkbounds(Bool, keypad, newposition)
                position = newposition
            end
        end
        push!(result, keypad[position])
    end
    foldl((a, b) -> 10a + b, result, init = 0)
end

@assert part1("example.txt") == 1985
part1("input.txt")

function part2(filename::AbstractString)
    keypad = [
        '0' '0' '1' '0' '0'
        '0' '2' '3' '4' '0'
        '5' '6' '7' '8' '9'
        '0' 'A' 'B' 'C' '0'
        '0' '0' 'D' '0' '0'
    ]
    position = CartesianIndex(3, 1)
    result = ""
    for line in eachline(filename)
        for c in line
            newposition = position + offset(c)
            if checkbounds(Bool, keypad, newposition) && keypad[newposition] != '0'
                position = newposition
            end
        end
        result *= keypad[position]
    end
    result
end

@assert part2("example.txt") == "5DB3"
part2("input.txt")
