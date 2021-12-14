text = readchomp("input.txt")
part1(text) = count(==('('), text) - count(==(')'), text)

function part2(text)
    total = 0
    for (i, c) in Iterators.enumerate(text)
        if c == '('
            total += 1
        elseif c == ')'
            total -= 1
            if total == -1
                return i
            end
        end
    end
end
