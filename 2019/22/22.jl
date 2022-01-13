if basename(pwd()) == "aoc"
    cd("2019/22")
end

function shuffle(filename::AbstractString, len)
    cards = [0:len-1...]
    for line in eachline(filename)
        if startswith(line, "deal with increment")
            n = parse(Int, line[21:end])
            cards[[((i - 1) * n) % len + 1 for i in eachindex(cards)]] .= cards
        elseif startswith(line, "deal into new stack")
            reverse!(cards)
        elseif startswith(line, "cut")
            cards = circshift(cards, -parse(Int, line[5:end]))
        end
    end
    cards
end

@assert shuffle("example1.txt", 10) == [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]
@assert shuffle("example2.txt", 10) == [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]
@assert shuffle("example3.txt", 10) == [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]
@assert shuffle("example4.txt", 10) == [9, 2, 5, 8, 1, 4, 7, 0, 3, 6]

function part1(filename::AbstractString, len)
    findfirst(==(2019), shuffle(filename, len)) - 1
end

part1("input.txt", 10007)
