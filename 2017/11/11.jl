if basename(pwd()) == "aoc"
    cd("2017/11")
end

function hexdistance(path::AbstractString)
    position = [0, 0]
    for step in split(path, ',')
        position .+= if step == "n"
            [0, 2]
        elseif step == "ne"
            [1, 1]
        elseif step == "se"
            [1, -1]
        elseif step == "nw"
            [-1, 1]
        elseif step == "sw"
            [-1, -1]
        else
            [0, -2]
        end
    end
    sum(abs.(position)) ÷ 2
end

@assert hexdistance("ne,ne,ne") == 3
@assert hexdistance("ne,ne,sw,sw") == 0
@assert hexdistance("ne,ne,s,s") == 2
@assert hexdistance("se,sw,se,sw,sw") == 3

hexdistance(readline("input.txt"))

function maxhexdistance(path::AbstractString)
    position = [0, 0]
    maxdist = 0
    for step in split(path, ',')
        position .+= if step == "n"
            [0, 2]
        elseif step == "ne"
            [1, 1]
        elseif step == "se"
            [1, -1]
        elseif step == "nw"
            [-1, 1]
        elseif step == "sw"
            [-1, -1]
        else
            [0, -2]
        end
        maxdist = max(maxdist, sum(abs.(position)) ÷ 2)
    end
    maxdist
end

maxhexdistance(readline("input.txt"))
