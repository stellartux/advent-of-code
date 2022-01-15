if basename(pwd()) == "aoc"
    cd("2016/9")
end

function decomplength(str::AbstractString)
    total = 0
    i = 1
    @views while i <= lastindex(str)
        c = str[i]
        if c == '('
            m = match(r"(\d+)x(\d+)\)", str[i+1:end])
            if !isnothing(m)
                x, y = parse.(Int, m)
                i += length(m.match) + 1
                total += y * count(!isspace, str[i:i+x-1])
                i += x - 1
            end
        elseif !isspace(c)
            total += 1
        end
        i += 1
    end
    total
end

@assert decomplength.(eachline("example.txt")) == [6, 7, 9, 11, 6, 18]
decomplength(readline("input.txt"))

function deepdecomplength(str::AbstractString)
    total = 0
    i = 1
    @views while i <= lastindex(str)
        c = str[i]
        if c == '('
            m = match(r"(\d+)x(\d+)\)", str[i+1:end])
            if !isnothing(m)
                x, y = parse.(Int, m)
                i += length(m.match) + 1
                total += y * deepdecomplength(str[i:i+x-1])
                i += x - 1
            end
        elseif !isspace(c)
            total += 1
        end
        i += 1
    end
    total
end

@assert deepdecomplength.(eachline("example2.txt")) == [9, 20, 241920, 445]
deepdecomplength(readline("input.txt"))
