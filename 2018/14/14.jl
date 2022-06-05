if basename(pwd()) == "aoc"
    cd("2018/14")
end

function part1(len=327901)
    recipes = [3, 7]
    function nextindex(i)
        i += recipes[i] + 1
        while i > length(recipes)
            i -= length(recipes)
        end
        i
    end
    i = 1
    j = 2
    while length(recipes) < len + 10
        append!(recipes, reverse(digits(recipes[i] + recipes[j])))
        i = nextindex(i)
        j = nextindex(j)
    end
    join(recipes[len+1:len+10])
end

@assert part1(9) == "5158916779"
@assert part1(5) == "0124515891"

@time part1()
