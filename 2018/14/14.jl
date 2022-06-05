if basename(pwd()) == "aoc"
    cd("2018/14")
end

function nextindex(i, recipes)
    i += recipes[i] + 1
    while i > length(recipes)
        i -= length(recipes)
    end
    i
end

function part1(len=327901)
    recipes = [3, 7]
    i = 1
    j = 2
    while length(recipes) < len + 10
        append!(recipes, reverse(digits(recipes[i] + recipes[j])))
        i = nextindex(i, recipes)
        j = nextindex(j, recipes)
    end
    join(recipes[len+1:len+10])
end

@assert part1(9) == "5158916779"
@assert part1(5) == "0124515891"

@time part1()

function part2(x=327901)
    recipes = [3, 7]
    ds = reverse(digits(x))
    l = length(ds)
    i = 1
    j = 2
    while true
        append!(recipes, reverse(digits(recipes[i] + recipes[j])))
        if length(recipes) >= l + 1 && recipes[end-l:end-1] == ds
            return length(recipes) - l - 1
        elseif length(recipes) >= l && recipes[end-l+1:end] == ds
            return length(recipes) - l
        end
        i = nextindex(i, recipes)
        j = nextindex(j, recipes)
    end
end

@assert part2(51589) == 9
@assert part2(92510) == 18
@assert part2(59414) == 2018
@time part2()
