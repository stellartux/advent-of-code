function part1(filename)
    layers = collect(Iterators.partition(readchomp(filename), 25 * 6))
    _, i = findmin(layer -> count(==('0'), layer), layers)
    count(==('1'), layers[i]) * count(==('2'), layers[i])
end

function part2(filename)
    layers = Iterators.Stateful(Iterators.partition(readchomp(filename), 25 * 6))
    image = popfirst!(layers)
    while any(==('2'), image)
        image = [old == '2' ? new : old for (old, new) in zip(image, popfirst!(layers))]
    end
    open("image.pbm", "w") do f
        write(f, "P1\n25 6\n", join(join.(Iterators.partition(image, 25), ' '), '\n'))
    end
end
