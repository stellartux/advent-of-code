if basename(pwd()) != "25"
    cd("2020/25")
end

loadnumbers(filename) = parse.(Int, eachline(filename))

function loopsize(target)
    value = 1
    count = 0
    while value != target
        value = value * 7 % 20201227
        count += 1
    end
    count
end

function transform(subject, loopsize)
    value = 1
    for _ = 1:loopsize
        value = value * subject % 20201227
    end
    value
end

crack(cardkey, doorkey) = transform(doorkey, loopsize(cardkey))

@assert crack(loadnumbers("example.txt")...) == 14897079
println(crack(loadnumbers("input.txt")...))
