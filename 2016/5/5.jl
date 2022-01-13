if basename(pwd()) == "aoc"
    cd("2016/5")
end

using MD5

function part1(input::AbstractString)
    result = []
    for i in Iterators.countfrom(0)
        hash = bytes2hex(md5(input * string(i)))
        if startswith(hash, "00000")
            push!(result, hash[6])
            if length(result) == 8
                return join(result)
            end
        end
    end
end

@assert part1("abc") == "18f47a30"
part1("uqwqemis")

function part2(input::AbstractString)
    result = repeat([' '], 8)
    for i in Iterators.countfrom(0)
        hash = bytes2hex(md5(input * string(i)))
        if startswith(hash, "00000")
            i = parse(Int, hash[6], base = 16)
            if i < 8 && isspace(result[i+1])
                result[i+1] = hash[7]
                if all(!isspace, result)
                    return join(result)
                end
            end
        end
    end
end
@assert part2("abc") == "05ace8e3"
part2("uqwqemis")
