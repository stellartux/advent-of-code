function factorize(n::Integer)
    fs = [1, n]
    for i in 2:isqrt(n)
        d, r = divrem(n, i)
        if iszero(r)
            push!(fs, d, i)
        end
    end
    unique!(sort!(fs))
end

function part1(i = 33100000 ÷ 10)
    Iterators.peel(Iterators.dropwhile(n -> sum(factorize(n)) < i, Iterators.countfrom(1)))[1]
end

function part2(i = 33100000 ÷ 11)
    Iterators.peel(Iterators.dropwhile(n -> sum(
            filter(factorize(n)) do x
                50x >= n
            end
        ) < i, Iterators.countfrom(1)))[1]
end
