if basename(pwd()) != "18"
    cd("2022/18")
end

function load18(filename)
    Set(
        CartesianIndex(parse.(Int, match(r"(\d+),(\d+),(\d+)", line))...)
        for line in eachline(filename)
    )
end

function neighbours3d(coord)
    (
        CartesianIndex(0, 0, 1) + coord,
        CartesianIndex(0, 0, -1) + coord,
        CartesianIndex(0, -1, 0) + coord,
        CartesianIndex(0, 1, 0) + coord,
        CartesianIndex(1, 0, 0) + coord,
        CartesianIndex(-1, 0, 0) + coord,
    )
end

partone18(filename::AbstractString) = partone18(load18(filename))
parttwo18(filename::AbstractString) = parttwo18(load18(filename))

partone18(cs) = sum(count(!(n in cs) for n in neighbours3d(c)) for c in cs)

isperimeter(c, A) = any(t == 1 || t == s for (t, s) in zip(Tuple(c), size(A)))

function parttwo18(coords)
    mini, maxi = extrema(coords)
    delta = one(mini) - mini
    maxi += delta
    coords = [c + delta for c in coords]
    A = zeros(UInt8, Tuple(maxi)...)
    A[coords] .= 0x02
    function flood(c)
        if c in keys(A) && iszero(A[c])
            A[c] = 0x01
            flood.(neighbours3d(c))
        end
    end
    for k in keys(A)
        if isperimeter(k, A)
            flood(k)
        end
    end
    total = 0
    for p in pairs(A)
        if p[2] == 0x02
            for n in neighbours3d(p[1])
                total += n in keys(A) ? A[n] & 1 : 1
            end
        end
    end
    total
end
