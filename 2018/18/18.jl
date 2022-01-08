if basename(pwd()) == "aoc"
    cd("2018/18")
end

@enum Acre Open = UInt8('.') Trees = UInt8('|') Lumberyard = UInt8('#')

function loadlumber(filename::AbstractString)
    cs = [Acre(UInt8(char)) for line in eachline(filename) for char in line]
    permutedims(reshape(cs, isqrt(length(cs)), :))
end

function prettyprint(as::Matrix{Acre})
    for row in eachrow(as)
        println((Char(UInt8(c)) for c in row)...)
    end
end

function prettyprint(acres::Matrix{Acre}, steps::Integer)
    println("Initial state:")
    prettyprint(acres)
    if steps <= 0
        return
    end
    current, previous = takestep(acres), copy(acres)
    println("\nAfter 1 minute:")
    prettyprint(current)
    for i in 2:steps
        current, previous = step!(previous, current), current
        println("\nAfter ", i, " minutes:")
        prettyprint(current)
    end
    current
end

takestep(acres::AbstractMatrix{Acre}) = step!(similar(acres), acres)

function neighbours(matrix::AbstractMatrix, index::CartesianIndex)
    (
        neighbour
        for neighbour in (index + i for i in filter(!iszero, CartesianIndices((-1:1, -1:1))))
        if checkbounds(Bool, matrix, neighbour)
    )
end

function step!(dst::AbstractMatrix{Acre}, src::AbstractMatrix{Acre})
    copy!(dst, src)
    for (index, acre) in pairs(IndexCartesian(), src)
        ns = collect(src[n] for n in neighbours(src, index))
        if acre == Open && count(==(Trees), ns) >= 3
            dst[index] = Trees
        elseif acre == Trees && count(==(Lumberyard), ns) >= 3
            dst[index] = Lumberyard
        elseif acre == Lumberyard && !(any(==(Trees), ns) && any(==(Lumberyard), ns))
            dst[index] = Open
        end
    end
    dst
end

function info(acres::AbstractMatrix{Acre})
    woods = count(==(Trees), acres)
    yards = count(==(Lumberyard), acres)
    println("there are ", woods, " wooded acres and ", yards, " lumberyards.")
    woods * yards
end

part1(filename::AbstractString) = info(prettyprint(loadlumber(filename), 10))

@assert part1("example.txt") == 1147
part1("input.txt")
