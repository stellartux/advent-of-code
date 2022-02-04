if basename(pwd()) == "aoc"
    cd("2017/14")
end

include("../10/10.jl")

function tobits(str::AbstractString)
    collect(
        b == '1'
        for c in str
        for b in bitstring(parse(UInt8, c, base = 16))[5:8]
    )
end

@assert tobits("a0c2017") == [
    1, 0, 1, 0,
    0, 0, 0, 0,
    1, 1, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 0,
    0, 0, 0, 1,
    0, 1, 1, 1
]

function loadsquares(input::AbstractString)
    squares = zeros(Bool, 128, 128)
    for row in 0:127
        for (col, b) in enumerate(tobits(densehash(input * "-" * string(row))))
            squares[row+1, col] = b
        end
    end
    squares
end

part1(input::AbstractString) = count(loadsquares(input))

@assert part1("flqrgnkx") == 8108
part1("xlqgujun")

function part2(input::AbstractString)
    squares = loadsquares(input)
    stack = CartesianIndex[]
    total = 0
    index = findfirst(squares)
    while !isnothing(index)
        total += 1
        squares[index] = false
        append!(stack, neighbours(index))
        while !isempty(stack)
            index = popfirst!(stack)
            if squares[index]
                squares[index] = false
                append!(stack, neighbours(index))
            end
        end
        index = findfirst(squares)
    end
    total
end

function neighbours(coord)
    [
        coord + c
        for c in [
            CartesianIndex(0, 1),
            CartesianIndex(0, -1),
            CartesianIndex(1, 0),
            CartesianIndex(-1, 0)
        ]
        if c[1] + coord[1] in 1:128 && c[2] + coord[2] in 1:128
    ]
end

@assert part2("flqrgnkx") == 1242
part2("xlqgujun")
