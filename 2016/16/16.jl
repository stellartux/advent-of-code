if basename(pwd()) == "aoc"
    cd("2016/16")
end

bitvec(str::AbstractString) = [c == '1' for c in str]
bitvec(xs::Vector{Int}) = Bool.(xs)

dragonrepeat(str::AbstractString, len::Int) = join(Int.(dragonrepeat(bitvec(str), len)))

function dragonrepeat(data::AbstractVector{Bool}, len::Int)
    while length(data) < len
        append!(data, [0], broadcast(!, reverse(data)))
    end
    data[begin:len]
end

@assert dragonrepeat("1", 3) == "100"
@assert dragonrepeat("0", 3) == "001"
@assert dragonrepeat("11111", 11) == "11111000000"
@assert dragonrepeat("111100001010", 25) == "1111000010100101011110000"

function dragonchecksum(data::AbstractVector{Bool})
    while iseven(length(data))
        data = collect(!(a ⊻ b) for (a, b) in Iterators.partition(data, 2))
    end
    data
end

part1(input::AbstractString, len) = join(Int.(part1(bitvec(input), len)))
part1(input::AbstractVector{Bool}, len) = dragonchecksum(dragonrepeat(input, len))

@assert part1("10000", 20) == "01100"
@time part1("10001001100000001", 272)
@time part1("10001001100000001", 35651584)
