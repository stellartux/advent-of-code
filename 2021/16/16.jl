if basename(pwd()) == "aoc"
    cd("2021/16")
end
using Base.Iterators

loadfile(filename::AbstractString) = tobits(readchomp(filename))

function tobits(str::AbstractString)
    Iterators.Stateful(flatten(
        reverse(
            digits(
                parse(Int, c, base = 16),
                base = 2,
                pad = 4
            )
        ) for c in str
    ))
end

frombits(bits)::Int = foldl((a, c) -> 2a + c, bits, init = 0)

destructure(filename::AbstractString) = destructure(loadfile(filename))
function destructure(bits)
    version = frombits(take(bits, 3))
    typeid = frombits(take(bits, 3))
    if typeid == 4 # literal
        keepreading = 1
        result = Int[]
        while !iszero(keepreading)
            keepreading = frombits(take(bits, 1))
            append!(result, take(bits, 4))
        end
        (version = version, typeid = typeid, value = frombits(result))
    else # operator
        if iszero(frombits(take(bits, 1)))
            subpackets = Iterators.Stateful(take(bits, frombits(take(bits, 15))))
            value = []
            while !isempty(subpackets)
                push!(value, destructure(subpackets))
            end
            (version = version, typeid = typeid, value = value)
        else
            (
                version = version,
                typeid = typeid,
                value = collect(destructure(bits) for n = 1:frombits(take(bits, 11)))
            )
        end
    end
end

versionsum(xs) = sum(versionsum.(xs), init = 0)
versionsum(packet::NamedTuple) = packet.version + versionsum(packet.value)
versionsum(i::Integer) = 0

part1(str) = versionsum(destructure(str))

@assert part1(tobits("8A004A801A8002F478")) == 16
@assert part1(tobits("620080001611562C8802118E34")) == 12
@assert part1(tobits("C0015000016115A2E0802F182340")) == 23
@assert part1(tobits("A0016C880162017C3686B18A3D4780")) == 31
@assert part1(loadfile("example.txt")) == 6
part1(loadfile("input.txt"))

function evaluate(packet)
    if packet.typeid == 0
        sum(evaluate.(packet.value))
    elseif packet.typeid == 1
        prod(evaluate.(packet.value))
    elseif packet.typeid == 2
        minimum(evaluate.(packet.value))
    elseif packet.typeid == 3
        maximum(evaluate.(packet.value))
    elseif packet.typeid == 4
        packet.value
    else
        l, r = evaluate.(packet.value)
        if packet.typeid == 5
            l > r ? 1 : 0
        elseif packet.typeid == 6
            l < r ? 1 : 0
        elseif packet.typeid == 7
            l == r ? 1 : 0
        end
    end
end

part2(str) = evaluate(destructure(str))

@assert part2(tobits("C200B40A82")) == 3
@assert part2(tobits("04005AC33890")) == 54
@assert part2(tobits("880086C3E88112")) == 7
@assert part2(tobits("CE00C43D881120")) == 9
@assert part2(tobits("D8005AC2A8F0")) == 1
@assert part2(tobits("F600BC2D8F")) == 0
@assert part2(tobits("9C005AC2F8F0")) == 0
@assert part2(tobits("9C0141080250320F1802104A08")) == 1
part2(loadfile("input.txt"))
