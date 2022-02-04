if basename(pwd()) == "aoc"
    cd("2017/15")
end

struct Generator
    initialvalue::Int
    multiplier::Int
    divider::Int
end
GeneratorA(init::Int, div = 1) = Generator(init, 16807, div)
GeneratorB(init::Int, div = 1) = Generator(init, 48271, div)

function Base.iterate(gen::Generator, value = gen.initialvalue)
    value = (value * gen.multiplier) % 2147483647
    while !iszero(value % gen.divider)
        value = (value * gen.multiplier) % 2147483647
    end
    (value, value)
end
Base.eltype(::Type{Generator}) = Int
Base.IteratorSize(::Type{Generator}) = Base.IsInfinite()

function part1(ainit::Int, binit::Int)
    count(
        a & 0xffff == b & 0xffff
        for (a, b) in Iterators.take(
            zip(
                GeneratorA(ainit),
                GeneratorB(binit)),
            40_000_000))
end

@assert part1(65, 8921) == 588
part1(289, 629)

function part2(ainit::Int, binit::Int)
    count(
        a & 0xffff == b & 0xffff
        for (a, b) in Iterators.take(
            zip(
                GeneratorA(ainit, 4),
                GeneratorB(binit, 8)),
            5_000_000))
end

@assert part2(65, 8921) == 309
part2(289, 629)
