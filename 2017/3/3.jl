if basename(pwd()) == "aoc"
    cd("2017/3")
end
using Base.Iterators

struct SpiralIndices end

function Base.getindex(::SpiralIndices, n::Integer)
    if n <= 0
        throw(ArgumentError("n must be positive"))
    end
    z = isqrt(n)
    z -= iseven(z)
    m = z^2
    pos = [z ÷ 2, z ÷ 2]
    if m < n
        m += 1
        pos[1] += 1
    end
    foreach(1:z) do _
        if m < n
            pos[2] -= 1
            m += 1
        end
    end
    foreach(1:z+1) do _
        if m < n
            pos[1] -= 1
            m += 1
        end
    end
    foreach(1:z+1) do _
        if m < n
            pos[2] += 1
            m += 1
        end
    end
    foreach(1:z) do _
        if m < n
            pos[1] += 1
            m += 1
        end
    end
    CartesianIndex(pos...)
end

function Base.iterate(::SpiralIndices)
    (CartesianIndex(0, 0), (CartesianIndex(0, 0), CartesianIndex(1, 0)))
end

function Base.iterate(::SpiralIndices, (pos, dir))
    if abs(pos[1]) == abs(pos[2])
        if pos[1] >= 0 && pos[2] >= 0
            pos += dir
            dir = CartesianIndex(0, -1)
        else
            dir = if pos[1] >= 0
                CartesianIndex(-1, 0)
            elseif pos[2] >= 0
                CartesianIndex(1, 0)
            else
                CartesianIndex(0, 1)
            end
            pos += dir
        end
    else
        pos += dir
    end
    (pos, (pos, dir))
end

Base.eltype(::Type{SpiralIndices}) = CartesianIndex{2}
Base.IteratorSize(::Type{SpiralIndices}) = Base.IsInfinite()

function manhattandistance(coord::CartesianIndex, other = zero(coord))
    sum(abs.(Tuple(coord)) .- abs.(Tuple(other)))
end

stepcount(n::Integer) = manhattandistance(SpiralIndices()[n])

@assert manhattandistance.(take(SpiralIndices(), 13)) == [
    0, 1, 2, 1, 2, 1, 2, 1, 2, 3, 2, 3, 4
]
@assert stepcount(23) == 2
@assert stepcount(1024) == 31
stepcount(347991)

function neighbours(coord::CartesianIndex)
    (c + coord
     for c in (
        CartesianIndex(-1, -1),
        CartesianIndex(-1, 0),
        CartesianIndex(-1, 1),
        CartesianIndex(0, -1),
        CartesianIndex(0, 1),
        CartesianIndex(1, -1),
        CartesianIndex(1, 0),
        CartesianIndex(1, 1)))
end

function part2(target::Integer)
    sp = Dict(CartesianIndex(0, 0) => 1)
    for coord in drop(SpiralIndices(), 1)
        sp[coord] = sum(get(sp, n, 0) for n in neighbours(coord))
        if sp[coord] > target
            return sp[coord]
        end
    end
end
part2(347991)
