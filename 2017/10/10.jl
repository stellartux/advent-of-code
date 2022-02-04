if basename(pwd()) == "aoc"
    cd("2017/10")
end

struct CircularBuffer
    data
end

CircularBuffer(n::Int) = CircularBuffer(collect(0:n-1))

function Base.getindex(buffer::CircularBuffer, index::Integer)
    buffer.data[index%length(buffer.data)+1]
end

function Base.getindex(buffer::CircularBuffer, indices::AbstractRange{<:Integer})
    collect(buffer[index] for index in indices)
end

function Base.setindex!(buffer::CircularBuffer, value, index::Integer)
    buffer.data[index%length(buffer.data)+1] = value
end

function Base.reverse!(buffer::CircularBuffer, start = 0, stop = length(buffer.data) - 1)
    for (i, v) in zip(start:stop, reverse(buffer[start:stop]))
        buffer[i] = v
    end
    buffer
end

function loadlengths(filename::AbstractString)
    parse.(Int, split(readline(filename), ","))
end

function knothash!(buffer::CircularBuffer, lengths)
    position = 0
    for (skip, len) in zip(Iterators.countfrom(0), lengths)
        reverse!(buffer, position, position + len - 1)
        position += skip + len
    end
    buffer
end

function part1(filename::AbstractString, n = 256)
    prod(knothash!(CircularBuffer(n), loadlengths(filename))[0:1])
end

@assert knothash!(CircularBuffer(5), [3, 4, 1, 5])[0:1] == [3, 4]
# part1("input.txt")

function sparsehash!(buffer, input::AbstractString)
    lengths = [(Int(c) for c in input)..., 17, 31, 73, 47, 23]
    position = 0
    for (skip, len) in zip(Iterators.countfrom(0), repeat(lengths, 64))
        reverse!(buffer, position, position + len - 1)
        position += skip + len
    end
    buffer
end
sparsehash(input::AbstractString) = sparsehash!(CircularBuffer(256), input)

function densehash(sparsehash::CircularBuffer)
    join(map(Iterators.partition(sparsehash.data, 16)) do section
        string(reduce(xor, section), base = 16, pad = 2)
    end)
end

densehash(input::AbstractString) = densehash(sparsehash(input))
part2(filename::AbstractString) = densehash(readline(filename))

@assert densehash("") == "a2582a3a0e66e6e86e3812dcb672a272"
@assert densehash("AoC 2017") == "33efeb34ea91902bb2f59c9920caa6cd"
@assert densehash("1,2,3") == "3efbe78a8d82f29979031a4aa0b16a9d"
@assert densehash("1,2,4") == "63960835bcdc130f0b66d7ff4f6a5a8e"

# part2("input.txt")
