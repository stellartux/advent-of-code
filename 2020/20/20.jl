if basename(pwd()) != "20"
    cd("2020/20")
end

struct Tile
    id::Int
    data::BitMatrix
end

function Tile(lines::AbstractVector{<:AbstractString})
    Tile(
        parse(Int, match(r"Tile (\d+):", lines[1]).captures[1]),
        [s[i] == '#' for s in lines[2:11], i = 1:10]
    )
end

Tile(s::AbstractString) = Tile(split(s, '\n'))
Base.:(==)(t::Tile, o::Tile) = t.id == o.id

function Base.print(io::IO, t::Tile)
    println(io, "Tile ", t.id, ':')
    for row in eachrow(t.data)
        println(io, (iszero(c) ? '.' : '#' for c in row)...)
    end
end

function Base.show(io::IO, t::Tile)
    if get(io, :compact, false)
        print(io, "Tile(", t.id, ", ...)")
    else
        println(io, "Tile(\"\"\"")
        print(io, t)
        print(io, "\"\"\")")
    end
end

loadtiles(filename::AbstractString) = Tile.(Iterators.partition(eachline(filename), 12))

pack(b::AbstractVector{Bool})::UInt16 = foldl((a, c) -> (a << 0x001) | c, b, init=0x0000)

top(t::Tile) = pack(view(t.data, 1, :))
bottom(t::Tile) = pack(view(t.data, lastindex(t.data, 1), :))
left(t::Tile) = pack(view(t.data, :, 1))
right(t::Tile) = pack(view(t.data, :, lastindex(t.data, 2)))

reversetop(t::Tile) = pack(reverse(view(t.data, 1, :)))
reversebottom(t::Tile) = pack(reverse(view(t.data, lastindex(t.data, 1), :)))
reverseleft(t::Tile) = pack(reverse(view(t.data, :, 1)))
reverseright(t::Tile) = pack(reverse(view(t.data, :, lastindex(t.data, 2))))

Base.rot180(t::Tile) = Tile(t.id, rot180(t.data))
Base.rotl90(t::Tile) = Tile(t.id, rotl90(t.data))
Base.rotr90(t::Tile) = Tile(t.id, rotr90(t.data))

function edgeset(t::Tile)
    t.id => Set((
        top(t), bottom(t), left(t), right(t),
        reversetop(t), reversebottom(t), reverseleft(t), reverseright(t)
    ))
end

function partone(filename::AbstractString)
    tiles = loadtiles(filename)
    edges = edgeset.(tiles)
    neighbours = Dict{Int,Set{UInt16}}(key => Set{UInt16}() for key in first.(edges))
    for (i, edge₁) in pairs(edges)
        for j in (i+1):lastindex(edges)
            edge₂ = edges[j]
            if !isempty(intersect(edge₁[2], edge₂[2]))
                push!(neighbours[edge₂[1]], edge₁[1])
                if length(push!(neighbours[edge₁[1]], edge₂[1])) == 4
                    break
                end
            end
        end
    end
    prod(key for (key, ns) in neighbours if length(ns) == 2)
end

