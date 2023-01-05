if basename(pwd()) != "24"
    cd("2020/24")
end

"""
See https://www.redblobgames.com/grids/hexagons/#coordinates-axial for details.
"""
struct HexCoord <: Base.AbstractCartesianIndex{2}
    q
    r
end
function HexCoord(q, r, s)
    @assert s == -q - r "Not a valid hexagonal coordinate"
    HexCoord(q, r)
end
Base.zero(::Type{HexCoord}) = HexCoord(0, 0)

function Base.getproperty(hac::HexCoord, sym::Symbol)
    if sym == :s
        -getfield(hac, :q) - getfield(hac, :r)
    else
        getfield(hac, sym)
    end
end
Base.propertynames(::HexCoord; private::Bool=false) = (:q, :r, :s)

Base.:+(a::HexCoord, b::HexCoord) = HexCoord(a.q + b.q, a.r + b.r)
Base.:-(a::HexCoord, b::HexCoord) = HexCoord(a.q - b.q, a.r - b.r)

distance(a::HexCoord, b::HexCoord) = max(abs(a.q - b.q), abs(a.r - b.r), abs(a.s - b.s))

"""
        0,-1    1,-1

    -1,0    q, r    1,0

        -1,1    0, 1

"""
directions = (
    nw=HexCoord(0, -1),
    ne=HexCoord(+1, -1),
    w=HexCoord(-1, 0),
    e=HexCoord(+1, 0),
    sw=HexCoord(-1, +1),
    se=HexCoord(0, +1),
)

neighbours(hc::HexCoord) = (hc + dir for dir in directions)

toggle!(coll, value) = (value in coll ? delete! : push!)(coll, value)

function load(filename::AbstractString)
    grid = Set{HexCoord}()
    for line in eachline(filename)
        pos = zero(HexCoord)
        for dir in eachmatch(r"[ns]?[ew]", line)
            pos += getfield(directions, Symbol(dir.match))
        end
        toggle!(grid, pos)
    end
    grid
end

function step(grid)
    neighbourcount = Dict{HexCoord,Int}()
    newgrid = empty(grid)
    for blacktile in grid
        count = 0
        for neighbour in neighbours(blacktile)
            if neighbour in grid
                count += 1
            else
                neighbourcount[neighbour] = get(neighbourcount, neighbour, 0) + 1
            end
        end
        if count == 1 || count == 2
            push!(newgrid, blacktile)
        end
    end
    for (coord, count) in neighbourcount
        if count == 2
            push!(newgrid, coord)
        end
    end
    newgrid
end

solve(filename::AbstractString, steps = 0) = solve(load(filename), steps)
function solve(grid, steps = 0)
    for _ in 1:steps
        grid = step(grid)
    end
    length(grid)
end

@assert solve("example.txt") == 10
@assert solve("example.txt", 100) == 2208
# solve("input.txt")
# solve("input.txt", 100)
