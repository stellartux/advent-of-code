if basename(pwd()) != "24"
    cd("2022/24")
end

@enum Blizzard::UInt8 Up = 0x08 Down = 0x04 Left = 0x02 Right = 0x01

function Base.convert(::Type{Blizzard}, c::AbstractChar)
    if c == '^'
        Up
    elseif c == '>'
        Right
    elseif c == '<'
        Left
    elseif c == 'v'
        Down
    else
        error("Can't convert $(typeof(c)) $(repr(c)) to a Blizzard.")
    end
end

function Base.convert(::Type{Char}, b::Blizzard)
    if b == Up
        '^'
    elseif b == Right
        '>'
    elseif b == Left
        '<'
    elseif b == Down
        'v'
    end
end

function Base.convert(::Type{CartesianIndex}, b::Blizzard)
    if b == Right
        CartesianIndex(0, 1)
    elseif b == Left
        CartesianIndex(0, -1)
    elseif b == Down
        CartesianIndex(1, 0)
    elseif b == Up
        CartesianIndex(-1, 0)
    end
end

Base.print(io::IO, b::Blizzard) = print(io, convert(Char, b))

mutable struct Blizzards <: AbstractSet{Blizzard}
    value::UInt8
    function Blizzards(i::Integer)
        if i in 0x0:0xf
            new(convert(UInt8, i))
        else
            error("Blizzard value out of range, expected value in 0:15, got $(i).")
        end
    end
end
Blizzards() = Blizzards(0x00)

Base.:|(b1::Blizzard, b2::Blizzard) = Blizzards(UInt8(b1) | UInt8(b2))
Base.:|(bs::Blizzards, b::Blizzard) = Blizzards(bs.value | UInt8(b))
Base.:(==)(b1::Blizzards, b2::Blizzards) = b1.value == b2.value

function Base.Char(b::Blizzards)
    p = length(b)
    if iszero(p)
        '.'
    elseif isone(p)
        if b.value == UInt8(Up)
            '^'
        elseif b.value == UInt8(Down)
            'v'
        elseif b.value == UInt8(Left)
            '<'
        elseif b.value == UInt8(Right)
            '>'
        end
    else
        p + '0'
    end
end

Base.empty!(b::Blizzards) = b.value = 0x00
Base.isempty(b::Blizzards) = iszero(b.value)

function Base.iterate(bs::Blizzards, f::UInt8=0x08)
    while !iszero(f)
        if !iszero(bs.value & f)
            return Blizzard(bs.value & f), f >> 1
        end
        f >>= 1
    end
end
Base.eltype(::Blizzards) = Blizzard
Base.length(b::Blizzards) = sum(((b.value & (1 << n)) >> n) for n in 0:3)
Base.print(io::IO, b::Blizzards) = print(io, Char(b))
function Base.push!(bs::Blizzards, b::Blizzard)
    bs.value |= UInt8(b)
    bs
end

struct Basin
    grid::Matrix{Blizzards}
end

function Basin(s::AbstractString)
    s = strip(s)
    Basin(first(s) == '#' ? split(s, '\n') : readlines(s))
end

function Basin(lines::AbstractVector{S}) where {S<:AbstractString}
    grid = [Blizzards() for _ in 3:length(lines), __ in 3:length(first(lines))]
    for (y, line) in enumerate(lines[2:end-1])
        for (x, c) in enumerate(line[2:end-1])
            if c != '.'
                push!(grid[y, x], convert(Blizzard, c))
            end
        end
    end
    Basin(grid)
end

"""

Returns an empty `Basin` the same size as the given `Basin`.
"""
Base.empty(b::Basin) = Basin(empty.(b.grid))
Base.empty!(b::Basin) = empty!.(b.grid)

function Base.getindex(b::Basin, i, j)
    sy, sx = size(b.grid)
    b.grid[(j+sy-1)%sy+1, (i+sx-1)%sx+1]
end

Base.getindex(b::Basin, c::CartesianIndex{2}) = b[c[2], c[1]]

Base.pairs(b::Basin) =
    (c => t for (c, ts) in pairs(b.grid) if !isempty(ts) for t in ts)
Base.size(b::Basin, i...) = size(b.grid, i...)

function Base.print(io::IO, b::Basin)
    println(io, "#." * '#'^size(b, 2))
    for row in eachrow(b.grid)
        print(io, '#')
        print.(io, Char.(row))
        println(io, '#')
    end
    println(io, '#'^size(b.grid, 2) * ".#")
end

function Base.show(io::IO, b::Basin)
    if get(io, :compact, false)
        print(io, "$(join(size(b), '×')) Basin")
    else
        println(io, "Basin(\"\"\"")
        print(io, b)
        print("\"\"\")")
    end
end

function Base.iterate(b::Basin)
    previous = collect(pairs(b))
    empty!(b)
    for (coord, blizzard) in previous
        push!(b[coord+convert(CartesianIndex, blizzard)], blizzard)
    end
    b, nothing
end
Base.eltype(::Basin) = Basin
Base.IteratorSize(::Type{Basin}) = Base.IsInfinite()

printframe(b, s, vs=[]) = printframe(stdout, b, s, vs)
function printframe(io::IO, b::Basin, s::AbstractString, visitors=[])
    println(io, "\e[2J\e[H", s)
    println(io, b)
    for visitor in visitors
        println(io, "\e[$(visitor[1] + 2);$(visitor[2] + 1)H\e[1mE\e[0m")
    end
    try
        readline()
    catch err
        if err isa InterruptException
            return false
        else
            rethrow(err)
        end
    end
    return true
end

function run(b::Basin)
    printframe(b, "Initial state:")
    for (i, s) in enumerate(b)
        if !printframe(s, "Minute $(i):")
            break
        end
    end
end

function neighbours(coord)
    (
        coord + CartesianIndex(-1, 0), coord + CartesianIndex(1, 0),
        coord + CartesianIndex(0, -1), coord + CartesianIndex(0, 1), coord
    )
end

partone(filename::AbstractString; kwargs...) = partone(Basin(filename); kwargs...)
function partone(b::Basin; verbose=false)
    visitors = []
    nextvisitors = Set{CartesianIndex{2}}((CartesianIndex(0, 1),))
    minute = 0
    target = CartesianIndex(size(b)) + convert(CartesianIndex, Down)
    if verbose
        println("\e[?1049h\e[?25l\e[s")
        printframe(b, "Initial state:", visitors)
    end
    while true
        iterate(b)
        minute += 1
        visitors = collect(nextvisitors)
        empty!(nextvisitors)
        push!(nextvisitors, CartesianIndex(0, 1))
        for visitor in visitors
            for n in neighbours(visitor)
                if n == target
                    if verbose
                        println("\e[?1049l\e[?25h\e[u")
                    end
                    return minute
                end
                if n in keys(b.grid) && isempty(b.grid[n])
                    push!(nextvisitors, n)
                end
            end
        end
        if verbose && !printframe(b, "Minute $(minute)", visitors)
            println("\e[?1049l\e[?25h\e[u")
            return -1
        end
    end
end
