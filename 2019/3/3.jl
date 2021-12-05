mutable struct WirePanel
    points::Dict{Tuple{Int,Int},Char}
    distances::Dict{Tuple{Int,Int},Int}
    left::Int
    top::Int
    right::Int
    bottom::Int
end
WirePanel() = WirePanel(Dict{Tuple{Int,Int},Char}(), Dict{Tuple{Int,Int},Int}(), 0, 0, 0, 0)

function WirePanel(directions::AbstractString)
    wirepanel = WirePanel()
    pos = (0, 0)
    dist = 0
    for m in split(directions, ',')
        n = parse(Int, m[2:end])
        islr = occursin(m[1], "LR")
        r = if islr
            m[1] == 'L' ? (pos[1]-1:-1:pos[1]-n) : (pos[1]+1:pos[1]+n)
        else
            m[1] == 'U' ? (pos[2]-1:-1:pos[2]-n) : (pos[2]+1:pos[2]+n)
        end
        for k in r
            pos = islr ? (k, pos[2]) : (pos[1], k)
            wirepanel[pos] = if wirepanel[pos] != '·' || k == last(r)
                '╋'
            elseif islr
                '━'
            else
                '┃'
            end
            dist += 1
            wirepanel.distances[pos] = dist
        end
    end
    wirepanel
end

function Base.empty!(w::WirePanel)
    empty!(w.points)
    w.left = w.right = w.top = w.bottom = 0
    w
end

function Base.getindex(wirepanel::WirePanel, t::Tuple{Int,Int})
    if iszero(t[1]) && iszero(t[2])
        'o'
    else
        get(wirepanel.points, t, '·')
    end
end

function Base.setindex!(panel::WirePanel, c::AbstractChar, t::Tuple{Int,Int})
    panel.points[t] = c
    x, y = t
    panel.left = min(x, panel.left)
    panel.right = max(x, panel.right)
    panel.top = min(y, panel.top)
    panel.bottom = max(y, panel.bottom)
end

function Base.show(io::IO, w::WirePanel)
    println(io, abs(w.right - w.left) + 1, '×', abs(w.top - w.bottom) + 1, " WirePanel")
    spareline = "  " * ("·" ^ (w.right - w.left + 3))
    println(io, spareline)
    for y in w.top:w.bottom
        print(io, "  ·")
        for x in w.left:w.right
            print(io, w[(x, y)])
        end
        println(io, '·')
    end
    print(io, spareline)
end

loadpanels(filename::AbstractString) = WirePanel.(eachline(filename))

manhattandistance(t::Tuple{Int,Int}) = abs(t[1]) + abs(t[2])

"Returns the Manhattan distance to the closest short to the origin."
function closestshort(panels::Vararg{WirePanel})
    minimum(manhattandistance(k) for k in shorts(panels))
end

function shorts(panels::Vector{WirePanel})
    intersect((keys(panel.points) for panel in panels)...)
end

function part1(filename::AbstractString)
    closestshort(loadpanels(filename)...)
end

Base.in(t::Tuple, panel::WirePanel) = t in keys(panel.points)

function paneldistance(panels, point::Tuple{Int,Int})
    sum(panel.distances[point] for panel in panels)
end

function part2(filename::AbstractString)
    panels = loadpanels(filename)
    minimum(paneldistance(panels, short) for short in shorts(panels))
end
