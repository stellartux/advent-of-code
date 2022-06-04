if basename(pwd()) == "aoc"
    cd("2018/6")
end

function loadfile(filename::AbstractString)
    [CartesianIndex(parse.(Int, split(line, ", "))...) for line in eachline(filename)]
end

points = loadfile("example.txt")

manhattandistance(this) = Base.Fix1(manhattandistance, this)

function manhattandistance(this, that)::Integer
    zip(Tuple(this), Tuple(that)) .|> collect .|> diff .|> only .|> abs |> sum
end

perimeter(points) = perimeter(extrema(points)...)
function perimeter(topleft, bottomright)
    (CartesianIndex(t) for t in Iterators.flatten((
        ((x, topleft[2]) for x in topleft[1]:bottomright[1]),
        ((bottomright[1], y) for y in topleft[2]+1:bottomright[2]),
        ((x, bottomright[2]) for x in bottomright[1]-1:-1:topleft[1]),
        ((topleft[1], y) for y in bottomright[2]-1:-1:topleft[1]+1)
    )))
end

"""

    closest(target, points)

Finds the closest point to the target by Manhattan distance, returning nothing if
the closest point is equidistant to another point.
"""
function closest(target, points)
    if isempty(points)
        return
    end
    pointlist = collect(points)
    distances = broadcast(manhattandistance(target), pointlist)
    mindist, minidx = findmin(distances)
    if minidx == findlast(==(mindist), distances)
        return pointlist[minidx]
    end
end

function viewwindow(points, topleft, bottomright; showclosest=true)
    chars = Dict(p => c for (p, c) in zip(points, 'A':'Z'))
    for y in topleft[2]:bottomright[2]
        for x in topleft[1]:bottomright[1]
            point = CartesianIndex(x, y)
            if point in points
                printstyled(chars[point], bold=showclosest)
            elseif showclosest
                print(lowercase(get(chars, closest(point, points), '.')))
            else
                print('.')
            end
        end
        print('\n')
    end
end

viewwindow(points; kwargs...) = viewwindow(points, extrema(points)...; kwargs...)

areasizes(points) = Dict(point => areasize(point, points) for point in points)

areasize(points) = Base.Fix2(areasize, points)

function areasize(point, points, target=point, seen=Set(), domain=range(extrema(points)...))::Real
    total = 0
    if !(point in seen) && closest(point, points) == target
        push!(seen, point)
        if point in domain
            total += 1
        else
            return Inf
        end
        for y in -1:1, x in -1:1
            coord = point + CartesianIndex(x, y)
            total += areasize(coord, points, target, seen, domain)
        end
    end
    total
end

part1(filename::AbstractString) = part1(loadfile(filename))
part1(points) = maximum(Iterators.filter(!=(Inf), values(areasizes(points))))
