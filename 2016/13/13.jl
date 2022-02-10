isspace(x, y, z = 1364) = iseven(count_ones(x^2 + 3x + 2x * y + y + y^2 + z))

neighbours(c::CartesianIndex) = (c + n for n in (
    CartesianIndex(0, 1),
    CartesianIndex(0, -1),
    CartesianIndex(1, 0),
    CartesianIndex(-1, 0)
))

function visitavailable(magic = 1364)
    queue = [CartesianIndex(1, 1) => 0]
    visited = Dict{CartesianIndex,Int}()
    while !isempty(queue)
        coord, visits = popfirst!(queue)
        if coord[1] >= 0 &&
           coord[2] >= 0 &&
           isspace(coord[1], coord[2], magic) &&
           (!haskey(visited, coord) || visited[coord] > visits)
            visited[coord] = visits
            push!(queue, (n => visits + 1 for n in neighbours(coord))...)
        end
    end
    visited
end
part1(x, y, magic = 1364) = visitavailable(magic)[CartesianIndex(x, y)]

@assert part1(7, 4, 10) == 11
part1(31, 39)

part2() = count(<=(50), values(visitavailable(1364)))
part2()

function prettyprint(magic::Integer = 1364)
    visited = visitavailable(magic)
    maxi = maximum(keys(visited))
    for j in 0:maxi[2]
        for i in 0:maxi[1]
            coord = CartesianIndex(i, j)
            if coord == CartesianIndex(1, 1)
                printstyled('X', color = :blue)
            elseif haskey(visited, coord)
                printstyled('O', color = visited[coord] <= 50 ? :green : :light_red)
            elseif isspace(i, j, magic)
                printstyled('.', color = :light_black)
            else
                print('#')
            end
        end
        println()
    end
end
# prettyprint()
# prettyprint(10)
