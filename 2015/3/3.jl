part1(filename::AbstractString) = length(visithouses(readchomp(filename)))

function visithouses(directions::AbstractString)
    x = 0
    y = 0
    visited = Set(((0, 0),))
    for dir in directions
        if dir == '<'
            x -= 1
        elseif dir == '>'
            x += 1
        elseif dir == '^'
            y -= 1
        elseif dir == 'v'
            y += 1
        end
        push!(visited, (x, y))
    end
    visited
end

function part2(filename::AbstractString)
    directions = readchomp(filename)
    human = directions[1:2:end]
    robot = directions[2:2:end]
    length(union(visithouses(human), visithouses(robot)))
end
