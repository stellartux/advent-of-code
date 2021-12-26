if basename(pwd()) == "aoc"
    cd("2021/25")
end

function loadfile(filename::AbstractString)
    permutedims(reshape(
        [c for c in readchomp(filename) if !isspace(c)],
        length(readline(filename)),
        :
    ))
end

step(src) = first(step!(similar(src), src))

function step!(dest::AbstractMatrix{Char}, src::AbstractMatrix{Char})
    copyto!(dest, src)
    h, w = size(dest)
    for y in 1:h
        for x in 1:w-1
            if src[y, x] == '>' && src[y, x+1] == '.'
                dest[y, x+1] = '>'
                dest[y, x] = '.'
            end
        end
        if src[y, w] == '>' && src[y, 1] == '.'
            dest[y, 1] = '>'
            dest[y, w] = '.'
        end
    end
    for x in 1:w
        for y in 1:h-1
            if src[y, x] == 'v' && dest[y+1, x] == '.'
                dest[y+1, x] = 'v'
                dest[y, x] = '.'
            end
        end
        if src[h, x] == 'v' && src[1, x] != 'v' && dest[1, x] == '.'
            dest[1, x] = 'v'
            dest[h, x] = '.'
        end
    end
    dest, src
end

function prettyprint(grid, msg = "")
    println(msg)
    println.(join(row) for row in eachrow(grid))
    println()
    nothing
end

function part1(filename::AbstractString; verbose::Bool = false)
    previous = loadfile(filename)
    current = step(previous)
    total = 1
    if verbose
        prettyprint(previous, "Initial state:")
        prettyprint(current, "After 1 step:")
    end
    while previous != current
        current, previous = step!(previous, current)
        total += 1
        if verbose
            prettyprint(current, "After $(total) steps:")
        end
    end
    total
end

@assert part1("example.txt") == 58
println("Part 1: ", part1("input.txt"))
