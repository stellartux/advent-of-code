function loadgrid(filename::AbstractString)
    len = length(readline(filename))
    A = falses(len, len)
    for (y, line) in Iterators.enumerate(eachline(filename))
        for (x, c) in Iterators.enumerate(line)
            if c == '#'
                A[y, x] = true
            end
        end
    end
    A
end

function prettyprint(A)
    for row in eachrow(A)
        println((b ? '#' : '.' for b in row)...)
    end
end

function eachneighbour(A, index)
    (
        A[i+index]
        for i in CartesianIndices(((-1:1), (-1:1)))
        if !iszero(i) && checkbounds(Bool, A, i + index)
    )
end

function step(A, len = 1)
    for _ in 1:len
        A = [
            begin
                ns = sum(eachneighbour(A, i))
                ns == 3 || ns == 2 && A[i]
            end
            for i in CartesianIndices(A)
        ]
    end
    A
end

part1() = count(step(loadgrid("input.txt"), 100))

function step2(A, len = 1)
    h, w = size(A)
    A[1, 1] = true
    A[1, w] = true
    A[h, 1] = true
    A[h, w] = true
    for _ in 1:len
        A = [
            begin
                ns = sum(eachneighbour(A, i))
                ns == 3 || ns == 2 && A[i]
            end
            for i in CartesianIndices(A)
        ]
        A[1, 1] = true
        A[1, w] = true
        A[h, 1] = true
        A[h, w] = true
    end
    A
end

part2() = count(step2(loadgrid("input.txt"), 100))
