if basename(pwd()) != "9"
    cd("2021/9")
end

function loadheightmap(filename::AbstractString)
    len = length(readline(filename))
    transpose(reshape([parse(Int, d) for line in eachline(filename) for d in line], len, :))
end

function part1(heights::AbstractMatrix{Int})
    sum(
        heights[i] + 1
        for i in CartesianIndices(heights)
        if all(>(heights[i]), neighbours(heights, i))
    )
end

function neighbourcoords(A, coord)
    (
        coord + offset
        for offset in (
            CartesianIndex(0, 1), CartesianIndex(0, -1),
            CartesianIndex(1, 0), CartesianIndex(-1, 0)
        )
        if coord + offset in CartesianIndices(A)
    )
end

neighbours(A, coord) = (A[c] for c in neighbourcoords(A, coord))

function part2(heights::AbstractMatrix{Int})
    prod(sort!(vec([floodnines(heights, i) for i in CartesianIndices(heights)]), rev=true)[1:3])
end

"""
Flood fills a basin with 9s and returns the number of locations flooded.
"""
function floodnines(heights::AbstractMatrix{Int}, i::CartesianIndex)
    if heights[i] != 9
        heights[i] = 9
        1 + sum(floodnines(heights, n) for n in neighbourcoords(heights, i))
    else
        0
    end
end
