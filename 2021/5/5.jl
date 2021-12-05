using SparseArrays
using .Base.Iterators

function part1(filename::AbstractString)
    lines = [parse.(Int, m.match for m in eachmatch(r"\d+", line)) for line in readlines(filename)]
    filter!(line -> line[1] == line[3] || line[2] == line[4], lines)
    len = maximum(maximum.(lines)) + 1
    A = spzeros(Int, (len, len))
    for line in lines
        x₁, y₁, x₂, y₂ = line .+ 1
        if x₂ < x₁ || y₂ < y₁
            x₁, y₁, x₂, y₂ = x₂, y₂, x₁, y₁
        end
        A[y₁:y₂, x₁:x₂] .+= 1
    end
    count(>(1), A)
end

function part2(filename::AbstractString)
    lines = (
        (k₁, k₂, k₃, k₄)
        for (k₁, k₂, k₃, k₄) in partition(parse.(Int, m.match for m in eachmatch(r"\d+", readchomp(filename))), 4)
        if k₁ == k₃ || k₂ == k₄ || abs(k₁ - k₃) == abs(k₂ - k₄)
    )
    len = maximum(maximum.(lines)) + 1
    A = spzeros(Int, (len, len))
    linearrange(k₁, k₂) = k₁ == k₂ ? repeated(k₁) : k₁:sign(k₂ - k₁):k₂
    for line in lines
        x₁, y₁, x₂, y₂ = line .+ 1
        for (x, y) in zip(linearrange(x₁, x₂), linearrange(y₁, y₂))
            A[y, x] += 1
        end
    end
    count(>(1), A)
end
