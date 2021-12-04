function loadmatrix(filename::AbstractString)
    transpose(reshape(parse.(Bool, Iterators.flatten(eachline(filename))), length(readline(filename)), :))
end

frombits(bs) = foldl((a, b) -> 2a + b, bs)

function mostcommonbits(A::AbstractMatrix)
    mid = size(A, 1) / 2
    [mid <= s for s in sum(A, dims=1)]
end

function part1(filename::AbstractString)
    bits = mostcommonbits(loadmatrix(filename))
    γ = frombits(bits)
    ϵ = frombits(.!bits)
    γ * ϵ
end

A = loadmatrix("example.txt")
# @time part1("input.txt")

function part2(filename::AbstractString)
    A = loadmatrix(filename)
    O₂ = airfilter(A)
    CO₂ = airfilter(A, !)
    O₂ * CO₂
end

function airfilter(A, fn = identity)
    for i in 1:size(A, 2)
        bits = fn.(mostcommonbits(A))
        A = transpose(hcat((row for row in eachrow(A) if row[i] == bits[i])...))
        if size(A, 1) == 1
            break
        end
    end
    frombits(vec(A))
end
