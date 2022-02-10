if basename(pwd()) == "aoc"
    cd("2016/18")
end

function countsafe(str::AbstractString, n::Integer)
    row = [c == '^' for c in str]
    next = falses(size(row))
    total = 0
    for _ in 1:n
        total += count(!, row)
        next[1] = row[2]
        next[2:end-1] = row[1:end-2] .⊻ row[3:end]
        next[end] = row[end-1]
        row, next = next, row
    end
    total
end

@assert countsafe(".^^.^.^^^^", 10) == 38
@time countsafe(readline("input.txt"), 40)
@time countsafe(readline("input.txt"), 400000)
