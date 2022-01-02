lookandsay(input::Integer, iters = 1) = lookandsay!(reverse(digits(input)), iters)
function lookandsay!(input::AbstractVector{Int}, iters = 1)
    result = input
    input = Int[]
    for _ in 1:iters
        input, result = result, input
        while !isempty(input)
            next = popfirst!(input)
            count = 1
            while !isempty(input) && first(input) == next
                popfirst!(input)
                count += 1
            end
            push!(result, reverse(digits(count))..., next)
        end
    end
    join(result)
end

@assert lookandsay(1) == "11"
@assert lookandsay(11) == "21"
@assert lookandsay(1, 2) == "21"
@assert lookandsay(1211) == "111221"
@assert lookandsay(111221) == "312211"
@assert lookandsay(1, 5) == "312211"
@time length(lookandsay(3113322113, 40))
#   0.213305 seconds (3.49 M allocations: 162.478 MiB, 8.26% gc time)
# 329356
@time length(lookandsay(3113322113, 50))
#   3.463646 seconds (49.40 M allocations: 2.089 GiB, 14.32% gc time)
# 4666278
