using MD5: md5

function day17(seed::AbstractString)
    stack = ["" => CartesianIndex(1, 1)]
    shortest = nothing
    longest = 0
    while !isempty(stack)
        value, position = popfirst!(stack)
        if position == CartesianIndex(4, 4)
            if isnothing(shortest)
                shortest = value
            end
            longest = length(value)
        else
            hash = md5(seed * value)[1:2] = md5(seed * value)[1:2]
            if hash[1] >> 4 > 10 && position[2] > 1
                push!(stack, value * 'U' => position - CartesianIndex(0, 1))
            end
            if hash[1] & 15 > 10 && position[2] < 4
                push!(stack, value * 'D' => position + CartesianIndex(0, 1))
            end
            if hash[2] >> 4 > 10 && position[1] > 1
                push!(stack, value * 'L' => position - CartesianIndex(1, 0))
            end
            if hash[2] & 15 > 10 && position[1] < 4
                push!(stack, value * 'R' => position + CartesianIndex(1, 0))
            end
            if hash[1] >> 4 > 10 && position[2] > 1
                push!(stack, value * 'U' => position - CartesianIndex(0, 1))
            end
            if hash[1] & 15 > 10 && position[2] < 4
                push!(stack, value * 'D' => position + CartesianIndex(0, 1))
            end
            if hash[2] >> 4 > 10 && position[1] > 1
                push!(stack, value * 'L' => position - CartesianIndex(1, 0))
            end
            if hash[2] & 15 > 10 && position[1] < 4
                push!(stack, value * 'R' => position + CartesianIndex(1, 0))
            end
        end
    end
    shortest, longest
end
