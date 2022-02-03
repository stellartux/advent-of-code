if basename(pwd()) == "aoc"
    cd("2017/24")
end
include("../../helpers/inherit.jl")

struct Magnet
    charges::Vector{Int}
end
@delegate_base Magnet charges first, firstindex, getindex, last, lastindex, length, setindex!

Magnet(str::AbstractString) = Magnet(parse.(Int, split(str, '/')))
loadmagnets(filename::AbstractString) = Magnet.(eachline(filename))

isattracted(left::Int, right::Magnet) = left == first(right) || left == last(right)
isattracted(left::Magnet, right::Magnet) = isattracted(last(left), right)
isattracted(left) = Base.Fix1(isattracted, left)

strength(magnet::Magnet) = 2 * sum(magnet.charges) - first(magnet) - last(magnet)
strength(magnets) = sum(strength.(magnets))

function maxchain(magnets, chain = Magnet(0))
    indices = findall(isattracted(chain), magnets)
    if !isempty(indices)
        append!(chain,
            first(sort!(
                [
                    begin

                        ms = copy(magnets)
                        c = copy(chain)
                        splice!(ms, i)
                        push!(c, magnets[i])
                        maxchain(ms, c)
                    end
                    for i in indices
                ],
                by = strength
            ))
        )
    end
    chain
end

part1(filename::AbstractString) = part1(loadmagnets(filename))
part1(magnets) = maximum(strength, maxchain(magnets))

# @assert part1("example.txt") == 31
