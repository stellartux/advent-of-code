fromdigits(n; base = 10) = foldl((a, b) -> base * a + b, n, init = 0)

alphabet = "abcdefghjkmnpqrstuvwxyz"
function nextpassword(password::AbstractString)
    n = toencoded(password) + 1
    while !isvalidpass(n)
        n += 1
    end
    fromencoded(n)
end

function toencoded(password::AbstractString)::Int
    fromdigits((findfirst(==(c), alphabet::String) - 1 for c ∈ password), base = 23)
end

function fromencoded(passnum::Integer)::String
    join(getindex.(alphabet::String, reverse(digits(passnum, base = 23)) .+ 1))
end

isvalidpass(password::AbstractString) = isvalidpass(toencoded(password))
isvalidpass(passnum::Integer) = isvalidpass(reverse(digits(passnum, base = 23)))
function isvalidpass(passdigits::AbstractVector{<:Integer})::Bool
    length(passdigits) == 8 &&
        any(a + 1 == b && b + 1 == c for (a, b, c) ∈ eachcons(passdigits, 3)) &&
        count(a == b for (a, b) ∈ Iterators.unique(eachcons(passdigits, 2))) > 1
end

function eachcons(xs::AbstractVector, n)
    (view(xs, i:i+n-oneunit(n)) for i ∈ firstindex(xs):lastindex(xs)-n+oneunit(n))
end

# @time nextpassword("cqjxjnds")
#   0.064901 seconds (306.64 k allocations: 37.737 MiB)
# "cqjxxyzz"

# @time nextpassword(ans)
#   0.275939 seconds (1.19 M allocations: 146.090 MiB, 10.42% gc time)
# "cqkaabcc"
