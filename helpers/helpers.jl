function eachcons(xs::AbstractVector, n)
    (view(xs, i:i+n-oneunit(n)) for i ∈ firstindex(xs):lastindex(xs)-n+oneunit(n))
end
