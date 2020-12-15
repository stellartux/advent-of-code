# https://i.stack.imgur.com/egDbs.png
"""
Does a math thing?
"""
function extendedeuclidean(a::Int64, b::Int64)::Tuple{Int64,String}
    d0 = (a, 1, 0)
    d1 = (b, 0, 1)

    while d1[1] != 0
        d0, d1 = d1, d0 .- (d0[1] ÷ d1[1]) .* d1
    end

    (d0, "$(d0[1]) = $(a) * $(d0[2]) + $(b) * $(d0[3])")
end
