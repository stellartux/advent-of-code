if basename(pwd()) != "25"
    cd("2022/25")
end

struct SNAFU
    x::Int
end

Base.:+(s::SNAFU, o::SNAFU) = SNAFU(s.x + o.x)

function Base.parse(::Type{SNAFU}, s::AbstractString)
    SNAFU(sum((findfirst(d, "=-012") - 3) * 5^(i - 1) for (i, d) in pairs(reverse(s))))
end

function Base.digits(snafu::SNAFU)
    ds = digits(snafu.x; base=5)
    for i in eachindex(ds)
        if ds[i] > 2
            ds[i] -= 5
            if i == lastindex(ds)
                push!(ds, 1)
            else
                ds[i+1] += 1
            end
        end
    end
    ds
end

function Base.print(io::IO, snafu::SNAFU)
    print.(io, (d == -2 ? '=' : d == -1 ? '-' : d) for d in reverse(digits(snafu)))
end
Base.show(io::IO, snafu::SNAFU) = print(io, "SNAFU(\"", snafu, "\")")

@assert string(SNAFU(1)) == "1"
@assert string(SNAFU(2)) == "2"
@assert string(SNAFU(3)) == "1="
@assert string(SNAFU(4)) == "1-"
@assert string(SNAFU(5)) == "10"
@assert string(SNAFU(6)) == "11"
@assert string(SNAFU(7)) == "12"
@assert string(SNAFU(8)) == "2="
@assert string(SNAFU(9)) == "2-"
@assert string(SNAFU(10)) == "20"
@assert string(SNAFU(15)) == "1=0"
@assert string(SNAFU(20)) == "1-0"
@assert string(SNAFU(2022)) == "1=11-2"
@assert string(SNAFU(12345)) == "1-0---0"
@assert string(SNAFU(314159265)) == "1121-1110-1=0"

partone(filename::AbstractString) = sum(parse.(SNAFU, eachline(filename)))

# @time partone("input.txt")
#   0.000072 seconds (401 allocations: 13.414 KiB)
# SNAFU("2-02===-21---2002==0")
