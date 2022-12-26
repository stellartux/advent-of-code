if basename(pwd()) != "25"
    cd("2022/25")
end

struct SNAFU
    x::Int
end

function snafudigit(c::AbstractChar)
    if c == '='
        -2
    elseif c == '-'
        -1
    else
        c - '0'
    end
end

function Base.parse(::Type{SNAFU}, s::AbstractString)
    sum(snafudigit(d) * (5 ^ i) for (i, d) in zip(Iterators.countfrom(0), reverse(s)))
end

function Base.digits(snafu::SNAFU)
    ds = digits(snafu.x; base = 5)
    for (i, d) in enumerate(ds)
        if d > 2
            if i == length(ds)
                push!(ds, 0)
            end
            ds[i] = d - 5
            ds[i + 1] += 1
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

function partone(filename::AbstractString)
    SNAFU(sum(parse(SNAFU, s) for s in eachline(filename)))
end
