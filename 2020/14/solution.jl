if basename(pwd()) != "14"
    cd("2020/14")
end
using SparseArrays

struct Mask{I<:Integer} # where I is at least 36 bits wide
    and::I
    or::I
end

function Mask(s::AbstractString)
    s = match(r"[X01]+", s).match
    and = parse(Int, replace(s, 'X' => '1'), base=2)
    or = parse(Int, replace(s, 'X' => '0'), base=2)
    Mask(and, or)
end

function Base.show(io::IO, m::Mask)
    print(io, typeof(m), "(\"",
        (isone(1 & m.and >> i) ? isone(1 & m.or >> i) ? '1' : 'X' : '0' for i = 35:-1:0)...,
        "\")")
end

mask(n::I, m::Mask{I}) where {I<:Integer} = (n & m.and) | m.or

function partone(filename::AbstractString)
    m = nothing
    memory = spzeros(Int, 2^36)
    for line in eachline(filename)
        if startswith(line, "mask = ")
            m = Mask(line)
        else
            addr, value = parse.(Int, match(r"mem\[(\d+)\] = (\d+)", line))
            memory[addr] = mask(value, m)
        end
    end
    sum(memory)
end

function fromdigits(ds; base=10)
    result = 0
    for d in ds
        result = base * result + d
    end
    result
end

function maskedaddresses(n::I, m::Mask{I}) where {I<:Integer}
    wild = m.and ⊻ m.or
    n |= m.or
    (
        begin
            ds = []
            xds = digits(x, base=2, pad=36)
            for (nd, wd) in zip(digits.((n, wild); base=2, pad=36)...)
                pushfirst!(ds, isone(wd) ? popfirst!(xds) : nd)
            end
            fromdigits(ds, base=2)
        end
        for x in 0:(2^count_ones(wild)-1)
    )
end

function parttwo(filename::AbstractString)
    m = nothing
    memory = spzeros(Int, 2^36)
    for line in eachline(filename)
        if startswith(line, "mask = ")
            m = Mask(line)
        else
            addr, value = parse.(Int, match(r"mem\[(\d+)\] = (\d+)", line))
            for a in maskedaddresses(addr, m)
                memory[a] = value
            end
        end
    end
    sum(memory)
end
