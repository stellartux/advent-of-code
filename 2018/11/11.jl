# #!/usr/bin/env julia
# # usage: julia 11.jl [SERIAL=18]

module AOC2018Day11

struct FuelCells
    cells::Matrix{Int}
end

FuelCells(serial::Int) =
    FuelCells(cumsum(cumsum([
                begin
                    id = c[1] + 10
                    rem(div((id * c[2] + serial) * id, 100), 10) - 5
                end
                for c in CartesianIndices((1:300, 1:300))
            ], dims=1), dims=2))

getsquare(fc::FuelCells, x::Int, y::Int, size::Int) =
    (fc.cells[x+size-1, y+size-1] -
     get(fc.cells, (x - 1, y + size - 1), 0) -
     get(fc.cells, (x + size - 1, y - 1), 0) +
     get(fc.cells, (x - 1, y - 1), 0))

Base.getindex(fc::FuelCells, x::Int, y::Int) = getsquare(fc, x, y, 1)

function findmaxinrange(fc::FuelCells, range)
    maxpower = typemin(Int)
    result = (0, 0, 0)
    for len in range, x in 1:301-len, y in 1:301-len
        power = getsquare(fc, x, y, len)
        if power > maxpower
            maxpower = power
            result = (x, y, len)
        end
    end
    result
end

partone(s::Integer) = partone(FuelCells(s))
partone(fc::FuelCells)::NTuple{2,Int} = findmaxinrange(fc, 3)[1:2]
parttwo(s::Integer) = parttwo(FuelCells(s))
parttwo(fc::FuelCells)::NTuple{3,Int} = findmaxinrange(fc, 1:300)

if abspath(PROGRAM_FILE) == @__FILE__
    input = FuelCells(parse(Int, get(ARGS, 1, "18")))
    println(join(partone(input), ","))
    println(join(parttwo(input), ","))
end

end # module
