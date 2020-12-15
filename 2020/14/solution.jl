instructions = string.(split(strip(read("input.txt", String)), "\n"))

struct Bitmask
    andmask::UInt64
    ormask::UInt64
end
Bitmask(raw::String) = Bitmask([parse(Int, replace(raw[8:end], "X" => l), base=2) for l in "10"]...)
mask(number::UInt64, bitmask::Bitmask) = (number & bitmask.andmask) | bitmask.ormask

struct Mutator
    address::UInt64
    value::UInt64
end
Mutator(raw::String) = Mutator(parse.(Int, match(r"\[(\d+)\] = (\d+)", raw).captures)...)

function run(instructions::Array{String})::UInt64
    program = [(startswith(instruction, "mask") ? Bitmask : Mutator)(instruction) for instruction in instructions]
    memory = zeros(UInt64, maximum(m -> m.value, filter(x -> isa(x, Mutator), program)))
    bitmask = nothing

    for instruction in program
        if instruction isa Bitmask
            bitmask = instruction
        elseif instruction isa Mutator
            memory[instruction.address] = mask(instruction.value, bitmask)
        end
    end
    
    sum(memory)
end

run(program)

function maskedaddresses(address::String, mask::String)
    result = []
    for maskno in 0:(1 << count("X", mask))
        b = string(maskno, base=2)
        push!(result, parse(Int64, address) | b)
    end
    result
end

using SparseArrays

function otherrun(instructions::Array{String})::UInt64
    memory = spzeros(UInt64, 2^36)
    bitmask = nothing

    for instruction in instructions
        if startswith("mask", instruction)
            bitmask = instruction[8:end]
        else
            address, value = parse.(Int, match(r"\[(\d+)\] = (\d+)", raw).captures)
            for maskedaddress in maskedaddresses(address, bitmask)
                memory[maskedaddress] = value
            end
        end
    end

    sum(memory)
end
