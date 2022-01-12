if basename(pwd()) == "aoc"
    cd("2017/6")
end

loadblocks(filename::AbstractString) = parse.(UInt8, split(readline(filename)))

part1(filename::AbstractString) = part1(loadblocks(filename))

function part1(blocks)
    previous = copy(blocks)
    seen = Set{typeof(blocks)}()
    steps = 0
    while !(previous in seen)
        @debug join(previous, ' ')
        steps += 1
        push!(seen, previous)
        maxval, maxind = findmax(blocks)
        blocks[maxind] = zero(blocks[maxind])
        rest = maxind+1:min(maxind + maxval, lastindex(blocks))
        blocks[rest] .+= oneunit(maxval)
        maxval -= length(rest)
        if !iszero(maxval)
            d, r = divrem(maxval, oftype(maxval, length(blocks)))
            blocks .+= d
            blocks[begin:begin+r-1] .+= oneunit(maxval)
        end
        previous = copy(blocks)
    end
    @debug join(previous, ' ')
    steps, blocks
end

@assert part1("example.txt")[1] == 5

part2(blocks) = part1(part1(blocks)[2])[1]

@assert part2("example.txt")[1] == 4
part2("input.txt")[1]
