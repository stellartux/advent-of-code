function loadleds(filename::AbstractString)
    (split.(split(line, " | ")) for line in eachline(filename))
end

function part1(data)
    sum(count(length(d) in (2, 3, 4, 7) for (_, ds) in data for d in ds))
end

using Test
if basename(pwd()) == "aoc"
    cd("2021\\8")
end
@test part1(loadleds("example.txt")) == 26
println("The answer to part 1 is: ", part1(loadleds("input.txt")))

part2(filename::AbstractString) = sum(decodeline(line) for line in eachline(filename))

function decodeline(line::AbstractString)
    signals, nums = split.(split(line, " | "))

    # Canonical configuration of signals is
    #    AAAA
    #   B    C
    #   B    C
    #    DDDD
    #   E    F
    #   E    F
    #    GGGG

    options = Dict(c => Set('a':'g') for c in 'A':'G')

    canon = Dict{Union{Set{Char},Int},Union{Set{Char},Int}}((
        Set("ABCEFG") => 0,
        Set("CF") => 1,
        Set("ACDEG") => 2,
        Set("ACDFG") => 3,
        Set("BCDF") => 4,
        Set("ABDFG") => 5,
        Set("ABDEFG") => 6,
        Set("ACF") => 7,
        Set("ABCDEFG") => 8,
        Set("ABCDFG") => 9
    ))
    push!(canon, (v => k for (k, v) in pairs(canon))...)

    deduce(canonchar::Char, chars) = intersect!(options[canonchar], chars)
    function deduce(canonchars, chars)
        foreach((cc) -> deduce(cc, chars), canonchars)
        options
    end

    function cleardeduced(options)
        for (cc, chars) in pairs(options)
            if length(chars) == 1
                for c in setdiff('A':'G', cc)
                    delete!(options[c], only(chars))
                end
            end
        end
        options
    end

    for signal in signals
        if length(signal) == 2 # v == 1
            deduce(canon[1], signal)
        elseif length(signal) == 3 # v == 7
            deduce(canon[7], signal)
        elseif length(signal) == 4 # v == 4
            deduce(canon[4], signal)
        elseif length(signal) == 5 # v in [2, 3, 5]
            deduce("ADG", signal)
        elseif length(signal) == 6 # v in [6, 9, 0]
            deduce("ABFG", signal)
        end # v == 8 tells us nothing
    end

    while any(!isone ∘ length, values(options))
        cleardeduced(options)
    end

    decoder = Dict(only(v) => k for (k, v) in pairs(options))

    mapfoldl(
        (n) -> canon[Set(replace(n, decoder...))],
        (a, b) -> 10a + b,
        nums,
        init=0
    )
end
