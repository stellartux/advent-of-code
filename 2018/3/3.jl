if basename(pwd()) == "aoc"
    cd("2018/3")
end

function loadfile(filename::AbstractString)
    lines = [
        begin
            x, y, w, h = parse.(Int, m)
            [x+1:x+w, y+1:y+h]
        end
        for m in eachmatch(r"#\d+ @ (\d+),(\d+): (\d+)x(\d+)", readchomp(filename))
    ]
    fabric = zeros(Int, (maximum.((last.(first.(lines)), last.(last.(lines)))) .+ 1)...)
    for (w, h) in lines
        fabric[h, w] .+= 1
    end
    lines, fabric
end

function part1(filename::AbstractString)
    _, fabric = loadfile(filename)
    count(>(1), fabric)
end

@assert part1("example.txt") == 4
part1("input.txt")

function part2(filename::AbstractString)
    lines, fabric = loadfile(filename)
    findfirst(lines) do (w, h)
        all(isone, fabric[h, w])
    end
end

@assert part2("example.txt") == 3
part2("input.txt")
