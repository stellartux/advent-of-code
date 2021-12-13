function loaddata(filename::AbstractString)
    numstr, instrstr = split(readchomp(filename), r"\r?\n\r?\n")
    nums = [CartesianIndex(reverse(parse.(Int, t) .+ 1)...) for t in split.(split(numstr, r"\r?\n"), ',')]
    maxi = maximum(nums)
    matrix = zeros(Bool, (maxi[1] + iseven(maxi[1]), maxi[2]))
    matrix[nums] .= true
    matrix, split(instrstr, r"\r?\n")
end

if basename(pwd()) == "aoc"
    cd("2021/13")
end

atlas, instrs = loaddata("example.txt")

function foldalong(atlas, instr::AbstractString)
    d, v = match(r"fold along ([xy])=(\d+)", instr)
    n = parse(Int, v) + 1
    println("Folding $(size(atlas)) at $d=$n")
    if d == "y"
        foldhorizontal(atlas, n)
    else
        foldvertical(atlas, n)
    end
end

function foldhorizontal(atlas::AbstractMatrix, y::Integer)
    top = atlas[begin:y-1, :]
    bottom = atlas[y+1:end, :]
    top .|= reverse(bottom, dims=1)
end

function foldvertical(atlas::AbstractMatrix, x::Integer)
    left = atlas[:, begin:x-1]
    right = atlas[:, x+1:end]
    left .|= reverse(right, dims=2)
end

part1(filename::AbstractString) = part1(loaddata(filename)...)
function part1(atlas::AbstractMatrix, instrs::Vector)
    count(foldalong(atlas, first(instrs)))
end

part2(filename::AbstractString) = part2(loaddata(filename)...)
function part2(atlas::AbstractMatrix, instrs)
    for instr in instrs
        atlas = foldalong(atlas, instr)
    end
    atlas
end

"Converts a bitmatrix to plain PBM format."
function pbm(bitmatrix)
    join(
        (
            "P1",
            join(reverse(size(bitmatrix)), ' '),
            (
                join(map(b -> b ? '1' : '0', row), ' ')
                for row in Iterators.partition(transpose(bitmatrix), 34)
            )...
        ),
        '\n'
    ) * '\n'
end

pbm(filename::AbstractString, bitmatrix) = open(f -> write(f, pbm(bitmatrix)), filename, "w")
