if basename(pwd()) == "aoc"
    cd("2016/8")
end

function part1(filename::AbstractString, screensize::Tuple{Int,Int})
    screen = falses(screensize)
    for instr in eachline(filename)
        A, B = parse.(Int, match(r"(\d+)(?:x| by )(\d+)", instr))
        if startswith(instr, "rect")
            screen[1:B, 1:A] .= true
        elseif startswith(instr, "rotate row")
            screen[A+1, :] = circshift(screen[A+1, :], B)
        else
            screen[:, A+1] = circshift(screen[:, A+1], B)
        end
    end
    screen
end

function prettyprint(A)
    for row in eachrow(A)
        println((b ? '#' : '.' for b in row)...)
    end
    println()
end

# part1("example.txt", (3, 7))
result = part1("input.txt", (6, 50))
count(result)
prettyprint(result)
