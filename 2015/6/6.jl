function part1(filename::AbstractString)
    lights = zeros(Bool, 1000, 1000)
    for line in eachline(filename)
        instr, x₁, y₁, x₂, y₂ = match(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", line)
        rx = parse(Int, x₁):parse(Int, x₂)
        ry = parse(Int, y₁):parse(Int, y₂)
        if instr == "turn on"
            lights[ry, rx] .= true
        elseif instr == "turn off"
            lights[ry, rx] .= false
        elseif instr == "toggle"
            lights[ry, rx] .⊻= true
        else
            throw(ArgumentError("Unrecognized instruction: '$(instr)'"))
        end
    end
    count(lights)
end

function part2(filename::AbstractString)
    lights = zeros(Int, 1000, 1000)
    for line in eachline(filename)
        instr, x₁, y₁, x₂, y₂ = match(r"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)", line)
        rx = parse(Int, x₁):parse(Int, x₂)
        ry = parse(Int, y₁):parse(Int, y₂)
        if instr == "turn on"
            lights[ry, rx] .+= 1
        elseif instr == "turn off"
            lights[ry, rx] .= max.(lights[ry, rx] .- 1, 0)
        elseif instr == "toggle"
            lights[ry, rx] .+= 2
        else
            throw(ArgumentError("Unrecognized instruction: '$(instr)'"))
        end
    end
    sum(lights)
end
